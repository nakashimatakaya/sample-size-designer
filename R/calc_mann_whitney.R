# R/calc_mann_whitney.R
# Mann-Whitney U 検定（Wilcoxon ランク和、両側・2 標本）の必要症例数。
# 計算は pwr::pwr.t.test の結果を ARE（漸近相対効率）で補正する。
#
# 入力:
#   mean_A, mean_B : 各群の想定位置パラメータ（正規なら平均）
#   sd_common      : 共通 SD（正規スケール）
#   distribution   : "normal" / "lognormal" / "exponential"
#   alpha, power   : 有意水準（両側）・目標検出力
#   dropout        : 脱落率
#
# 公式（Lehmann 1975; Hollander & Wolfe 1999）:
#   ARE(Wilcoxon vs t) の理論値:
#     normal        : 3/π  ≈ 0.955
#     logistic      : π²/9 ≈ 1.097
#     exponential   : 3.00  （位置シフト・Wilcoxon が t より効率的）
#     double-exp    : 1.5
#   本関数は (normal, lognormal, exponential) に対応。
#   lognormal はヘビーテイルの近似として ARE = 1.5 を使用
#   （Hollander & Wolfe 1999 の近似推奨値）。
#
#   n_MW = ceiling(n_t / ARE)
#   ここで n_t は pwr::pwr.t.test で得られる t 検定必要症例数。
#
# 出典:
#   - Hollander M, Wolfe DA (1999) Nonparametric Statistical Methods,
#     2nd ed. Wiley.
#   - Lehmann EL (1975) Nonparametrics: Statistical Methods Based on
#     Ranks. Holden-Day.
#   - Noether GE (1987) Sample size determination for some common
#     nonparametric tests. JASA 82:645-647.
#
# 備考:
#   分布形状によって ARE は変化する。正規では MW はわずかに非効率（~4.5%
#   増の n）、裾の重い分布では MW が t より効率的で n は減る。

.are_table <- c(
  normal      = 3 / pi,         # 0.9549
  lognormal   = 1.5,            # 近似（中等度の歪度）
  exponential = 3.0             # Lehmann 1975 Table
)

are_for_distribution <- function(distribution) {
  distribution <- match.arg(distribution,
                            choices = names(.are_table))
  unname(.are_table[distribution])
}

calc_n_mann_whitney <- function(mean_A, mean_B, sd_common,
                                distribution = "normal",
                                alpha, power = 0.80, dropout = 0) {
  stopifnot(is.numeric(sd_common), sd_common > 0)
  d <- abs(mean_A - mean_B) / sd_common
  stopifnot(d > 1e-12)
  ARE <- are_for_distribution(distribution)

  res_t <- pwr::pwr.t.test(
    d = d, sig.level = alpha, power = power,
    type = "two.sample", alternative = "two.sided"
  )
  n_mw_raw <- res_t$n / ARE

  make_result(
    n_per_arm_evaluable = ceiling(n_mw_raw),
    dropout = dropout,
    n_arms  = 2L,
    achieved_power = power,
    backend_pkg = "pwr",
    backend_fun = paste0("pwr.t.test(two.sample, two.sided) / ARE(",
                         distribution, ")"),
    formula_ref = "Hollander & Wolfe 1999",
    extras = list(
      d = d, distribution = distribution, ARE = ARE,
      n_t_per_arm = ceiling(res_t$n),
      n_raw = n_mw_raw
    )
  )
}

calc_power_mann_whitney <- function(mean_A, mean_B, sd_common,
                                    distribution = "normal",
                                    alpha, n) {
  d <- abs(mean_A - mean_B) / sd_common
  ARE <- are_for_distribution(distribution)
  # 効率補正後の有効 n を t 検定に渡す: n_eff = n * ARE
  pwr::pwr.t.test(
    n = n * ARE, d = d, sig.level = alpha,
    type = "two.sample", alternative = "two.sided"
  )$power
}
