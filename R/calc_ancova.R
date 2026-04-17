# R/calc_ancova.R
# 共変量調整（ANCOVA）ありの 2 標本 t 型比較の必要症例数。
# 計算は pwr::pwr.t.test に委譲する（分散低減のみ自前で適用）。
#
# 入力:
#   mean_A, mean_B : A 群・B 群の平均
#   sd_common      : 共通 SD（ベースライン調整前）
#   r              : アウトカムと共変量の相関係数（|r| < 1）
#   alpha, power   : 有意水準（両側）・目標検出力
#   dropout        : 脱落率
#
# 公式（Borm GF, et al. 2007）:
#   共変量調整後の有効 SD:
#     SD_adj = SD * sqrt(1 - r^2)
#   効果量:
#     d_adj  = |mean_A - mean_B| / SD_adj
#   これを pwr::pwr.t.test(type='two.sample', alternative='two.sided')
#   に渡し、必要症例数を得る。
#
# 出典:
#   - Borm GF, Fransen J, Lemmens WAJG (2007) A simple sample size
#     formula for analysis of covariance in randomized clinical
#     trials. J Clin Epidemiol 60:1234-1238.
#   - pwr::pwr.t.test (Champely 2020).
#
# 備考:
#   r = 0 なら通常の 2 標本 t 検定と同じ。
#   |r| が大きいほど（強い共変量を使うほど）必要症例数は減る。

calc_d_ancova <- function(mean_A, mean_B, sd_common, r) {
  stopifnot(is.numeric(sd_common), sd_common > 0,
            is.numeric(r), r > -1, r < 1)
  sd_adj <- sd_common * sqrt(1 - r^2)
  abs(mean_A - mean_B) / sd_adj
}

calc_power_ancova <- function(mean_A, mean_B, sd_common, r, alpha, n) {
  d_adj <- calc_d_ancova(mean_A, mean_B, sd_common, r)
  pwr::pwr.t.test(
    n = n, d = d_adj, sig.level = alpha,
    type = "two.sample", alternative = "two.sided"
  )$power
}

calc_n_ancova <- function(mean_A, mean_B, sd_common, r, alpha,
                          power = 0.80, dropout = 0) {
  d_adj <- calc_d_ancova(mean_A, mean_B, sd_common, r)
  stopifnot(d_adj > 1e-12)  # 平均に差がないと計算不能
  res <- pwr::pwr.t.test(
    d = d_adj, sig.level = alpha, power = power,
    type = "two.sample", alternative = "two.sided"
  )
  make_result(
    n_per_arm_evaluable = ceiling(res$n),
    dropout = dropout,
    n_arms  = 2L,
    achieved_power = power,
    backend_pkg = "pwr",
    backend_fun = "pwr.t.test(two.sample, two.sided) with SD×sqrt(1-r^2)",
    formula_ref = "Borm 2007",
    extras = list(
      d = d_adj, r = r,
      sd_adj = sd_common * sqrt(1 - r^2),
      n_raw = res$n
    )
  )
}
