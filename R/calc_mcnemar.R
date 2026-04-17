# R/calc_mcnemar.R
# 対応のある二値（McNemar 検定・両側）の必要ペア数。
# 計算は base R（stats::qnorm）のみで自前実装する。
#
# 入力:
#   p_disc : 不一致ペアの割合 = p10 + p01
#   psi    : 不一致ペア中 A 群が B 群より優位な割合 = p10 / p_disc
#   alpha  : 有意水準（両側）
#   power  : 目標検出力
#   dropout: 脱落率（ペア単位、0 以上 1 未満）
#
# 公式（Connor 1987、Miettinen 1968 の正規近似・標準形）:
#   δ   = (2ψ - 1) * p_disc        不一致ペアの正味差
#   n   = (z_{α/2} + z_β*sqrt(1 - (2ψ-1)^2 * p_disc))^2 /
#         ((2ψ-1)^2 * p_disc)
#
# 出典:
#   - Connor RJ (1987) Sample size for testing differences in
#     proportions for the paired-sample design. Biometrics 43:207-211.
#   - Miettinen OS (1968) The matched pairs design in the case of
#     all-or-none responses. Biometrics 24:339-352.
#
# 備考:
#   ψ = 0.5（p10 = p01）では分母が 0 となり計算不能。
#   psi が 0.5 から離れるほど、また p_disc が大きいほど n は小さい。

calc_n_mcnemar <- function(p_disc, psi, alpha,
                           power = 0.80, dropout = 0) {
  stopifnot(is.numeric(p_disc), p_disc > 0, p_disc <= 1,
            is.numeric(psi),    psi >= 0,    psi <= 1,
            is.numeric(alpha),  alpha > 0,  alpha < 1,
            is.numeric(power),  power > 0,  power < 1)

  two_psi_m1 <- 2 * psi - 1
  stopifnot(abs(two_psi_m1) > 1e-8)   # ψ = 0.5 では計算不能

  z_half_alpha <- stats::qnorm(1 - alpha / 2)  # 両側
  z_beta       <- stats::qnorm(power)

  inner <- 1 - two_psi_m1^2 * p_disc
  stopifnot(inner >= 0)   # 極端なケースのみ問題

  n_raw <- (z_half_alpha + z_beta * sqrt(inner))^2 /
           (two_psi_m1^2 * p_disc)

  make_result(
    n_per_arm_evaluable = ceiling(n_raw),
    dropout = dropout,
    n_arms  = 1L,          # 対応ありはペア数＝総被験者数（1 群扱い）
    achieved_power = power,
    backend_pkg = "stats",
    backend_fun = "qnorm (Connor 1987 McNemar sample size)",
    formula_ref = "Connor 1987",
    extras = list(
      p_disc = p_disc, psi = psi,
      delta = two_psi_m1 * p_disc,
      z_half_alpha = z_half_alpha, z_beta = z_beta,
      n_raw = n_raw
    )
  )
}

# 与えられたペア数 n における検出力の逆算（感度分析グラフ用）。
calc_power_mcnemar <- function(p_disc, psi, alpha, n) {
  two_psi_m1 <- 2 * psi - 1
  if (abs(two_psi_m1) < 1e-8) return(NA_real_)
  inner <- 1 - two_psi_m1^2 * p_disc
  if (inner < 0) return(NA_real_)
  z_half_alpha <- stats::qnorm(1 - alpha / 2)
  # 公式を n について解いた z_β を求め、Φ を適用
  z_beta_hat <- (sqrt(n * two_psi_m1^2 * p_disc) - z_half_alpha) / sqrt(inner)
  stats::pnorm(z_beta_hat)
}
