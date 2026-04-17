# R/calc_ttest_ni.R
# 非劣性・2 標本 t 検定（片側）。
#
# 仮説:
#   H0: μ_A - μ_B <= -M   （A は B より M 以上劣っている）
#   H1: μ_A - μ_B >  -M   （A は B より M 以上悪くない＝非劣性）
#   ここで M（マージン）は正の値（平均差の単位）。
#
# 実装:
#   pwr::pwr.t.test(type = "two.sample", alternative = "greater") に
#   シフトした効果量 d_ni = (Δ + M) / sd_pooled を渡す。
#   Δ = mean_A - mean_B（符号付き）。
#
# 提供する関数:
#   calc_d_ttest_ni(diff, sd_A, sd_B, margin)
#   calc_power_ttest_ni(diff, sd_A, sd_B, margin, alpha, n)
#   calc_n_ttest_ni(diff, sd_A, sd_B, margin, alpha,
#                   power = 0.80, dropout = 0)
#
# 出典:
#   - Chow, Shao, Wang, Lokhnygina (2018). Sample Size Calculations in
#     Clinical Research, 3rd ed. CRC Press.

calc_d_ttest_ni <- function(diff, sd_A, sd_B, margin) {
  stopifnot(is.numeric(sd_A), sd_A > 0,
            is.numeric(sd_B), sd_B > 0,
            is.numeric(margin), margin > 0)
  sd_pooled <- sqrt((sd_A^2 + sd_B^2) / 2)
  (diff + margin) / sd_pooled
}

calc_power_ttest_ni <- function(diff, sd_A, sd_B, margin, alpha, n) {
  d_ni <- calc_d_ttest_ni(diff, sd_A, sd_B, margin)
  pwr::pwr.t.test(
    n = n, d = d_ni, sig.level = alpha,
    type = "two.sample", alternative = "greater"
  )$power
}

calc_n_ttest_ni <- function(diff, sd_A, sd_B, margin, alpha,
                            power = 0.80, dropout = 0) {
  d_ni <- calc_d_ttest_ni(diff, sd_A, sd_B, margin)
  stopifnot(d_ni > 0)   # Δ + M <= 0 では検出力は達成不能
  res <- pwr::pwr.t.test(
    d = d_ni, sig.level = alpha, power = power,
    type = "two.sample", alternative = "greater"
  )
  make_result(
    n_per_arm_evaluable = ceiling(res$n),
    dropout = dropout,
    n_arms = 2L,
    achieved_power = power,
    backend_pkg = "pwr",
    backend_fun = "pwr.t.test(type='two.sample', alternative='greater')",
    formula_ref = "Chow et al. 2018",
    extras = list(d_ni = d_ni, margin = margin, n_raw = res$n)
  )
}
