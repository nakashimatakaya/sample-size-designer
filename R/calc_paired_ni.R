# R/calc_paired_ni.R
# 非劣性・対応のある t 検定（片側）。
#
# 仮説:
#   H0: 差の平均 <= -M
#   H1: 差の平均 >  -M
#   M（マージン）は正の値（平均差の単位）。
#
# 実装:
#   pwr::pwr.t.test(type = "paired", alternative = "greater") に
#   d_ni = (|diff_mean| + M) / sd_diff を渡す。
#
# 提供する関数:
#   calc_d_paired_ni(diff_mean, sd_diff, margin)
#   calc_power_paired_ni(diff_mean, sd_diff, margin, alpha, n)
#   calc_n_paired_ni(diff_mean, sd_diff, margin, alpha,
#                    power = 0.80, dropout = 0)
#
# 出典:
#   - Chow et al. (2018). Sample Size Calculations in Clinical Research, 3rd ed.

calc_d_paired_ni <- function(diff_mean, sd_diff, margin) {
  stopifnot(is.numeric(sd_diff), sd_diff > 0,
            is.numeric(margin), margin > 0)
  (abs(diff_mean) + margin) / sd_diff
}

calc_power_paired_ni <- function(diff_mean, sd_diff, margin, alpha, n) {
  d_ni <- calc_d_paired_ni(diff_mean, sd_diff, margin)
  pwr::pwr.t.test(
    n = n, d = d_ni, sig.level = alpha,
    type = "paired", alternative = "greater"
  )$power
}

calc_n_paired_ni <- function(diff_mean, sd_diff, margin, alpha,
                             power = 0.80, dropout = 0) {
  d_ni <- calc_d_paired_ni(diff_mean, sd_diff, margin)
  res <- pwr::pwr.t.test(
    d = d_ni, sig.level = alpha, power = power,
    type = "paired", alternative = "greater"
  )
  make_result(
    n_per_arm_evaluable = ceiling(res$n),
    dropout = dropout,
    n_arms = 1L,
    achieved_power = power,
    backend_pkg = "pwr",
    backend_fun = "pwr.t.test(type='paired', alternative='greater')",
    formula_ref = "Chow et al. 2018",
    extras = list(d_ni = d_ni, margin = margin, n_raw = res$n)
  )
}
