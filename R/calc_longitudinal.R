# R/calc_longitudinal.R
# 反復測定（longitudinal・並行群間）の必要症例数・簡易版。
# 計算は pwr::pwr.t.test に委譲する（分散スケーリングのみ自前）。
#
# 入力:
#   mean_A, mean_B : 各群の post-baseline 平均
#   sd_common      : 1 時点の SD（全時点共通と仮定）
#   k              : 測定時点数（>= 2）
#   rho            : 同一被験者内の相関（複合対称 compound symmetry）
#                    0 <= rho < 1
#   alpha, power   : 有意水準（両側）・目標検出力
#   dropout        : 脱落率
#
# 公式（Diggle PJ et al. 2002）:
#   k 回測定の平均値の分散（compound symmetry）:
#     Var(mean of k) = σ^2 * (1 + (k-1)ρ) / k
#   有効 SD:
#     SD_eff = σ * sqrt((1 + (k-1)ρ) / k)
#   効果量:
#     d_eff  = |mean_A - mean_B| / SD_eff
#   pwr::pwr.t.test(type='two.sample', alternative='two.sided') に d_eff
#   を渡して必要症例数を得る。
#
# 出典:
#   - Diggle PJ, Liang KY, Zeger SL (2002) Analysis of Longitudinal
#     Data, 2nd ed. Oxford University Press（compound symmetry の平均
#     の分散）.
#   - pwr::pwr.t.test (Champely 2020).
#
# 備考:
#   ρ = 0 なら N_single_measure / k、ρ = 1 なら N_single_measure。
#   k が大きく ρ が小さいほど必要症例数は減る。

calc_sd_eff_longitudinal <- function(sd_common, k, rho) {
  stopifnot(is.numeric(sd_common), sd_common > 0,
            is.numeric(k), k >= 1, k == as.integer(k),
            is.numeric(rho), rho >= 0, rho < 1)
  sd_common * sqrt((1 + (k - 1) * rho) / k)
}

calc_d_longitudinal <- function(mean_A, mean_B, sd_common, k, rho) {
  sd_eff <- calc_sd_eff_longitudinal(sd_common, k, rho)
  abs(mean_A - mean_B) / sd_eff
}

calc_power_longitudinal <- function(mean_A, mean_B, sd_common, k, rho,
                                    alpha, n) {
  d_eff <- calc_d_longitudinal(mean_A, mean_B, sd_common, k, rho)
  pwr::pwr.t.test(
    n = n, d = d_eff, sig.level = alpha,
    type = "two.sample", alternative = "two.sided"
  )$power
}

calc_n_longitudinal <- function(mean_A, mean_B, sd_common, k, rho,
                                alpha, power = 0.80, dropout = 0) {
  d_eff <- calc_d_longitudinal(mean_A, mean_B, sd_common, k, rho)
  stopifnot(d_eff > 1e-12)
  res <- pwr::pwr.t.test(
    d = d_eff, sig.level = alpha, power = power,
    type = "two.sample", alternative = "two.sided"
  )
  make_result(
    n_per_arm_evaluable = ceiling(res$n),
    dropout = dropout,
    n_arms  = 2L,
    achieved_power = power,
    backend_pkg = "pwr",
    backend_fun = "pwr.t.test(two.sample, two.sided) with compound-symmetric SD",
    formula_ref = "Diggle 2002",
    extras = list(
      d = d_eff, k = as.integer(k), rho = rho,
      sd_eff = calc_sd_eff_longitudinal(sd_common, k, rho),
      n_raw = res$n
    )
  )
}
