# R/calc_binary_chisq.R
# 二値アウトカムの 2 群比較・χ² 検定（優越性・両側）。
# 計算は pwr::pwr.2p.test に委譲する。
#
# 提供する関数:
#   calc_h_cohen(p_A, p_B)
#   calc_power_binary_chisq(p_A, p_B, alpha, n)
#   calc_n_binary_chisq(p_A, p_B, alpha,
#                       power = 0.80, dropout = 0)
#
# 出典:
#   - pwr::pwr.2p.test (Champely 2020)
#   - Cohen (1988), Chapter 6（Cohen's h の定義）

# Cohen's h: h = 2*asin(sqrt(p_A)) - 2*asin(sqrt(p_B))
calc_h_cohen <- function(p_A, p_B) {
  stopifnot(p_A >= 0, p_A <= 1, p_B >= 0, p_B <= 1)
  2 * asin(sqrt(p_A)) - 2 * asin(sqrt(p_B))
}

calc_power_binary_chisq <- function(p_A, p_B, alpha, n) {
  h <- calc_h_cohen(p_A, p_B)
  # pwr.2p.test は |h| で計算する（符号非依存）
  pwr::pwr.2p.test(
    h = h, n = n, sig.level = alpha, alternative = "two.sided"
  )$power
}

calc_n_binary_chisq <- function(p_A, p_B, alpha,
                                power = 0.80, dropout = 0) {
  h <- calc_h_cohen(p_A, p_B)
  stopifnot(abs(h) > 1e-12)  # p_A == p_B では計算不能
  res <- pwr::pwr.2p.test(
    h = h, sig.level = alpha, power = power, alternative = "two.sided"
  )
  make_result(
    n_per_arm_evaluable = ceiling(res$n),
    dropout = dropout,
    n_arms = 2L,
    achieved_power = power,
    backend_pkg = "pwr",
    backend_fun = "pwr.2p.test(alternative='two.sided')",
    formula_ref = "Cohen 1988 (h)",
    extras = list(h = h, p_A = p_A, p_B = p_B, n_raw = res$n)
  )
}
