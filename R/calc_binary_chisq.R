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

calc_power_binary_chisq <- function(p_A, p_B, alpha, n,
                                    allocation_ratio = 1) {
  h <- calc_h_cohen(p_A, p_B)
  if (allocation_ratio == 1) {
    return(pwr::pwr.2p.test(
      h = h, n = n, sig.level = alpha, alternative = "two.sided"
    )$power)
  }
  # 不均等割付: 入力 n を「対照群の n」として扱い、介入群は割付比で算出。
  # pwr.2p2n.test は arcsine 変換ベースの 2 群正規近似で n1, n2 を別個に扱う。
  n_C <- n
  n_T <- ceiling(n_C * allocation_ratio)
  pwr::pwr.2p2n.test(
    h = h, n1 = n_T, n2 = n_C, sig.level = alpha,
    alternative = "two.sided"
  )$power
}

calc_n_binary_chisq <- function(p_A, p_B, alpha,
                                power = 0.80, dropout = 0,
                                allocation_ratio = 1) {
  h <- calc_h_cohen(p_A, p_B)
  stopifnot(abs(h) > 1e-12)  # p_A == p_B では計算不能
  res <- pwr::pwr.2p.test(
    h = h, sig.level = alpha, power = power, alternative = "two.sided"
  )
  if (allocation_ratio == 1) {
    return(make_result(
      n_per_arm_evaluable = ceiling(res$n),
      dropout = dropout,
      n_arms = 2L,
      achieved_power = power,
      backend_pkg = "pwr",
      backend_fun = "pwr.2p.test(alternative='two.sided')",
      formula_ref = "Cohen 1988 (h)",
      extras = list(h = h, p_A = p_A, p_B = p_B, n_raw = res$n,
                    allocation_ratio = 1)
    ))
  }
  # 不均等割付: arcsine 変換ベースの正規近似下で、有効標本数
  #   n_eff = n_T * n_C / (n_T + n_C)
  # が等割付時の n_per_group / 2 と一致するよう n_C, n_T を求める。
  # n_T = r * n_C を入れて整理すると:
  #   n_C = n_per_group * (1+r) / (2r),  n_T = n_per_group * (1+r) / 2
  r <- allocation_ratio
  n_per_eq <- res$n  # 等割付時の 1 群あたり raw n
  n_C_raw <- n_per_eq * (1 + r) / (2 * r)
  n_T_raw <- n_per_eq * (1 + r) / 2
  res2 <- make_result(
    n_per_arm_evaluable = ceiling(max(n_T_raw, n_C_raw)),
    dropout = dropout,
    n_arms = 2L,
    achieved_power = power,
    backend_pkg = "pwr",
    backend_fun = "pwr.2p2n.test(alternative='two.sided')",
    formula_ref = "Cohen 1988 (h) / unequal allocation",
    extras = list(h = h, p_A = p_A, p_B = p_B,
                  n_T_raw = n_T_raw, n_C_raw = n_C_raw)
  )
  .apply_unequal_allocation(res2, n_T_raw, n_C_raw, allocation_ratio, dropout)
}
