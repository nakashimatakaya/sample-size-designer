# R/calc_ttest.R
# 2 標本 t 検定（優越性・両側）の計算ロジック層。
# 計算はすべて pwr::pwr.t.test に委譲する。
#
# 提供する関数:
#   [直接効果量版]
#     calc_ttest_ss(d, alpha, power)
#         効果量 d を直接受け取り、2 標本(1:1) 両側 t 検定の必要症例数を返す。
#   [モード1: 群別入力（平均・SD を群ごとに）]
#     calc_d_mode1(mean_A, sd_A, mean_B, sd_B)
#     calc_power_mode1(mean_A, sd_A, mean_B, sd_B, alpha, n)
#     calc_n_mode1(mean_A, sd_A, mean_B, sd_B, alpha,
#                  power = 0.80, dropout = 0)
#   [モード2: 差と群別 SD で入力（2 標本 t 検定）]
#     calc_d_mode2(diff, sd_A, sd_B)
#     calc_power_mode2(diff, sd_A, sd_B, alpha, n)
#     calc_n_mode2(diff, sd_A, sd_B, alpha,
#                  power = 0.80, dropout = 0)
#
# 出典:
#   - pwr パッケージ: Champely S (2020). pwr: Basic Functions for Power Analysis.
#     https://CRAN.R-project.org/package=pwr
#   - 効果量の定義: Cohen, J. (1988). Statistical Power Analysis for the
#     Behavioral Sciences (2nd ed.). Lawrence Erlbaum Associates.
#
# 対応のある t 検定は R/calc_paired.R に分離した。

# =========================================================================
# 既存: 効果量 d を直接受け取る版
# =========================================================================

# 連続量アウトカムの 2 群比較（優越性、両側 t 検定）の必要症例数を返す。
# 戻り値は後方互換のためシンプルな list（n_per_group, n_total, n_raw）。
calc_ttest_ss <- function(d, alpha, power) {
  res <- pwr::pwr.t.test(
    d = d, sig.level = alpha, power = power,
    type = "two.sample", alternative = "two.sided"
  )
  n_per_group <- ceiling(res$n)
  list(
    n_per_group = n_per_group,
    n_total     = n_per_group * 2L,
    n_raw       = res$n
  )
}

# =========================================================================
# モード1: 群別入力（2 標本 t 検定・等分散近似・両側・1:1）
# =========================================================================

# プール分散ベース: d = |mean_A - mean_B| / sqrt((sd_A^2 + sd_B^2) / 2)
# 出典: Cohen (1988), Chapter 2（等分散ケースの d の定義）。
calc_d_mode1 <- function(mean_A, sd_A, mean_B, sd_B) {
  pooled_sd <- sqrt((sd_A^2 + sd_B^2) / 2)
  abs(mean_A - mean_B) / pooled_sd
}

calc_power_mode1 <- function(mean_A, sd_A, mean_B, sd_B, alpha, n) {
  d <- calc_d_mode1(mean_A, sd_A, mean_B, sd_B)
  pwr::pwr.t.test(
    n = n, d = d, sig.level = alpha,
    type = "two.sample", alternative = "two.sided"
  )$power
}

# 不均等割付時の n 拡張ヘルパー。
# allocation_ratio = r = n_intervention / n_control
# p_T = r/(1+r), p_C = 1/(1+r)
# Chow 2nd ed. Section 3.2 Remarks の公式:
#   N_total = (z_{1-α/2} + z_{1-β})² × (σ²_A/p_T + σ²_B/p_C) / Δ²
# ここで A = 介入（治療）、B = 対照 の想定。r = 1 なら等割付。
#
# 戻り値: list(n_T, n_C, n_total, n_per_arm_max)
.calc_unequal_two_sample <- function(diff, sd_A, sd_B, alpha, power,
                                     allocation_ratio,
                                     alternative = c("two.sided", "greater"),
                                     shift = 0) {
  alternative <- match.arg(alternative)
  r <- allocation_ratio
  p_T <- r / (1 + r); p_C <- 1 / (1 + r)
  z_a <- if (alternative == "two.sided") {
    stats::qnorm(1 - alpha / 2)
  } else {
    stats::qnorm(1 - alpha)
  }
  z_b <- stats::qnorm(power)
  var_factor <- sd_A^2 / p_T + sd_B^2 / p_C
  delta_eff  <- diff + shift
  stopifnot(abs(delta_eff) > 1e-12)
  N_total <- (z_a + z_b)^2 * var_factor / delta_eff^2
  n_T <- N_total * p_T
  n_C <- N_total * p_C
  list(n_T = n_T, n_C = n_C, n_total = N_total,
       n_per_arm_max = max(n_T, n_C))
}

# make_result() の合計値を allocation_ratio に応じて上書きする。
.apply_unequal_allocation <- function(res, n_T, n_C, allocation_ratio,
                                      dropout) {
  if (allocation_ratio == 1) return(res)
  n_T_e <- as.integer(ceiling(n_T))
  n_C_e <- as.integer(ceiling(n_C))
  res$n_per_arm_evaluable  <- max(n_T_e, n_C_e)
  res$n_per_arm_randomized <- as.integer(ceiling(res$n_per_arm_evaluable / (1 - dropout)))
  res$n_total_evaluable    <- n_T_e + n_C_e
  res$n_total_randomized   <- as.integer(
    ceiling(n_T_e / (1 - dropout)) + ceiling(n_C_e / (1 - dropout))
  )
  res$n_intervention_evaluable  <- n_T_e
  res$n_control_evaluable       <- n_C_e
  res$n_intervention_randomized <- as.integer(ceiling(n_T_e / (1 - dropout)))
  res$n_control_randomized      <- as.integer(ceiling(n_C_e / (1 - dropout)))
  res$allocation_ratio          <- allocation_ratio
  res
}

# 目標検出力 power を達成するための 1 群あたり n（切り上げ）を返す。
# 共通 result schema を返す（make_result を使用）。
# allocation_ratio: n_介入 / n_対照（既定 1）。1 以外では Chow Sec 3.2 の
# 不均等割付公式（正規近似）を使う。1 のときは既存の pwr::pwr.t.test を使う
# （既存テストの結果と完全一致させるため）。
calc_n_mode1 <- function(mean_A, sd_A, mean_B, sd_B, alpha,
                         power = 0.80, dropout = 0,
                         allocation_ratio = 1) {
  d <- calc_d_mode1(mean_A, sd_A, mean_B, sd_B)
  if (allocation_ratio == 1) {
    res_pwr <- pwr::pwr.t.test(
      d = d, sig.level = alpha, power = power,
      type = "two.sample", alternative = "two.sided"
    )
    return(make_result(
      n_per_arm_evaluable = ceiling(res_pwr$n),
      dropout = dropout,
      n_arms = 2L,
      achieved_power = power,
      backend_pkg = "pwr",
      backend_fun = "pwr.t.test(type='two.sample', alternative='two.sided')",
      formula_ref = "Cohen 1988",
      extras = list(d = d, n_raw = res_pwr$n,
                    allocation_ratio = 1)
    ))
  }
  # 不均等割付: 正規近似（Chow Sec 3.2）
  u <- .calc_unequal_two_sample(
    diff = mean_A - mean_B, sd_A = sd_A, sd_B = sd_B,
    alpha = alpha, power = power,
    allocation_ratio = allocation_ratio,
    alternative = "two.sided"
  )
  res <- make_result(
    n_per_arm_evaluable = ceiling(u$n_per_arm_max),
    dropout = dropout, n_arms = 2L, achieved_power = power,
    backend_pkg = "stats",
    backend_fun = "qnorm (Chow 2008 Sec 3.2 unequal allocation)",
    formula_ref = "Chow Sec 3.2 / Cohen 1988",
    extras = list(d = d, n_T_raw = u$n_T, n_C_raw = u$n_C,
                  n_total_raw = u$n_total)
  )
  .apply_unequal_allocation(res, u$n_T, u$n_C, allocation_ratio, dropout)
}

# =========================================================================
# モード2: 差と群別 SD で入力（2 標本 t 検定・等分散近似・両側・1:1）
# =========================================================================
# 入力を (差 Δ、A の SD、B の SD) にしただけで、モード1 と同じ公式。
# d = |Δ| / sqrt((sd_A^2 + sd_B^2) / 2)

calc_d_mode2 <- function(diff, sd_A, sd_B) {
  pooled_sd <- sqrt((sd_A^2 + sd_B^2) / 2)
  abs(diff) / pooled_sd
}

calc_power_mode2 <- function(diff, sd_A, sd_B, alpha, n) {
  d <- calc_d_mode2(diff, sd_A, sd_B)
  pwr::pwr.t.test(
    n = n, d = d, sig.level = alpha,
    type = "two.sample", alternative = "two.sided"
  )$power
}

calc_n_mode2 <- function(diff, sd_A, sd_B, alpha,
                         power = 0.80, dropout = 0,
                         allocation_ratio = 1) {
  d <- calc_d_mode2(diff, sd_A, sd_B)
  if (allocation_ratio == 1) {
    res_pwr <- pwr::pwr.t.test(
      d = d, sig.level = alpha, power = power,
      type = "two.sample", alternative = "two.sided"
    )
    return(make_result(
      n_per_arm_evaluable = ceiling(res_pwr$n),
      dropout = dropout, n_arms = 2L, achieved_power = power,
      backend_pkg = "pwr",
      backend_fun = "pwr.t.test(type='two.sample', alternative='two.sided')",
      formula_ref = "Cohen 1988",
      extras = list(d = d, n_raw = res_pwr$n,
                    allocation_ratio = 1)
    ))
  }
  u <- .calc_unequal_two_sample(
    diff = diff, sd_A = sd_A, sd_B = sd_B,
    alpha = alpha, power = power,
    allocation_ratio = allocation_ratio,
    alternative = "two.sided"
  )
  res <- make_result(
    n_per_arm_evaluable = ceiling(u$n_per_arm_max),
    dropout = dropout, n_arms = 2L, achieved_power = power,
    backend_pkg = "stats",
    backend_fun = "qnorm (Chow 2008 Sec 3.2 unequal allocation)",
    formula_ref = "Chow Sec 3.2 / Cohen 1988",
    extras = list(d = d, n_T_raw = u$n_T, n_C_raw = u$n_C,
                  n_total_raw = u$n_total)
  )
  .apply_unequal_allocation(res, u$n_T, u$n_C, allocation_ratio, dropout)
}
