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

# 目標検出力 power を達成するための 1 群あたり n（切り上げ）を返す。
# 共通 result schema を返す（make_result を使用）。
calc_n_mode1 <- function(mean_A, sd_A, mean_B, sd_B, alpha,
                         power = 0.80, dropout = 0) {
  d <- calc_d_mode1(mean_A, sd_A, mean_B, sd_B)
  res <- pwr::pwr.t.test(
    d = d, sig.level = alpha, power = power,
    type = "two.sample", alternative = "two.sided"
  )
  make_result(
    n_per_arm_evaluable = ceiling(res$n),
    dropout = dropout,
    n_arms = 2L,
    achieved_power = power,
    backend_pkg = "pwr",
    backend_fun = "pwr.t.test(type='two.sample', alternative='two.sided')",
    formula_ref = "Cohen 1988",
    extras = list(d = d, n_raw = res$n)
  )
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
                         power = 0.80, dropout = 0) {
  d <- calc_d_mode2(diff, sd_A, sd_B)
  res <- pwr::pwr.t.test(
    d = d, sig.level = alpha, power = power,
    type = "two.sample", alternative = "two.sided"
  )
  make_result(
    n_per_arm_evaluable = ceiling(res$n),
    dropout = dropout,
    n_arms = 2L,
    achieved_power = power,
    backend_pkg = "pwr",
    backend_fun = "pwr.t.test(type='two.sample', alternative='two.sided')",
    formula_ref = "Cohen 1988",
    extras = list(d = d, n_raw = res$n)
  )
}
