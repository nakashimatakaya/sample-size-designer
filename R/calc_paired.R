# R/calc_paired.R
# 対応のある t 検定（優越性・両側）の計算ロジック層。
# 計算は pwr::pwr.t.test(type = "paired") に委譲する。
#
# 提供する関数:
#   calc_d_paired(diff_mean, sd_diff)
#   calc_power_paired(diff_mean, sd_diff, alpha, n)
#   calc_n_paired(diff_mean, sd_diff, alpha,
#                 power = 0.80, dropout = 0)
#
# 出典:
#   - pwr::pwr.t.test (Champely 2020)
#   - Cohen (1988), Chapter 2（対応ありにおける d の定義）

# 効果量 d = |差の平均| / 差の SD
calc_d_paired <- function(diff_mean, sd_diff) {
  stopifnot(is.numeric(sd_diff), length(sd_diff) == 1, sd_diff > 0)
  abs(diff_mean) / sd_diff
}

calc_power_paired <- function(diff_mean, sd_diff, alpha, n) {
  d <- calc_d_paired(diff_mean, sd_diff)
  pwr::pwr.t.test(
    n = n, d = d, sig.level = alpha,
    type = "paired", alternative = "two.sided"
  )$power
}

# 目標検出力 power を達成するための必要ペア数（切り上げ）を返す。
# 「群」という概念がないため n_arms = 1 で make_result に渡す
# （n_per_arm_evaluable と n_total_evaluable が同値になる）。
calc_n_paired <- function(diff_mean, sd_diff, alpha,
                          power = 0.80, dropout = 0) {
  d <- calc_d_paired(diff_mean, sd_diff)
  res <- pwr::pwr.t.test(
    d = d, sig.level = alpha, power = power,
    type = "paired", alternative = "two.sided"
  )
  make_result(
    n_per_arm_evaluable = ceiling(res$n),
    dropout = dropout,
    n_arms = 1L,
    achieved_power = power,
    backend_pkg = "pwr",
    backend_fun = "pwr.t.test(type='paired', alternative='two.sided')",
    formula_ref = "Cohen 1988",
    extras = list(d = d, n_raw = res$n)
  )
}

# =========================================================================
# 相関係数モード: 治療前後の平均・SD と相関係数から差の SD を換算
# =========================================================================
# 公式: sd_diff = sqrt(sd_1^2 + sd_2^2 - 2 * r * sd_1 * sd_2)
# 出典: 対応のある観測の差の分散に関する基本公式
#       （Rosner, Fundamentals of Biostatistics 等）。
#
# 差の平均 = mean_2 - mean_1。calc_d_paired が |.| を取るため符号は任意。
# engine の重複を作らず、下の calc_paired_from_corr から calc_paired() に
# 委譲する。

calc_sd_diff_from_corr <- function(sd_1, sd_2, r) {
  stopifnot(is.numeric(sd_1), length(sd_1) == 1, sd_1 > 0,
            is.numeric(sd_2), length(sd_2) == 1, sd_2 > 0,
            is.numeric(r),    length(r)    == 1, r >= -1, r <= 1)
  v <- sd_1^2 + sd_2^2 - 2 * r * sd_1 * sd_2
  # r = 1 かつ sd_1 = sd_2 で浮動小数点誤差により僅かに負になるのを防ぐ
  v <- max(v, 0)
  sqrt(v)
}

# 相関係数モードで現在の n における検出力を返すラッパー。
calc_power_paired_from_corr <- function(mean_1, mean_2, sd_1, sd_2, r,
                                        alpha, n) {
  sd_diff   <- calc_sd_diff_from_corr(sd_1, sd_2, r)
  diff_mean <- mean_2 - mean_1
  calc_power_paired(diff_mean, sd_diff, alpha, n)
}

# 相関係数モードで必要ペア数を返すラッパー。
# 内部で calc_sd_diff_from_corr() で差の SD を算出し、既存の
# calc_n_paired() に委譲する（engine の重複は作らない）。
# 戻り値は calc_n_paired と同じ共通 schema に、相関モード固有の入力値と
# 換算された差の SD／差の平均を後付けで差し込んで返す。
calc_paired_from_corr <- function(mean_1, mean_2, sd_1, sd_2, r, alpha,
                                  power = 0.80, dropout = 0) {
  sd_diff   <- calc_sd_diff_from_corr(sd_1, sd_2, r)
  diff_mean <- mean_2 - mean_1
  res <- calc_n_paired(diff_mean = diff_mean, sd_diff = sd_diff,
                       alpha = alpha, power = power, dropout = dropout)
  res$mean_1    <- mean_1
  res$mean_2    <- mean_2
  res$sd_1      <- sd_1
  res$sd_2      <- sd_2
  res$r         <- r
  res$sd_diff   <- sd_diff
  res$diff_mean <- diff_mean
  res
}
