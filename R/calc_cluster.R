# R/calc_cluster.R
# クラスターランダム化比較試験（並行群間）の必要症例数・必要クラスター数。
# 連続量／二値の両方に対応。計算は pwr::pwr.t.test / pwr::pwr.2p.test に委譲。
# Design Effect（分散インフレ）だけ自前で適用する。
#
# 入力（連続量 outcome = "continuous"）:
#   mean_A, mean_B, sd_common, m, ICC, alpha, power, dropout
# 入力（二値 outcome = "binary"）:
#   p_A, p_B, m, ICC, alpha, power, dropout
#   m   : 平均クラスターサイズ（>=2）
#   ICC : 級内相関係数（0 <= ICC < 1）
#
# 公式（Donner & Klar 2000）:
#   Design Effect: DE = 1 + (m - 1) * ICC
#   iid 並行群間の n_iid（1 群）を計算し、
#     n_cluster_per_arm = n_iid * DE
#     K_per_arm         = ceiling(n_cluster_per_arm / m)
#     n_per_arm         = K_per_arm * m         (クラスター丸めの実症例数)
#
# 出典:
#   - Donner A, Klar N (2000) Design and Analysis of Cluster
#     Randomization Trials in Health Research. Arnold, Section 5.1.
#   - Campbell MJ et al. (2007) Sample size calculator for cluster
#     randomized trials. Comput Biol Med 37:282-286.

calc_design_effect <- function(m, ICC) {
  stopifnot(is.numeric(m), m >= 1,
            is.numeric(ICC), ICC >= 0, ICC < 1)
  1 + (m - 1) * ICC
}

# 連続量版: mean_A, mean_B, sd_common, m, ICC
calc_n_cluster_continuous <- function(mean_A, mean_B, sd_common,
                                      m, ICC, alpha,
                                      power = 0.80, dropout = 0) {
  stopifnot(is.numeric(sd_common), sd_common > 0)
  d <- abs(mean_A - mean_B) / sd_common
  stopifnot(d > 1e-12)
  DE <- calc_design_effect(m, ICC)
  n_iid <- pwr::pwr.t.test(
    d = d, sig.level = alpha, power = power,
    type = "two.sample", alternative = "two.sided"
  )$n
  n_per_arm_raw <- n_iid * DE
  K_per_arm <- ceiling(n_per_arm_raw / m)
  n_per_arm <- K_per_arm * m  # クラスター丸めで実際に組み込まれる症例数

  make_result(
    n_per_arm_evaluable = n_per_arm,
    dropout = dropout,
    n_arms  = 2L,
    achieved_power = power,
    backend_pkg = "pwr",
    backend_fun = "pwr.t.test(two.sample, two.sided) x Design Effect",
    formula_ref = "Donner & Klar 2000",
    extras = list(
      outcome_type = "continuous",
      d = d, DE = DE, m = as.integer(m), ICC = ICC,
      n_iid_per_arm = ceiling(n_iid),
      K_per_arm = as.integer(K_per_arm),
      n_per_arm_raw = n_per_arm_raw
    )
  )
}

# 二値版: p_A, p_B, m, ICC
calc_n_cluster_binary <- function(p_A, p_B, m, ICC, alpha,
                                  power = 0.80, dropout = 0) {
  stopifnot(is.numeric(p_A), p_A >= 0, p_A <= 1,
            is.numeric(p_B), p_B >= 0, p_B <= 1)
  h <- 2 * asin(sqrt(p_A)) - 2 * asin(sqrt(p_B))
  stopifnot(abs(h) > 1e-12)
  DE <- calc_design_effect(m, ICC)
  n_iid <- pwr::pwr.2p.test(
    h = h, sig.level = alpha, power = power, alternative = "two.sided"
  )$n
  n_per_arm_raw <- n_iid * DE
  K_per_arm <- ceiling(n_per_arm_raw / m)
  n_per_arm <- K_per_arm * m

  make_result(
    n_per_arm_evaluable = n_per_arm,
    dropout = dropout,
    n_arms  = 2L,
    achieved_power = power,
    backend_pkg = "pwr",
    backend_fun = "pwr.2p.test(two.sided) x Design Effect",
    formula_ref = "Donner & Klar 2000",
    extras = list(
      outcome_type = "binary",
      h = h, DE = DE, m = as.integer(m), ICC = ICC,
      p_A = p_A, p_B = p_B,
      n_iid_per_arm = ceiling(n_iid),
      K_per_arm = as.integer(K_per_arm),
      n_per_arm_raw = n_per_arm_raw
    )
  )
}

# 統一ディスパッチ: outcome = "continuous" / "binary"
calc_n_cluster <- function(outcome = c("continuous", "binary"),
                           ...) {
  outcome <- match.arg(outcome)
  if (outcome == "continuous") {
    calc_n_cluster_continuous(...)
  } else {
    calc_n_cluster_binary(...)
  }
}

# 検出力逆算: 連続量版
calc_power_cluster_continuous <- function(mean_A, mean_B, sd_common,
                                          m, ICC, alpha, n) {
  d <- abs(mean_A - mean_B) / sd_common
  DE <- calc_design_effect(m, ICC)
  # 実効 n_iid = n / DE
  pwr::pwr.t.test(
    n = n / DE, d = d, sig.level = alpha,
    type = "two.sample", alternative = "two.sided"
  )$power
}

# 検出力逆算: 二値版
calc_power_cluster_binary <- function(p_A, p_B, m, ICC, alpha, n) {
  h <- 2 * asin(sqrt(p_A)) - 2 * asin(sqrt(p_B))
  DE <- calc_design_effect(m, ICC)
  pwr::pwr.2p.test(
    h = h, n = n / DE, sig.level = alpha, alternative = "two.sided"
  )$power
}
