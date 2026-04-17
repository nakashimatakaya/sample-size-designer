# R/calc_logrank.R
# 生存時間解析（log-rank 検定・優越性）の必要イベント数・症例数。
# 計算は base R（stats::qnorm, exp）のみで自前実装する。
#
# 入力:
#   median_C : 対照群の中央生存期間（月）       --- OR ---
#   lambda_C : 対照群の指数分布ハザード率（/月）  のいずれか一方
#   HR       : ハザード比（例 0.75、HR < 1 で治療優位）
#   accrual  : アクルー期間 a（月、rectangular accrual 仮定）
#   followup : 追加フォローアップ期間 f（月）
#   alpha    : 片側 α
#   power    : 目標検出力
#   p_alloc  : 治療群への割付比（0 < p < 1、デフォルト 0.5）
#   dropout  : 脱落率（症例単位）
#
# 公式（Schoenfeld 1981）:
#   必要イベント数:
#     D = (z_α + z_β)^2 / (p(1-p) * (log HR)^2)
#   指数分布 + rectangular accrual 下でのイベント確率（群別）:
#     q_j = 1 - (exp(-λ_j f) - exp(-λ_j (a+f))) / (λ_j a)
#   総必要症例数:
#     N = D / (p * q_A + (1-p) * q_C)       q_A = q(λ_C * HR)
#   群別症例数:
#     N_A = p * N,   N_C = (1-p) * N
#
# 出典:
#   - Schoenfeld DA (1981) The asymptotic properties of nonparametric
#     tests for comparing survival distributions. Biometrika 68:316-319.
#   - Collett D (2015) Modelling Survival Data in Medical Research,
#     3rd ed. Chapman & Hall/CRC（rectangular accrual のイベント
#     確率計算）.

# 中央生存期間 m からハザード率 λ = log(2)/m（指数分布仮定）
lambda_from_median <- function(m) log(2) / m

# rectangular accrual 下の 1 群イベント確率
event_prob_exp_accrual <- function(lambda, accrual, followup) {
  a <- accrual; f <- followup
  stopifnot(a > 0, f >= 0, lambda > 0)
  1 - (exp(-lambda * f) - exp(-lambda * (a + f))) / (lambda * a)
}

calc_n_logrank <- function(median_C = NULL, lambda_C = NULL,
                           HR, accrual, followup, alpha,
                           power = 0.80, p_alloc = 0.5,
                           dropout = 0) {
  stopifnot(is.numeric(HR), HR > 0, HR != 1,
            is.numeric(accrual), accrual > 0,
            is.numeric(followup), followup >= 0,
            is.numeric(alpha), alpha > 0, alpha < 1,
            is.numeric(power), power > 0, power < 1,
            is.numeric(p_alloc), p_alloc > 0, p_alloc < 1)
  if (is.null(lambda_C)) {
    stopifnot(!is.null(median_C), is.numeric(median_C), median_C > 0)
    lambda_C <- lambda_from_median(median_C)
  }
  lambda_A <- lambda_C * HR   # 治療群

  z_alpha <- stats::qnorm(1 - alpha)   # 片側
  z_beta  <- stats::qnorm(power)

  # Schoenfeld: 必要イベント数
  D <- (z_alpha + z_beta)^2 / (p_alloc * (1 - p_alloc) * (log(HR))^2)

  # 各群のイベント確率（指数 + rectangular accrual）
  q_C <- event_prob_exp_accrual(lambda_C, accrual, followup)
  q_A <- event_prob_exp_accrual(lambda_A, accrual, followup)
  q_pooled <- p_alloc * q_A + (1 - p_alloc) * q_C

  stopifnot(q_pooled > 1e-8)
  N_total <- D / q_pooled

  # 群別: 解析対象 n_per_arm（ここでは治療群を "arm A" として)
  n_A <- p_alloc * N_total
  n_C <- (1 - p_alloc) * N_total
  # 1:1 のときは make_result(n_per_arm=max, n_arms=2) で総数 N_total を自然に得る
  n_per_arm <- max(n_A, n_C)

  make_result(
    n_per_arm_evaluable = ceiling(n_per_arm),
    dropout = dropout,
    n_arms  = 2L,
    achieved_power = power,
    backend_pkg = "stats",
    backend_fun = "qnorm (Schoenfeld 1981 log-rank, exponential + rect accrual)",
    formula_ref = "Schoenfeld 1981",
    extras = list(
      HR = HR, lambda_C = lambda_C, lambda_A = lambda_A,
      accrual = accrual, followup = followup,
      p_alloc = p_alloc,
      events_required = ceiling(D),
      q_C = q_C, q_A = q_A, q_pooled = q_pooled,
      N_total_raw = N_total,
      n_A_raw = n_A, n_C_raw = n_C
    )
  )
}

# 与えられた症例数 N_total での検出力の逆算（感度分析用）。
calc_power_logrank <- function(median_C = NULL, lambda_C = NULL,
                               HR, accrual, followup, alpha,
                               N_total, p_alloc = 0.5) {
  if (is.null(lambda_C)) lambda_C <- lambda_from_median(median_C)
  lambda_A <- lambda_C * HR
  q_C <- event_prob_exp_accrual(lambda_C, accrual, followup)
  q_A <- event_prob_exp_accrual(lambda_A, accrual, followup)
  q_pooled <- p_alloc * q_A + (1 - p_alloc) * q_C
  events <- N_total * q_pooled
  z_alpha <- stats::qnorm(1 - alpha)
  z_beta_hat <- sqrt(events * p_alloc * (1 - p_alloc)) *
                abs(log(HR)) - z_alpha
  stats::pnorm(z_beta_hat)
}
