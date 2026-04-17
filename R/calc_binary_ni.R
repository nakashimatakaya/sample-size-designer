# R/calc_binary_ni.R
# 非劣性・二値（リスク差ベース、正規近似）。
#
# 公式（Chow et al. 2018, Sec 4.2）:
#   n_per_arm ≈ (z_{1-α} + z_{1-β})^2 *
#               [p_A * (1 - p_A) + p_B * (1 - p_B)] /
#               (p_A - p_B + M)^2
#
# 仮説:
#   H0: p_A - p_B <= -M   （A は B より M 以上劣っている）
#   H1: p_A - p_B >  -M   （非劣性）
#   M は正の値（リスク差の単位、例: 0.05 = 5%）。
#
# 提供する関数:
#   calc_n_binary_ni(p_A, p_B, margin, alpha,
#                    power = 0.80, dropout = 0)
#
# 備考:
#   より正確な計算には Farrington-Manning (1990) の制約付き尤度法を
#   推奨する（実装が重いため本アプリでは採用しない）。
#   画面にはその旨の警告を表示する。
#
# 出典:
#   - Chow, Shao, Wang, Lokhnygina (2018). Sample Size Calculations in
#     Clinical Research, 3rd ed. CRC Press, Section 4.2.

calc_n_binary_ni <- function(p_A, p_B, margin, alpha,
                             power = 0.80, dropout = 0) {
  stopifnot(is.numeric(p_A), p_A >= 0, p_A <= 1,
            is.numeric(p_B), p_B >= 0, p_B <= 1,
            is.numeric(margin), margin > 0,
            is.numeric(alpha), alpha > 0, alpha < 1,
            is.numeric(power), power > 0, power < 1)

  z_alpha <- stats::qnorm(1 - alpha)   # 片側
  z_beta  <- stats::qnorm(power)
  num     <- (z_alpha + z_beta)^2 *
             (p_A * (1 - p_A) + p_B * (1 - p_B))
  denom   <- (p_A - p_B + margin)^2
  stopifnot(denom > 1e-12)   # 分母ゼロは検出力不能
  n_raw <- num / denom

  res <- make_result(
    n_per_arm_evaluable = ceiling(n_raw),
    dropout = dropout,
    n_arms = 2L,
    achieved_power = power,
    backend_pkg = "stats",
    backend_fun = "qnorm (Chow 2018 Sec 4.2 normal approximation)",
    formula_ref = "Chow et al. 2018 Sec 4.2",
    extras = list(
      margin = margin, p_A = p_A, p_B = p_B,
      z_alpha = z_alpha, z_beta = z_beta, n_raw = n_raw
    )
  )
  res$warning <- paste0(
    "リスク差ベースの正規近似を使用しています。",
    "より正確な計算には Farrington-Manning 法を推奨します。"
  )
  res
}

# 現在の n における検出力の逆算（正規近似ベース）。
calc_power_binary_ni <- function(p_A, p_B, margin, alpha, n) {
  z_alpha <- stats::qnorm(1 - alpha)
  denom   <- sqrt((p_A * (1 - p_A) + p_B * (1 - p_B)) / n)
  if (denom <= 0) return(NA_real_)
  # n から z_beta を逆算
  z_beta <- (p_A - p_B + margin) / denom - z_alpha
  stats::pnorm(z_beta)
}
