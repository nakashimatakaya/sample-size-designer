# R/calc_one_mean_precision.R
# 1 標本・平均・精度ベース（信頼区間の半幅から n を計算）。
# 検定ではなく「95% CI の半幅 E 以下で平均を推定する」ための n を返す。
#
# 提供する関数:
#   calc_n_one_mean_precision(sd, half_width,
#                             conf_level = 0.95, dropout = 0)
#
# 公式:
#   n = (z_{1-α/2} * SD / E)^2 を切り上げ
# 出典:
#   - 正規分布に基づく標準的な精度計算（例: Rosner, Fundamentals of
#     Biostatistics）。R 側は stats::qnorm のみを使用し、自前公式は避けて
#     qnorm を呼ぶだけに留める。

calc_n_one_mean_precision <- function(sd, half_width,
                                      conf_level = 0.95, dropout = 0) {
  stopifnot(is.numeric(sd), sd > 0,
            is.numeric(half_width), half_width > 0,
            is.numeric(conf_level), conf_level > 0, conf_level < 1)
  alpha <- 1 - conf_level
  z <- stats::qnorm(1 - alpha / 2)
  n_raw <- (z * sd / half_width)^2
  make_result(
    n_per_arm_evaluable = ceiling(n_raw),
    dropout = dropout,
    n_arms = 1L,
    achieved_power = NA_real_,   # 精度ベースなので検出力の概念なし
    backend_pkg = "stats",
    backend_fun = "qnorm (z_{1-α/2} * SD / E)^2",
    formula_ref = "Standard precision formula (e.g., Rosner)",
    extras = list(
      z = z, sd = sd, half_width = half_width,
      conf_level = conf_level, n_raw = n_raw
    )
  )
}
