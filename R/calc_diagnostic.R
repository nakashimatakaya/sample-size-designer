# R/calc_diagnostic.R
# 診断精度試験（感度・特異度の 95% CI 半幅ベース）の必要症例数。
# 計算は base R（stats::qnorm）のみで自前実装。
#
# 入力:
#   Se         : 予想感度（0 <= Se <= 1）
#   Sp         : 予想特異度（0 <= Sp <= 1）
#   prev       : 有病率（0 < prev < 1）
#   half_width : 目標 CI 半幅 E（例 0.05）
#   conf_level : 信頼水準（例 0.95）
#   dropout    : 脱落率（症例単位）
#
# 公式（Buderer 1996 の正規近似）:
#   疾患あり必要例数 n_dis = z^2 * Se*(1-Se) / E^2
#   疾患なし必要例数 n_non = z^2 * Sp*(1-Sp) / E^2
#   総必要例数 N = max(n_dis / prev, n_non / (1-prev))
#   ここで z = z_{1-(1-conf)/2}
#
# 出典:
#   - Buderer NM (1996) Statistical methodology: I. Incorporating the
#     prevalence of disease into the sample size calculation for
#     sensitivity and specificity. Acad Emerg Med 3:895-900.
#   - Hajian-Tilaki K (2014) Sample size estimation in diagnostic test
#     studies of biomedical informatics. J Biomed Inform 48:193-204.
#
# 備考:
#   N_total は「疾患あり／なしの要件のうち多い方」を有病率で割り戻した値。
#   achieved_power は該当しないため NA。

calc_n_diagnostic <- function(Se, Sp, prev, half_width,
                              conf_level = 0.95, dropout = 0) {
  stopifnot(is.numeric(Se), Se >= 0, Se <= 1,
            is.numeric(Sp), Sp >= 0, Sp <= 1,
            is.numeric(prev), prev > 0, prev < 1,
            is.numeric(half_width), half_width > 0, half_width < 1,
            is.numeric(conf_level), conf_level > 0, conf_level < 1)

  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  n_dis <- z^2 * Se * (1 - Se) / half_width^2
  n_non <- z^2 * Sp * (1 - Sp) / half_width^2

  N_total_raw <- max(n_dis / prev, n_non / (1 - prev))
  # 疾患あり／なしの期待数（切り上げ）
  n_dis_req <- ceiling(n_dis)
  n_non_req <- ceiling(n_non)

  # 診断精度は 1 標本扱い（n_arms = 1、per_arm = total）
  make_result(
    n_per_arm_evaluable = ceiling(N_total_raw),
    dropout = dropout,
    n_arms  = 1L,
    achieved_power = NA_real_,     # 精度ベース
    backend_pkg = "stats",
    backend_fun = "qnorm (Buderer 1996 sensitivity/specificity CI)",
    formula_ref = "Buderer 1996",
    extras = list(
      Se = Se, Sp = Sp, prev = prev,
      half_width = half_width, conf_level = conf_level,
      n_dis_required = n_dis_req,
      n_non_required = n_non_req,
      N_total_raw = N_total_raw
    )
  )
}
