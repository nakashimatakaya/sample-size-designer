# R/calc_group_sequential.R
# 群逐次デザイン（Pocock / O'Brien-Fleming）の必要症例数・簡易版。
# 連続量 2 群比較のみ対応。計算は pwr::pwr.t.test + inflation factor。
#
# 入力:
#   mean_A, mean_B, sd_common : 連続量 2 群比較の平均と共通 SD
#   alpha, power              : 全体 α（両側）・目標検出力
#   K                         : 最終解析を含む解析回数（2 〜 5）
#   boundary                  : "OBF"（O'Brien-Fleming）または "Pocock"
#   dropout                   : 脱落率
#
# 公式（Jennison & Turnbull 2000）:
#   1. 固定デザインの n_fixed を pwr::pwr.t.test で計算（α は全体のまま）
#   2. 境界タイプに応じた inflation factor I(K) を掛ける
#      n_GS = ceiling(n_fixed * I(K))
#
# Inflation factor（Jennison & Turnbull 2000 Table 2.3 / Table 2.6 より抜粋）:
#   O'Brien-Fleming (α=0.05 両側, 1-β=0.80):
#     K=2: 1.008, K=3: 1.017, K=4: 1.024, K=5: 1.028
#   Pocock (同上):
#     K=2: 1.110, K=3: 1.166, K=4: 1.202, K=5: 1.229
#
# 出典:
#   - Jennison C, Turnbull BW (2000) Group Sequential Methods with
#     Applications to Clinical Trials. Chapman & Hall/CRC.
#   - O'Brien PC, Fleming TR (1979) A multiple testing procedure for
#     clinical trials. Biometrics 35:549-556.
#   - Pocock SJ (1977) Group sequential methods in the design and
#     analysis of clinical trials. Biometrika 64:191-199.
#
# 備考:
#   境界値自体（nominal α_k）は本実装では計算しない（pwr に委譲しないため）。
#   inflation factor は α=0.05 両側、power=0.80 近傍で十分近似する。
#   異なる α/power では多少ずれることを UI で注記する。

# K → inflation factor の参照表（K = 1 は境界なし = 1.000）
.gs_inflation_obf <- c("1" = 1.000, "2" = 1.008, "3" = 1.017,
                       "4" = 1.024, "5" = 1.028)
.gs_inflation_pocock <- c("1" = 1.000, "2" = 1.110, "3" = 1.166,
                          "4" = 1.202, "5" = 1.229)

gs_inflation_factor <- function(K, boundary) {
  K_chr <- as.character(as.integer(K))
  tbl <- switch(boundary,
                OBF    = .gs_inflation_obf,
                Pocock = .gs_inflation_pocock,
                stop("boundary must be 'OBF' or 'Pocock'"))
  if (!K_chr %in% names(tbl)) {
    stop("K must be 1..5")
  }
  unname(tbl[K_chr])
}

calc_d_group_sequential <- function(mean_A, mean_B, sd_common) {
  stopifnot(is.numeric(sd_common), sd_common > 0)
  abs(mean_A - mean_B) / sd_common
}

calc_power_group_sequential <- function(mean_A, mean_B, sd_common,
                                        alpha, K, boundary, n) {
  d <- calc_d_group_sequential(mean_A, mean_B, sd_common)
  infl <- gs_inflation_factor(K, boundary)
  # n_fixed = n / infl に対して検出力を計算
  pwr::pwr.t.test(
    n = n / infl, d = d, sig.level = alpha,
    type = "two.sample", alternative = "two.sided"
  )$power
}

calc_n_group_sequential <- function(mean_A, mean_B, sd_common, alpha,
                                    power = 0.80, K = 2L,
                                    boundary = "OBF", dropout = 0) {
  stopifnot(K %in% 1:5)
  d <- calc_d_group_sequential(mean_A, mean_B, sd_common)
  stopifnot(d > 1e-12)
  res_fix <- pwr::pwr.t.test(
    d = d, sig.level = alpha, power = power,
    type = "two.sample", alternative = "two.sided"
  )
  infl <- gs_inflation_factor(K, boundary)
  n_gs  <- res_fix$n * infl

  make_result(
    n_per_arm_evaluable = ceiling(n_gs),
    dropout = dropout,
    n_arms  = 2L,
    achieved_power = power,
    backend_pkg = "pwr",
    backend_fun = paste0("pwr.t.test(two.sample, two.sided) x inflation(",
                         boundary, ", K=", K, ")"),
    formula_ref = "Jennison & Turnbull 2000",
    extras = list(
      d = d, K = as.integer(K), boundary = boundary,
      inflation = infl,
      n_fixed_per_arm = ceiling(res_fix$n),
      n_raw = n_gs
    )
  )
}
