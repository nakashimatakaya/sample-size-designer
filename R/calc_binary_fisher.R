# R/calc_binary_fisher.R
# 二値アウトカムの 2 群比較・Fisher の正確検定（優越性・両側）。
#
# 実装方針（重要）:
#   Fisher の正確検定そのものの検出力計算は pwr パッケージに
#   存在しないため、ここでは χ² 検定（pwr::pwr.2p.test）の結果を
#   そのまま借りて表示し、「Fisher は χ² より若干保守的」である旨の
#   警告メッセージを返り値に含める。
#   希少事象（p<0.05 または p>0.95）ではシミュレーションベースの
#   検出力計算を推奨する。
#
# 提供する関数:
#   calc_n_binary_fisher(p_A, p_B, alpha,
#                        power = 0.80, dropout = 0)
#
# 出典:
#   - pwr::pwr.2p.test (Champely 2020) を近似として使用
#   - Cohen (1988), Chapter 6

calc_power_binary_fisher <- function(p_A, p_B, alpha, n) {
  # χ² 近似として pwr::pwr.2p.test の検出力を返す
  calc_power_binary_chisq(p_A, p_B, alpha, n)
}

calc_n_binary_fisher <- function(p_A, p_B, alpha,
                                 power = 0.80, dropout = 0) {
  base <- calc_n_binary_chisq(p_A, p_B, alpha, power, dropout)
  base$backend_fun  <- "pwr.2p.test(alternative='two.sided')  # Fisher 近似"
  base$formula_ref  <- "Cohen 1988 (h) ※Fisher 近似"
  base$warning <- paste0(
    "Fisher の正確検定は χ² 検定より若干保守的になります。",
    "希少事象（p < 0.05 または p > 0.95）ではシミュレーションベースの",
    "検出力計算を推奨します。"
  )
  base
}
