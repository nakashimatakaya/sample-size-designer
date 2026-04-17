# R/common.R
# すべての engine 関数で共有するヘルパー。
# 各 engine は make_result() を使って統一された result schema を返す。
#
# 共通 result schema（list）:
#   n_per_arm_evaluable  : 1 群あたり解析対象必要数（整数）
#   n_per_arm_randomized : 1 群あたり登録必要数 = ceiling(evaluable / (1 - L))
#   n_total_evaluable    : 合計解析対象必要数
#   n_total_randomized   : 合計登録必要数
#   achieved_power       : 達成検出力（精度ベースなど該当しない場合は NA）
#   backend_pkg          : 計算に使用した R パッケージ名
#   backend_fun          : 計算に使用した R 関数（シグネチャ込み）
#   formula_ref          : 公式の出典文字列（例: "Cohen 1988"）
#   dropout              : 脱落率 L
#   n_arms               : 群数（2 群比較 = 2、1 標本・対応あり = 1）
#   （以降は各 engine が extras で追加する任意フィールド）

# 引数:
#   n_per_arm_evaluable : 1 群あたり解析対象必要数（整数）
#   dropout             : 脱落率 L（0 以上 1 未満）
#   n_arms              : 群数（1 または 2）
#   achieved_power      : 達成検出力（該当しない場合 NA_real_）
#   backend_pkg         : パッケージ名
#   backend_fun         : 関数シグネチャ
#   formula_ref         : 参考文献
#   extras              : 追加情報（list、任意）
make_result <- function(n_per_arm_evaluable, dropout = 0, n_arms = 2L,
                        achieved_power = NA_real_,
                        backend_pkg, backend_fun, formula_ref,
                        extras = list()) {
  stopifnot(is.numeric(dropout), length(dropout) == 1,
            dropout >= 0, dropout < 1,
            n_arms %in% c(1L, 2L))
  n_arms <- as.integer(n_arms)
  n_per_arm_evaluable  <- as.integer(ceiling(n_per_arm_evaluable))
  n_per_arm_randomized <- as.integer(ceiling(n_per_arm_evaluable / (1 - dropout)))

  base <- list(
    n_per_arm_evaluable  = n_per_arm_evaluable,
    n_per_arm_randomized = n_per_arm_randomized,
    n_total_evaluable    = n_per_arm_evaluable  * n_arms,
    n_total_randomized   = n_per_arm_randomized * n_arms,
    achieved_power       = achieved_power,
    backend_pkg          = backend_pkg,
    backend_fun          = backend_fun,
    formula_ref          = formula_ref,
    dropout              = dropout,
    n_arms               = n_arms
  )
  c(base, extras)
}
