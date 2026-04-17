# R/calc_one_prop_precision.R
# 1 標本・割合・精度ベース（信頼区間の半幅から n を計算）。
# 信頼区間の方法は以下から選択:
#   "normal"  : 正規近似 n = z^2 * p * (1 - p) / E^2
#   "wilson"  : binom::binom.confint(method = "wilson") で半幅 <= E になる
#               最小の n を探索
#   "exact"   : binom::binom.confint(method = "exact")（Clopper-Pearson）
#               で半幅 <= E になる最小の n を探索
#
# 提供する関数:
#   calc_n_one_prop_precision(p, half_width,
#                             conf_level = 0.95,
#                             method = "wilson",
#                             dropout = 0)
#
# 出典:
#   - 正規近似: 標準教科書（Rosner 等）
#   - Wilson: Wilson (1927) J. Am. Stat. Assoc.
#   - Exact (Clopper-Pearson): Clopper & Pearson (1934) Biometrika

calc_n_one_prop_precision <- function(p, half_width,
                                      conf_level = 0.95,
                                      method = c("wilson", "exact", "normal"),
                                      dropout = 0) {
  method <- match.arg(method)
  stopifnot(is.numeric(p), p >= 0, p <= 1,
            is.numeric(half_width), half_width > 0,
            is.numeric(conf_level), conf_level > 0, conf_level < 1)
  alpha <- 1 - conf_level
  z <- stats::qnorm(1 - alpha / 2)

  n_eval <- if (method == "normal") {
    ceiling(z^2 * p * (1 - p) / half_width^2)
  } else {
    .find_n_binom_halfwidth(p, half_width, conf_level, method)
  }

  pkg <- if (method == "normal") "stats" else "binom"
  fun <- switch(method,
    normal = "qnorm : n = z^2 * p * (1-p) / E^2",
    wilson = "binom::binom.confint(method='wilson')",
    exact  = "binom::binom.confint(method='exact')"
  )
  ref <- switch(method,
    normal = "Standard normal approximation",
    wilson = "Wilson 1927",
    exact  = "Clopper & Pearson 1934"
  )

  make_result(
    n_per_arm_evaluable = n_eval,
    dropout = dropout,
    n_arms = 1L,
    achieved_power = NA_real_,
    backend_pkg = pkg,
    backend_fun = fun,
    formula_ref = ref,
    extras = list(
      p = p, half_width = half_width,
      conf_level = conf_level, method = method, z = z
    )
  )
}

# binom パッケージを使って半幅 <= half_width になる最小 n を探索。
# x = round(p * n) を観測成功数と仮定して CI 幅を計算する。
# 正規近似で得た初期値から上下に線形調整する。
.find_n_binom_halfwidth <- function(p, half_width, conf_level, method) {
  check <- function(n) {
    x <- round(p * n)
    ci <- binom::binom.confint(
      x = x, n = n, conf.level = conf_level, methods = method
    )
    hw <- (ci$upper - ci$lower) / 2
    !is.na(hw) && hw <= half_width
  }
  alpha <- 1 - conf_level
  z <- stats::qnorm(1 - alpha / 2)
  n_start <- max(ceiling(z^2 * p * (1 - p) / half_width^2), 2L)

  if (check(n_start)) {
    # 下向きに最小解を探す
    n <- n_start
    while (n > 2 && check(n - 1)) n <- n - 1
    return(as.integer(n))
  }
  # 上向きに探す（上限 1e6 で打ち切り）
  n <- n_start
  limit <- 1e6
  while (!check(n) && n < limit) n <- n + 1L
  if (n >= limit) stop("探索上限 (n = 1e6) を超えました。半幅の指定が厳しすぎる可能性があります。")
  as.integer(n)
}
