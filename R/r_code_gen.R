# R/r_code_gen.R
# 各デザインについて「手元で再現できる R コード」を文字列で生成する。
# UI の「R コード」タブに貼り付けることで、
# pwr / binom / stats の素の呼び出しだけで結果を再現できる。

.fmt <- function(x) format(x)

gen_r_code <- function(design_id, params, power_target = 0.80,
                       calc_mode = "sample_size") {
  if (calc_mode == "power_calc") {
    return(.rcode_power_calc_dispatch(design_id, params))
  }
  switch(design_id,
    ttest_m1      = .rcode_ttest_m1(params, power_target),
    ttest_m2      = .rcode_ttest_m2(params, power_target),
    paired        = .rcode_paired(params, power_target),
    paired_corr   = .rcode_paired_corr(params, power_target),
    binary_chisq  = .rcode_binary_chisq(params, power_target),
    binary_fisher = .rcode_binary_fisher(params, power_target),
    one_mean      = .rcode_one_mean(params),
    one_prop      = .rcode_one_prop(params),
    ttest_ni      = .rcode_ttest_ni(params, power_target),
    ttest_m2_ni   = .rcode_ttest_m2_ni(params, power_target),
    paired_ni     = .rcode_paired_ni(params, power_target),
    binary_ni     = .rcode_binary_ni(params, power_target),
    mcnemar       = .rcode_mcnemar(params, power_target),
    ancova        = .rcode_ancova(params, power_target),
    logrank       = .rcode_logrank(params, power_target),
    longitudinal  = .rcode_longitudinal(params, power_target),
    group_sequential = .rcode_group_sequential(params, power_target),
    cluster_cont  = .rcode_cluster_cont(params, power_target),
    cluster_bin   = .rcode_cluster_bin(params, power_target),
    diagnostic    = .rcode_diagnostic(params),
    mann_whitney  = .rcode_mann_whitney(params, power_target),
    stop("unknown design_id: ", design_id)
  )
}

# 検出力モード用の R コード: 与えられた n から達成検出力を計算する。
.rcode_power_calc_dispatch <- function(design_id, p) {
  switch(design_id,
    ttest_m1 = paste(
      "library(pwr)",
      sprintf("mean_A <- %s",  .fmt(p$mean_A)),
      sprintf("sd_A   <- %s",  .fmt(p$sd_A)),
      sprintf("mean_B <- %s",  .fmt(p$mean_B)),
      sprintf("sd_B   <- %s",  .fmt(p$sd_B)),
      sprintf("alpha  <- %s",  .fmt(p$alpha)),
      sprintf("n      <- %s",  .fmt(p$n)),
      "",
      "d <- abs(mean_A - mean_B) / sqrt((sd_A^2 + sd_B^2) / 2)",
      "pwr::pwr.t.test(n = n, d = d, sig.level = alpha,",
      "                type = 'two.sample', alternative = 'two.sided')$power",
      sep = "\n"
    ),
    ttest_m2 = paste(
      "library(pwr)",
      sprintf("diff  <- %s", .fmt(p$diff)),
      sprintf("sd_A  <- %s", .fmt(p$sd_A)),
      sprintf("sd_B  <- %s", .fmt(p$sd_B)),
      sprintf("alpha <- %s", .fmt(p$alpha)),
      sprintf("n     <- %s", .fmt(p$n)),
      "",
      "d <- abs(diff) / sqrt((sd_A^2 + sd_B^2) / 2)",
      "pwr::pwr.t.test(n = n, d = d, sig.level = alpha,",
      "                type = 'two.sample', alternative = 'two.sided')$power",
      sep = "\n"
    ),
    paired = paste(
      "library(pwr)",
      sprintf("diff_mean <- %s", .fmt(p$diff_mean)),
      sprintf("sd_diff   <- %s", .fmt(p$sd_diff)),
      sprintf("alpha     <- %s", .fmt(p$alpha)),
      sprintf("n         <- %s", .fmt(p$n)),
      "",
      "d <- abs(diff_mean) / sd_diff",
      "pwr::pwr.t.test(n = n, d = d, sig.level = alpha,",
      "                type = 'paired', alternative = 'two.sided')$power",
      sep = "\n"
    ),
    paired_corr = paste(
      "library(pwr)",
      sprintf("mean_1 <- %s", .fmt(p$mean_1)),
      sprintf("mean_2 <- %s", .fmt(p$mean_2)),
      sprintf("sd_1   <- %s", .fmt(p$sd_1)),
      sprintf("sd_2   <- %s", .fmt(p$sd_2)),
      sprintf("r      <- %s", .fmt(p$r)),
      sprintf("alpha  <- %s", .fmt(p$alpha)),
      sprintf("n      <- %s", .fmt(p$n)),
      "",
      "sd_diff <- sqrt(sd_1^2 + sd_2^2 - 2 * r * sd_1 * sd_2)",
      "d       <- abs(mean_2 - mean_1) / sd_diff",
      "pwr::pwr.t.test(n = n, d = d, sig.level = alpha,",
      "                type = 'paired', alternative = 'two.sided')$power",
      sep = "\n"
    ),
    binary_chisq = paste(
      "library(pwr)",
      sprintf("p_A   <- %s", .fmt(p$p_A)),
      sprintf("p_B   <- %s", .fmt(p$p_B)),
      sprintf("alpha <- %s", .fmt(p$alpha)),
      sprintf("n     <- %s", .fmt(p$n)),
      "",
      "h <- 2 * asin(sqrt(p_A)) - 2 * asin(sqrt(p_B))",
      "pwr::pwr.2p.test(h = h, n = n, sig.level = alpha,",
      "                 alternative = 'two.sided')$power",
      sep = "\n"
    ),
    binary_fisher = paste(
      "# Fisher の正確検定は χ² 近似として pwr.2p.test で計算",
      "library(pwr)",
      sprintf("p_A   <- %s", .fmt(p$p_A)),
      sprintf("p_B   <- %s", .fmt(p$p_B)),
      sprintf("alpha <- %s", .fmt(p$alpha)),
      sprintf("n     <- %s", .fmt(p$n)),
      "",
      "h <- 2 * asin(sqrt(p_A)) - 2 * asin(sqrt(p_B))",
      "pwr::pwr.2p.test(h = h, n = n, sig.level = alpha,",
      "                 alternative = 'two.sided')$power",
      sep = "\n"
    ),
    ttest_ni = paste(
      "library(pwr)",
      sprintf("mean_A <- %s", .fmt(p$mean_A)),
      sprintf("sd_A   <- %s", .fmt(p$sd_A)),
      sprintf("mean_B <- %s", .fmt(p$mean_B)),
      sprintf("sd_B   <- %s", .fmt(p$sd_B)),
      sprintf("margin <- %s", .fmt(p$margin)),
      sprintf("alpha  <- %s  # 片側", .fmt(p$alpha)),
      sprintf("n      <- %s", .fmt(p$n)),
      "",
      "d_ni <- ((mean_A - mean_B) + margin) / sqrt((sd_A^2 + sd_B^2) / 2)",
      "pwr::pwr.t.test(n = n, d = d_ni, sig.level = alpha,",
      "                type = 'two.sample', alternative = 'greater')$power",
      sep = "\n"
    ),
    ttest_m2_ni = paste(
      "library(pwr)",
      sprintf("diff   <- %s", .fmt(p$diff)),
      sprintf("sd_A   <- %s", .fmt(p$sd_A)),
      sprintf("sd_B   <- %s", .fmt(p$sd_B)),
      sprintf("margin <- %s", .fmt(p$margin)),
      sprintf("alpha  <- %s  # 片側", .fmt(p$alpha)),
      sprintf("n      <- %s", .fmt(p$n)),
      "",
      "d_ni <- (diff + margin) / sqrt((sd_A^2 + sd_B^2) / 2)",
      "pwr::pwr.t.test(n = n, d = d_ni, sig.level = alpha,",
      "                type = 'two.sample', alternative = 'greater')$power",
      sep = "\n"
    ),
    paired_ni = paste(
      "library(pwr)",
      sprintf("diff_mean <- %s", .fmt(p$diff_mean)),
      sprintf("sd_diff   <- %s", .fmt(p$sd_diff)),
      sprintf("margin    <- %s", .fmt(p$margin)),
      sprintf("alpha     <- %s  # 片側", .fmt(p$alpha)),
      sprintf("n         <- %s", .fmt(p$n)),
      "",
      "d_ni <- (abs(diff_mean) + margin) / sd_diff",
      "pwr::pwr.t.test(n = n, d = d_ni, sig.level = alpha,",
      "                type = 'paired', alternative = 'greater')$power",
      sep = "\n"
    ),
    binary_ni = paste(
      "# Chow et al. (2018) Sec 4.2 正規近似での検出力逆算",
      sprintf("p_A    <- %s", .fmt(p$p_A)),
      sprintf("p_B    <- %s", .fmt(p$p_B)),
      sprintf("margin <- %s", .fmt(p$margin)),
      sprintf("alpha  <- %s  # 片側", .fmt(p$alpha)),
      sprintf("n      <- %s", .fmt(p$n)),
      "",
      "z_alpha <- qnorm(1 - alpha)",
      "se      <- sqrt((p_A * (1 - p_A) + p_B * (1 - p_B)) / n)",
      "z_beta  <- (p_A - p_B + margin) / se - z_alpha",
      "pnorm(z_beta)  # 達成検出力",
      sep = "\n"
    ),
    # 新規デザインは検出力モードでも通常モードと同じ構造で提示
    # （n 指定のコードを別途持たせず、必要 n 計算コードを表示しておく）
    mcnemar          = .rcode_mcnemar(p, NA_real_, power_mode = TRUE),
    ancova           = .rcode_ancova(p, NA_real_, power_mode = TRUE),
    logrank          = .rcode_logrank(p, NA_real_, power_mode = TRUE),
    longitudinal     = .rcode_longitudinal(p, NA_real_, power_mode = TRUE),
    group_sequential = .rcode_group_sequential(p, NA_real_, power_mode = TRUE),
    cluster_cont     = .rcode_cluster_cont(p, NA_real_, power_mode = TRUE),
    cluster_bin      = .rcode_cluster_bin(p, NA_real_, power_mode = TRUE),
    mann_whitney     = .rcode_mann_whitney(p, NA_real_, power_mode = TRUE),
    stop("unknown design_id: ", design_id)
  )
}

.rcode_ttest_m1 <- function(p, pw) {
  paste(
    "library(pwr)",
    sprintf("mean_A  <- %s", .fmt(p$mean_A)),
    sprintf("sd_A    <- %s", .fmt(p$sd_A)),
    sprintf("mean_B  <- %s", .fmt(p$mean_B)),
    sprintf("sd_B    <- %s", .fmt(p$sd_B)),
    sprintf("alpha   <- %s", .fmt(p$alpha)),
    sprintf("power   <- %s", .fmt(pw)),
    sprintf("dropout <- %s", .fmt(p$dropout)),
    "",
    "d <- abs(mean_A - mean_B) / sqrt((sd_A^2 + sd_B^2) / 2)",
    "res <- pwr::pwr.t.test(d = d, sig.level = alpha, power = power,",
    "                       type = 'two.sample', alternative = 'two.sided')",
    "n_eval <- ceiling(res$n)                  # 1 群あたり解析対象",
    "n_rand <- ceiling(n_eval / (1 - dropout)) # 1 群あたり登録必要",
    "n_eval; n_rand",
    sep = "\n"
  )
}

.rcode_ttest_m2 <- function(p, pw) {
  paste(
    "library(pwr)",
    sprintf("diff    <- %s", .fmt(p$diff)),
    sprintf("sd_A    <- %s", .fmt(p$sd_A)),
    sprintf("sd_B    <- %s", .fmt(p$sd_B)),
    sprintf("alpha   <- %s", .fmt(p$alpha)),
    sprintf("power   <- %s", .fmt(pw)),
    sprintf("dropout <- %s", .fmt(p$dropout)),
    "",
    "d <- abs(diff) / sqrt((sd_A^2 + sd_B^2) / 2)",
    "res <- pwr::pwr.t.test(d = d, sig.level = alpha, power = power,",
    "                       type = 'two.sample', alternative = 'two.sided')",
    "n_eval <- ceiling(res$n)",
    "n_rand <- ceiling(n_eval / (1 - dropout))",
    "n_eval; n_rand",
    sep = "\n"
  )
}

.rcode_paired <- function(p, pw) {
  paste(
    "library(pwr)",
    sprintf("diff_mean <- %s", .fmt(p$diff_mean)),
    sprintf("sd_diff   <- %s", .fmt(p$sd_diff)),
    sprintf("alpha     <- %s", .fmt(p$alpha)),
    sprintf("power     <- %s", .fmt(pw)),
    sprintf("dropout   <- %s", .fmt(p$dropout)),
    "",
    "d <- abs(diff_mean) / sd_diff",
    "res <- pwr::pwr.t.test(d = d, sig.level = alpha, power = power,",
    "                       type = 'paired', alternative = 'two.sided')",
    "n_eval <- ceiling(res$n)                  # 解析対象ペア数",
    "n_rand <- ceiling(n_eval / (1 - dropout)) # 登録必要ペア数",
    "n_eval; n_rand",
    sep = "\n"
  )
}

.rcode_paired_corr <- function(p, pw) {
  paste(
    "library(pwr)",
    sprintf("mean_1  <- %s", .fmt(p$mean_1)),
    sprintf("mean_2  <- %s", .fmt(p$mean_2)),
    sprintf("sd_1    <- %s", .fmt(p$sd_1)),
    sprintf("sd_2    <- %s", .fmt(p$sd_2)),
    sprintf("r       <- %s", .fmt(p$r)),
    sprintf("alpha   <- %s", .fmt(p$alpha)),
    sprintf("power   <- %s", .fmt(pw)),
    sprintf("dropout <- %s", .fmt(p$dropout)),
    "",
    "# 相関係数から差の SD を換算",
    "sd_diff   <- sqrt(sd_1^2 + sd_2^2 - 2 * r * sd_1 * sd_2)",
    "diff_mean <- mean_2 - mean_1",
    "d         <- abs(diff_mean) / sd_diff",
    "",
    "res <- pwr::pwr.t.test(d = d, sig.level = alpha, power = power,",
    "                       type = 'paired', alternative = 'two.sided')",
    "n_eval <- ceiling(res$n)                  # 解析対象ペア数",
    "n_rand <- ceiling(n_eval / (1 - dropout)) # 登録必要ペア数",
    "sd_diff; n_eval; n_rand",
    sep = "\n"
  )
}

.rcode_binary_chisq <- function(p, pw) {
  paste(
    "library(pwr)",
    sprintf("p_A     <- %s", .fmt(p$p_A)),
    sprintf("p_B     <- %s", .fmt(p$p_B)),
    sprintf("alpha   <- %s", .fmt(p$alpha)),
    sprintf("power   <- %s", .fmt(pw)),
    sprintf("dropout <- %s", .fmt(p$dropout)),
    "",
    "h <- 2 * asin(sqrt(p_A)) - 2 * asin(sqrt(p_B))",
    "res <- pwr::pwr.2p.test(h = h, sig.level = alpha, power = power,",
    "                        alternative = 'two.sided')",
    "n_eval <- ceiling(res$n)",
    "n_rand <- ceiling(n_eval / (1 - dropout))",
    "n_eval; n_rand",
    sep = "\n"
  )
}

.rcode_binary_fisher <- function(p, pw) {
  paste(
    "# NOTE: Fisher の正確検定の厳密な検出力計算は pwr に未搭載のため、",
    "#       χ² 検定の結果を近似として用いる。",
    "library(pwr)",
    sprintf("p_A     <- %s", .fmt(p$p_A)),
    sprintf("p_B     <- %s", .fmt(p$p_B)),
    sprintf("alpha   <- %s", .fmt(p$alpha)),
    sprintf("power   <- %s", .fmt(pw)),
    sprintf("dropout <- %s", .fmt(p$dropout)),
    "",
    "h <- 2 * asin(sqrt(p_A)) - 2 * asin(sqrt(p_B))",
    "res <- pwr::pwr.2p.test(h = h, sig.level = alpha, power = power,",
    "                        alternative = 'two.sided')",
    "n_eval <- ceiling(res$n)",
    "n_rand <- ceiling(n_eval / (1 - dropout))",
    "n_eval; n_rand",
    sep = "\n"
  )
}

.rcode_one_mean <- function(p) {
  paste(
    sprintf("sd         <- %s", .fmt(p$sd)),
    sprintf("half_width <- %s", .fmt(p$half_width)),
    sprintf("conf_level <- %s", .fmt(p$conf_level)),
    sprintf("dropout    <- %s", .fmt(p$dropout)),
    "",
    "alpha <- 1 - conf_level",
    "z <- qnorm(1 - alpha / 2)",
    "n_eval <- ceiling((z * sd / half_width)^2)",
    "n_rand <- ceiling(n_eval / (1 - dropout))",
    "n_eval; n_rand",
    sep = "\n"
  )
}

.rcode_one_prop <- function(p) {
  if (p$method == "normal") {
    paste(
      sprintf("p_hat      <- %s", .fmt(p$p)),
      sprintf("half_width <- %s", .fmt(p$half_width)),
      sprintf("conf_level <- %s", .fmt(p$conf_level)),
      sprintf("dropout    <- %s", .fmt(p$dropout)),
      "",
      "alpha <- 1 - conf_level",
      "z <- qnorm(1 - alpha / 2)",
      "n_eval <- ceiling(z^2 * p_hat * (1 - p_hat) / half_width^2)",
      "n_rand <- ceiling(n_eval / (1 - dropout))",
      "n_eval; n_rand",
      sep = "\n"
    )
  } else {
    paste(
      "library(binom)",
      sprintf("p_hat      <- %s", .fmt(p$p)),
      sprintf("half_width <- %s", .fmt(p$half_width)),
      sprintf("conf_level <- %s", .fmt(p$conf_level)),
      sprintf("method     <- '%s'", p$method),
      sprintf("dropout    <- %s", .fmt(p$dropout)),
      "",
      "# 半幅 <= half_width を満たす最小の n を探索",
      "alpha <- 1 - conf_level",
      "z <- qnorm(1 - alpha / 2)",
      "n_start <- max(ceiling(z^2 * p_hat * (1 - p_hat) / half_width^2), 2)",
      "check <- function(n) {",
      "  ci <- binom::binom.confint(x = round(p_hat * n), n = n,",
      "                             conf.level = conf_level, methods = method)",
      "  (ci$upper - ci$lower) / 2 <= half_width",
      "}",
      "n <- n_start",
      "if (check(n)) while (n > 2 && check(n - 1)) n <- n - 1",
      "else          while (!check(n)) n <- n + 1",
      "n_eval <- n",
      "n_rand <- ceiling(n_eval / (1 - dropout))",
      "n_eval; n_rand",
      sep = "\n"
    )
  }
}

.rcode_ttest_m2_ni <- function(p, pw) {
  paste(
    "library(pwr)",
    sprintf("diff   <- %s", .fmt(p$diff)),
    sprintf("sd_A   <- %s", .fmt(p$sd_A)),
    sprintf("sd_B   <- %s", .fmt(p$sd_B)),
    sprintf("margin <- %s", .fmt(p$margin)),
    sprintf("alpha  <- %s  # 片側", .fmt(p$alpha)),
    sprintf("power  <- %s", .fmt(pw)),
    sprintf("dropout <- %s", .fmt(p$dropout)),
    "",
    "sd_pooled <- sqrt((sd_A^2 + sd_B^2) / 2)",
    "d_ni      <- (diff + margin) / sd_pooled",
    "res <- pwr::pwr.t.test(d = d_ni, sig.level = alpha, power = power,",
    "                       type = 'two.sample', alternative = 'greater')",
    "n_eval <- ceiling(res$n)",
    "n_rand <- ceiling(n_eval / (1 - dropout))",
    "n_eval; n_rand",
    sep = "\n"
  )
}

.rcode_ttest_ni <- function(p, pw) {
  paste(
    "library(pwr)",
    sprintf("mean_A  <- %s", .fmt(p$mean_A)),
    sprintf("sd_A    <- %s", .fmt(p$sd_A)),
    sprintf("mean_B  <- %s", .fmt(p$mean_B)),
    sprintf("sd_B    <- %s", .fmt(p$sd_B)),
    sprintf("margin  <- %s", .fmt(p$margin)),
    sprintf("alpha   <- %s  # 片側", .fmt(p$alpha)),
    sprintf("power   <- %s", .fmt(pw)),
    sprintf("dropout <- %s", .fmt(p$dropout)),
    "",
    "diff      <- mean_A - mean_B",
    "sd_pooled <- sqrt((sd_A^2 + sd_B^2) / 2)",
    "d_ni      <- (diff + margin) / sd_pooled",
    "res <- pwr::pwr.t.test(d = d_ni, sig.level = alpha, power = power,",
    "                       type = 'two.sample', alternative = 'greater')",
    "n_eval <- ceiling(res$n)",
    "n_rand <- ceiling(n_eval / (1 - dropout))",
    "n_eval; n_rand",
    sep = "\n"
  )
}

.rcode_paired_ni <- function(p, pw) {
  paste(
    "library(pwr)",
    sprintf("diff_mean <- %s", .fmt(p$diff_mean)),
    sprintf("sd_diff   <- %s", .fmt(p$sd_diff)),
    sprintf("margin    <- %s", .fmt(p$margin)),
    sprintf("alpha     <- %s  # 片側", .fmt(p$alpha)),
    sprintf("power     <- %s", .fmt(pw)),
    sprintf("dropout   <- %s", .fmt(p$dropout)),
    "",
    "d_ni <- (abs(diff_mean) + margin) / sd_diff",
    "res <- pwr::pwr.t.test(d = d_ni, sig.level = alpha, power = power,",
    "                       type = 'paired', alternative = 'greater')",
    "n_eval <- ceiling(res$n)",
    "n_rand <- ceiling(n_eval / (1 - dropout))",
    "n_eval; n_rand",
    sep = "\n"
  )
}

.rcode_binary_ni <- function(p, pw) {
  paste(
    "# Chow et al. (2018) Sec 4.2 のリスク差ベース正規近似",
    sprintf("p_A     <- %s", .fmt(p$p_A)),
    sprintf("p_B     <- %s", .fmt(p$p_B)),
    sprintf("margin  <- %s", .fmt(p$margin)),
    sprintf("alpha   <- %s  # 片側", .fmt(p$alpha)),
    sprintf("power   <- %s", .fmt(pw)),
    sprintf("dropout <- %s", .fmt(p$dropout)),
    "",
    "z_alpha <- qnorm(1 - alpha)",
    "z_beta  <- qnorm(power)",
    "num     <- (z_alpha + z_beta)^2 * (p_A * (1 - p_A) + p_B * (1 - p_B))",
    "denom   <- (p_A - p_B + margin)^2",
    "n_eval  <- ceiling(num / denom)",
    "n_rand  <- ceiling(n_eval / (1 - dropout))",
    "n_eval; n_rand",
    sep = "\n"
  )
}

# =========================================================================
# 新規デザイン（Phase 2 / 3）の R コードテンプレート
# power_mode = TRUE のときは検出力モード用の呼び出し（達成検出力を返す）。
# いずれも base R / pwr / binom のみを使う。
# =========================================================================

.rcode_mcnemar <- function(p, pw, power_mode = FALSE) {
  paste(
    "# Connor (1987) McNemar 対応のある二値",
    sprintf("p_disc <- %s  # 不一致ペア割合", .fmt(p$p_disc)),
    sprintf("psi    <- %s  # A 優位割合",     .fmt(p$psi)),
    sprintf("alpha  <- %s",                    .fmt(p$alpha)),
    if (power_mode) sprintf("n      <- %s  # ペア数", .fmt(p$n %||% NA))
    else            sprintf("power  <- %s",            .fmt(pw)),
    if (!power_mode) sprintf("dropout<- %s", .fmt(p$dropout)) else "",
    "",
    "z_a   <- qnorm(1 - alpha/2)",
    "inner <- 1 - (2*psi - 1)^2 * p_disc",
    if (power_mode) paste(
      "z_b_hat <- (sqrt(n * (2*psi - 1)^2 * p_disc) - z_a) / sqrt(inner)",
      "pnorm(z_b_hat)  # 達成検出力", sep = "\n")
    else paste(
      "z_b   <- qnorm(power)",
      "n_eval<- ceiling((z_a + z_b*sqrt(inner))^2 / ((2*psi - 1)^2 * p_disc))",
      "n_rand<- ceiling(n_eval / (1 - dropout))",
      "n_eval; n_rand", sep = "\n"),
    sep = "\n"
  )
}

.rcode_ancova <- function(p, pw, power_mode = FALSE) {
  paste(
    "# Borm (2007) ANCOVA: SD×sqrt(1-r^2) で分散を縮めて pwr に渡す",
    "library(pwr)",
    sprintf("mean_A    <- %s",  .fmt(p$mean_A)),
    sprintf("mean_B    <- %s",  .fmt(p$mean_B)),
    sprintf("sd_common <- %s",  .fmt(p$sd_common)),
    sprintf("r         <- %s",  .fmt(p$r)),
    sprintf("alpha     <- %s",  .fmt(p$alpha)),
    if (power_mode) sprintf("n         <- %s", .fmt(p$n %||% NA))
    else            sprintf("power     <- %s", .fmt(pw)),
    if (!power_mode) sprintf("dropout   <- %s", .fmt(p$dropout)) else "",
    "",
    "sd_adj <- sd_common * sqrt(1 - r^2)",
    "d_adj  <- abs(mean_A - mean_B) / sd_adj",
    if (power_mode) paste(
      "pwr::pwr.t.test(n = n, d = d_adj, sig.level = alpha,",
      "                type = 'two.sample', alternative = 'two.sided')$power",
      sep = "\n")
    else paste(
      "res    <- pwr::pwr.t.test(d = d_adj, sig.level = alpha, power = power,",
      "                          type = 'two.sample', alternative = 'two.sided')",
      "n_eval <- ceiling(res$n)",
      "n_rand <- ceiling(n_eval / (1 - dropout))",
      "n_eval; n_rand", sep = "\n"),
    sep = "\n"
  )
}

.rcode_logrank <- function(p, pw, power_mode = FALSE) {
  paste(
    "# Schoenfeld (1981) log-rank（指数 + rectangular accrual）",
    sprintf("median_C <- %s  # 対照中央生存（月）", .fmt(p$median_C)),
    sprintf("HR       <- %s",                        .fmt(p$HR)),
    sprintf("accrual  <- %s  # アクルー期間（月）",  .fmt(p$accrual)),
    sprintf("followup <- %s  # 追加 FU（月）",        .fmt(p$followup)),
    sprintf("alpha    <- %s  # 片側",                 .fmt(p$alpha)),
    if (power_mode) sprintf("N_total  <- %s  # 総症例数", .fmt(p$n %||% NA))
    else            sprintf("power    <- %s",              .fmt(pw)),
    sprintf("p_alloc  <- %s  # 治療群割付", .fmt(p$p_alloc %||% 0.5)),
    if (!power_mode) sprintf("dropout  <- %s",  .fmt(p$dropout)) else "",
    "",
    "lambda_C <- log(2) / median_C",
    "lambda_A <- lambda_C * HR",
    "q <- function(lam) 1 - (exp(-lam*followup) - exp(-lam*(accrual+followup))) / (lam*accrual)",
    "q_C <- q(lambda_C); q_A <- q(lambda_A)",
    "q_pooled <- p_alloc * q_A + (1 - p_alloc) * q_C",
    "z_a <- qnorm(1 - alpha)",
    if (power_mode) paste(
      "events <- N_total * q_pooled",
      "z_b_hat <- sqrt(events * p_alloc*(1-p_alloc)) * abs(log(HR)) - z_a",
      "pnorm(z_b_hat)  # 達成検出力", sep = "\n")
    else paste(
      "z_b <- qnorm(power)",
      "D_events  <- (z_a + z_b)^2 / (p_alloc*(1-p_alloc) * (log(HR))^2)",
      "N_total   <- D_events / q_pooled",
      "n_per_arm <- ceiling(max(p_alloc, 1-p_alloc) * N_total)",
      "events    <- ceiling(D_events)",
      "n_rand    <- ceiling(n_per_arm / (1 - dropout))",
      "events; n_per_arm; n_rand", sep = "\n"),
    sep = "\n"
  )
}

.rcode_longitudinal <- function(p, pw, power_mode = FALSE) {
  paste(
    "# Diggle (2002) 反復測定: SD×sqrt((1+(k-1)ρ)/k)",
    "library(pwr)",
    sprintf("mean_A    <- %s",  .fmt(p$mean_A)),
    sprintf("mean_B    <- %s",  .fmt(p$mean_B)),
    sprintf("sd_common <- %s",  .fmt(p$sd_common)),
    sprintf("k         <- %s",  .fmt(as.integer(p$k))),
    sprintf("rho       <- %s",  .fmt(p$rho)),
    sprintf("alpha     <- %s",  .fmt(p$alpha)),
    if (power_mode) sprintf("n         <- %s", .fmt(p$n %||% NA))
    else            sprintf("power     <- %s", .fmt(pw)),
    if (!power_mode) sprintf("dropout   <- %s", .fmt(p$dropout)) else "",
    "",
    "sd_eff <- sd_common * sqrt((1 + (k-1)*rho) / k)",
    "d_eff  <- abs(mean_A - mean_B) / sd_eff",
    if (power_mode) paste(
      "pwr::pwr.t.test(n = n, d = d_eff, sig.level = alpha,",
      "                type = 'two.sample', alternative = 'two.sided')$power",
      sep = "\n")
    else paste(
      "res    <- pwr::pwr.t.test(d = d_eff, sig.level = alpha, power = power,",
      "                          type = 'two.sample', alternative = 'two.sided')",
      "n_eval <- ceiling(res$n)",
      "n_rand <- ceiling(n_eval / (1 - dropout))",
      "n_eval; n_rand", sep = "\n"),
    sep = "\n"
  )
}

.rcode_group_sequential <- function(p, pw, power_mode = FALSE) {
  paste(
    "# Jennison & Turnbull (2000) 群逐次: 固定 n を inflation factor で補正",
    "library(pwr)",
    sprintf("mean_A    <- %s",  .fmt(p$mean_A)),
    sprintf("mean_B    <- %s",  .fmt(p$mean_B)),
    sprintf("sd_common <- %s",  .fmt(p$sd_common)),
    sprintf("alpha     <- %s",  .fmt(p$alpha)),
    sprintf("K         <- %s",  .fmt(as.integer(p$K))),
    sprintf("boundary  <- '%s'", p$boundary %||% "OBF"),
    if (power_mode) sprintf("n         <- %s", .fmt(p$n %||% NA))
    else            sprintf("power     <- %s", .fmt(pw)),
    if (!power_mode) sprintf("dropout   <- %s", .fmt(p$dropout)) else "",
    "",
    "infl_obf    <- c('1'=1.000,'2'=1.008,'3'=1.017,'4'=1.024,'5'=1.028)",
    "infl_pocock <- c('1'=1.000,'2'=1.110,'3'=1.166,'4'=1.202,'5'=1.229)",
    "infl <- if (boundary == 'OBF') infl_obf[as.character(K)] else infl_pocock[as.character(K)]",
    "d <- abs(mean_A - mean_B) / sd_common",
    if (power_mode) paste(
      "pwr::pwr.t.test(n = n / infl, d = d, sig.level = alpha,",
      "                type = 'two.sample', alternative = 'two.sided')$power",
      sep = "\n")
    else paste(
      "res_fix <- pwr::pwr.t.test(d = d, sig.level = alpha, power = power,",
      "                            type = 'two.sample', alternative = 'two.sided')",
      "n_eval  <- ceiling(res_fix$n * infl)",
      "n_rand  <- ceiling(n_eval / (1 - dropout))",
      "n_eval; n_rand", sep = "\n"),
    sep = "\n"
  )
}

.rcode_cluster_cont <- function(p, pw, power_mode = FALSE) {
  paste(
    "# Donner & Klar (2000) クラスターランダム化・連続量",
    "library(pwr)",
    sprintf("mean_A    <- %s",  .fmt(p$mean_A)),
    sprintf("mean_B    <- %s",  .fmt(p$mean_B)),
    sprintf("sd_common <- %s",  .fmt(p$sd_common)),
    sprintf("m         <- %s  # 平均クラスターサイズ", .fmt(as.integer(p$m))),
    sprintf("ICC       <- %s",  .fmt(p$ICC)),
    sprintf("alpha     <- %s",  .fmt(p$alpha)),
    if (power_mode) sprintf("n         <- %s", .fmt(p$n %||% NA))
    else            sprintf("power     <- %s", .fmt(pw)),
    if (!power_mode) sprintf("dropout   <- %s", .fmt(p$dropout)) else "",
    "",
    "DE <- 1 + (m - 1) * ICC",
    "d  <- abs(mean_A - mean_B) / sd_common",
    if (power_mode) paste(
      "pwr::pwr.t.test(n = n / DE, d = d, sig.level = alpha,",
      "                type = 'two.sample', alternative = 'two.sided')$power",
      sep = "\n")
    else paste(
      "res    <- pwr::pwr.t.test(d = d, sig.level = alpha, power = power,",
      "                          type = 'two.sample', alternative = 'two.sided')",
      "n_iid  <- res$n",
      "K_arm  <- ceiling(n_iid * DE / m)  # 各群のクラスター数",
      "n_eval <- K_arm * m",
      "n_rand <- ceiling(n_eval / (1 - dropout))",
      "K_arm; n_eval; n_rand", sep = "\n"),
    sep = "\n"
  )
}

.rcode_cluster_bin <- function(p, pw, power_mode = FALSE) {
  paste(
    "# Donner & Klar (2000) クラスターランダム化・二値",
    "library(pwr)",
    sprintf("p_A     <- %s", .fmt(p$p_A)),
    sprintf("p_B     <- %s", .fmt(p$p_B)),
    sprintf("m       <- %s", .fmt(as.integer(p$m))),
    sprintf("ICC     <- %s", .fmt(p$ICC)),
    sprintf("alpha   <- %s", .fmt(p$alpha)),
    if (power_mode) sprintf("n       <- %s", .fmt(p$n %||% NA))
    else            sprintf("power   <- %s", .fmt(pw)),
    if (!power_mode) sprintf("dropout <- %s", .fmt(p$dropout)) else "",
    "",
    "DE <- 1 + (m - 1) * ICC",
    "h  <- 2*asin(sqrt(p_A)) - 2*asin(sqrt(p_B))",
    if (power_mode) paste(
      "pwr::pwr.2p.test(h = h, n = n / DE, sig.level = alpha,",
      "                 alternative = 'two.sided')$power",
      sep = "\n")
    else paste(
      "res    <- pwr::pwr.2p.test(h = h, sig.level = alpha, power = power,",
      "                           alternative = 'two.sided')",
      "n_iid  <- res$n",
      "K_arm  <- ceiling(n_iid * DE / m)",
      "n_eval <- K_arm * m",
      "n_rand <- ceiling(n_eval / (1 - dropout))",
      "K_arm; n_eval; n_rand", sep = "\n"),
    sep = "\n"
  )
}

.rcode_diagnostic <- function(p) {
  paste(
    "# Buderer (1996) 診断精度: 感度・特異度の CI 半幅ベース",
    sprintf("Se         <- %s", .fmt(p$Se)),
    sprintf("Sp         <- %s", .fmt(p$Sp)),
    sprintf("prev       <- %s", .fmt(p$prev)),
    sprintf("half_width <- %s  # 目標 CI 半幅 E", .fmt(p$half_width)),
    sprintf("conf_level <- %s", .fmt(p$conf_level)),
    sprintf("dropout    <- %s", .fmt(p$dropout)),
    "",
    "z       <- qnorm(1 - (1 - conf_level)/2)",
    "n_dis   <- z^2 * Se*(1-Se) / half_width^2",
    "n_non   <- z^2 * Sp*(1-Sp) / half_width^2",
    "N_total <- ceiling(max(n_dis / prev, n_non / (1 - prev)))",
    "n_rand  <- ceiling(N_total / (1 - dropout))",
    "ceiling(n_dis); ceiling(n_non); N_total; n_rand",
    sep = "\n"
  )
}

.rcode_mann_whitney <- function(p, pw, power_mode = FALSE) {
  dist_lbl <- p$distribution %||% "normal"
  paste(
    "# Hollander & Wolfe (1999) Mann-Whitney: t 検定結果 / ARE",
    "library(pwr)",
    sprintf("mean_A    <- %s", .fmt(p$mean_A)),
    sprintf("mean_B    <- %s", .fmt(p$mean_B)),
    sprintf("sd_common <- %s", .fmt(p$sd_common)),
    sprintf("distribution <- '%s'  # 'normal'/'lognormal'/'exponential'",
            dist_lbl),
    sprintf("alpha     <- %s", .fmt(p$alpha)),
    if (power_mode) sprintf("n         <- %s", .fmt(p$n %||% NA))
    else            sprintf("power     <- %s", .fmt(pw)),
    if (!power_mode) sprintf("dropout   <- %s", .fmt(p$dropout)) else "",
    "",
    "ARE <- c(normal = 3/pi, lognormal = 1.5, exponential = 3)[distribution]",
    "d   <- abs(mean_A - mean_B) / sd_common",
    if (power_mode) paste(
      "pwr::pwr.t.test(n = n * ARE, d = d, sig.level = alpha,",
      "                type = 'two.sample', alternative = 'two.sided')$power",
      sep = "\n")
    else paste(
      "res_t  <- pwr::pwr.t.test(d = d, sig.level = alpha, power = power,",
      "                          type = 'two.sample', alternative = 'two.sided')",
      "n_eval <- ceiling(res_t$n / ARE)",
      "n_rand <- ceiling(n_eval / (1 - dropout))",
      "n_eval; n_rand", sep = "\n"),
    sep = "\n"
  )
}
