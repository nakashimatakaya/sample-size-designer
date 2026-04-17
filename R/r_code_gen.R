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
    paired_ni     = .rcode_paired_ni(params, power_target),
    binary_ni     = .rcode_binary_ni(params, power_target),
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
