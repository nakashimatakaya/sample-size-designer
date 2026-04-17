# R/ui_helpers.R
# UI とサーバーの双方で使う小さなヘルパー群。
# ・入力ラベルにツールチップ「？」を付ける labeled_input()
# ・デザイン横断で計算を呼び分ける compute_y_dispatch() など
# ・感度分析用の X 軸レンジ／凡例値の生成
# ・結果 value_box・計算の根拠パネル・感度分析プロット

# ------------------------------------------------------------------------
# NULL フォールバック
# ------------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ------------------------------------------------------------------------
# 入力ウィジェットに ? ツールチップを付ける
# ------------------------------------------------------------------------
labeled_input <- function(inputTag, tooltip_key = NULL) {
  if (is.null(tooltip_key)) return(inputTag)
  tip <- tooltip_texts[[tooltip_key]]
  if (is.null(tip)) return(inputTag)
  bslib::tooltip(
    htmltools::tagAppendChild(
      inputTag,
      htmltools::tags$span(" ?", class = "help-badge",
                           style = "color:#1b7ab9;cursor:help;font-weight:700;")
    ),
    tip,
    placement = "right"
  )
}

# ------------------------------------------------------------------------
# デザインごとの「プロットで使える変数」辞書
# ------------------------------------------------------------------------
design_plot_vars <- function(design_id) {
  switch(design_id,
    ttest_m1      = c(mean_A = "A の平均値",  sd_A = "A の SD",
                      mean_B = "B の平均値", sd_B = "B の SD",
                      alpha  = "有意水準 α", n    = "1 群あたり n"),
    ttest_m2      = c(diff   = "群間差 Δ",    sd_A = "A の SD",
                      sd_B   = "B の SD",    alpha = "有意水準 α",
                      n      = "1 群あたり n"),
    paired        = c(diff_mean = "差の平均", sd_diff = "差の SD",
                      alpha = "有意水準 α",   n       = "ペア数 n"),
    paired_corr   = c(mean_1 = "治療前の平均", mean_2 = "治療後の平均",
                      sd_1 = "治療前の SD",    sd_2  = "治療後の SD",
                      r    = "相関係数 r",     alpha = "有意水準 α",
                      n    = "ペア数 n"),
    binary_chisq  = c(p_A = "A 群の割合", p_B = "B 群の割合",
                      alpha = "有意水準 α",  n = "1 群あたり n"),
    binary_fisher = c(p_A = "A 群の割合", p_B = "B 群の割合",
                      alpha = "有意水準 α",  n = "1 群あたり n"),
    one_mean      = c(sd = "SD", half_width = "目標半幅 E",
                      conf_level = "信頼水準"),
    one_prop      = c(p  = "予想される割合 p", half_width = "目標半幅 E",
                      conf_level = "信頼水準"),
    ttest_ni      = c(diff = "群間差 Δ", sd_A = "A の SD", sd_B = "B の SD",
                      margin = "マージン M", alpha = "片側 α",
                      n = "1 群あたり n"),
    ttest_m2_ni   = c(diff = "群間差 Δ", sd_A = "A の SD", sd_B = "B の SD",
                      margin = "マージン M", alpha = "片側 α",
                      n = "1 群あたり n"),
    paired_ni     = c(diff_mean = "差の平均", sd_diff = "差の SD",
                      margin = "マージン M", alpha = "片側 α",
                      n = "ペア数 n"),
    binary_ni     = c(p_A = "A 群の割合", p_B = "B 群の割合",
                      margin = "マージン M", alpha = "片側 α",
                      n = "1 群あたり n"),
    # --- 新規デザイン (Phase 2 / 3) -------------------------------------
    mcnemar       = c(p_disc = "不一致ペア割合 p_disc",
                      psi = "A 優位割合 ψ",
                      alpha = "有意水準 α", n = "ペア数 n"),
    ancova        = c(mean_A = "A の平均値", mean_B = "B の平均値",
                      sd_common = "共通 SD", r = "共変量相関 r",
                      alpha = "有意水準 α", n = "1 群あたり n"),
    logrank       = c(median_C = "対照中央生存 m_C",
                      HR = "ハザード比 HR",
                      accrual = "アクルー期間 a",
                      followup = "FU 期間 f",
                      alpha = "片側 α", n = "総症例数 N"),
    longitudinal  = c(mean_A = "A の平均", mean_B = "B の平均",
                      sd_common = "1 時点の SD",
                      k = "測定回数 k", rho = "被験者内相関 ρ",
                      alpha = "有意水準 α", n = "1 群あたり n"),
    group_sequential = c(mean_A = "A の平均", mean_B = "B の平均",
                         sd_common = "共通 SD",
                         alpha = "全体 α", n = "1 群あたり n"),
    cluster_cont  = c(mean_A = "A の平均", mean_B = "B の平均",
                      sd_common = "共通 SD",
                      m = "クラスターサイズ m", ICC = "級内相関 ICC",
                      alpha = "有意水準 α", n = "1 群あたり n"),
    cluster_bin   = c(p_A = "A 群の割合", p_B = "B 群の割合",
                      m = "クラスターサイズ m", ICC = "級内相関 ICC",
                      alpha = "有意水準 α", n = "1 群あたり n"),
    diagnostic    = c(Se = "予想感度", Sp = "予想特異度",
                      prev = "有病率", half_width = "CI 半幅 E",
                      conf_level = "信頼水準"),
    mann_whitney  = c(mean_A = "A の平均", mean_B = "B の平均",
                      sd_common = "共通 SD",
                      alpha = "有意水準 α", n = "1 群あたり n"),
    character(0)
  )
}

# ------------------------------------------------------------------------
# ggplot 用の英語ラベル辞書
# Shinylive (WebAssembly) 環境に日本語フォントが含まれていないため、
# グラフの軸ラベル・凡例タイトルは英語で描画する。
# UI の selectbox などは引き続き design_plot_vars()（日本語）を使う。
# key は design_plot_vars() と完全一致させること。
# ------------------------------------------------------------------------
design_plot_vars_en <- function(design_id) {
  switch(design_id,
    ttest_m1      = c(mean_A = "Mean of Group A", sd_A = "SD of Group A",
                      mean_B = "Mean of Group B", sd_B = "SD of Group B",
                      alpha  = "Alpha (α)",       n    = "Sample size per group"),
    ttest_m2      = c(diff   = "Mean difference",  sd_A = "SD of Group A",
                      sd_B   = "SD of Group B",    alpha = "Alpha (α)",
                      n      = "Sample size per group"),
    paired        = c(diff_mean = "Mean of differences", sd_diff = "SD of differences",
                      alpha = "Alpha (α)",                n       = "Number of pairs"),
    paired_corr   = c(mean_1 = "Mean at time 1", mean_2 = "Mean at time 2",
                      sd_1   = "SD at time 1",   sd_2   = "SD at time 2",
                      r      = "Correlation coefficient", alpha = "Alpha (α)",
                      n      = "Number of pairs"),
    binary_chisq  = c(p_A = "Proportion in Group A", p_B = "Proportion in Group B",
                      alpha = "Alpha (α)",             n = "Sample size per group"),
    binary_fisher = c(p_A = "Proportion in Group A", p_B = "Proportion in Group B",
                      alpha = "Alpha (α)",             n = "Sample size per group"),
    one_mean      = c(sd = "SD", half_width = "CI half-width",
                      conf_level = "Confidence level"),
    one_prop      = c(p  = "Expected proportion", half_width = "CI half-width",
                      conf_level = "Confidence level"),
    ttest_ni      = c(diff = "Mean difference", sd_A = "SD of Group A",
                      sd_B = "SD of Group B",
                      margin = "Non-inferiority margin", alpha = "One-sided alpha",
                      n = "Sample size per group"),
    ttest_m2_ni   = c(diff = "Mean difference", sd_A = "SD of Group A",
                      sd_B = "SD of Group B",
                      margin = "Non-inferiority margin", alpha = "One-sided alpha",
                      n = "Sample size per group"),
    paired_ni     = c(diff_mean = "Mean of differences", sd_diff = "SD of differences",
                      margin = "Non-inferiority margin",   alpha = "One-sided alpha",
                      n = "Number of pairs"),
    binary_ni     = c(p_A = "Proportion in Group A", p_B = "Proportion in Group B",
                      margin = "Non-inferiority margin",
                      alpha = "One-sided alpha", n = "Sample size per group"),
    # --- 新規デザイン (Phase 2 / 3) -------------------------------------
    mcnemar       = c(p_disc = "Discordant-pair proportion",
                      psi = "Proportion favoring A (ψ)",
                      alpha = "Alpha (α)", n = "Number of pairs"),
    ancova        = c(mean_A = "Mean of Group A", mean_B = "Mean of Group B",
                      sd_common = "Common SD",
                      r = "Correlation coefficient",
                      alpha = "Alpha (α)", n = "Sample size per group"),
    logrank       = c(median_C = "Median survival (control)",
                      HR = "Hazard ratio (HR)",
                      accrual = "Accrual period (months)",
                      followup = "Additional follow-up (months)",
                      alpha = "One-sided alpha",
                      n = "Total sample size"),
    longitudinal  = c(mean_A = "Mean of Group A", mean_B = "Mean of Group B",
                      sd_common = "SD at each time point",
                      k = "Number of measurements (k)",
                      rho = "Within-subject correlation (ρ)",
                      alpha = "Alpha (α)", n = "Sample size per group"),
    group_sequential = c(mean_A = "Mean of Group A", mean_B = "Mean of Group B",
                         sd_common = "Common SD",
                         alpha = "Overall alpha", n = "Sample size per group"),
    cluster_cont  = c(mean_A = "Mean of Group A", mean_B = "Mean of Group B",
                      sd_common = "Common SD",
                      m = "Cluster size (m)",
                      ICC = "Intraclass correlation (ICC)",
                      alpha = "Alpha (α)", n = "Sample size per group"),
    cluster_bin   = c(p_A = "Proportion in Group A", p_B = "Proportion in Group B",
                      m = "Cluster size (m)",
                      ICC = "Intraclass correlation (ICC)",
                      alpha = "Alpha (α)", n = "Sample size per group"),
    diagnostic    = c(Se = "Expected sensitivity",
                      Sp = "Expected specificity",
                      prev = "Prevalence",
                      half_width = "CI half-width",
                      conf_level = "Confidence level"),
    mann_whitney  = c(mean_A = "Mean of Group A", mean_B = "Mean of Group B",
                      sd_common = "Common SD",
                      alpha = "Alpha (α)", n = "Sample size per group"),
    character(0)
  )
}

# 検出力軸をサポートするデザインか
design_supports_power <- function(design_id) {
  !design_id %in% c("one_mean", "one_prop", "diagnostic")
}

# デザインごとの backend パッケージ・関数・公式出典を返す。
# power_calc モードで engine を呼ばず result を組み立てるときに使う。
backend_info_for <- function(design_id) {
  switch(design_id,
    ttest_m1      = list(pkg = "pwr",
                         fun = "pwr.t.test(type='two.sample', alternative='two.sided')",
                         ref = "Cohen 1988"),
    ttest_m2      = list(pkg = "pwr",
                         fun = "pwr.t.test(type='two.sample', alternative='two.sided')",
                         ref = "Cohen 1988"),
    paired        = list(pkg = "pwr",
                         fun = "pwr.t.test(type='paired', alternative='two.sided')",
                         ref = "Cohen 1988"),
    paired_corr   = list(pkg = "pwr",
                         fun = "pwr.t.test(type='paired', alternative='two.sided')",
                         ref = "Cohen 1988"),
    binary_chisq  = list(pkg = "pwr",
                         fun = "pwr.2p.test(alternative='two.sided')",
                         ref = "Cohen 1988 (h)"),
    binary_fisher = list(pkg = "pwr",
                         fun = "pwr.2p.test(alternative='two.sided')  # Fisher 近似",
                         ref = "Cohen 1988 (h) ※Fisher 近似"),
    ttest_ni      = list(pkg = "pwr",
                         fun = "pwr.t.test(type='two.sample', alternative='greater')",
                         ref = "Chow et al. 2018"),
    ttest_m2_ni   = list(pkg = "pwr",
                         fun = "pwr.t.test(type='two.sample', alternative='greater')",
                         ref = "Chow et al. 2018"),
    paired_ni     = list(pkg = "pwr",
                         fun = "pwr.t.test(type='paired', alternative='greater')",
                         ref = "Chow et al. 2018"),
    binary_ni     = list(pkg = "stats",
                         fun = "qnorm (Chow 2018 Sec 4.2 normal approximation)",
                         ref = "Chow et al. 2018 Sec 4.2"),
    # --- 新規デザイン (Phase 2 / 3) -------------------------------------
    mcnemar       = list(pkg = "stats",
                         fun = "qnorm (Connor 1987 McNemar)",
                         ref = "Connor 1987"),
    ancova        = list(pkg = "pwr",
                         fun = "pwr.t.test(two.sample, two.sided) with SD×sqrt(1-r^2)",
                         ref = "Borm 2007"),
    logrank       = list(pkg = "stats",
                         fun = "qnorm (Schoenfeld 1981, exp+rect accrual)",
                         ref = "Schoenfeld 1981"),
    longitudinal  = list(pkg = "pwr",
                         fun = "pwr.t.test(two.sample) with compound-symmetric SD",
                         ref = "Diggle 2002"),
    group_sequential = list(pkg = "pwr",
                            fun = "pwr.t.test x inflation(OBF/Pocock)",
                            ref = "Jennison & Turnbull 2000"),
    cluster_cont  = list(pkg = "pwr",
                         fun = "pwr.t.test(two.sample) x Design Effect",
                         ref = "Donner & Klar 2000"),
    cluster_bin   = list(pkg = "pwr",
                         fun = "pwr.2p.test x Design Effect",
                         ref = "Donner & Klar 2000"),
    diagnostic    = list(pkg = "stats",
                         fun = "qnorm (Buderer 1996 Se/Sp CI)",
                         ref = "Buderer 1996"),
    mann_whitney  = list(pkg = "pwr",
                         fun = "pwr.t.test / ARE(dist)",
                         ref = "Hollander & Wolfe 1999"),
    NULL
  )
}

# ------------------------------------------------------------------------
# デザイン × (params) → 結果 dispatch
# calc_mode:
#   "sample_size" : 目標検出力 power_target を達成する必要 n を返す（既定）
#   "power_calc"  : 与えられた n での達成検出力を返す
# ------------------------------------------------------------------------
compute_result_dispatch <- function(design_id, p, power_target = 0.80,
                                    calc_mode = "sample_size") {
  if (calc_mode == "power_calc" && design_supports_power(design_id)) {
    return(.result_power_calc(design_id, p))
  }
  res <- switch(design_id,
    ttest_m1 = calc_n_mode1(p$mean_A, p$sd_A, p$mean_B, p$sd_B,
                            p$alpha, power_target, p$dropout),
    ttest_m2 = calc_n_mode2(p$diff, p$sd_A, p$sd_B,
                            p$alpha, power_target, p$dropout),
    paired   = calc_n_paired(p$diff_mean, p$sd_diff,
                             p$alpha, power_target, p$dropout),
    paired_corr = calc_paired_from_corr(p$mean_1, p$mean_2,
                                        p$sd_1, p$sd_2, p$r,
                                        p$alpha, power_target, p$dropout),
    binary_chisq  = calc_n_binary_chisq(p$p_A, p$p_B,
                                        p$alpha, power_target, p$dropout),
    binary_fisher = calc_n_binary_fisher(p$p_A, p$p_B,
                                         p$alpha, power_target, p$dropout),
    one_mean = calc_n_one_mean_precision(p$sd, p$half_width,
                                         p$conf_level, p$dropout),
    one_prop = calc_n_one_prop_precision(p$p, p$half_width,
                                         p$conf_level, p$method, p$dropout),
    ttest_ni = calc_n_ttest_ni(p$diff, p$sd_A, p$sd_B, p$margin,
                               p$alpha, power_target, p$dropout),
    ttest_m2_ni = calc_n_ttest_ni(p$diff, p$sd_A, p$sd_B, p$margin,
                                  p$alpha, power_target, p$dropout),
    paired_ni = calc_n_paired_ni(p$diff_mean, p$sd_diff, p$margin,
                                 p$alpha, power_target, p$dropout),
    binary_ni = calc_n_binary_ni(p$p_A, p$p_B, p$margin,
                                 p$alpha, power_target, p$dropout),
    # --- 新規デザイン (Phase 2 / 3) -------------------------------------
    mcnemar = calc_n_mcnemar(p$p_disc, p$psi, p$alpha,
                             power_target, p$dropout),
    ancova  = calc_n_ancova(p$mean_A, p$mean_B, p$sd_common, p$r,
                            p$alpha, power_target, p$dropout),
    logrank = calc_n_logrank(median_C = p$median_C, HR = p$HR,
                             accrual = p$accrual, followup = p$followup,
                             alpha = p$alpha, power = power_target,
                             p_alloc = p$p_alloc %||% 0.5,
                             dropout = p$dropout),
    longitudinal = calc_n_longitudinal(p$mean_A, p$mean_B, p$sd_common,
                                       p$k, p$rho, p$alpha,
                                       power_target, p$dropout),
    group_sequential = calc_n_group_sequential(p$mean_A, p$mean_B,
                                               p$sd_common, p$alpha,
                                               power_target,
                                               K = p$K, boundary = p$boundary,
                                               dropout = p$dropout),
    cluster_cont = calc_n_cluster_continuous(p$mean_A, p$mean_B, p$sd_common,
                                             p$m, p$ICC, p$alpha,
                                             power_target, p$dropout),
    cluster_bin  = calc_n_cluster_binary(p$p_A, p$p_B, p$m, p$ICC, p$alpha,
                                         power_target, p$dropout),
    diagnostic   = calc_n_diagnostic(p$Se, p$Sp, p$prev, p$half_width,
                                     p$conf_level, p$dropout),
    mann_whitney = calc_n_mann_whitney(p$mean_A, p$mean_B, p$sd_common,
                                       distribution = p$distribution,
                                       alpha = p$alpha, power = power_target,
                                       dropout = p$dropout),
    stop("unknown design_id: ", design_id)
  )
  res$calc_mode <- "sample_size"
  res
}

# 検出力モード: 与えられた n から達成検出力を計算し、make_result() で包んで返す。
# engine を呼び直さず、compute_y_dispatch() の power 出力だけを組み込む。
.result_power_calc <- function(design_id, p) {
  pw   <- compute_y_dispatch(design_id, p, "power")
  info <- backend_info_for(design_id)
  n_arms <- if (design_id %in% c("paired", "paired_corr", "paired_ni",
                                  "mcnemar")) 1L
            else 2L

  extras <- list()
  # paired_corr: 換算された差の SD／差の平均も保持（表示・論文用）
  if (design_id == "paired_corr") {
    extras$sd_diff   <- calc_sd_diff_from_corr(p$sd_1, p$sd_2, p$r)
    extras$diff_mean <- p$mean_2 - p$mean_1
  }
  if (design_id == "binary_fisher") {
    extras$warning <- paste0(
      "Fisher の正確検定は χ² 検定より若干保守的になります。",
      "希少事象（p < 0.05 または p > 0.95）ではシミュレーションベースの",
      "検出力計算を推奨します。"
    )
  }
  if (design_id == "binary_ni") {
    extras$warning <- paste0(
      "リスク差ベースの正規近似を使用しています。",
      "より正確な計算には Farrington-Manning 法を推奨します。"
    )
  }

  res <- make_result(
    n_per_arm_evaluable = p$n,
    dropout             = p$dropout,
    n_arms              = n_arms,
    achieved_power      = pw,
    backend_pkg         = info$pkg,
    backend_fun         = info$fun,
    formula_ref         = info$ref,
    extras              = extras
  )
  res$calc_mode <- "power_calc"
  res
}

# デザイン × (params, y_axis) → y の値（検出力 or 必要 n）
compute_y_dispatch <- function(design_id, p, y_axis,
                               req_n_kind = "randomized",
                               power_target = 0.80) {
  if (y_axis == "power") {
    switch(design_id,
      ttest_m1 = calc_power_mode1(p$mean_A, p$sd_A, p$mean_B, p$sd_B,
                                  p$alpha, p$n),
      ttest_m2 = calc_power_mode2(p$diff, p$sd_A, p$sd_B, p$alpha, p$n),
      paired   = calc_power_paired(p$diff_mean, p$sd_diff, p$alpha, p$n),
      paired_corr = calc_power_paired_from_corr(p$mean_1, p$mean_2,
                                                p$sd_1, p$sd_2, p$r,
                                                p$alpha, p$n),
      binary_chisq  = calc_power_binary_chisq(p$p_A, p$p_B, p$alpha, p$n),
      binary_fisher = calc_power_binary_fisher(p$p_A, p$p_B, p$alpha, p$n),
      ttest_ni  = calc_power_ttest_ni(p$diff, p$sd_A, p$sd_B, p$margin,
                                      p$alpha, p$n),
      ttest_m2_ni = calc_power_ttest_ni(p$diff, p$sd_A, p$sd_B, p$margin,
                                        p$alpha, p$n),
      paired_ni = calc_power_paired_ni(p$diff_mean, p$sd_diff, p$margin,
                                       p$alpha, p$n),
      binary_ni = calc_power_binary_ni(p$p_A, p$p_B, p$margin, p$alpha, p$n),
      # --- 新規デザイン (Phase 2 / 3) -----------------------------------
      mcnemar = calc_power_mcnemar(p$p_disc, p$psi, p$alpha, p$n),
      ancova  = calc_power_ancova(p$mean_A, p$mean_B, p$sd_common, p$r,
                                  p$alpha, p$n),
      logrank = calc_power_logrank(median_C = p$median_C, HR = p$HR,
                                   accrual = p$accrual, followup = p$followup,
                                   alpha = p$alpha, N_total = p$n,
                                   p_alloc = p$p_alloc %||% 0.5),
      longitudinal = calc_power_longitudinal(p$mean_A, p$mean_B, p$sd_common,
                                             p$k, p$rho, p$alpha, p$n),
      group_sequential = calc_power_group_sequential(p$mean_A, p$mean_B,
                                                     p$sd_common, p$alpha,
                                                     p$K, p$boundary, p$n),
      cluster_cont = calc_power_cluster_continuous(p$mean_A, p$mean_B,
                                                   p$sd_common, p$m, p$ICC,
                                                   p$alpha, p$n),
      cluster_bin  = calc_power_cluster_binary(p$p_A, p$p_B, p$m, p$ICC,
                                               p$alpha, p$n),
      mann_whitney = calc_power_mann_whitney(p$mean_A, p$mean_B, p$sd_common,
                                             distribution = p$distribution,
                                             alpha = p$alpha, n = p$n),
      NA_real_
    )
  } else {
    r <- compute_result_dispatch(design_id, p, power_target)
    if (req_n_kind == "evaluable") r$n_per_arm_evaluable
    else                           r$n_per_arm_randomized
  }
}

# ------------------------------------------------------------------------
# X 軸のレンジ／値列／凡例値
# ------------------------------------------------------------------------
default_x_range <- function(var, current_val) {
  if (var %in% c("mean_A", "mean_B", "diff", "diff_mean",
                 "mean_1", "mean_2")) {
    spread <- abs(current_val) * 1.0
    if (spread < 1e-9) spread <- 1
    c(current_val - spread, current_val + spread)
  } else if (var %in% c("sd", "sd_A", "sd_B", "sd_diff", "sd_1", "sd_2")) {
    c(current_val * 0.25, current_val * 2.5)
  } else if (var == "alpha") {
    c(0.001, 0.15)
  } else if (var == "n") {
    c(max(2, current_val * 0.5), current_val * 3.0)
  } else if (var %in% c("p", "p_A", "p_B")) {
    c(max(0.001, current_val * 0.5), min(0.999, current_val * 1.5))
  } else if (var == "margin") {
    c(current_val * 0.25, current_val * 2.5)
  } else if (var == "half_width") {
    c(current_val * 0.5, current_val * 2.0)
  } else if (var == "conf_level") {
    c(0.80, 0.999)
  } else if (var == "r") {
    # 相関係数: [-1, 1] の範囲で動かす。端は数値誤差を避けて内側に寄せる。
    c(-0.95, 0.95)
  } else if (var %in% c("rho", "ICC")) {
    # 相関／級内相関: [0, 1) に制限
    c(0, 0.9)
  } else if (var == "HR") {
    # ハザード比: 現在値が 1 を挟まないよう対数スケール的に
    lo <- min(current_val * 0.5, 0.5)
    hi <- max(current_val * 1.5, 1.5)
    c(lo, hi)
  } else if (var %in% c("median_C", "accrual", "followup")) {
    c(max(0.5, current_val * 0.5), current_val * 2)
  } else if (var == "k") {
    c(2, max(6, current_val * 2))
  } else if (var == "m") {
    c(2, max(20, current_val * 2))
  } else if (var %in% c("Se", "Sp")) {
    c(max(0.5, current_val - 0.3), min(0.999, current_val + 0.1))
  } else if (var == "prev") {
    c(max(0.01, current_val * 0.5), min(0.9, current_val * 2))
  } else if (var == "p_disc") {
    c(max(0.01, current_val * 0.5), min(0.99, current_val * 2))
  } else if (var == "psi") {
    c(0.51, 0.95)
  } else {
    c(current_val * 0.5, current_val * 1.5)
  }
}

make_x_seq <- function(var, current_val, range_mode = "auto",
                       manual_min = NA, manual_max = NA) {
  rng <- if (range_mode == "auto" || is.na(manual_min) || is.na(manual_max)) {
    default_x_range(var, current_val)
  } else {
    c(manual_min, manual_max)
  }
  out <- seq(rng[1], rng[2], length.out = 30)
  if (var == "n") {
    out <- unique(round(out))
    out <- out[out >= 2]
  } else if (var %in% c("k", "m")) {
    out <- unique(round(out))
    out <- out[out >= 2]
  }
  out
}

make_legend_vals <- function(var, current_val) {
  if (var == "alpha") {
    cands <- c(0.005, 0.01, 0.025, 0.05, 0.10)
    sort(cands[order(abs(cands - current_val))][1:4])
  } else if (var == "n") {
    v <- round(c(0.5, 1.0, 1.5, 2.0) * current_val)
    sort(unique(v[v >= 2]))
  } else if (var == "conf_level") {
    cands <- c(0.80, 0.90, 0.95, 0.99)
    sort(cands[order(abs(cands - current_val))][1:4])
  } else if (var %in% c("p", "p_A", "p_B")) {
    v <- c(0.5, 1.0, 1.5, 2.0) * current_val
    sort(unique(pmin(pmax(v, 0.001), 0.999)))
  } else if (var == "r") {
    cands <- c(0.0, 0.3, 0.5, 0.7, 0.9)
    sort(cands[order(abs(cands - current_val))][1:4])
  } else {
    sort(unique(c(0.5, 1.0, 1.5, 2.0) * current_val))
  }
}

# ------------------------------------------------------------------------
# 結果パネルの value_box（n_arms で分岐）
# ------------------------------------------------------------------------
render_result_boxes <- function(result) {
  n_arms <- result$n_arms    %||% 2L
  mode   <- result$calc_mode %||% "sample_size"
  pw <- result$achieved_power
  power_str <- if (is.null(pw) || is.na(pw)) "—"
               else sprintf("%.1f%%", pw * 100)

  if (mode == "power_calc") {
    # 検出力モード: 達成検出力が主、n は入力値として参考表示
    if (n_arms == 2L) {
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        bslib::value_box(
          title = "達成検出力",
          value = power_str,
          theme = "primary"
        ),
        bslib::value_box(
          title = "各群 n（入力）",
          value = sprintf("%d 例", result$n_per_arm_evaluable),
          theme = "secondary"
        ),
        bslib::value_box(
          title = "合計 n",
          value = sprintf("%d 例", result$n_total_evaluable),
          theme = "light"
        )
      )
    } else {
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::value_box(
          title = "達成検出力",
          value = power_str,
          theme = "primary"
        ),
        bslib::value_box(
          title = "n（入力）",
          value = sprintf("%d 例", result$n_per_arm_evaluable),
          theme = "secondary"
        )
      )
    }
  } else if (n_arms == 2L) {
    # 必要症例数モード（2 群）
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      bslib::value_box(
        title = "解析対象（各群）",
        value = sprintf("%d 例", result$n_per_arm_evaluable),
        theme = "primary"
      ),
      bslib::value_box(
        title = "登録必要（各群）",
        value = sprintf("%d 例", result$n_per_arm_randomized),
        theme = "primary"
      ),
      bslib::value_box(
        title = "合計（登録必要）",
        value = sprintf("%d 例", result$n_total_randomized),
        theme = "secondary"
      ),
      bslib::value_box(
        title = "目標検出力",
        value = power_str,
        theme = "light"
      )
    )
  } else {
    # 必要症例数モード（1 群・対応あり・1 標本）
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::value_box(
        title = "解析対象",
        value = sprintf("%d 例", result$n_per_arm_evaluable),
        theme = "primary"
      ),
      bslib::value_box(
        title = "登録必要",
        value = sprintf("%d 例", result$n_per_arm_randomized),
        theme = "primary"
      ),
      bslib::value_box(
        title = "目標検出力",
        value = power_str,
        theme = "light"
      )
    )
  }
}

# ------------------------------------------------------------------------
# 計算の根拠パネル
# ------------------------------------------------------------------------
render_citation <- function(design_id, result) {
  refs <- paper_references(design_id)
  refs_html <- paste(sprintf("<li>%s</li>", refs), collapse = "")
  warn_html <- if (!is.null(result$warning) && nzchar(result$warning)) {
    sprintf('<div class="alert alert-warning">%s</div>', result$warning)
  } else {
    ""
  }
  htmltools::HTML(paste0(
    warn_html,
    "<ul>",
    sprintf("<li><b>使用パッケージ</b>: %s</li>", result$backend_pkg %||% ""),
    sprintf("<li><b>使用関数</b>: <code>%s</code></li>",
            htmltools::htmlEscape(result$backend_fun %||% "")),
    sprintf("<li><b>公式／出典</b>: %s</li>",
            htmltools::htmlEscape(result$formula_ref %||% "")),
    "<li><b>脱落率の反映</b>: 登録必要症例数 = ⌈解析対象必要数 / (1 − L)⌉</li>",
    "</ul>",
    "<p><b>参考文献</b></p><ul>", refs_html, "</ul>"
  ))
}

# ------------------------------------------------------------------------
# 感度分析プロット
# ------------------------------------------------------------------------
make_sensitivity_plot <- function(design_id, params, result,
                                  x_var, legend_var,
                                  y_axis, req_n_kind = "randomized",
                                  power_target = 0.80) {
  labs_m  <- design_plot_vars(design_id)     # 存在チェック用（key は同じ）
  labs_en <- design_plot_vars_en(design_id)  # ggplot のラベルはこちらを使う
  if (!(x_var %in% names(labs_m)) || !(x_var %in% names(params))) return(NULL)
  x_cur <- params[[x_var]]
  x_seq <- make_x_seq(x_var, x_cur)

  has_legend <- isTruthy(legend_var) && legend_var != "__none__" &&
                legend_var %in% names(labs_m) && legend_var %in% names(params)

  compute_one <- function(overrides) {
    p <- params
    for (nm in names(overrides)) p[[nm]] <- overrides[[nm]]
    compute_y_dispatch(design_id, p, y_axis, req_n_kind, power_target)
  }

  df <- if (!has_legend) {
    y <- vapply(x_seq, function(xv) compute_one(setNames(list(xv), x_var)),
                numeric(1))
    data.frame(x = x_seq, y = y, legend = NA_real_)
  } else {
    l_cur <- params[[legend_var]]
    lvals <- make_legend_vals(legend_var, l_cur)
    grid <- expand.grid(x = x_seq, legend = lvals)
    grid$y <- mapply(function(xv, lv) {
      compute_one(setNames(list(xv, lv), c(x_var, legend_var)))
    }, grid$x, grid$legend)
    grid
  }

  x_label <- labs_en[[x_var]]
  y_label <- if (y_axis == "power") {
    "Power (1 - β)"
  } else if (req_n_kind == "evaluable") {
    "Required sample size (evaluable)"
  } else {
    "Required sample size (randomized)"
  }

  y_guide <- if (y_axis == "power") {
    power_target
  } else {
    if (req_n_kind == "evaluable") result$n_per_arm_evaluable
    else                           result$n_per_arm_randomized
  }

  gg <- if (!has_legend) {
    ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(linewidth = 1, color = "#08306b")
  } else {
    # 凡例値を離散ラベル化して具体的な値を表示する。
    # 色は既存グラデーション（薄青→濃紺）を値の数だけ補間して割り当てる。
    lvals_lbl <- trimws(format(lvals, trim = TRUE))
    pal <- grDevices::colorRampPalette(c("#9ecae1", "#08306b"))(length(lvals))
    df$legend_f <- factor(trimws(format(df$legend, trim = TRUE)),
                          levels = lvals_lbl)
    ggplot2::ggplot(df, ggplot2::aes(x = x, y = y,
                                     color = legend_f,
                                     group = legend_f)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::scale_color_manual(
        values = setNames(pal, lvals_lbl),
        name   = labs_en[[legend_var]]
      )
  }
  gg <- gg +
    ggplot2::geom_hline(yintercept = y_guide,
                        linetype = "dashed", color = "#4a7bb8") +
    ggplot2::geom_vline(xintercept = x_cur,
                        linetype = "dashed", color = "#d03030") +
    ggplot2::labs(x = x_label, y = y_label) +
    ggplot2::theme_minimal(base_size = 13)
  if (y_axis == "power") {
    gg <- gg + ggplot2::scale_y_continuous(limits = c(0, 1))
  }
  gg
}
