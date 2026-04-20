# R/paper_text.R
# 論文記載用テキスト生成（日本語版・英語版）。
# design_id で分岐してテンプレートを返す。
#
# design_id の一覧:
#   ttest_m1, ttest_m2, paired, binary_chisq, binary_fisher,
#   one_mean, one_prop, ttest_ni, paired_ni, binary_ni
#
# 各関数のシグネチャ:
#   gen_paper_jp(design_id, params, result, power_target = 0.80)
#   gen_paper_en(design_id, params, result, power_target = 0.80)
# params: UI から受け取る名前付き list（各デザイン固有の入力）
# result: make_result() が返す list

get_pwr_version <- function() {
  tryCatch(as.character(packageVersion("pwr")),
           error = function(e) "unknown")
}
get_binom_version <- function() {
  tryCatch(as.character(packageVersion("binom")),
           error = function(e) "unknown")
}

# 共通の数値フォーマット
.fmt_alpha <- function(a) sprintf("%.3f", a)
.fmt_pct   <- function(x) sprintf("%d", round(x * 100))

# 日本語・論文末尾の引用ブロック（R + pwr + Chow）。
# section_suffix は "Section 3.2" のような文字列。Chow に詳述のない
# デザインの場合は chow_section_text を alt_text で置き換える。
.cite_footer_jp <- function(section_text, pwr = TRUE,
                            alt_text = NULL) {
  pwr_line <- if (pwr) {
    sprintf("計算は R (R Core Team, 2024) の pwr パッケージ（Champely, 2020）を用いて実行した。")
  } else {
    "計算は R (R Core Team, 2024) の標準パッケージ stats を用いて実行した。"
  }
  tail_line <- if (is.null(alt_text)) {
    sprintf(
      "詳細な手法は Chow, Shao, Wang (2008) Sample Size Calculations in Clinical Research 2nd ed., %s に準拠している。",
      section_text
    )
  } else {
    alt_text
  }
  paste("", pwr_line, tail_line, sep = "\n")
}

.cite_footer_en <- function(section_text, pwr = TRUE,
                            alt_text = NULL) {
  pwr_line <- if (pwr) {
    "Calculations were performed in R (R Core Team, 2024) using the pwr package (Champely, 2020)."
  } else {
    "Calculations were performed in R (R Core Team, 2024) using the base stats package."
  }
  tail_line <- if (is.null(alt_text)) {
    sprintf(
      "The methodology follows Chow, Shao, & Wang (2008), Sample Size Calculations in Clinical Research (2nd ed.), %s.",
      section_text
    )
  } else {
    alt_text
  }
  paste("", pwr_line, tail_line, sep = "\n")
}

# =========================================================================
# 日本語版ディスパッチャ
# =========================================================================
gen_paper_jp <- function(design_id, params, result, power_target = 0.80,
                         calc_mode = "sample_size") {
  if (calc_mode == "power_calc") {
    return(.jp_power_calc_dispatch(design_id, params, result))
  }
  switch(design_id,
    ttest_m1      = .jp_ttest_m1(params, result, power_target),
    ttest_m2      = .jp_ttest_m2(params, result, power_target),
    paired        = .jp_paired(params, result, power_target),
    paired_corr   = .jp_paired_corr(params, result, power_target),
    binary_chisq  = .jp_binary_chisq(params, result, power_target),
    binary_fisher = .jp_binary_fisher(params, result, power_target),
    one_mean      = .jp_one_mean(params, result),
    one_prop      = .jp_one_prop(params, result),
    ttest_ni      = .jp_ttest_ni(params, result, power_target),
    ttest_m2_ni   = .jp_ttest_m2_ni(params, result, power_target),
    paired_ni     = .jp_paired_ni(params, result, power_target),
    binary_ni     = .jp_binary_ni(params, result, power_target),
    mcnemar       = .jp_mcnemar(params, result, power_target),
    ancova        = .jp_ancova(params, result, power_target),
    logrank       = .jp_logrank(params, result, power_target),
    longitudinal  = .jp_longitudinal(params, result, power_target),
    group_sequential = .jp_group_sequential(params, result, power_target),
    cluster_cont  = .jp_cluster_cont(params, result, power_target),
    cluster_bin   = .jp_cluster_bin(params, result, power_target),
    diagnostic    = .jp_diagnostic(params, result),
    mann_whitney  = .jp_mann_whitney(params, result, power_target),
    stop("unknown design_id: ", design_id)
  )
}

# =========================================================================
# 英語版ディスパッチャ
# =========================================================================
gen_paper_en <- function(design_id, params, result, power_target = 0.80,
                         calc_mode = "sample_size") {
  if (calc_mode == "power_calc") {
    return(.en_power_calc_dispatch(design_id, params, result))
  }
  switch(design_id,
    ttest_m1      = .en_ttest_m1(params, result, power_target),
    ttest_m2      = .en_ttest_m2(params, result, power_target),
    paired        = .en_paired(params, result, power_target),
    paired_corr   = .en_paired_corr(params, result, power_target),
    binary_chisq  = .en_binary_chisq(params, result, power_target),
    binary_fisher = .en_binary_fisher(params, result, power_target),
    one_mean      = .en_one_mean(params, result),
    one_prop      = .en_one_prop(params, result),
    ttest_ni      = .en_ttest_ni(params, result, power_target),
    ttest_m2_ni   = .en_ttest_m2_ni(params, result, power_target),
    paired_ni     = .en_paired_ni(params, result, power_target),
    binary_ni     = .en_binary_ni(params, result, power_target),
    mcnemar       = .en_mcnemar(params, result, power_target),
    ancova        = .en_ancova(params, result, power_target),
    logrank       = .en_logrank(params, result, power_target),
    longitudinal  = .en_longitudinal(params, result, power_target),
    group_sequential = .en_group_sequential(params, result, power_target),
    cluster_cont  = .en_cluster_cont(params, result, power_target),
    cluster_bin   = .en_cluster_bin(params, result, power_target),
    diagnostic    = .en_diagnostic(params, result),
    mann_whitney  = .en_mann_whitney(params, result, power_target),
    stop("unknown design_id: ", design_id)
  )
}

# =========================================================================
# 検出力モードのテンプレート（日英）
# =========================================================================

.fmt_power <- function(x) sprintf("%.1f", x * 100)

.jp_power_calc_dispatch <- function(design_id, p, r) {
  # 共通の末尾: 脱落率を見込んだ登録必要数
  drop_tail <- sprintf(
    "脱落率を %s%% と見込む場合、登録必要症例数は各群 %d 例（合計 %d 例）となる。",
    .fmt_pct(p$dropout), r$n_per_arm_randomized, r$n_total_randomized
  )
  drop_tail_1 <- sprintf(
    "脱落率を %s%% と見込む場合、登録必要ペア数は %d ペアとなる。",
    .fmt_pct(p$dropout), r$n_per_arm_randomized
  )

  body <- switch(design_id,
    ttest_m1 = sprintf(paste(
      "本研究の設計において、介入群平均を %.2f（標準偏差 %.2f）、",
      "対照群平均を %.2f（標準偏差 %.2f）と想定した。",
      "両側有意水準 %s のもと、各群 %d 例（合計 %d 例）を登録した場合の",
      "2 標本 t 検定の達成検出力を算出した。",
      "計算には R パッケージ pwr（version %s）の pwr.t.test 関数を用いた。",
      "その結果、達成検出力は %s%% であった。",
      drop_tail,
      sep = "\n"),
      p$mean_A, p$sd_A, p$mean_B, p$sd_B,
      .fmt_alpha(p$alpha),
      r$n_per_arm_evaluable, r$n_total_evaluable,
      get_pwr_version(),
      .fmt_power(r$achieved_power)
    ),
    ttest_m2 = sprintf(paste(
      "本研究の設計において、介入群と 対照群の平均値の差（群間差 Δ）を %.2f、",
      "介入群の SD を %.2f、対照群の SD を %.2f と想定した。",
      "両側有意水準 %s のもと、各群 %d 例（合計 %d 例）における",
      "2 標本 t 検定の達成検出力を R パッケージ pwr（version %s）の",
      "pwr.t.test 関数を用いて算出した。その結果、達成検出力は %s%% であった。",
      drop_tail,
      sep = "\n"),
      p$diff, p$sd_A, p$sd_B,
      .fmt_alpha(p$alpha),
      r$n_per_arm_evaluable, r$n_total_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    paired = sprintf(paste(
      "本研究の設計において、対応のある観測の差の平均を %.2f、",
      "差の SD を %.2f と想定した。",
      "両側有意水準 %s のもと、%d ペアにおける",
      "対応のある t 検定の達成検出力を pwr（version %s）の",
      "pwr.t.test 関数を用いて算出した。その結果、達成検出力は %s%% であった。",
      drop_tail_1,
      sep = "\n"),
      p$diff_mean, p$sd_diff,
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    paired_corr = sprintf(paste(
      "本研究の設計において、治療前の平均を %.2f（SD %.2f）、",
      "治療後の平均を %.2f（SD %.2f）、",
      "前後の相関係数を %.2f と想定した。",
      "これより差の SD は sqrt(%.2f^2 + %.2f^2 - 2 × %.2f × %.2f × %.2f) = %.2f、",
      "差の平均は %.2f と換算された。",
      "両側有意水準 %s のもと、%d ペアにおける",
      "対応のある t 検定の達成検出力を pwr（version %s）の pwr.t.test 関数で",
      "算出した。その結果、達成検出力は %s%% であった。",
      drop_tail_1,
      sep = "\n"),
      p$mean_1, p$sd_1, p$mean_2, p$sd_2, p$r,
      p$sd_1, p$sd_2, p$r, p$sd_1, p$sd_2, r$sd_diff %||% NA_real_,
      r$diff_mean %||% NA_real_,
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    binary_chisq = sprintf(paste(
      "本研究の設計において、介入群の発生割合を %s%%、対照群の発生割合を %s%% と想定した。",
      "両側有意水準 %s のもと、各群 %d 例（合計 %d 例）における",
      "χ² 検定の達成検出力を pwr（version %s）の pwr.2p.test 関数で算出した。",
      "その結果、達成検出力は %s%% であった。",
      drop_tail,
      sep = "\n"),
      .fmt_pct(p$p_A), .fmt_pct(p$p_B),
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable, r$n_total_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    binary_fisher = sprintf(paste(
      "本研究の設計において、介入群の発生割合を %s%%、対照群の発生割合を %s%% と想定した。",
      "両側有意水準 %s のもと、各群 %d 例（合計 %d 例）における",
      "Fisher の正確検定の達成検出力を、χ² 検定ベースの pwr.2p.test 関数",
      "（pwr version %s）による近似を用いて算出した。",
      "その結果、達成検出力は %s%% であった。",
      drop_tail,
      sep = "\n"),
      .fmt_pct(p$p_A), .fmt_pct(p$p_B),
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable, r$n_total_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    ttest_ni = sprintf(paste(
      "本研究の設計において、介入群の平均を %.2f（SD %.2f）、対照群の平均を %.2f（SD %.2f）、",
      "非劣性マージン M を %.2f と想定した。",
      "片側有意水準 %s のもと、各群 %d 例（合計 %d 例）における",
      "2 標本 t 検定（片側）の達成検出力を pwr（version %s）の",
      "pwr.t.test 関数（alternative = 'greater'）を用いて算出した。",
      "その結果、達成検出力は %s%% であった。",
      drop_tail,
      sep = "\n"),
      p$mean_A, p$sd_A, p$mean_B, p$sd_B, p$margin,
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable, r$n_total_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    ttest_m2_ni = sprintf(paste(
      "本研究の設計において、介入群と 対照群の平均値の差（群間差 Δ）を %.2f、",
      "介入群の SD を %.2f、対照群の SD を %.2f、",
      "非劣性マージン M を %.2f と想定した。",
      "片側有意水準 %s のもと、各群 %d 例（合計 %d 例）における",
      "2 標本 t 検定（片側）の達成検出力を pwr（version %s）の",
      "pwr.t.test 関数（alternative = 'greater'）を用いて算出した。",
      "その結果、達成検出力は %s%% であった。",
      drop_tail,
      sep = "\n"),
      p$diff, p$sd_A, p$sd_B, p$margin,
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable, r$n_total_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    paired_ni = sprintf(paste(
      "本研究の設計において、差の平均を %.2f、差の SD を %.2f、",
      "非劣性マージン M を %.2f と想定した。",
      "片側有意水準 %s のもと、%d ペアにおける",
      "対応のある t 検定（片側）の達成検出力を pwr（version %s）の",
      "pwr.t.test 関数で算出した。その結果、達成検出力は %s%% であった。",
      drop_tail_1,
      sep = "\n"),
      p$diff_mean, p$sd_diff, p$margin,
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    binary_ni = sprintf(paste(
      "本研究の設計において、介入群の割合を %s%%、対照群の割合を %s%%、",
      "非劣性マージン M を %s%%（リスク差の単位）と想定した。",
      "片側有意水準 %s のもと、各群 %d 例（合計 %d 例）における",
      "達成検出力を、Chow ら（2018）の正規近似公式（Sec 4.2）に基づき",
      "R の stats::qnorm を用いて算出した。その結果、達成検出力は %s%% であった。",
      drop_tail,
      sep = "\n"),
      .fmt_pct(p$p_A), .fmt_pct(p$p_B), .fmt_pct(p$margin),
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable, r$n_total_evaluable,
      .fmt_power(r$achieved_power)
    ),
    # 新規デザインは汎用テンプレートで検出力を報告
    .jp_power_calc_generic(design_id, p, r, drop_tail, drop_tail_1)
  )
  body
}

# 新規デザインの検出力モード汎用テンプレート
.jp_power_calc_generic <- function(design_id, p, r, drop_tail, drop_tail_1) {
  info <- backend_info_for(design_id)
  pw   <- .fmt_power(r$achieved_power %||% 0)
  n_per <- r$n_per_arm_evaluable
  n_tot <- r$n_total_evaluable
  is_paired <- design_id == "mcnemar"
  tail <- if (is_paired) drop_tail_1 else drop_tail
  design_label <- switch(design_id,
    mcnemar       = "対応のある二値（McNemar 検定）",
    ancova        = "共変量調整（ANCOVA）を用いた 2 群比較",
    logrank       = "生存時間の log-rank 検定",
    longitudinal  = "反復測定（longitudinal）による 2 群比較",
    group_sequential = "群逐次デザイン（Pocock / OBF）",
    cluster_cont  = "クラスターランダム化（連続量アウトカム）",
    cluster_bin   = "クラスターランダム化（二値アウトカム）",
    mann_whitney  = "Mann-Whitney U 検定",
    diagnostic    = "診断精度（感度・特異度）",
    design_id
  )
  n_line <- if (is_paired) {
    sprintf("%d ペア", n_per)
  } else {
    sprintf("各群 %d 例（合計 %d 例）", n_per, n_tot)
  }
  # drop_tail / drop_tail_1 は既に sprintf 済みなので、本文とは paste で連結する
  body <- sprintf(paste(
    "本研究の設計は「%s」である。",
    "%s における達成検出力を、R の %s パッケージ（%s）を用いて算出した。",
    "その結果、達成検出力は %s%% であった。",
    "公式の出典は %s。",
    sep = "\n"),
    design_label, n_line,
    info$pkg, info$fun, pw, info$ref
  )
  paste(body, tail, sep = "\n")
}

.en_power_calc_generic <- function(design_id, p, r, drop_tail, drop_tail_1) {
  info <- backend_info_for(design_id)
  pw   <- .fmt_power(r$achieved_power %||% 0)
  n_per <- r$n_per_arm_evaluable
  n_tot <- r$n_total_evaluable
  is_paired <- design_id == "mcnemar"
  tail <- if (is_paired) drop_tail_1 else drop_tail
  design_label <- switch(design_id,
    mcnemar       = "a paired binary outcome analysed by McNemar's test",
    ancova        = "a two-group comparison with covariate adjustment (ANCOVA)",
    logrank       = "a time-to-event comparison using the log-rank test",
    longitudinal  = "a longitudinal (repeated-measures) two-group comparison",
    group_sequential = "a group-sequential design (Pocock / OBF boundaries)",
    cluster_cont  = "a cluster-randomised trial with a continuous outcome",
    cluster_bin   = "a cluster-randomised trial with a binary outcome",
    mann_whitney  = "a Mann-Whitney U test",
    diagnostic    = "a diagnostic-accuracy study (sensitivity and specificity)",
    design_id
  )
  n_line <- if (is_paired) {
    sprintf("%d pairs", n_per)
  } else {
    sprintf("%d participants per arm (total %d)", n_per, n_tot)
  }
  body <- sprintf(paste(
    "The study design was %s.",
    "The achieved power with %s was calculated using the %s R package (%s).",
    "The achieved power was %s%%.",
    "The source of the formula is %s.",
    sep = "\n"),
    design_label, n_line,
    info$pkg, info$fun, pw, info$ref
  )
  paste(body, tail, sep = "\n")
}

.en_power_calc_dispatch <- function(design_id, p, r) {
  drop_tail <- sprintf(
    "Accounting for an anticipated dropout rate of %s%%, the target number of randomized participants would be %d per arm (total %d).",
    .fmt_pct(p$dropout), r$n_per_arm_randomized, r$n_total_randomized
  )
  drop_tail_1 <- sprintf(
    "Accounting for an anticipated dropout rate of %s%%, the target number of enrolled pairs would be %d.",
    .fmt_pct(p$dropout), r$n_per_arm_randomized
  )

  body <- switch(design_id,
    ttest_m1 = sprintf(paste(
      "For this study design, we assumed a mean of %.2f (SD %.2f) in Group A",
      "and %.2f (SD %.2f) in Group B. Assuming a two-sided significance level",
      "of %s and %d evaluable participants per arm (total %d), the achieved",
      "power of a two-sample t-test was calculated using the pwr.t.test",
      "function in the R package pwr (version %s). The achieved power was %s%%.",
      drop_tail,
      sep = "\n"),
      p$mean_A, p$sd_A, p$mean_B, p$sd_B,
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable, r$n_total_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    ttest_m2 = sprintf(paste(
      "For this study design, we assumed a between-group mean difference of",
      "%.2f with standard deviations of %.2f and %.2f in Groups A and B.",
      "Assuming a two-sided significance level of %s and %d evaluable",
      "participants per arm (total %d), the achieved power of a two-sample",
      "t-test was calculated using the pwr.t.test function in the R package",
      "pwr (version %s). The achieved power was %s%%.",
      drop_tail,
      sep = "\n"),
      p$diff, p$sd_A, p$sd_B,
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable, r$n_total_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    paired = sprintf(paste(
      "For this study design, we assumed a mean paired difference of %.2f",
      "with a standard deviation of %.2f. Assuming a two-sided significance",
      "level of %s and %d evaluable pairs, the achieved power of a paired",
      "t-test was calculated using the pwr.t.test function in the R package",
      "pwr (version %s). The achieved power was %s%%.",
      drop_tail_1,
      sep = "\n"),
      p$diff_mean, p$sd_diff,
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    paired_corr = sprintf(paste(
      "For this study design, we assumed a mean of %.2f (SD %.2f) at baseline",
      "and %.2f (SD %.2f) post-treatment, with a correlation of r = %.2f.",
      "The corresponding SD of paired differences was calculated as",
      "sqrt(%.2f^2 + %.2f^2 - 2 x %.2f x %.2f x %.2f) = %.2f (mean difference = %.2f).",
      "Assuming a two-sided significance level of %s and %d evaluable pairs,",
      "the achieved power of a paired t-test was calculated using the",
      "pwr.t.test function in the R package pwr (version %s).",
      "The achieved power was %s%%.",
      drop_tail_1,
      sep = "\n"),
      p$mean_1, p$sd_1, p$mean_2, p$sd_2, p$r,
      p$sd_1, p$sd_2, p$r, p$sd_1, p$sd_2,
      r$sd_diff %||% NA_real_, r$diff_mean %||% NA_real_,
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    binary_chisq = sprintf(paste(
      "For this study design, we assumed event rates of %s%% in Group A and",
      "%s%% in Group B. Assuming a two-sided significance level of %s and",
      "%d evaluable participants per arm (total %d), the achieved power of a",
      "chi-squared test was calculated using the pwr.2p.test function in the",
      "R package pwr (version %s). The achieved power was %s%%.",
      drop_tail,
      sep = "\n"),
      .fmt_pct(p$p_A), .fmt_pct(p$p_B),
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable, r$n_total_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    binary_fisher = sprintf(paste(
      "For this study design, we assumed event rates of %s%% in Group A and",
      "%s%% in Group B. Assuming a two-sided significance level of %s and",
      "%d evaluable participants per arm (total %d), the achieved power of",
      "Fisher's exact test was approximated using the chi-squared-based",
      "pwr.2p.test function in the R package pwr (version %s). The achieved",
      "power was %s%%.",
      drop_tail,
      sep = "\n"),
      .fmt_pct(p$p_A), .fmt_pct(p$p_B),
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable, r$n_total_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    ttest_ni = sprintf(paste(
      "For this study design, we assumed means of %.2f (SD %.2f) in Group A",
      "and %.2f (SD %.2f) in Group B, with a non-inferiority margin of %.2f.",
      "Assuming a one-sided significance level of %s and %d evaluable",
      "participants per arm (total %d), the achieved power of a one-sided",
      "two-sample t-test was calculated using pwr.t.test (alternative =",
      "'greater') in the R package pwr (version %s). The achieved power was %s%%.",
      drop_tail,
      sep = "\n"),
      p$mean_A, p$sd_A, p$mean_B, p$sd_B, p$margin,
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable, r$n_total_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    ttest_m2_ni = sprintf(paste(
      "For this study design, we assumed a between-group mean difference of",
      "%.2f with standard deviations of %.2f and %.2f in Groups A and B, and",
      "a non-inferiority margin of %.2f. Assuming a one-sided significance",
      "level of %s and %d evaluable participants per arm (total %d), the",
      "achieved power of a one-sided two-sample t-test was calculated using",
      "pwr.t.test (alternative = 'greater') in the R package pwr (version %s).",
      "The achieved power was %s%%.",
      drop_tail,
      sep = "\n"),
      p$diff, p$sd_A, p$sd_B, p$margin,
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable, r$n_total_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    paired_ni = sprintf(paste(
      "For this study design, we assumed a mean paired difference of %.2f,",
      "a SD of differences of %.2f, and a non-inferiority margin of %.2f.",
      "Assuming a one-sided significance level of %s and %d evaluable pairs,",
      "the achieved power of a one-sided paired t-test was calculated using",
      "pwr.t.test in the R package pwr (version %s). The achieved power was %s%%.",
      drop_tail_1,
      sep = "\n"),
      p$diff_mean, p$sd_diff, p$margin,
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable,
      get_pwr_version(), .fmt_power(r$achieved_power)
    ),
    binary_ni = sprintf(paste(
      "For this study design, we assumed event rates of %s%% in Group A and",
      "%s%% in Group B, with a non-inferiority margin of %s%% (on the",
      "risk-difference scale). Assuming a one-sided significance level of %s",
      "and %d evaluable participants per arm (total %d), the achieved power",
      "was calculated using the normal approximation of Chow et al. (2018,",
      "Section 4.2) implemented via stats::qnorm. The achieved power was %s%%.",
      drop_tail,
      sep = "\n"),
      .fmt_pct(p$p_A), .fmt_pct(p$p_B), .fmt_pct(p$margin),
      .fmt_alpha(p$alpha), r$n_per_arm_evaluable, r$n_total_evaluable,
      .fmt_power(r$achieved_power)
    ),
    .en_power_calc_generic(design_id, p, r, drop_tail, drop_tail_1)
  )
  body
}

# =========================================================================
# 日本語テンプレート
# =========================================================================

.jp_ttest_m1 <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "本研究の主要エンドポイントについて、介入群と 対照群の平均値の差を",
      "検出する 2 群並行群間比較試験を計画した。",
      "事前の情報（[文献を記載してください]）に基づき、",
      "介入群の平均を %.2f、対照群の平均を %.2f、",
      "介入群の SD を %.2f、対照群の SD を %.2f と想定した。",
      "両側有意水準 α=%s、検出力 1-β=%s%%、最終的な脱落割合 %s%% を",
      "考慮し、必要症例数を算出した結果、解析対象として各群 %d 例、",
      "登録必要例数は各群 %d 例（合計 %d 例）となった。",
      "必要症例数の公式は各群の SD を別々に用いる Welch 型の",
      "n = (z_{1-α/2} + z_{1-β})^2 × (σ_A^2 + σ_B^2) / Δ^2 に基づく。",
      sep = "\n"
    ),
    p$mean_A, p$mean_B, p$sd_A, p$sd_B,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable,
    r$n_per_arm_randomized, r$n_total_randomized
  )
  paste0(body, .cite_footer_jp("Section 3.2"))
}

.jp_ttest_m2 <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "本研究の主要エンドポイントについて、介入群と 対照群の平均値の差",
      "（群間差 Δ = %.2f）を検出する 2 群並行群間比較試験を計画した。",
      "事前の情報（[文献を記載してください]）に基づき、",
      "介入群の SD を %.2f、対照群の SD を %.2f と想定した。",
      "両側有意水準 α=%s、検出力 1-β=%s%%、最終的な脱落割合 %s%% を",
      "考慮し、必要症例数を算出した結果、解析対象として各群 %d 例、",
      "登録必要例数は各群 %d 例（合計 %d 例）となった。",
      "必要症例数の公式は各群の SD を別々に用いる Welch 型の",
      "n = (z_{1-α/2} + z_{1-β})^2 × (σ_A^2 + σ_B^2) / Δ^2 に基づく。",
      sep = "\n"
    ),
    p$diff, p$sd_A, p$sd_B,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable,
    r$n_per_arm_randomized, r$n_total_randomized
  )
  paste0(body, .cite_footer_jp("Section 3.2"))
}

.jp_paired <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "本研究の主要エンドポイントについて、同一被験者内の対応のある",
      "観測（例: 治療前後）の差を検出する試験を計画した。",
      "事前の情報（[文献を記載してください]）に基づき、",
      "差の平均を %.2f、差の SD を %.2f と想定した。",
      "両側有意水準 α=%s、検出力 1-β=%s%%、最終的な脱落割合 %s%% を",
      "考慮し、必要ペア数を算出した結果、解析対象として %d ペア、",
      "登録必要ペア数は %d ペアとなった。",
      sep = "\n"
    ),
    p$diff_mean, p$sd_diff,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized
  )
  paste0(body, .cite_footer_jp("Section 3.3"))
}

.jp_paired_corr <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "本研究の主要エンドポイントについて、同一被験者内の対応のある",
      "観測（例: 治療前後）の差を検出する試験を計画した。",
      "事前の情報（[文献を記載してください]）に基づき、",
      "治療前の平均を %.2f（SD %.2f）、治療後の平均を %.2f（SD %.2f）、",
      "前後の相関係数を %.2f と想定した。",
      "これより差の SD は",
      "sqrt(%.2f^2 + %.2f^2 − 2 × %.2f × %.2f × %.2f) = %.2f（差の平均 %.2f）",
      "と換算された。",
      "両側有意水準 α=%s、検出力 1-β=%s%%、最終的な脱落割合 %s%% を",
      "考慮し、必要ペア数を算出した結果、解析対象として %d ペア、",
      "登録必要ペア数は %d ペアとなった。",
      sep = "\n"
    ),
    p$mean_1, p$sd_1, p$mean_2, p$sd_2, p$r,
    p$sd_1, p$sd_2, p$r, p$sd_1, p$sd_2, r$sd_diff, r$diff_mean,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized
  )
  paste0(body, .cite_footer_jp("Section 3.3"))
}

.jp_binary_chisq <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "本研究の主要エンドポイントについて、介入群と 対照群の発生割合の差を",
      "検出する 2 群並行群間比較試験を計画した。",
      "事前の情報（[文献を記載してください]）に基づき、",
      "介入群の割合を %s%%、対照群の割合を %s%% と想定した。",
      "両側有意水準 α=%s、検出力 1-β=%s%%、最終的な脱落割合 %s%% を",
      "考慮し、χ² 検定（1:1 割付）に必要な症例数を算出した結果、",
      "解析対象として各群 %d 例、登録必要例数は各群 %d 例",
      "（合計 %d 例）となった。",
      sep = "\n"
    ),
    .fmt_pct(p$p_A), .fmt_pct(p$p_B),
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized, r$n_total_randomized
  )
  paste0(body, .cite_footer_jp("Section 4.2"))
}

.jp_binary_fisher <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "本研究の主要エンドポイントについて、介入群と 対照群の発生割合の差を",
      "Fisher の正確検定で検出する 2 群並行群間比較試験を計画した。",
      "事前の情報（[文献を記載してください]）に基づき、",
      "介入群の割合を %s%%、対照群の割合を %s%% と想定した。",
      "両側有意水準 α=%s、検出力 1-β=%s%%、最終的な脱落割合 %s%% を",
      "考慮し、必要症例数を算出した結果、解析対象として各群 %d 例、",
      "登録必要例数は各群 %d 例（合計 %d 例）となった。",
      "なお Fisher の正確検定の厳密な検出力計算は pwr に未搭載のため、",
      "χ² 検定ベースの近似を用いている。",
      sep = "\n"
    ),
    .fmt_pct(p$p_A), .fmt_pct(p$p_B),
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized, r$n_total_randomized
  )
  paste0(body, .cite_footer_jp("Section 5.2"))
}

.jp_one_mean <- function(p, r) {
  body <- sprintf(
    paste(
      "本研究の主要エンドポイントについて、平均値を 1 標本精度ベースで",
      "推定する試験を計画した。",
      "事前の情報（[文献を記載してください]）に基づき、SD を %.2f と想定し、",
      "%s%% 信頼区間の半幅 %s の精度で推定することを目標とした。",
      "必要症例数の公式 n = (z_{1-α/2} × SD / E)^2 を用い、",
      "最終的な脱落割合 %s%% を考慮して算出した結果、",
      "解析対象として %d 例、登録必要例数は %d 例となった。",
      sep = "\n"
    ),
    p$sd,
    .fmt_pct(p$conf_level), format(p$half_width),
    .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized
  )
  paste0(body, .cite_footer_jp("Section 3.1, pp. 50-57", pwr = FALSE))
}

.jp_one_prop <- function(p, r) {
  method_jp <- switch(p$method,
    normal = "正規近似",
    wilson = "Wilson 法",
    exact  = "Exact 法（Clopper-Pearson）"
  )
  body <- sprintf(
    paste(
      "本研究の主要エンドポイントについて、発生割合を 1 標本精度ベースで",
      "推定する試験を計画した。",
      "事前の情報（[文献を記載してください]）に基づき、",
      "予想される発生割合を %s%%、信頼区間の計算法は %s と設定し、",
      "%s%% 信頼区間の半幅 %s の精度で推定することを目標とした。",
      "最終的な脱落割合 %s%% を考慮して算出した結果、解析対象として %d 例、",
      "登録必要例数は %d 例となった。",
      sep = "\n"
    ),
    .fmt_pct(p$p), method_jp,
    .fmt_pct(p$conf_level), format(p$half_width),
    .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized
  )
  pwr_flag <- p$method != "normal"  # binom パッケージを使う
  footer <- if (pwr_flag) {
    paste(
      "",
      "計算は R (R Core Team, 2024) の binom パッケージ（Dorai-Raj, 2022）を用いて実行した。",
      "詳細な手法は Chow, Shao, Wang (2008) Sample Size Calculations in Clinical Research 2nd ed., Section 4.1 に準拠している。",
      sep = "\n"
    )
  } else {
    .cite_footer_jp("Section 4.1", pwr = FALSE)
  }
  paste0(body, footer)
}

.jp_ttest_m2_ni <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "本研究の主要エンドポイントについて、介入群（新治療）が 対照群（既存治療）",
      "に対して平均値で非劣性であることを示す 2 群並行群間比較試験を計画した。",
      "事前の情報（[文献を記載してください]）に基づき、",
      "介入群と 対照群の平均値の差（群間差 Δ）を %.2f、介入群の SD を %.2f、",
      "対照群の SD を %.2f と想定し、非劣性マージン M を %.2f と設定した。",
      "片側有意水準 α=%s、検出力 1-β=%s%%、最終的な脱落割合 %s%% を",
      "考慮し、必要症例数を算出した結果、解析対象として各群 %d 例、",
      "登録必要例数は各群 %d 例（合計 %d 例）となった。",
      "必要症例数の公式は各群の SD を別々に用いる",
      "n = (z_{1-α} + z_{1-β})^2 × (σ_A^2 + σ_B^2) / (Δ + M)^2 に基づく。",
      sep = "\n"
    ),
    p$diff, p$sd_A, p$sd_B, p$margin,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable,
    r$n_per_arm_randomized, r$n_total_randomized
  )
  paste0(body, .cite_footer_jp("Section 3.2"))
}

.jp_ttest_ni <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "本研究の主要エンドポイントについて、介入群（新治療）が 対照群（既存治療）",
      "に対して平均値で非劣性であることを示す 2 群並行群間比較試験を計画した。",
      "事前の情報（[文献を記載してください]）に基づき、",
      "介入群の平均を %.2f、対照群の平均を %.2f、介入群の SD を %.2f、",
      "対照群の SD を %.2f と想定し、非劣性マージン M を %.2f と設定した。",
      "片側有意水準 α=%s、検出力 1-β=%s%%、最終的な脱落割合 %s%% を",
      "考慮し、必要症例数を算出した結果、解析対象として各群 %d 例、",
      "登録必要例数は各群 %d 例（合計 %d 例）となった。",
      "必要症例数の公式は各群の SD を別々に用いる",
      "n = (z_{1-α} + z_{1-β})^2 × (σ_A^2 + σ_B^2) / (Δ + M)^2 に基づく",
      "（Δ = μ_A − μ_B）。",
      sep = "\n"
    ),
    p$mean_A, p$mean_B, p$sd_A, p$sd_B, p$margin,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable,
    r$n_per_arm_randomized, r$n_total_randomized
  )
  paste0(body, .cite_footer_jp("Section 3.2"))
}

.jp_paired_ni <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "本研究の主要エンドポイントについて、同一被験者内で新治療が既存治療",
      "に対して非劣性であることを示す試験を計画した。",
      "事前の情報（[文献を記載してください]）に基づき、差の平均を %.2f、",
      "差の SD を %.2f と想定し、非劣性マージン M を %.2f と設定した。",
      "片側有意水準 α=%s、検出力 1-β=%s%%、最終的な脱落割合 %s%% を",
      "考慮し、必要ペア数を算出した結果、解析対象として %d ペア、",
      "登録必要ペア数は %d ペアとなった。",
      sep = "\n"
    ),
    p$diff_mean, p$sd_diff, p$margin,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized
  )
  paste0(body, .cite_footer_jp("Section 3.3"))
}

.jp_binary_ni <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "本研究の主要エンドポイントについて、介入群（新治療）が 対照群（既存治療）",
      "に対して発生割合で非劣性であることを示す 2 群並行群間比較試験を計画した。",
      "事前の情報（[文献を記載してください]）に基づき、",
      "介入群の割合を %s%%、対照群の割合を %s%% と想定し、",
      "非劣性マージン M を %s%%（リスク差の単位）と設定した。",
      "片側有意水準 α=%s、検出力 1-β=%s%%、最終的な脱落割合 %s%% を",
      "考慮し、リスク差ベースの正規近似に基づき必要症例数を算出した結果、",
      "解析対象として各群 %d 例、登録必要例数は各群 %d 例",
      "（合計 %d 例）となった。",
      "なお、より正確な計算には Farrington-Manning 法の使用を推奨する。",
      sep = "\n"
    ),
    .fmt_pct(p$p_A), .fmt_pct(p$p_B), .fmt_pct(p$margin),
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized, r$n_total_randomized
  )
  paste0(body, .cite_footer_jp("Section 4.2", pwr = FALSE))
}

# =========================================================================
# 英語テンプレート
# =========================================================================

.en_ttest_m1 <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "For the primary endpoint, we planned a two-arm parallel-group trial",
      "to detect a difference in means between Group A and Group B.",
      "Based on prior evidence ([please insert reference]), we assumed a",
      "mean of %.2f in Group A and %.2f in Group B, with SDs of %.2f and",
      "%.2f, respectively. With a two-sided significance level of α=%s,",
      "power 1-β=%s%%, and an anticipated final dropout rate of %s%%, the",
      "calculation yielded %d evaluable participants per arm and a target",
      "enrolment of %d per arm (total %d). The sample-size formula uses",
      "each group's SD separately:",
      "n = (z_{1-α/2} + z_{1-β})^2 × (σ_A^2 + σ_B^2) / Δ^2.",
      sep = "\n"
    ),
    p$mean_A, p$mean_B, p$sd_A, p$sd_B,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable,
    r$n_per_arm_randomized, r$n_total_randomized
  )
  paste0(body, .cite_footer_en("Section 3.2"))
}

.en_ttest_m2 <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "For the primary endpoint, we planned a two-arm parallel-group trial",
      "to detect a between-group difference in means (Δ = %.2f).",
      "Based on prior evidence ([please insert reference]), we assumed",
      "SDs of %.2f and %.2f in Groups A and B, respectively. With a",
      "two-sided significance level of α=%s, power 1-β=%s%%, and an",
      "anticipated final dropout rate of %s%%, the calculation yielded",
      "%d evaluable participants per arm and a target enrolment of %d",
      "per arm (total %d). The sample-size formula uses each group's SD",
      "separately:",
      "n = (z_{1-α/2} + z_{1-β})^2 × (σ_A^2 + σ_B^2) / Δ^2.",
      sep = "\n"
    ),
    p$diff, p$sd_A, p$sd_B,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable,
    r$n_per_arm_randomized, r$n_total_randomized
  )
  paste0(body, .cite_footer_en("Section 3.2"))
}

.en_paired <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "For the primary endpoint, we planned a trial to detect a",
      "within-subject difference between paired observations (e.g. pre-",
      "and post-treatment). Based on prior evidence ([please insert",
      "reference]), we assumed a mean paired difference of %.2f and an",
      "SD of differences of %.2f. With a two-sided significance level",
      "of α=%s, power 1-β=%s%%, and an anticipated final dropout rate of",
      "%s%%, the calculation yielded %d evaluable pairs and a target",
      "enrolment of %d pairs.",
      sep = "\n"
    ),
    p$diff_mean, p$sd_diff,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized
  )
  paste0(body, .cite_footer_en("Section 3.3"))
}

.en_paired_corr <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "For the primary endpoint, we planned a trial to detect a",
      "within-subject difference between paired observations.",
      "Based on prior evidence ([please insert reference]), we assumed a",
      "mean of %.2f (SD %.2f) at baseline and %.2f (SD %.2f) post-",
      "treatment, with a correlation coefficient of r = %.2f. The SD of",
      "paired differences was calculated as",
      "sqrt(%.2f^2 + %.2f^2 - 2 x %.2f x %.2f x %.2f) = %.2f (mean",
      "difference = %.2f). With α=%s (two-sided), power 1-β=%s%%, and",
      "an anticipated final dropout rate of %s%%, the calculation yielded",
      "%d evaluable pairs and a target enrolment of %d pairs.",
      sep = "\n"
    ),
    p$mean_1, p$sd_1, p$mean_2, p$sd_2, p$r,
    p$sd_1, p$sd_2, p$r, p$sd_1, p$sd_2, r$sd_diff, r$diff_mean,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized
  )
  paste0(body, .cite_footer_en("Section 3.3"))
}

.en_binary_chisq <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "For the primary endpoint, we planned a two-arm parallel-group trial",
      "to detect a difference in event proportions between Group A and",
      "Group B using the chi-squared test.",
      "Based on prior evidence ([please insert reference]), we assumed",
      "event rates of %s%% in Group A and %s%% in Group B. With α=%s",
      "(two-sided), power 1-β=%s%%, and an anticipated final dropout rate",
      "of %s%%, the calculation yielded %d evaluable participants per arm",
      "and a target enrolment of %d per arm (total %d).",
      sep = "\n"
    ),
    .fmt_pct(p$p_A), .fmt_pct(p$p_B),
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized, r$n_total_randomized
  )
  paste0(body, .cite_footer_en("Section 4.2"))
}

.en_binary_fisher <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "For the primary endpoint, we planned a two-arm parallel-group trial",
      "to detect a difference in event proportions between Group A and",
      "Group B using Fisher's exact test.",
      "Based on prior evidence ([please insert reference]), we assumed",
      "event rates of %s%% in Group A and %s%% in Group B. With α=%s",
      "(two-sided), power 1-β=%s%%, and an anticipated final dropout rate",
      "of %s%%, the calculation yielded %d evaluable participants per arm",
      "and a target enrolment of %d per arm (total %d). As an exact-test",
      "power calculation is not implemented in pwr, a chi-squared-based",
      "approximation was used.",
      sep = "\n"
    ),
    .fmt_pct(p$p_A), .fmt_pct(p$p_B),
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized, r$n_total_randomized
  )
  paste0(body, .cite_footer_en("Section 5.2"))
}

.en_one_mean <- function(p, r) {
  body <- sprintf(
    paste(
      "For the primary endpoint, we planned a one-sample precision-based",
      "study to estimate a mean. Based on prior evidence ([please insert",
      "reference]), we assumed an SD of %.2f and targeted a %s%%",
      "confidence-interval half-width of %s.",
      "Using n = (z_{1-α/2} × SD / E)^2 and an anticipated final dropout",
      "rate of %s%%, the calculation yielded %d evaluable participants",
      "and a target enrolment of %d.",
      sep = "\n"
    ),
    p$sd,
    .fmt_pct(p$conf_level), format(p$half_width),
    .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized
  )
  paste0(body, .cite_footer_en("Section 3.1, pp. 50-57", pwr = FALSE))
}

.en_one_prop <- function(p, r) {
  method_en <- switch(p$method,
    normal = "the normal approximation",
    wilson = "Wilson's method",
    exact  = "the exact Clopper-Pearson method"
  )
  body <- sprintf(
    paste(
      "For the primary endpoint, we planned a one-sample precision-based",
      "study to estimate a proportion. Based on prior evidence ([please",
      "insert reference]), we assumed an expected proportion of %s%% and",
      "targeted a %s%% confidence-interval half-width of %s, computed",
      "using %s. With an anticipated final dropout rate of %s%%, the",
      "calculation yielded %d evaluable participants and a target",
      "enrolment of %d.",
      sep = "\n"
    ),
    .fmt_pct(p$p), .fmt_pct(p$conf_level), format(p$half_width),
    method_en, .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized
  )
  footer <- if (p$method != "normal") {
    paste(
      "",
      "Calculations were performed in R (R Core Team, 2024) using the binom package (Dorai-Raj, 2022).",
      "The methodology follows Chow, Shao, & Wang (2008), Sample Size Calculations in Clinical Research (2nd ed.), Section 4.1.",
      sep = "\n"
    )
  } else {
    .cite_footer_en("Section 4.1", pwr = FALSE)
  }
  paste0(body, footer)
}

.en_ttest_m2_ni <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "For the primary endpoint, we planned a two-arm parallel-group trial",
      "to demonstrate non-inferiority of Group A (new treatment) to",
      "Group B (standard of care). Based on prior evidence ([please insert",
      "reference]), we assumed a between-group mean difference of %.2f,",
      "SDs of %.2f and %.2f in Groups A and B, and pre-specified a",
      "non-inferiority margin of M = %.2f (on the mean-difference scale).",
      "With a one-sided significance level of α=%s, power 1-β=%s%%, and",
      "an anticipated final dropout rate of %s%%, the calculation yielded",
      "%d evaluable participants per arm and a target enrolment of %d per",
      "arm (total %d). The sample-size formula uses each group's SD",
      "separately:",
      "n = (z_{1-α} + z_{1-β})^2 × (σ_A^2 + σ_B^2) / (Δ + M)^2.",
      sep = "\n"
    ),
    p$diff, p$sd_A, p$sd_B, p$margin,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable,
    r$n_per_arm_randomized, r$n_total_randomized
  )
  paste0(body, .cite_footer_en("Section 3.2"))
}

.en_ttest_ni <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "For the primary endpoint, we planned a two-arm parallel-group trial",
      "to demonstrate non-inferiority of Group A (new treatment) to",
      "Group B (standard of care).",
      "Based on prior evidence ([please insert reference]), we assumed",
      "means of %.2f in Group A and %.2f in Group B, with SDs of %.2f and",
      "%.2f, and pre-specified a non-inferiority margin of M = %.2f.",
      "With α=%s (one-sided), power 1-β=%s%%, and an anticipated final",
      "dropout rate of %s%%, the calculation yielded %d evaluable",
      "participants per arm and a target enrolment of %d per arm",
      "(total %d). The sample-size formula uses each group's SD separately:",
      "n = (z_{1-α} + z_{1-β})^2 × (σ_A^2 + σ_B^2) / (Δ + M)^2",
      "(Δ = μ_A − μ_B).",
      sep = "\n"
    ),
    p$mean_A, p$mean_B, p$sd_A, p$sd_B, p$margin,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable,
    r$n_per_arm_randomized, r$n_total_randomized
  )
  paste0(body, .cite_footer_en("Section 3.2"))
}

.en_paired_ni <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "For the primary endpoint, we planned a within-subject comparison",
      "to demonstrate non-inferiority of the new treatment to the",
      "standard treatment. Based on prior evidence ([please insert",
      "reference]), we assumed a mean paired difference of %.2f with an",
      "SD of differences of %.2f, and pre-specified a non-inferiority",
      "margin of M = %.2f.",
      "With α=%s (one-sided), power 1-β=%s%%, and an anticipated final",
      "dropout rate of %s%%, the calculation yielded %d evaluable pairs",
      "and a target enrolment of %d pairs.",
      sep = "\n"
    ),
    p$diff_mean, p$sd_diff, p$margin,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized
  )
  paste0(body, .cite_footer_en("Section 3.3"))
}

.en_binary_ni <- function(p, r, pw) {
  body <- sprintf(
    paste(
      "For the primary endpoint, we planned a two-arm parallel-group trial",
      "to demonstrate non-inferiority of Group A (new treatment) to",
      "Group B (standard of care) in event proportions.",
      "Based on prior evidence ([please insert reference]), we assumed",
      "event rates of %s%% in Group A and %s%% in Group B, and",
      "pre-specified a non-inferiority margin of M = %s%% (on the",
      "risk-difference scale). With α=%s (one-sided), power 1-β=%s%%,",
      "and an anticipated final dropout rate of %s%%, the calculation",
      "using a normal-approximation risk-difference formula yielded",
      "%d evaluable participants per arm and a target enrolment of %d",
      "per arm (total %d). Note that a more accurate calculation using",
      "the Farrington-Manning method is recommended.",
      sep = "\n"
    ),
    .fmt_pct(p$p_A), .fmt_pct(p$p_B), .fmt_pct(p$margin),
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized, r$n_total_randomized
  )
  paste0(body, .cite_footer_en("Section 4.2", pwr = FALSE))
}

# =========================================================================
# 新規デザイン（Phase 2 / 3）の論文テンプレート
# =========================================================================

# ---- D1 McNemar ----
.jp_mcnemar <- function(p, r, pw) {
  body <- sprintf(paste(
    "本研究の主要エンドポイントについて、対応のある二値アウトカムの",
    "前後（または 2 条件間）の発生割合の差を McNemar 検定で検出する",
    "試験を計画した。事前の情報（[文献を記載してください]）に基づき、",
    "不一致ペアの割合 p_disc を %.3f、そのうち A が優位である割合 ψ を %.2f",
    "と仮定した。",
    "両側有意水準 α=%s、検出力 1-β=%s%%、最終的な脱落割合 %s%% を",
    "考慮し、Connor (1987) の公式に基づき必要ペア数を算出した結果、",
    "解析対象として %d ペア、登録必要ペア数は %d ペアとなった。",
    sep = "\n"),
    p$p_disc, p$psi,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized
  )
  paste0(body, .cite_footer_jp(
    section_text = NULL, pwr = FALSE,
    alt_text = "必要ペア数の公式は Connor, R. J. (1987), Biometrics 43: 207-211 に準拠している。"
  ))
}
.en_mcnemar <- function(p, r, pw) {
  body <- sprintf(paste(
    "For the primary endpoint, we planned a study to detect a difference",
    "in a paired-binary outcome between two conditions using McNemar's",
    "test. Based on prior evidence ([please insert reference]), we assumed",
    "a discordant-pair proportion of %.3f (p_disc) and a proportion of",
    "%.2f (ψ) favouring A among discordant pairs.",
    "With α=%s (two-sided), power 1-β=%s%%, and an anticipated final",
    "dropout rate of %s%%, the calculation yielded %d evaluable pairs and",
    "a target enrolment of %d pairs.",
    sep = "\n"),
    p$p_disc, p$psi,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$n_per_arm_evaluable, r$n_per_arm_randomized
  )
  paste0(body, .cite_footer_en(
    section_text = NULL, pwr = FALSE,
    alt_text = "The sample-size formula follows Connor, R. J. (1987), Biometrics 43: 207-211."
  ))
}

# ---- D2 ANCOVA ----
.jp_ancova <- function(p, r, pw) {
  sprintf(paste(
    "本研究では、連続量アウトカム [アウトカム名を指定] における 2 群の",
    "平均値の差を、共変量（例: ベースライン測定値）で調整した ANCOVA",
    "モデルで検定することを主目的とした。",
    "過去の知見 [参考文献を挿入] から、介入群平均を %.2f、対照群平均を %.2f、",
    "共通 SD を %.2f、共変量とアウトカムの相関係数を %.2f と想定した。",
    "両側有意水準 %s、目標検出力 %s%% のもと、Borm ら (2007) の分散低減",
    "公式（SD_adj = SD × sqrt(1 - r²)）を用い、pwr パッケージ（version %s）",
    "の pwr.t.test 関数により必要症例数を計算した。",
    "その結果、解析対象として各群 %d 例（合計 %d 例）が必要であった。",
    "脱落率を %s%% と見込む場合、登録必要症例数は各群 %d 例（合計 %d 例）となる。",
    sep = "\n"),
    p$mean_A, p$mean_B, p$sd_common, p$r,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable, r$n_total_evaluable,
    .fmt_pct(p$dropout), r$n_per_arm_randomized, r$n_total_randomized
  )
}
.en_ancova <- function(p, r, pw) {
  sprintf(paste(
    "The primary objective was to compare the continuous outcome [please",
    "specify] between two groups using an ANCOVA model adjusted for a",
    "covariate (e.g. baseline measurement). Based on prior evidence [please",
    "insert reference], we assumed means of %.2f and %.2f in Groups A and",
    "B with a common SD of %.2f, and a correlation of %.2f between the",
    "covariate and the outcome. Assuming a two-sided significance level of",
    "%s and a power of %s%%, the required sample size was calculated using",
    "the variance-reduction formula of Borm et al. (2007) (SD_adj = SD ×",
    "sqrt(1 - r²)) and pwr.t.test in the R package pwr (version %s).",
    "The calculation indicated that %d evaluable participants per arm (total",
    "%d) would be required. Accounting for an anticipated dropout rate of",
    "%s%%, the target number of randomized participants was set to %d per",
    "arm (total %d).",
    sep = "\n"),
    p$mean_A, p$mean_B, p$sd_common, p$r,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable, r$n_total_evaluable,
    .fmt_pct(p$dropout), r$n_per_arm_randomized, r$n_total_randomized
  )
}

# ---- D3 log-rank ----
.jp_logrank <- function(p, r, pw) {
  HR_used <- r$HR %||% p$HR %||% NA_real_
  mode <- p$input_mode %||% (if (!is.null(p$median_T)) "mst" else "hr")
  assumption <- if (mode == "mst") {
    sprintf(paste0("対照群の中央生存期間を %.1f、治療群の中央生存期間を %.1f と想定",
                   "（指数分布仮定により HR = %.3f と換算）"),
            p$median_C, p$median_T %||% NA_real_, HR_used)
  } else {
    sprintf("対照群の中央生存期間を %.1f、ハザード比を %.2f と想定",
            p$median_C, HR_used)
  }
  alloc_str <- sprintf("%.2g:1（治療群:対照群）", p$allocation_ratio %||% 1)
  body <- sprintf(paste(
    "本研究の主要エンドポイントである生存時間アウトカムについて、2 群の",
    "生存関数の差を log-rank 検定で検出する試験を計画した。",
    "事前の情報（[文献を記載してください]）に基づき、%s し、",
    "登録期間 %.1f、追跡期間 %.1f を仮定（指数分布、一様リクルート）した。",
    "割付比は %s、片側有意水準 α=%s、検出力 1-β=%s%%、",
    "最終的な脱落割合 %s%% を考慮し、必要イベント数および総症例数を算出した結果、",
    "必要イベント数 %d 件、総症例数 %d 例（各群 %d 例）、",
    "登録必要例数は各群 %d 例（合計 %d 例）となった。",
    sep = "\n"),
    assumption, p$accrual, p$followup, alloc_str,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$events_required %||% NA, r$n_total_evaluable, r$n_per_arm_evaluable,
    r$n_per_arm_randomized, r$n_total_randomized
  )
  ref_alt <- if (mode == "mst") {
    paste("必要イベント数の公式は Schoenfeld, D. A. (1981), Biometrika",
          "68: 316-319 に基づき、指数分布下での MST-HR 換算は Lakatos, E.",
          "(1988), Biometrics 44: 229-241 の簡易版に準拠した。",
          "詳細は Chow, Shao, Wang (2008) Sample Size Calculations in",
          "Clinical Research 2nd ed., Section 7.4 に依拠している。")
  } else {
    paste("必要イベント数の公式は Schoenfeld, D. A. (1981), Biometrika",
          "68: 316-319 に基づき、Chow, Shao, Wang (2008) Sample Size",
          "Calculations in Clinical Research 2nd ed., Section 7.4 に準拠している。")
  }
  paste0(body, .cite_footer_jp(
    section_text = NULL, pwr = FALSE, alt_text = ref_alt
  ))
}
.en_logrank <- function(p, r, pw) {
  HR_used <- r$HR %||% p$HR %||% NA_real_
  mode <- p$input_mode %||% (if (!is.null(p$median_T)) "mst" else "hr")
  assumption <- if (mode == "mst") {
    sprintf(paste0("a median survival of %.1f in the control arm and %.1f ",
                   "in the treatment arm (giving HR = %.3f under an ",
                   "exponential assumption)"),
            p$median_C, p$median_T %||% NA_real_, HR_used)
  } else {
    sprintf("a median survival of %.1f in the control arm and a hazard ratio of %.2f",
            p$median_C, HR_used)
  }
  alloc_str <- sprintf("%.2g:1 (treatment:control)", p$allocation_ratio %||% 1)
  body <- sprintf(paste(
    "For the primary time-to-event endpoint, we planned a trial comparing",
    "survival between two groups using the log-rank test. Based on prior",
    "evidence ([please insert reference]), we assumed %s,",
    "with an enrolment period of %.1f and additional follow-up of %.1f",
    "(exponential survival, uniform recruitment). The allocation ratio was %s.",
    "With α=%s (one-sided), power 1-β=%s%%, and an anticipated final",
    "dropout rate of %s%%, the calculation yielded %d required events and",
    "a total of %d evaluable participants (%d per arm), with a target",
    "enrolment of %d per arm (total %d).",
    sep = "\n"),
    assumption, p$accrual, p$followup, alloc_str,
    .fmt_alpha(p$alpha), .fmt_pct(pw), .fmt_pct(p$dropout),
    r$events_required %||% NA, r$n_total_evaluable, r$n_per_arm_evaluable,
    r$n_per_arm_randomized, r$n_total_randomized
  )
  ref_alt <- if (mode == "mst") {
    paste("The number-of-events formula follows Schoenfeld, D. A. (1981),",
          "Biometrika 68: 316-319, and the MST-to-HR conversion uses a",
          "simplified version of Lakatos, E. (1988), Biometrics 44: 229-241.",
          "Details follow Chow, Shao, & Wang (2008), Sample Size Calculations",
          "in Clinical Research (2nd ed.), Section 7.4.")
  } else {
    paste("The number-of-events formula follows Schoenfeld, D. A. (1981),",
          "Biometrika 68: 316-319, and Chow, Shao, & Wang (2008), Sample",
          "Size Calculations in Clinical Research (2nd ed.), Section 7.4.")
  }
  paste0(body, .cite_footer_en(
    section_text = NULL, pwr = FALSE, alt_text = ref_alt
  ))
}

# ---- D4 longitudinal ----
.jp_longitudinal <- function(p, r, pw) {
  sprintf(paste(
    "本研究では、%d 時点の反復測定による連続量アウトカム [アウトカム名を",
    "指定] を、両群の post-baseline 平均値で比較することを主目的とした。",
    "過去の知見 [参考文献を挿入] から、介入群平均を %.2f、対照群平均を %.2f、",
    "1 時点の SD を %.2f、被験者内相関（compound symmetry）を %.2f と",
    "想定した。両側有意水準 %s、目標検出力 %s%% のもと、Diggle ら (2002)",
    "による平均の有効 SD 公式（SD × sqrt((1 + (k-1)ρ)/k)）を用い、",
    "pwr パッケージ（version %s）の pwr.t.test 関数により必要症例数を",
    "計算した。その結果、解析対象として各群 %d 例（合計 %d 例）が必要であった。",
    "脱落率を %s%% と見込む場合、登録必要症例数は各群 %d 例（合計 %d 例）となる。",
    sep = "\n"),
    as.integer(p$k), p$mean_A, p$mean_B, p$sd_common, p$rho,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable, r$n_total_evaluable,
    .fmt_pct(p$dropout), r$n_per_arm_randomized, r$n_total_randomized
  )
}
.en_longitudinal <- function(p, r, pw) {
  sprintf(paste(
    "The primary objective was to compare post-baseline means of a",
    "continuous outcome [please specify], measured %d times, between two",
    "groups. Based on prior evidence [please insert reference], we assumed",
    "means of %.2f and %.2f in Groups A and B with a common SD of %.2f at",
    "each time point, and a within-subject correlation (compound symmetry)",
    "of %.2f. Assuming a two-sided significance level of %s and a power of",
    "%s%%, the required sample size was calculated using the effective-SD",
    "formula of Diggle et al. (2002) (SD × sqrt((1 + (k-1)ρ)/k)) and",
    "pwr.t.test in the R package pwr (version %s). The calculation",
    "indicated that %d evaluable participants per arm (total %d) would be",
    "required. Accounting for an anticipated dropout rate of %s%%, the",
    "target number of randomized participants was set to %d per arm",
    "(total %d).",
    sep = "\n"),
    as.integer(p$k), p$mean_A, p$mean_B, p$sd_common, p$rho,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable, r$n_total_evaluable,
    .fmt_pct(p$dropout), r$n_per_arm_randomized, r$n_total_randomized
  )
}

# ---- D5 group sequential ----
.jp_group_sequential <- function(p, r, pw) {
  sprintf(paste(
    "本研究では、連続量アウトカム [アウトカム名を指定] における 2 群比較",
    "を、%s 境界を用いた %d 回の解析（最終解析を含む）による群逐次デザインで",
    "実施することとした。過去の知見 [参考文献を挿入] から、介入群平均を %.2f、",
    "対照群平均を %.2f、共通 SD を %.2f と想定した。両側有意水準 %s、",
    "目標検出力 %s%% のもと、固定デザインの必要症例数を pwr パッケージ",
    "（version %s）の pwr.t.test 関数で算出し、Jennison & Turnbull (2000)",
    "による inflation factor（%s 境界、K=%d）%.3f を適用した。",
    "その結果、解析対象として各群 %d 例（合計 %d 例）が必要であった。",
    "脱落率を %s%% と見込む場合、登録必要症例数は各群 %d 例（合計 %d 例）となる。",
    sep = "\n"),
    p$boundary, as.integer(p$K),
    p$mean_A, p$mean_B, p$sd_common,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    p$boundary, as.integer(p$K), r$inflation %||% NA_real_,
    r$n_per_arm_evaluable, r$n_total_evaluable,
    .fmt_pct(p$dropout), r$n_per_arm_randomized, r$n_total_randomized
  )
}
.en_group_sequential <- function(p, r, pw) {
  sprintf(paste(
    "The trial used a group-sequential design with %d analyses (including",
    "the final analysis) and %s boundaries for a two-group comparison of",
    "a continuous outcome [please specify]. Based on prior evidence [please",
    "insert reference], we assumed means of %.2f and %.2f with a common SD",
    "of %.2f. Assuming a two-sided overall significance level of %s and a",
    "power of %s%%, the fixed-design sample size was calculated via",
    "pwr.t.test in the R package pwr (version %s), and the inflation factor",
    "of Jennison & Turnbull (2000) for the %s boundary at K=%d (%.3f) was",
    "applied. The calculation indicated that %d evaluable participants per",
    "arm (total %d) would be required. Accounting for an anticipated",
    "dropout rate of %s%%, the target number of randomized participants",
    "was set to %d per arm (total %d).",
    sep = "\n"),
    as.integer(p$K), p$boundary,
    p$mean_A, p$mean_B, p$sd_common,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    p$boundary, as.integer(p$K), r$inflation %||% NA_real_,
    r$n_per_arm_evaluable, r$n_total_evaluable,
    .fmt_pct(p$dropout), r$n_per_arm_randomized, r$n_total_randomized
  )
}

# ---- D6 cluster (continuous) ----
.jp_cluster_cont <- function(p, r, pw) {
  sprintf(paste(
    "本研究は、連続量アウトカム [アウトカム名を指定] を主要評価項目とした",
    "クラスターランダム化比較試験として計画した。",
    "過去の知見 [参考文献を挿入] から、介入群平均を %.2f、対照群平均を %.2f、",
    "共通 SD を %.2f、平均クラスターサイズを %d、級内相関係数 ICC を %.3f",
    "と想定した。両側有意水準 %s、目標検出力 %s%% のもと、Donner & Klar (2000)",
    "に基づき Design Effect DE = 1 + (m-1)ICC = %.3f を適用し、",
    "pwr パッケージ（version %s）の pwr.t.test 関数による iid 必要症例数に",
    "DE を掛けて必要症例数を算出した。",
    "その結果、解析対象として各群 %d 例、必要クラスター数は各群 %d と",
    "算出された。脱落率を %s%% と見込む場合、登録必要症例数は各群 %d 例",
    "（合計 %d 例）となる。",
    sep = "\n"),
    p$mean_A, p$mean_B, p$sd_common, as.integer(p$m), p$ICC,
    .fmt_alpha(p$alpha), .fmt_pct(pw),
    r$DE %||% NA_real_, get_pwr_version(),
    r$n_per_arm_evaluable, as.integer(r$K_per_arm %||% 0L),
    .fmt_pct(p$dropout), r$n_per_arm_randomized, r$n_total_randomized
  )
}
.en_cluster_cont <- function(p, r, pw) {
  sprintf(paste(
    "The trial was designed as a cluster-randomised controlled trial with a",
    "continuous primary outcome [please specify]. Based on prior evidence",
    "[please insert reference], we assumed means of %.2f and %.2f with a",
    "common SD of %.2f, an average cluster size of %d, and an intra-cluster",
    "correlation (ICC) of %.3f. Assuming a two-sided significance level of",
    "%s and a power of %s%%, the required sample size was obtained by",
    "multiplying the iid sample size from pwr.t.test (pwr version %s) by",
    "the design effect DE = 1 + (m-1)ICC = %.3f (Donner & Klar 2000).",
    "The calculation indicated that %d evaluable participants per arm and",
    "%d clusters per arm would be required. Accounting for an anticipated",
    "dropout rate of %s%%, the target number of randomized participants was",
    "set to %d per arm (total %d).",
    sep = "\n"),
    p$mean_A, p$mean_B, p$sd_common, as.integer(p$m), p$ICC,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$DE %||% NA_real_,
    r$n_per_arm_evaluable, as.integer(r$K_per_arm %||% 0L),
    .fmt_pct(p$dropout), r$n_per_arm_randomized, r$n_total_randomized
  )
}

# ---- D6 cluster (binary) ----
.jp_cluster_bin <- function(p, r, pw) {
  sprintf(paste(
    "本研究は、二値アウトカム [アウトカム名を指定] を主要評価項目とした",
    "クラスターランダム化比較試験として計画した。",
    "過去の知見 [参考文献を挿入] から、介入群の発生割合を %s%%、対照群の",
    "発生割合を %s%%、平均クラスターサイズを %d、級内相関係数 ICC を %.3f",
    "と想定した。両側有意水準 %s、目標検出力 %s%% のもと、Donner & Klar (2000)",
    "に基づき Design Effect DE = 1 + (m-1)ICC = %.3f を適用し、",
    "pwr パッケージ（version %s）の pwr.2p.test 関数による iid 必要症例数に",
    "DE を掛けて必要症例数を算出した。",
    "その結果、解析対象として各群 %d 例、必要クラスター数は各群 %d と",
    "算出された。脱落率を %s%% と見込む場合、登録必要症例数は各群 %d 例",
    "（合計 %d 例）となる。",
    sep = "\n"),
    .fmt_pct(p$p_A), .fmt_pct(p$p_B), as.integer(p$m), p$ICC,
    .fmt_alpha(p$alpha), .fmt_pct(pw),
    r$DE %||% NA_real_, get_pwr_version(),
    r$n_per_arm_evaluable, as.integer(r$K_per_arm %||% 0L),
    .fmt_pct(p$dropout), r$n_per_arm_randomized, r$n_total_randomized
  )
}
.en_cluster_bin <- function(p, r, pw) {
  sprintf(paste(
    "The trial was designed as a cluster-randomised controlled trial with a",
    "binary primary outcome [please specify]. Based on prior evidence",
    "[please insert reference], we assumed event proportions of %s%% and",
    "%s%% in Groups A and B, an average cluster size of %d, and an",
    "intra-cluster correlation (ICC) of %.3f. Assuming a two-sided",
    "significance level of %s and a power of %s%%, the required sample",
    "size was obtained by multiplying the iid sample size from pwr.2p.test",
    "(pwr version %s) by the design effect DE = 1 + (m-1)ICC = %.3f",
    "(Donner & Klar 2000). The calculation indicated that %d evaluable",
    "participants per arm and %d clusters per arm would be required.",
    "Accounting for an anticipated dropout rate of %s%%, the target number",
    "of randomized participants was set to %d per arm (total %d).",
    sep = "\n"),
    .fmt_pct(p$p_A), .fmt_pct(p$p_B), as.integer(p$m), p$ICC,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$DE %||% NA_real_,
    r$n_per_arm_evaluable, as.integer(r$K_per_arm %||% 0L),
    .fmt_pct(p$dropout), r$n_per_arm_randomized, r$n_total_randomized
  )
}

# ---- D7 diagnostic ----
.jp_diagnostic <- function(p, r) {
  body <- sprintf(paste(
    "本研究の主要エンドポイントについて、診断検査の精度（感度 Se・特異度 Sp）",
    "を指定した信頼区間の半幅で推定する試験を計画した。",
    "事前の情報（[文献を記載してください]）に基づき、予想感度を %s%%、",
    "予想特異度を %s%%、対象集団の有病率を %s%% と想定し、%s%% 信頼区間",
    "の目標半幅を %.3f とした。最終的な脱落割合 %s%% を考慮し、",
    "Buderer (1996) の正規近似に基づき必要症例数を算出した結果、",
    "解析対象として %d 例（疾患あり %d 例、疾患なし %d 例の要件を満たす）、",
    "登録必要例数は %d 例となった。",
    sep = "\n"),
    .fmt_pct(p$Se), .fmt_pct(p$Sp), .fmt_pct(p$prev),
    .fmt_pct(p$conf_level), p$half_width, .fmt_pct(p$dropout),
    r$n_per_arm_evaluable,
    r$n_dis_required %||% 0L, r$n_non_required %||% 0L,
    r$n_per_arm_randomized
  )
  paste0(body, .cite_footer_jp(
    section_text = NULL, pwr = FALSE,
    alt_text = "必要症例数の公式は Buderer, N. M. (1996), Acad Emerg Med 3: 895-900 に準拠している。"
  ))
}
.en_diagnostic <- function(p, r) {
  body <- sprintf(paste(
    "For the primary endpoint, we planned a study to estimate the",
    "diagnostic accuracy (both sensitivity and specificity) of a test.",
    "Based on prior evidence ([please insert reference]), we assumed an",
    "expected sensitivity of %s%%, specificity of %s%%, and prevalence of",
    "%s%%, targeting a %s%% confidence-interval half-width of %.3f.",
    "With an anticipated final dropout rate of %s%%, the normal-",
    "approximation calculation yielded %d evaluable participants",
    "(satisfying %d diseased and %d non-diseased participants) and a",
    "target enrolment of %d.",
    sep = "\n"),
    .fmt_pct(p$Se), .fmt_pct(p$Sp), .fmt_pct(p$prev),
    .fmt_pct(p$conf_level), p$half_width, .fmt_pct(p$dropout),
    r$n_per_arm_evaluable,
    r$n_dis_required %||% 0L, r$n_non_required %||% 0L,
    r$n_per_arm_randomized
  )
  paste0(body, .cite_footer_en(
    section_text = NULL, pwr = FALSE,
    alt_text = "The sample-size formula follows Buderer, N. M. (1996), Acad Emerg Med 3: 895-900."
  ))
}

# ---- D8 Mann-Whitney ----
.jp_mann_whitney <- function(p, r, pw) {
  dist_label <- switch(p$distribution %||% "normal",
    normal = "正規分布", lognormal = "対数正規分布",
    exponential = "指数分布", p$distribution)
  sprintf(paste(
    "本研究では、連続量アウトカム [アウトカム名を指定] における 2 群の",
    "位置パラメータを Mann-Whitney U 検定（Wilcoxon ランク和検定）で",
    "比較することを主目的とした。過去の知見 [参考文献を挿入] から、",
    "介入群の平均を %.2f、対照群の平均を %.2f、共通 SD を %.2f、分布形状は",
    "%s と想定した。両側有意水準 %s、目標検出力 %s%% のもと、",
    "pwr パッケージ（version %s）の pwr.t.test 関数による t 検定必要例数を",
    "ARE = %.3f（Hollander & Wolfe 1999）で補正して算出した。",
    "その結果、解析対象として各群 %d 例（合計 %d 例）が必要であった。",
    "脱落率を %s%% と見込む場合、登録必要症例数は各群 %d 例（合計 %d 例）となる。",
    sep = "\n"),
    p$mean_A, p$mean_B, p$sd_common, dist_label,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$ARE %||% NA_real_,
    r$n_per_arm_evaluable, r$n_total_evaluable,
    .fmt_pct(p$dropout), r$n_per_arm_randomized, r$n_total_randomized
  )
}
.en_mann_whitney <- function(p, r, pw) {
  dist_label <- switch(p$distribution %||% "normal",
    normal = "normal", lognormal = "log-normal",
    exponential = "exponential", p$distribution)
  sprintf(paste(
    "The primary objective was to compare the location of a continuous",
    "outcome [please specify] between two groups using the Mann-Whitney U",
    "test (Wilcoxon rank-sum test). Based on prior evidence [please insert",
    "reference], we assumed means of %.2f and %.2f in Groups A and B with",
    "a common SD of %.2f, and a %s distributional shape. Assuming a",
    "two-sided significance level of %s and a power of %s%%, the required",
    "sample size was obtained by dividing the two-sample t-test sample size",
    "(computed via pwr.t.test in pwr version %s) by the asymptotic relative",
    "efficiency (ARE) of %.3f (Hollander & Wolfe 1999). The calculation",
    "indicated that %d evaluable participants per arm (total %d) would be",
    "required. Accounting for an anticipated dropout rate of %s%%, the",
    "target number of randomized participants was set to %d per arm",
    "(total %d).",
    sep = "\n"),
    p$mean_A, p$mean_B, p$sd_common, dist_label,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$ARE %||% NA_real_,
    r$n_per_arm_evaluable, r$n_total_evaluable,
    .fmt_pct(p$dropout), r$n_per_arm_randomized, r$n_total_randomized
  )
}

# =========================================================================
# 参考文献（設計別）
#
# 3 層構造:
#   1. 公式の原典（Chow 該当セクション、または Chow に詳述のない
#      デザインでは原典論文）
#   2. 関連する古典文献（Cohen 1988, Fleiss 1981, Schoenfeld 1981 など）
#   3. 使用ソフトウェア（R Core Team, pwr, binom など）
# =========================================================================
paper_references <- function(design_id) {
  chow_cite <- "Chow, S.-C., Shao, J., & Wang, H. (2008). Sample Size Calculations in Clinical Research (2nd ed.). Chapman & Hall/CRC."
  r_cite    <- "R Core Team (2024). R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing."
  pwr_cite  <- "Champely, S. (2020). pwr: Basic Functions for Power Analysis. R package version 1.3-0."
  binom_cite <- "Dorai-Raj, S. (2022). binom: Binomial Confidence Intervals for Several Parameterizations. R package."

  # 1. 主引用（公式の原典）
  primary <- switch(design_id,
    ttest_m1      = sprintf("[主引用] %s Section 3.2, pp. 57-65.", chow_cite),
    ttest_m2      = sprintf("[主引用] %s Section 3.2, pp. 57-65.", chow_cite),
    paired        = sprintf("[主引用] %s Section 3.3, pp. 65-70.", chow_cite),
    paired_corr   = sprintf("[主引用] %s Section 3.3, pp. 65-70.", chow_cite),
    binary_chisq  = sprintf("[主引用] %s Section 4.2, pp. 89-95.", chow_cite),
    binary_fisher = sprintf("[主引用] %s Section 5.2, pp. 121-124.", chow_cite),
    one_mean      = sprintf("[主引用] %s Section 3.1, pp. 50-57; Section 1.3.2, pp. 15-16.", chow_cite),
    one_prop      = sprintf("[主引用] %s Section 4.1, pp. 84-89.", chow_cite),
    ttest_ni      = sprintf("[主引用] %s Section 3.2, pp. 57-65.", chow_cite),
    ttest_m2_ni   = sprintf("[主引用] %s Section 3.2, pp. 57-65.", chow_cite),
    paired_ni     = sprintf("[主引用] %s Section 3.3, pp. 65-70.", chow_cite),
    binary_ni     = sprintf("[主引用] %s Section 4.2, pp. 89-95.", chow_cite),
    logrank       = sprintf("[主引用] %s Section 7.4, pp. 179-185.", chow_cite),
    mcnemar       = "[主引用] Connor, R. J. (1987). Sample size for testing differences in proportions for the paired-sample design. Biometrics 43: 207-211.",
    ancova        = "[主引用] Borm, G. F., Fransen, J., Lemmens, W. A. J. G. (2007). A simple sample size formula for analysis of covariance in randomized clinical trials. J Clin Epidemiol 60: 1234-1238.",
    longitudinal  = "[主引用] Diggle, P. J., Liang, K. Y., Zeger, S. L. (2002). Analysis of Longitudinal Data (2nd ed.). Oxford University Press.",
    group_sequential = "[主引用] Jennison, C., Turnbull, B. W. (2000). Group Sequential Methods with Applications to Clinical Trials. Chapman & Hall/CRC.",
    cluster_cont  = "[主引用] Donner, A., Klar, N. (2000). Design and Analysis of Cluster Randomization Trials in Health Research. Arnold.",
    cluster_bin   = "[主引用] Donner, A., Klar, N. (2000). Design and Analysis of Cluster Randomization Trials in Health Research. Arnold.",
    diagnostic    = "[主引用] Buderer, N. M. (1996). Statistical methodology: I. Incorporating the prevalence of disease into the sample size calculation for sensitivity and specificity. Acad Emerg Med 3: 895-900.",
    mann_whitney  = "[主引用] Hollander, M., Wolfe, D. A. (1999). Nonparametric Statistical Methods (2nd ed.). Wiley.",
    NULL
  )

  # 2. 関連する古典文献
  related_list <- switch(design_id,
    ttest_m1      = c("Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Routledge."),
    ttest_m2      = c("Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Routledge."),
    paired        = c("Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Routledge."),
    paired_corr   = c("Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Routledge."),
    binary_chisq  = c("Fleiss, J. L. (1981). Statistical Methods for Rates and Proportions (2nd ed.). Wiley.",
                      "Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Routledge."),
    binary_fisher = c("Casagrande, J. T., Pike, M. C., Smith, P. G. (1978). An improved approximate formula for calculating sample sizes for comparing two binomial distributions. Biometrics 34: 483-486.",
                      "Fleiss, J. L. (1981). Statistical Methods for Rates and Proportions (2nd ed.). Wiley."),
    one_mean      = c("Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Routledge."),
    one_prop      = c("Wilson, E. B. (1927). Probable inference, the law of succession, and statistical inference. JASA 22: 209-212.",
                      "Clopper, C. J., Pearson, E. S. (1934). The use of confidence or fiducial limits. Biometrika 26: 404-413."),
    ttest_ni      = c("Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Routledge."),
    ttest_m2_ni   = c("Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Routledge."),
    paired_ni     = c("Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Routledge."),
    binary_ni     = c("Fleiss, J. L. (1981). Statistical Methods for Rates and Proportions (2nd ed.). Wiley.",
                      "Farrington, C. P., Manning, G. (1990). Test statistics and sample size formulae for comparative binomial trials with null hypothesis of non-zero risk difference or non-unity relative risk. Stat Med 9: 1447-1454."),
    mcnemar       = c("Miettinen, O. S. (1968). The matched pairs design in the case of all-or-none responses. Biometrics 24: 339-352."),
    logrank       = c("Schoenfeld, D. A. (1981). The asymptotic properties of nonparametric tests for comparing survival distributions. Biometrika 68: 316-319.",
                      "Lakatos, E. (1988). Sample sizes based on the log-rank statistic in complex clinical trials. Biometrics 44: 229-241."),
    ancova        = c("Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Routledge."),
    diagnostic    = c("Flahault, A., Cadilhac, M., Thomas, G. (2005). Sample size calculation should be performed for design accuracy in diagnostic test studies. J Clin Epidemiol 58: 859-862."),
    NULL
  )
  related <- if (is.null(related_list)) character(0)
             else paste0("[関連] ", related_list)

  # 3. ソフトウェアの引用
  uses_pwr <- !(design_id %in% c("one_mean", "one_prop", "binary_ni",
                                 "mcnemar", "diagnostic", "logrank"))
  uses_binom <- design_id == "one_prop"
  sw <- c(paste0("[ソフトウェア] ", r_cite))
  if (uses_pwr)   sw <- c(sw, paste0("[ソフトウェア] ", pwr_cite))
  if (uses_binom) sw <- c(sw, paste0("[ソフトウェア] ", binom_cite))

  c(primary, related, sw)
}
