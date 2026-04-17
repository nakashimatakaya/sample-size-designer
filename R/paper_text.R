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
      "本研究の設計において、A 群平均を %.2f（標準偏差 %.2f）、",
      "B 群平均を %.2f（標準偏差 %.2f）と想定した。",
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
      "本研究の設計において、A 群と B 群の平均値の差（群間差 Δ）を %.2f、",
      "A 群の SD を %.2f、B 群の SD を %.2f と想定した。",
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
      "本研究の設計において、A 群の発生割合を %s%%、B 群の発生割合を %s%% と想定した。",
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
      "本研究の設計において、A 群の発生割合を %s%%、B 群の発生割合を %s%% と想定した。",
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
      "本研究の設計において、A 群の平均を %.2f（SD %.2f）、B 群の平均を %.2f（SD %.2f）、",
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
      "本研究の設計において、A 群と B 群の平均値の差（群間差 Δ）を %.2f、",
      "A 群の SD を %.2f、B 群の SD を %.2f、",
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
      "本研究の設計において、A 群の割合を %s%%、B 群の割合を %s%%、",
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
  sprintf(
    paste(
      "本研究では、A 群と B 群の [アウトカム名を記載してください] の平均値の差を",
      "検出することを目的とした。",
      "過去の研究（[文献を記載してください]）に基づき、",
      "A 群の平均値を %.2f（標準偏差 %.2f）、",
      "B 群の平均値を %.2f（標準偏差 %.2f）と想定した。",
      "両側有意水準 %s、検出力 %s%% の下で、",
      "2 標本 t 検定（等分散、1:1 割付）に必要な症例数は、",
      "R パッケージ pwr（version %s）の pwr.t.test 関数を用いて算出した。",
      "その結果、解析対象として各群 %d 例（合計 %d 例）が必要であった。",
      "脱落率を %s%% と見込み、登録必要症例数は各群 %d 例（合計 %d 例）とした。",
      sep = "\n"
    ),
    p$mean_A, p$sd_A, p$mean_B, p$sd_B,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,  r$n_total_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized, r$n_total_randomized
  )
}

.jp_ttest_m2 <- function(p, r, pw) {
  sprintf(
    paste(
      "本研究では、A 群と B 群の [アウトカム名を記載してください] の平均値の差",
      "（群間差 Δ = %.2f）を検出することを目的とした。",
      "過去の研究（[文献を記載してください]）に基づき、",
      "A 群の標準偏差を %.2f、B 群の標準偏差を %.2f と想定した。",
      "両側有意水準 %s、検出力 %s%% の下で、",
      "2 標本 t 検定（等分散近似、1:1 割付）に必要な症例数は、",
      "R パッケージ pwr（version %s）の pwr.t.test 関数を用いて算出した。",
      "その結果、解析対象として各群 %d 例（合計 %d 例）が必要であった。",
      "脱落率を %s%% と見込み、登録必要症例数は各群 %d 例（合計 %d 例）とした。",
      sep = "\n"
    ),
    p$diff, p$sd_A, p$sd_B,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,  r$n_total_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized, r$n_total_randomized
  )
}

.jp_paired <- function(p, r, pw) {
  sprintf(
    paste(
      "本研究では、対応のある観測（例: 治療前と治療後）の",
      "[アウトカム名を記載してください] の差を検出することを目的とした。",
      "過去の研究（[文献を記載してください]）に基づき、",
      "差の平均を %.2f、差の標準偏差を %.2f と想定した。",
      "両側有意水準 %s、検出力 %s%% の下で、",
      "対応のある t 検定に必要なペア数は、",
      "R パッケージ pwr（version %s）の pwr.t.test 関数を用いて算出した。",
      "その結果、解析対象として %d ペアが必要であった。",
      "脱落率を %s%% と見込み、登録必要ペア数は %d ペアとした。",
      sep = "\n"
    ),
    p$diff_mean, p$sd_diff,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized
  )
}

.jp_paired_corr <- function(p, r, pw) {
  sprintf(
    paste(
      "本研究では、対応のある観測（例: 治療前と治療後）の",
      "[アウトカム名を記載してください] の差を検出することを目的とした。",
      "過去の研究（[文献を記載してください]）に基づき、",
      "治療前の平均を %.2f（標準偏差 %.2f）、",
      "治療後の平均を %.2f（標準偏差 %.2f）、",
      "前後の相関係数を %.2f と想定した。",
      "これより差の SD は",
      "sqrt(%.2f^2 + %.2f^2 − 2 × %.2f × %.2f × %.2f) = %.2f",
      "と計算された（差の平均は %.2f）。",
      "両側有意水準 %s、検出力 %s%% の下で、",
      "対応のある t 検定に必要なペア数は、",
      "R パッケージ pwr（version %s）の pwr.t.test 関数を用いて算出した。",
      "その結果、解析対象として %d ペアが必要であった。",
      "脱落率を %s%% と見込み、登録必要ペア数は %d ペアとした。",
      sep = "\n"
    ),
    p$mean_1, p$sd_1, p$mean_2, p$sd_2, p$r,
    p$sd_1, p$sd_2, p$r, p$sd_1, p$sd_2, r$sd_diff,
    r$diff_mean,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized
  )
}

.jp_binary_chisq <- function(p, r, pw) {
  sprintf(
    paste(
      "本研究では、A 群と B 群における [アウトカム名を記載してください] の",
      "発生割合の差を検出することを目的とした。",
      "過去の研究（[文献を記載してください]）に基づき、",
      "A 群の発生割合を %s%%、B 群の発生割合を %s%% と想定した。",
      "両側有意水準 %s、検出力 %s%% の下で、",
      "χ² 検定（1:1 割付）に必要な症例数は、",
      "R パッケージ pwr（version %s）の pwr.2p.test 関数を用いて算出した。",
      "その結果、解析対象として各群 %d 例（合計 %d 例）が必要であった。",
      "脱落率を %s%% と見込み、登録必要症例数は各群 %d 例（合計 %d 例）とした。",
      sep = "\n"
    ),
    .fmt_pct(p$p_A), .fmt_pct(p$p_B),
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,  r$n_total_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized, r$n_total_randomized
  )
}

.jp_binary_fisher <- function(p, r, pw) {
  sprintf(
    paste(
      "本研究では、A 群と B 群における [アウトカム名を記載してください] の",
      "発生割合の差を検出することを目的とした。",
      "過去の研究（[文献を記載してください]）に基づき、",
      "A 群の発生割合を %s%%、B 群の発生割合を %s%% と想定した。",
      "両側有意水準 %s、検出力 %s%% の下で、",
      "Fisher の正確検定（1:1 割付）に必要な症例数は、",
      "R パッケージ pwr（version %s）の pwr.2p.test 関数による",
      "χ² 検定ベースの近似を用いて算出した",
      "（Fisher の厳密な検出力計算は pwr に未搭載のため）。",
      "その結果、解析対象として各群 %d 例（合計 %d 例）が必要であった。",
      "脱落率を %s%% と見込み、登録必要症例数は各群 %d 例（合計 %d 例）とした。",
      sep = "\n"
    ),
    .fmt_pct(p$p_A), .fmt_pct(p$p_B),
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,  r$n_total_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized, r$n_total_randomized
  )
}

.jp_one_mean <- function(p, r) {
  sprintf(
    paste(
      "本研究では、[アウトカム名を記載してください] の平均値を",
      "%s%% 信頼区間の半幅 %s の精度で推定することを目的とした。",
      "過去の研究（[文献を記載してください]）に基づき、",
      "標準偏差を %.2f と想定した。",
      "公式 n = (z_{1-α/2} × SD / E)² を用いて、",
      "R の stats::qnorm 関数で算出した結果、",
      "解析対象として %d 例が必要であった。",
      "脱落率を %s%% と見込み、登録必要症例数は %d 例とした。",
      sep = "\n"
    ),
    .fmt_pct(p$conf_level), format(p$half_width),
    p$sd,
    r$n_per_arm_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized
  )
}

.jp_one_prop <- function(p, r) {
  method_jp <- switch(p$method,
    normal = "正規近似",
    wilson = "Wilson 法",
    exact  = "Exact 法（Clopper-Pearson）"
  )
  tool_line <- if (p$method == "normal") {
    "R の stats::qnorm 関数を用いて公式 n = z² p(1−p) / E² で算出した結果、"
  } else {
    sprintf(
      "R パッケージ binom（version %s）の binom.confint（method = \"%s\"）を",
      get_binom_version(), p$method
    )
  }
  tool_tail <- if (p$method == "normal") {
    ""
  } else {
    "用いて、信頼区間の半幅が指定値以下となる最小の n を探索した結果、"
  }
  sprintf(
    paste(
      "本研究では、[アウトカム名を記載してください] の発生割合を",
      "%s%% 信頼区間の半幅 %s の精度で推定することを目的とした。",
      "過去の研究（[文献を記載してください]）に基づき、",
      "予想される発生割合を %s%% と想定した。",
      "信頼区間の計算には %s を採用した。",
      "%s%s",
      "解析対象として %d 例が必要であった。",
      "脱落率を %s%% と見込み、登録必要症例数は %d 例とした。",
      sep = "\n"
    ),
    .fmt_pct(p$conf_level), format(p$half_width),
    .fmt_pct(p$p),
    method_jp,
    tool_line, tool_tail,
    r$n_per_arm_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized
  )
}

.jp_ttest_m2_ni <- function(p, r, pw) {
  sprintf(
    paste(
      "本研究では、A 群（新治療）が B 群（既存治療）に対して",
      "[アウトカム名を記載してください] の平均値で非劣性であることを",
      "示すことを目的とした。",
      "非劣性マージン M を %.2f（平均差の単位）と設定した。",
      "過去の研究（[文献を記載してください]）に基づき、",
      "A 群と B 群の平均値の差（群間差 Δ）を %.2f、",
      "A 群の標準偏差を %.2f、B 群の標準偏差を %.2f と想定した。",
      "片側有意水準 %s、検出力 %s%% の下で、",
      "2 標本 t 検定（等分散近似、1:1 割付、片側）に必要な症例数は、",
      "R パッケージ pwr（version %s）の pwr.t.test 関数",
      "（alternative = \"greater\"）を用いて算出した。",
      "その結果、解析対象として各群 %d 例（合計 %d 例）が必要であった。",
      "脱落率を %s%% と見込み、登録必要症例数は各群 %d 例（合計 %d 例）とした。",
      sep = "\n"
    ),
    p$margin, p$diff, p$sd_A, p$sd_B,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,  r$n_total_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized, r$n_total_randomized
  )
}

.jp_ttest_ni <- function(p, r, pw) {
  sprintf(
    paste(
      "本研究では、A 群（新治療）が B 群（既存治療）に対して",
      "[アウトカム名を記載してください] の平均値で非劣性であることを",
      "示すことを目的とした。",
      "非劣性マージン M を %.2f（平均差の単位）と設定した。",
      "過去の研究（[文献を記載してください]）に基づき、",
      "A 群の平均値を %.2f（標準偏差 %.2f）、",
      "B 群の平均値を %.2f（標準偏差 %.2f）と想定した。",
      "片側有意水準 %s、検出力 %s%% の下で、",
      "2 標本 t 検定（等分散近似、1:1 割付、片側）に必要な症例数は、",
      "R パッケージ pwr（version %s）の pwr.t.test 関数",
      "（alternative = \"greater\"）を用いて算出した。",
      "その結果、解析対象として各群 %d 例（合計 %d 例）が必要であった。",
      "脱落率を %s%% と見込み、登録必要症例数は各群 %d 例（合計 %d 例）とした。",
      sep = "\n"
    ),
    p$margin,
    p$mean_A, p$sd_A, p$mean_B, p$sd_B,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,  r$n_total_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized, r$n_total_randomized
  )
}

.jp_paired_ni <- function(p, r, pw) {
  sprintf(
    paste(
      "本研究では、対応のある観測（例: 新治療と既存治療）の",
      "[アウトカム名を記載してください] において、",
      "新治療が既存治療に対して非劣性であることを示すことを目的とした。",
      "非劣性マージン M を %.2f と設定した。",
      "過去の研究（[文献を記載してください]）に基づき、",
      "差の平均を %.2f、差の標準偏差を %.2f と想定した。",
      "片側有意水準 %s、検出力 %s%% の下で、",
      "対応のある t 検定（片側）に必要なペア数は、",
      "R パッケージ pwr（version %s）の pwr.t.test 関数",
      "（alternative = \"greater\"）を用いて算出した。",
      "その結果、解析対象として %d ペアが必要であった。",
      "脱落率を %s%% と見込み、登録必要ペア数は %d ペアとした。",
      sep = "\n"
    ),
    p$margin,
    p$diff_mean, p$sd_diff,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized
  )
}

.jp_binary_ni <- function(p, r, pw) {
  sprintf(
    paste(
      "本研究では、A 群（新治療）が B 群（既存治療）に対して",
      "[アウトカム名を記載してください] の発生割合で非劣性であることを",
      "示すことを目的とした。",
      "非劣性マージン M を %s%%（リスク差の単位）と設定した。",
      "過去の研究（[文献を記載してください]）に基づき、",
      "A 群の発生割合を %s%%、B 群の発生割合を %s%% と想定した。",
      "片側有意水準 %s、検出力 %s%% の下で、",
      "Chow ら（2018）の正規近似公式（Sec 4.2）に基づき、",
      "R の stats::qnorm 関数を用いて必要症例数を算出した。",
      "その結果、解析対象として各群 %d 例（合計 %d 例）が必要であった。",
      "脱落率を %s%% と見込み、登録必要症例数は各群 %d 例（合計 %d 例）とした。",
      "なお、より正確な計算には Farrington-Manning 法の使用を推奨する。",
      sep = "\n"
    ),
    .fmt_pct(p$margin),
    .fmt_pct(p$p_A), .fmt_pct(p$p_B),
    .fmt_alpha(p$alpha), .fmt_pct(pw),
    r$n_per_arm_evaluable,  r$n_total_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized, r$n_total_randomized
  )
}

# =========================================================================
# 英語テンプレート
# =========================================================================

.en_ttest_m1 <- function(p, r, pw) {
  sprintf(
    paste(
      "The primary objective of this study was to detect a difference in",
      "[outcome name, please specify] means between Group A and Group B.",
      "Based on prior evidence (please insert reference), we assumed a",
      "mean of %.2f (standard deviation %.2f) in Group A and a mean of",
      "%.2f (standard deviation %.2f) in Group B. Assuming a two-sided",
      "significance level of %s and a power of %s%%, the required sample",
      "size for a two-sample t-test with equal variance and 1:1 allocation",
      "was calculated using the pwr.t.test function in the R package pwr",
      "(version %s). The calculation indicated that %d evaluable",
      "participants per arm (total %d) would be required. Accounting for",
      "an anticipated dropout rate of %s%%, the target number of",
      "randomized participants was set to %d per arm (total %d).",
      sep = "\n"
    ),
    p$mean_A, p$sd_A, p$mean_B, p$sd_B,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,  r$n_total_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized, r$n_total_randomized
  )
}

.en_ttest_m2 <- function(p, r, pw) {
  sprintf(
    paste(
      "The primary objective of this study was to detect a between-group",
      "difference of %.2f in the [outcome name, please specify] means",
      "between Group A and Group B. Based on prior evidence (please",
      "insert reference), we assumed standard deviations of %.2f and %.2f",
      "in Groups A and B, respectively. Assuming a two-sided significance",
      "level of %s and a power of %s%%, the required sample size for a",
      "two-sample t-test (equal-variance approximation, 1:1 allocation)",
      "was calculated using the pwr.t.test function in the R package pwr",
      "(version %s). The calculation indicated that %d evaluable",
      "participants per arm (total %d) would be required. Accounting for",
      "an anticipated dropout rate of %s%%, the target number of",
      "randomized participants was set to %d per arm (total %d).",
      sep = "\n"
    ),
    p$diff, p$sd_A, p$sd_B,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,  r$n_total_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized, r$n_total_randomized
  )
}

.en_paired <- function(p, r, pw) {
  sprintf(
    paste(
      "The primary objective of this study was to detect a within-subject",
      "difference in [outcome name, please specify] between paired",
      "observations (e.g., pre- and post-treatment). Based on prior",
      "evidence (please insert reference), we assumed a mean difference",
      "of %.2f with a standard deviation of differences of %.2f.",
      "Assuming a two-sided significance level of %s and a power of %s%%,",
      "the required number of pairs for a paired t-test was calculated",
      "using the pwr.t.test function in the R package pwr (version %s).",
      "The calculation indicated that %d evaluable pairs would be",
      "required. Accounting for an anticipated dropout rate of %s%%,",
      "the target number of enrolled pairs was set to %d.",
      sep = "\n"
    ),
    p$diff_mean, p$sd_diff,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized
  )
}

.en_paired_corr <- function(p, r, pw) {
  sprintf(
    paste(
      "The primary objective of this study was to detect a within-subject",
      "difference in [outcome name, please specify] between paired",
      "observations (e.g., pre- and post-treatment). Based on prior",
      "evidence (please insert reference), we assumed a mean of %.2f",
      "(SD %.2f) at baseline and %.2f (SD %.2f) post-treatment, with a",
      "correlation coefficient of r = %.2f between the two time points.",
      "The resulting SD of the paired differences was calculated as",
      "sqrt(%.2f^2 + %.2f^2 - 2 x %.2f x %.2f x %.2f) = %.2f (paired mean",
      "difference = %.2f). Assuming a two-sided significance level of %s",
      "and a power of %s%%, the required number of pairs for a paired",
      "t-test was calculated using the pwr.t.test function in the R",
      "package pwr (version %s). The calculation indicated that %d",
      "evaluable pairs would be required. Accounting for an anticipated",
      "dropout rate of %s%%, the target number of enrolled pairs was set",
      "to %d.",
      sep = "\n"
    ),
    p$mean_1, p$sd_1, p$mean_2, p$sd_2, p$r,
    p$sd_1, p$sd_2, p$r, p$sd_1, p$sd_2, r$sd_diff,
    r$diff_mean,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized
  )
}

.en_binary_chisq <- function(p, r, pw) {
  sprintf(
    paste(
      "The primary objective of this study was to detect a difference in",
      "[outcome name, please specify] proportions between Group A and",
      "Group B. Based on prior evidence (please insert reference), we",
      "assumed an event rate of %s%% in Group A and %s%% in Group B.",
      "Assuming a two-sided significance level of %s and a power of %s%%,",
      "the required sample size for a chi-squared test with 1:1",
      "allocation was calculated using the pwr.2p.test function in the R",
      "package pwr (version %s). The calculation indicated that %d",
      "evaluable participants per arm (total %d) would be required.",
      "Accounting for an anticipated dropout rate of %s%%, the target",
      "number of randomized participants was set to %d per arm (total %d).",
      sep = "\n"
    ),
    .fmt_pct(p$p_A), .fmt_pct(p$p_B),
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,  r$n_total_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized, r$n_total_randomized
  )
}

.en_binary_fisher <- function(p, r, pw) {
  sprintf(
    paste(
      "The primary objective of this study was to detect a difference in",
      "[outcome name, please specify] proportions between Group A and",
      "Group B. Based on prior evidence (please insert reference), we",
      "assumed an event rate of %s%% in Group A and %s%% in Group B.",
      "Assuming a two-sided significance level of %s and a power of %s%%,",
      "the required sample size for Fisher's exact test with 1:1",
      "allocation was calculated using the chi-squared approximation",
      "provided by pwr.2p.test in the R package pwr (version %s), as an",
      "exact-test power calculation is not implemented in pwr. The",
      "calculation indicated that %d evaluable participants per arm",
      "(total %d) would be required. Accounting for an anticipated",
      "dropout rate of %s%%, the target number of randomized participants",
      "was set to %d per arm (total %d).",
      sep = "\n"
    ),
    .fmt_pct(p$p_A), .fmt_pct(p$p_B),
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,  r$n_total_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized, r$n_total_randomized
  )
}

.en_one_mean <- function(p, r) {
  sprintf(
    paste(
      "The primary objective of this study was to estimate the mean of",
      "[outcome name, please specify] with a %s%% confidence-interval",
      "half-width of %s. Based on prior evidence (please insert",
      "reference), we assumed a standard deviation of %.2f. Using the",
      "formula n = (z_{1-alpha/2} * SD / E)^2 implemented via stats::qnorm",
      "in R, the calculation indicated that %d evaluable participants",
      "would be required. Accounting for an anticipated dropout rate of",
      "%s%%, the target number of recruited participants was set to %d.",
      sep = "\n"
    ),
    .fmt_pct(p$conf_level), format(p$half_width),
    p$sd,
    r$n_per_arm_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized
  )
}

.en_one_prop <- function(p, r) {
  method_en <- switch(p$method,
    normal = "the normal approximation",
    wilson = "Wilson's method (Wilson 1927)",
    exact  = "the exact Clopper-Pearson method (Clopper & Pearson 1934)"
  )
  tool_line <- if (p$method == "normal") {
    sprintf(
      paste(
        "Using the formula n = z^2 * p * (1-p) / E^2 implemented via",
        "stats::qnorm in R, the calculation indicated that %d",
        "evaluable participants would be required.",
        sep = "\n"
      ),
      r$n_per_arm_evaluable
    )
  } else {
    sprintf(
      paste(
        "The smallest sample size for which the CI half-width was at most",
        "%s was found by iterative search using the binom.confint function",
        "(method = \"%s\") in the R package binom (version %s). The",
        "calculation indicated that %d evaluable participants would be",
        "required.",
        sep = "\n"
      ),
      format(p$half_width), p$method, get_binom_version(),
      r$n_per_arm_evaluable
    )
  }
  sprintf(
    paste(
      "The primary objective of this study was to estimate the",
      "proportion of [outcome name, please specify] with a %s%%",
      "confidence-interval half-width of %s. Based on prior evidence",
      "(please insert reference), we assumed a proportion of %s%%.",
      "Confidence intervals were computed using %s.",
      "%s",
      "Accounting for an anticipated dropout rate of %s%%, the target",
      "number of recruited participants was set to %d.",
      sep = "\n"
    ),
    .fmt_pct(p$conf_level), format(p$half_width),
    .fmt_pct(p$p),
    method_en,
    tool_line,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized
  )
}

.en_ttest_m2_ni <- function(p, r, pw) {
  sprintf(
    paste(
      "The primary objective of this study was to demonstrate",
      "non-inferiority of Group A (new treatment) to Group B (standard of",
      "care) with respect to [outcome name, please specify]. A",
      "non-inferiority margin of %.2f (on the scale of mean differences)",
      "was pre-specified. Based on prior evidence (please insert",
      "reference), we assumed a between-group mean difference of %.2f",
      "with standard deviations of %.2f and %.2f in Groups A and B,",
      "respectively. Assuming a one-sided significance level of %s and a",
      "power of %s%%, the required sample size for a one-sided two-sample",
      "t-test (equal-variance approximation, 1:1 allocation) was",
      "calculated using pwr.t.test (alternative = \"greater\") in the R",
      "package pwr (version %s). The calculation indicated that %d",
      "evaluable participants per arm (total %d) would be required.",
      "Accounting for an anticipated dropout rate of %s%%, the target",
      "number of randomized participants was set to %d per arm (total %d).",
      sep = "\n"
    ),
    p$margin, p$diff, p$sd_A, p$sd_B,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,  r$n_total_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized, r$n_total_randomized
  )
}

.en_ttest_ni <- function(p, r, pw) {
  sprintf(
    paste(
      "The primary objective of this study was to demonstrate",
      "non-inferiority of Group A (new treatment) to Group B (standard of",
      "care) with respect to [outcome name, please specify]. A",
      "non-inferiority margin of %.2f (on the scale of mean differences)",
      "was pre-specified. Based on prior evidence (please insert",
      "reference), we assumed means of %.2f (SD %.2f) in Group A and %.2f",
      "(SD %.2f) in Group B. Assuming a one-sided significance level of %s",
      "and a power of %s%%, the required sample size for a one-sided",
      "two-sample t-test (equal-variance approximation, 1:1 allocation)",
      "was calculated using pwr.t.test (alternative = \"greater\") in the",
      "R package pwr (version %s). The calculation indicated that %d",
      "evaluable participants per arm (total %d) would be required.",
      "Accounting for an anticipated dropout rate of %s%%, the target",
      "number of randomized participants was set to %d per arm (total %d).",
      sep = "\n"
    ),
    p$margin,
    p$mean_A, p$sd_A, p$mean_B, p$sd_B,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,  r$n_total_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized, r$n_total_randomized
  )
}

.en_paired_ni <- function(p, r, pw) {
  sprintf(
    paste(
      "The primary objective of this study was to demonstrate",
      "non-inferiority of the new treatment to the standard treatment",
      "within each subject (paired comparison), with respect to",
      "[outcome name, please specify]. A non-inferiority margin of %.2f",
      "was pre-specified. Based on prior evidence (please insert",
      "reference), we assumed a mean paired difference of %.2f with a",
      "standard deviation of differences of %.2f. Assuming a one-sided",
      "significance level of %s and a power of %s%%, the required number",
      "of pairs for a one-sided paired t-test was calculated using",
      "pwr.t.test (type = \"paired\", alternative = \"greater\") in the R",
      "package pwr (version %s). The calculation indicated that %d",
      "evaluable pairs would be required. Accounting for an anticipated",
      "dropout rate of %s%%, the target number of enrolled pairs was set",
      "to %d.",
      sep = "\n"
    ),
    p$margin,
    p$diff_mean, p$sd_diff,
    .fmt_alpha(p$alpha), .fmt_pct(pw), get_pwr_version(),
    r$n_per_arm_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized
  )
}

.en_binary_ni <- function(p, r, pw) {
  sprintf(
    paste(
      "The primary objective of this study was to demonstrate",
      "non-inferiority of Group A (new treatment) to Group B (standard of",
      "care) with respect to the proportion of [outcome name, please",
      "specify]. A non-inferiority margin of %s%% (on the risk-difference",
      "scale) was pre-specified. Based on prior evidence (please insert",
      "reference), we assumed event rates of %s%% in Group A and %s%% in",
      "Group B. Assuming a one-sided significance level of %s and a power",
      "of %s%%, the required sample size was calculated using the normal",
      "approximation formula of Chow et al. (2018, Section 4.2),",
      "implemented via stats::qnorm in R. The calculation indicated that",
      "%d evaluable participants per arm (total %d) would be required.",
      "Accounting for an anticipated dropout rate of %s%%, the target",
      "number of randomized participants was set to %d per arm (total %d).",
      "Note that a more accurate calculation using the Farrington-Manning",
      "method is recommended.",
      sep = "\n"
    ),
    .fmt_pct(p$margin),
    .fmt_pct(p$p_A), .fmt_pct(p$p_B),
    .fmt_alpha(p$alpha), .fmt_pct(pw),
    r$n_per_arm_evaluable,  r$n_total_evaluable,
    .fmt_pct(p$dropout),
    r$n_per_arm_randomized, r$n_total_randomized
  )
}

# =========================================================================
# 新規デザイン（Phase 2 / 3）の論文テンプレート
# =========================================================================

# ---- D1 McNemar ----
.jp_mcnemar <- function(p, r, pw) {
  sprintf(paste(
    "本研究では、対応のある二値アウトカム [アウトカム名を指定] における",
    "前後（または 2 つの条件下）の発生割合の差を検出することを主目的とした。",
    "過去の知見 [参考文献を挿入] から、不一致ペアの割合 p_disc を %.3f、",
    "そのうち A が優位である割合 ψ を %.2f と仮定した。",
    "両側有意水準 %s、目標検出力 %s%% のもと、必要ペア数を Connor (1987) の",
    "McNemar 検定用公式に基づき R の stats::qnorm で計算した。",
    "その結果、解析対象として %d ペアが必要と算出された。",
    "脱落率を %s%% と見込む場合、登録必要ペア数は %d ペアとなる。",
    sep = "\n"),
    p$p_disc, p$psi,
    .fmt_alpha(p$alpha), .fmt_pct(pw),
    r$n_per_arm_evaluable,
    .fmt_pct(p$dropout), r$n_per_arm_randomized
  )
}
.en_mcnemar <- function(p, r, pw) {
  sprintf(paste(
    "The primary objective of this study was to detect a difference in the",
    "paired-binary outcome [please specify] between two conditions (before/",
    "after or A/B). Based on prior evidence [please insert reference], we",
    "assumed a discordant-pair proportion of %.3f (p_disc) and a proportion",
    "of %.2f (ψ) in which A was superior. Assuming a two-sided significance",
    "level of %s and a target power of %s%%, the required number of pairs",
    "was calculated using the McNemar sample-size formula of Connor (1987),",
    "implemented via stats::qnorm in R. The calculation indicated that %d",
    "evaluable pairs would be required. Accounting for an anticipated",
    "dropout rate of %s%%, the target number of enrolled pairs was set to %d.",
    sep = "\n"),
    p$p_disc, p$psi,
    .fmt_alpha(p$alpha), .fmt_pct(pw),
    r$n_per_arm_evaluable,
    .fmt_pct(p$dropout), r$n_per_arm_randomized
  )
}

# ---- D2 ANCOVA ----
.jp_ancova <- function(p, r, pw) {
  sprintf(paste(
    "本研究では、連続量アウトカム [アウトカム名を指定] における 2 群の",
    "平均値の差を、共変量（例: ベースライン測定値）で調整した ANCOVA",
    "モデルで検定することを主目的とした。",
    "過去の知見 [参考文献を挿入] から、A 群平均を %.2f、B 群平均を %.2f、",
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
  sprintf(paste(
    "本研究では、生存時間アウトカム [アウトカム名を指定] における 2 群の",
    "生存関数の差を log-rank 検定で検出することを主目的とした。",
    "過去の知見 [参考文献を挿入] から、対照群の中央生存期間を %.1f か月、",
    "ハザード比を %.2f と想定した。アクルー期間は %.1f か月、追加",
    "フォローアップ期間は %.1f か月とし、生存分布は指数分布、アクルーは",
    "一様（rectangular）と仮定した。片側有意水準 %s、目標検出力 %s%% のもと、",
    "Schoenfeld (1981) の公式に基づき R の stats::qnorm で必要イベント数と",
    "総症例数を算出した。その結果、必要イベント数は %d 件、総症例数は",
    "%d 例（各群 %d 例）であった。",
    "脱落率を %s%% と見込む場合、登録必要症例数は各群 %d 例（合計 %d 例）となる。",
    sep = "\n"),
    p$median_C, p$HR, p$accrual, p$followup,
    .fmt_alpha(p$alpha), .fmt_pct(pw),
    r$events_required %||% NA, r$n_total_evaluable, r$n_per_arm_evaluable,
    .fmt_pct(p$dropout), r$n_per_arm_randomized, r$n_total_randomized
  )
}
.en_logrank <- function(p, r, pw) {
  sprintf(paste(
    "The primary objective was to compare survival between two groups using",
    "the log-rank test for [please specify the time-to-event outcome].",
    "Based on prior evidence [please insert reference], we assumed a median",
    "survival of %.1f months in the control group and a hazard ratio of",
    "%.2f. The accrual period was %.1f months and the additional follow-up",
    "was %.1f months, assuming exponential survival and rectangular accrual.",
    "Assuming a one-sided significance level of %s and a power of %s%%,",
    "the required number of events and the total sample size were calculated",
    "using the formula of Schoenfeld (1981) implemented via stats::qnorm in",
    "R. The calculation indicated that %d events and a total of %d",
    "participants (%d per arm) would be required. Accounting for an",
    "anticipated dropout rate of %s%%, the target number of randomized",
    "participants was set to %d per arm (total %d).",
    sep = "\n"),
    p$median_C, p$HR, p$accrual, p$followup,
    .fmt_alpha(p$alpha), .fmt_pct(pw),
    r$events_required %||% NA, r$n_total_evaluable, r$n_per_arm_evaluable,
    .fmt_pct(p$dropout), r$n_per_arm_randomized, r$n_total_randomized
  )
}

# ---- D4 longitudinal ----
.jp_longitudinal <- function(p, r, pw) {
  sprintf(paste(
    "本研究では、%d 時点の反復測定による連続量アウトカム [アウトカム名を",
    "指定] を、両群の post-baseline 平均値で比較することを主目的とした。",
    "過去の知見 [参考文献を挿入] から、A 群平均を %.2f、B 群平均を %.2f、",
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
    "実施することとした。過去の知見 [参考文献を挿入] から、A 群平均を %.2f、",
    "B 群平均を %.2f、共通 SD を %.2f と想定した。両側有意水準 %s、",
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
    "過去の知見 [参考文献を挿入] から、A 群平均を %.2f、B 群平均を %.2f、",
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
    "過去の知見 [参考文献を挿入] から、A 群の発生割合を %s%%、B 群の",
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
  sprintf(paste(
    "本研究は、[対象疾患を指定] に対する [検査名を指定] の診断精度を、",
    "感度と特異度の %s%% 信頼区間の半幅で評価することを主目的とした。",
    "過去の知見 [参考文献を挿入] から、予想感度を %s%%、予想特異度を",
    "%s%%、対象集団の有病率を %s%% と想定し、目標 CI 半幅を %.3f とした。",
    "Buderer (1996) の正規近似公式（n_疾患あり = z² × Se(1-Se)/E²、",
    "n_疾患なし = z² × Sp(1-Sp)/E²、総必要例数 = max(n_疾患あり/prev,",
    "n_疾患なし/(1-prev))）に基づき R の stats::qnorm で算出した。",
    "その結果、解析対象として %d 例（疾患あり %d 例、疾患なし %d 例の",
    "要件を満たす）が必要と算出された。",
    "脱落率を %s%% と見込む場合、登録必要例数は %d 例となる。",
    sep = "\n"),
    .fmt_pct(p$conf_level),
    .fmt_pct(p$Se), .fmt_pct(p$Sp), .fmt_pct(p$prev), p$half_width,
    r$n_per_arm_evaluable,
    r$n_dis_required %||% 0L, r$n_non_required %||% 0L,
    .fmt_pct(p$dropout), r$n_per_arm_randomized
  )
}
.en_diagnostic <- function(p, r) {
  sprintf(paste(
    "The primary objective was to estimate the diagnostic accuracy (both",
    "sensitivity and specificity) of [please specify the test] for [please",
    "specify the target condition], with a target %s%% confidence-interval",
    "half-width. Based on prior evidence [please insert reference], we",
    "assumed an expected sensitivity of %s%%, specificity of %s%%, and",
    "prevalence in the target population of %s%%, with a target CI",
    "half-width of %.3f. The sample size was calculated using the normal",
    "approximation of Buderer (1996): n_disease = z² × Se(1-Se)/E²,",
    "n_non-disease = z² × Sp(1-Sp)/E², and total N = max(n_disease/prev,",
    "n_non-disease/(1-prev)), implemented via stats::qnorm in R.",
    "The calculation indicated that %d evaluable participants would be",
    "required (satisfying %d diseased and %d non-diseased participants).",
    "Accounting for an anticipated dropout rate of %s%%, the target number",
    "of enrolled participants was set to %d.",
    sep = "\n"),
    .fmt_pct(p$conf_level),
    .fmt_pct(p$Se), .fmt_pct(p$Sp), .fmt_pct(p$prev), p$half_width,
    r$n_per_arm_evaluable,
    r$n_dis_required %||% 0L, r$n_non_required %||% 0L,
    .fmt_pct(p$dropout), r$n_per_arm_randomized
  )
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
    "A 群の平均を %.2f、B 群の平均を %.2f、共通 SD を %.2f、分布形状は",
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
# =========================================================================
paper_references <- function(design_id) {
  base <- c(
    "Champely, S. (2020). pwr: Basic Functions for Power Analysis. R package.",
    "Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences (2nd ed.). Lawrence Erlbaum Associates."
  )
  extra <- switch(design_id,
    ttest_ni      = "Chow, S.-C., Shao, J., Wang, H., Lokhnygina, Y. (2018). Sample Size Calculations in Clinical Research (3rd ed.). CRC Press.",
    paired_ni     = "Chow, S.-C., Shao, J., Wang, H., Lokhnygina, Y. (2018). Sample Size Calculations in Clinical Research (3rd ed.). CRC Press.",
    binary_ni     = "Chow, S.-C., Shao, J., Wang, H., Lokhnygina, Y. (2018). Sample Size Calculations in Clinical Research (3rd ed.). CRC Press, Section 4.2.",
    one_prop      = "Wilson, E. B. (1927). Probable inference, the law of succession, and statistical inference. JASA 22: 209-212. / Clopper, C. J., Pearson, E. S. (1934). The use of confidence or fiducial limits. Biometrika 26: 404-413.",
    mcnemar       = "Connor, R. J. (1987). Sample size for testing differences in proportions for the paired-sample design. Biometrics 43: 207-211.",
    ancova        = "Borm, G. F., Fransen, J., Lemmens, W. A. J. G. (2007). A simple sample size formula for analysis of covariance in randomized clinical trials. J Clin Epidemiol 60: 1234-1238.",
    logrank       = "Schoenfeld, D. A. (1981). The asymptotic properties of nonparametric tests for comparing survival distributions. Biometrika 68: 316-319. / Collett, D. (2015). Modelling Survival Data in Medical Research (3rd ed.). Chapman & Hall/CRC.",
    longitudinal  = "Diggle, P. J., Liang, K. Y., Zeger, S. L. (2002). Analysis of Longitudinal Data (2nd ed.). Oxford University Press.",
    group_sequential = "Jennison, C., Turnbull, B. W. (2000). Group Sequential Methods with Applications to Clinical Trials. Chapman & Hall/CRC. / O'Brien, P. C., Fleming, T. R. (1979). Biometrics 35: 549-556. / Pocock, S. J. (1977). Biometrika 64: 191-199.",
    cluster_cont  = "Donner, A., Klar, N. (2000). Design and Analysis of Cluster Randomization Trials in Health Research. Arnold.",
    cluster_bin   = "Donner, A., Klar, N. (2000). Design and Analysis of Cluster Randomization Trials in Health Research. Arnold.",
    diagnostic    = "Buderer, N. M. (1996). Statistical methodology: I. Incorporating the prevalence of disease into the sample size calculation for sensitivity and specificity. Acad Emerg Med 3: 895-900.",
    mann_whitney  = "Hollander, M., Wolfe, D. A. (1999). Nonparametric Statistical Methods (2nd ed.). Wiley. / Lehmann, E. L. (1975). Nonparametrics: Statistical Methods Based on Ranks. Holden-Day.",
    NULL
  )
  c(base, extra)
}
