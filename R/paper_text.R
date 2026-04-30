# R/paper_text.R
# 論文記載用テキスト生成（日本語版・英語版）。
# design_id で分岐してテンプレートを返す。
#
# 文字列補間は glue::glue() に統一している。理由は:
#   - sprintf の "%" エスケープ漏れによる "unrecognised format specification"
#     エラーを根本的に避けたい
#   - 引数の数と書式指定子の数のミスマッチ（"too few arguments"）を避けたい
#   - 補間を {var} で書けるので可読性が上がる
# glue は app.R の library(glue) でロード済み（Shinylive 動作確認済み）。
#
# design_id の一覧:
#   ttest_m1, ttest_m2, paired, paired_corr, binary_chisq, binary_fisher,
#   one_mean, one_prop, ttest_ni, ttest_m2_ni, paired_ni, binary_ni,
#   mcnemar, ancova, logrank, longitudinal, group_sequential,
#   cluster_cont, cluster_bin, diagnostic, mann_whitney
#
# 各関数のシグネチャ:
#   gen_paper_jp(design_id, params, result, power_target = 0.80,
#                calc_mode = "sample_size")
#   gen_paper_en(design_id, params, result, power_target = 0.80,
#                calc_mode = "sample_size")

get_pwr_version <- function() {
  tryCatch(as.character(packageVersion("pwr")),
           error = function(e) "unknown")
}
get_binom_version <- function() {
  tryCatch(as.character(packageVersion("binom")),
           error = function(e) "unknown")
}

# =========================================================================
# 数値フォーマットの小ヘルパー（glue 内の {} で呼ぶ）
# =========================================================================
.f1 <- function(x) sprintf("%.1f", x)
.f2 <- function(x) sprintf("%.2f", x)
.f3 <- function(x) sprintf("%.3f", x)
.fmt_alpha <- function(a) sprintf("%.3f", a)
.fmt_pct   <- function(x) sprintf("%d", round(x * 100))
.fmt_power <- function(x) sprintf("%.1f", x * 100)

# 数値が NULL/NA でも安全に表示するためのヘルパー
.fmt_or_na <- function(x, fmt = .f2) {
  if (is.null(x) || (is.numeric(x) && is.na(x))) "NA" else fmt(x)
}

# 多行 glue を 1 関数で書くためのラッパー。
# 各行は文字列ベクトルとして渡し、改行で結合する。
# .envir は呼び出し元のフレームを使う（既定）。
.glue_lines <- function(..., .envir = parent.frame()) {
  lines <- c(...)
  glue::glue_collapse(
    vapply(lines, function(x) {
      as.character(glue::glue(x, .envir = .envir))
    }, character(1)),
    sep = "\n"
  )
}

# =========================================================================
# 引用フッター（日英）
# section_text は "Section 3.2" のような文字列。Chow に詳述のない
# デザインの場合は alt_text で置き換える。
# =========================================================================
.cite_footer_jp <- function(section_text, pwr = TRUE,
                            alt_text = NULL) {
  pwr_line <- if (pwr) {
    "計算は R (R Core Team, 2024) の pwr パッケージ（Champely, 2020）を用いて実行した。"
  } else {
    "計算は R (R Core Team, 2024) の標準パッケージ stats を用いて実行した。"
  }
  tail_line <- if (is.null(alt_text)) {
    paste0(
      "詳細な手法は Chow, Shao, Wang (2008) Sample Size Calculations in ",
      "Clinical Research 2nd ed., ", section_text, " に準拠している。"
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
    paste0(
      "The methodology follows Chow, Shao, & Wang (2008), Sample Size ",
      "Calculations in Clinical Research (2nd ed.), ", section_text, "."
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

# 不均等割付かどうかを判定（result から）
.is_unequal_alloc <- function(r) {
  !is.null(r$allocation_ratio) && r$allocation_ratio != 1 &&
    !is.null(r$n_intervention_randomized) &&
    !is.null(r$n_control_randomized)
}

# 検出力モードの「脱落率を見込んだ登録必要数」末尾文（日本語）
.jp_drop_tail <- function(p, r) {
  if (.is_unequal_alloc(r)) {
    glue::glue(
      "脱落率を {.fmt_pct(p$dropout)}% と見込む場合、",
      "登録必要症例数は対照群 {r$n_control_randomized} 例、",
      "介入群 {r$n_intervention_randomized} 例（合計 {r$n_total_randomized} 例）となる。"
    )
  } else {
    glue::glue(
      "脱落率を {.fmt_pct(p$dropout)}% と見込む場合、",
      "登録必要症例数は各群 {r$n_per_arm_randomized} 例（合計 {r$n_total_randomized} 例）となる。"
    )
  }
}
.jp_drop_tail_pair <- function(p, r) {
  glue::glue(
    "脱落率を {.fmt_pct(p$dropout)}% と見込む場合、",
    "登録必要ペア数は {r$n_per_arm_randomized} ペアとなる。"
  )
}
.en_drop_tail <- function(p, r) {
  if (.is_unequal_alloc(r)) {
    glue::glue(
      "Accounting for an anticipated dropout rate of {.fmt_pct(p$dropout)}%, ",
      "the target enrolment would be {r$n_control_randomized} controls and ",
      "{r$n_intervention_randomized} in the intervention arm ",
      "(total {r$n_total_randomized})."
    )
  } else {
    glue::glue(
      "Accounting for an anticipated dropout rate of {.fmt_pct(p$dropout)}%, ",
      "the target number of randomized participants would be ",
      "{r$n_per_arm_randomized} per arm (total {r$n_total_randomized})."
    )
  }
}
.en_drop_tail_pair <- function(p, r) {
  glue::glue(
    "Accounting for an anticipated dropout rate of {.fmt_pct(p$dropout)}%, ",
    "the target number of enrolled pairs would be {r$n_per_arm_randomized}."
  )
}

# 不均等割付のときの本文用 n フレーズ（日英）
.jp_n_phrase <- function(r) {
  if (.is_unequal_alloc(r)) {
    glue::glue(
      "対照群 {r$n_control_evaluable} 例、",
      "介入群 {r$n_intervention_evaluable} 例",
      "（合計 {r$n_total_evaluable} 例、",
      "割付比 {.f2(r$allocation_ratio)}:1）"
    )
  } else {
    glue::glue(
      "各群 {r$n_per_arm_evaluable} 例（合計 {r$n_total_evaluable} 例）"
    )
  }
}
.en_n_phrase <- function(r) {
  if (.is_unequal_alloc(r)) {
    glue::glue(
      "{r$n_control_evaluable} controls and ",
      "{r$n_intervention_evaluable} in the intervention arm ",
      "(total {r$n_total_evaluable}, ",
      "allocation {.f2(r$allocation_ratio)}:1)"
    )
  } else {
    glue::glue(
      "{r$n_per_arm_evaluable} evaluable participants per arm ",
      "(total {r$n_total_evaluable})"
    )
  }
}

# ----- 日本語: 検出力モード -----
.jp_power_calc_dispatch <- function(design_id, p, r) {
  drop_tail   <- .jp_drop_tail(p, r)
  drop_tail_1 <- .jp_drop_tail_pair(p, r)
  n_phrase    <- .jp_n_phrase(r)
  pw          <- .fmt_power(r$achieved_power %||% NA_real_)
  alpha       <- .fmt_alpha(p$alpha)
  pwr_v       <- get_pwr_version()

  body <- switch(design_id,
    ttest_m1 = glue::glue(
      "本研究の設計において、",
      "介入群平均を {.f2(p$mean_A)}（標準偏差 {.f2(p$sd_A)}）、",
      "対照群平均を {.f2(p$mean_B)}（標準偏差 {.f2(p$sd_B)}）と想定した。",
      "両側有意水準 {alpha} のもと、{n_phrase}を登録した場合の",
      "2 標本 t 検定の達成検出力を算出した。",
      "計算には R パッケージ pwr（version {pwr_v}）の pwr.t.test 関数を用いた。",
      "その結果、達成検出力は {pw}% であった。",
      "{drop_tail}",
      .sep = "\n"
    ),
    ttest_m2 = glue::glue(
      "本研究の設計において、介入群と 対照群の平均値の差（群間差 Δ）を {.f2(p$diff)}、",
      "介入群の SD を {.f2(p$sd_A)}、対照群の SD を {.f2(p$sd_B)} と想定した。",
      "両側有意水準 {alpha} のもと、{n_phrase}における",
      "2 標本 t 検定の達成検出力を R パッケージ pwr（version {pwr_v}）の",
      "pwr.t.test 関数を用いて算出した。",
      "その結果、達成検出力は {pw}% であった。",
      "{drop_tail}",
      .sep = "\n"
    ),
    paired = glue::glue(
      "本研究の設計において、対応のある観測の差の平均を {.f2(p$diff_mean)}、",
      "差の SD を {.f2(p$sd_diff)} と想定した。",
      "両側有意水準 {alpha} のもと、{r$n_per_arm_evaluable} ペアにおける",
      "対応のある t 検定の達成検出力を pwr（version {pwr_v}）の",
      "pwr.t.test 関数を用いて算出した。",
      "その結果、達成検出力は {pw}% であった。",
      "{drop_tail_1}",
      .sep = "\n"
    ),
    paired_corr = glue::glue(
      "本研究の設計において、",
      "治療前の平均を {.f2(p$mean_1)}（SD {.f2(p$sd_1)}）、",
      "治療後の平均を {.f2(p$mean_2)}（SD {.f2(p$sd_2)}）、",
      "前後の相関係数を {.f2(p$r)} と想定した。",
      "これより差の SD は sqrt({.f2(p$sd_1)}^2 + {.f2(p$sd_2)}^2 − ",
      "2 × {.f2(p$r)} × {.f2(p$sd_1)} × {.f2(p$sd_2)}) = ",
      "{.fmt_or_na(r$sd_diff)}、",
      "差の平均は {.fmt_or_na(r$diff_mean)} と換算された。",
      "両側有意水準 {alpha} のもと、{r$n_per_arm_evaluable} ペアにおける",
      "対応のある t 検定の達成検出力を pwr（version {pwr_v}）の pwr.t.test 関数で",
      "算出した。その結果、達成検出力は {pw}% であった。",
      "{drop_tail_1}",
      .sep = "\n"
    ),
    binary_chisq = glue::glue(
      "本研究の設計において、",
      "介入群の発生割合を {.fmt_pct(p$p_A)}%、",
      "対照群の発生割合を {.fmt_pct(p$p_B)}% と想定した。",
      "両側有意水準 {alpha} のもと、{n_phrase}における",
      "χ² 検定の達成検出力を pwr（version {pwr_v}）の pwr.2p.test 関数で算出した。",
      "その結果、達成検出力は {pw}% であった。",
      "{drop_tail}",
      .sep = "\n"
    ),
    binary_fisher = glue::glue(
      "本研究の設計において、",
      "介入群の発生割合を {.fmt_pct(p$p_A)}%、",
      "対照群の発生割合を {.fmt_pct(p$p_B)}% と想定した。",
      "両側有意水準 {alpha} のもと、{n_phrase}における",
      "Fisher の正確検定の達成検出力を、χ² 検定ベースの pwr.2p.test 関数",
      "（pwr version {pwr_v}）による近似を用いて算出した。",
      "その結果、達成検出力は {pw}% であった。",
      "{drop_tail}",
      .sep = "\n"
    ),
    ttest_ni = glue::glue(
      "本研究の設計において、",
      "介入群の平均を {.f2(p$mean_A)}（SD {.f2(p$sd_A)}）、",
      "対照群の平均を {.f2(p$mean_B)}（SD {.f2(p$sd_B)}）、",
      "非劣性マージン M を {.f2(p$margin)} と想定した。",
      "片側有意水準 {alpha} のもと、{n_phrase}における",
      "2 標本 t 検定（片側）の達成検出力を pwr（version {pwr_v}）の",
      "pwr.t.test 関数（alternative = 'greater'）を用いて算出した。",
      "その結果、達成検出力は {pw}% であった。",
      "{drop_tail}",
      .sep = "\n"
    ),
    ttest_m2_ni = glue::glue(
      "本研究の設計において、",
      "介入群と 対照群の平均値の差（群間差 Δ）を {.f2(p$diff)}、",
      "介入群の SD を {.f2(p$sd_A)}、対照群の SD を {.f2(p$sd_B)}、",
      "非劣性マージン M を {.f2(p$margin)} と想定した。",
      "片側有意水準 {alpha} のもと、{n_phrase}における",
      "2 標本 t 検定（片側）の達成検出力を pwr（version {pwr_v}）の",
      "pwr.t.test 関数（alternative = 'greater'）を用いて算出した。",
      "その結果、達成検出力は {pw}% であった。",
      "{drop_tail}",
      .sep = "\n"
    ),
    paired_ni = glue::glue(
      "本研究の設計において、差の平均を {.f2(p$diff_mean)}、",
      "差の SD を {.f2(p$sd_diff)}、",
      "非劣性マージン M を {.f2(p$margin)} と想定した。",
      "片側有意水準 {alpha} のもと、{r$n_per_arm_evaluable} ペアにおける",
      "対応のある t 検定（片側）の達成検出力を pwr（version {pwr_v}）の",
      "pwr.t.test 関数で算出した。",
      "その結果、達成検出力は {pw}% であった。",
      "{drop_tail_1}",
      .sep = "\n"
    ),
    binary_ni = glue::glue(
      "本研究の設計において、",
      "介入群の割合を {.fmt_pct(p$p_A)}%、",
      "対照群の割合を {.fmt_pct(p$p_B)}%、",
      "非劣性マージン M を {.fmt_pct(p$margin)}%（リスク差の単位）と想定した。",
      "片側有意水準 {alpha} のもと、",
      "各群 {r$n_per_arm_evaluable} 例（合計 {r$n_total_evaluable} 例）における",
      "達成検出力を、Chow ら（2018）の正規近似公式（Sec 4.2）に基づき",
      "R の stats::qnorm を用いて算出した。",
      "その結果、達成検出力は {pw}% であった。",
      "{drop_tail}",
      .sep = "\n"
    ),
    .jp_power_calc_generic(design_id, p, r, drop_tail, drop_tail_1)
  )
  as.character(body)
}

# ----- 英語: 検出力モード -----
.en_power_calc_dispatch <- function(design_id, p, r) {
  drop_tail   <- .en_drop_tail(p, r)
  drop_tail_1 <- .en_drop_tail_pair(p, r)
  n_phrase    <- .en_n_phrase(r)
  pw          <- .fmt_power(r$achieved_power %||% NA_real_)
  alpha       <- .fmt_alpha(p$alpha)
  pwr_v       <- get_pwr_version()

  body <- switch(design_id,
    ttest_m1 = glue::glue(
      "For this study design, we assumed a mean of {.f2(p$mean_A)} ",
      "(SD {.f2(p$sd_A)}) in Group A and {.f2(p$mean_B)} ",
      "(SD {.f2(p$sd_B)}) in Group B. ",
      "Assuming a two-sided significance level of {alpha} and {n_phrase}, ",
      "the achieved power of a two-sample t-test was calculated using ",
      "the pwr.t.test function in the R package pwr (version {pwr_v}). ",
      "The achieved power was {pw}%.",
      "{drop_tail}",
      .sep = "\n"
    ),
    ttest_m2 = glue::glue(
      "For this study design, we assumed a between-group mean difference of {.f2(p$diff)}",
      "with standard deviations of {.f2(p$sd_A)} and {.f2(p$sd_B)} in Groups A and B.",
      "Assuming a two-sided significance level of {alpha} and {n_phrase},",
      "the achieved power of a two-sample t-test was calculated using",
      "the pwr.t.test function in the R package pwr (version {pwr_v}).",
      "The achieved power was {pw}%.",
      "{drop_tail}",
      .sep = "\n"
    ),
    paired = glue::glue(
      "For this study design, we assumed a mean paired difference of ",
      "{.f2(p$diff_mean)} with a standard deviation of {.f2(p$sd_diff)}. ",
      "Assuming a two-sided significance level of {alpha} and ",
      "{r$n_per_arm_evaluable} evaluable pairs, ",
      "the achieved power of a paired t-test was calculated using ",
      "the pwr.t.test function in the R package pwr (version {pwr_v}). ",
      "The achieved power was {pw}%.",
      "{drop_tail_1}",
      .sep = "\n"
    ),
    paired_corr = glue::glue(
      "For this study design, we assumed a mean of {.f2(p$mean_1)} ",
      "(SD {.f2(p$sd_1)}) at baseline and {.f2(p$mean_2)} ",
      "(SD {.f2(p$sd_2)}) post-treatment, with a correlation of ",
      "r = {.f2(p$r)}. ",
      "The corresponding SD of paired differences was calculated as ",
      "sqrt({.f2(p$sd_1)}^2 + {.f2(p$sd_2)}^2 - 2 x {.f2(p$r)} x ",
      "{.f2(p$sd_1)} x {.f2(p$sd_2)}) = {.fmt_or_na(r$sd_diff)} ",
      "(mean difference = {.fmt_or_na(r$diff_mean)}). ",
      "Assuming a two-sided significance level of {alpha} and ",
      "{r$n_per_arm_evaluable} evaluable pairs, ",
      "the achieved power of a paired t-test was calculated using the ",
      "pwr.t.test function in the R package pwr (version {pwr_v}). ",
      "The achieved power was {pw}%.",
      "{drop_tail_1}",
      .sep = "\n"
    ),
    binary_chisq = glue::glue(
      "For this study design, we assumed event rates of ",
      "{.fmt_pct(p$p_A)}% in Group A and {.fmt_pct(p$p_B)}% in Group B. ",
      "Assuming a two-sided significance level of {alpha} and {n_phrase}, ",
      "the achieved power of a chi-squared test was calculated using ",
      "the pwr.2p.test function in the R package pwr (version {pwr_v}). ",
      "The achieved power was {pw}%.",
      "{drop_tail}",
      .sep = "\n"
    ),
    binary_fisher = glue::glue(
      "For this study design, we assumed event rates of ",
      "{.fmt_pct(p$p_A)}% in Group A and {.fmt_pct(p$p_B)}% in Group B. ",
      "Assuming a two-sided significance level of {alpha} and {n_phrase}, ",
      "the achieved power of Fisher's exact test was approximated using ",
      "the chi-squared-based pwr.2p.test function in the R package pwr ",
      "(version {pwr_v}). The achieved power was {pw}%.",
      "{drop_tail}",
      .sep = "\n"
    ),
    ttest_ni = glue::glue(
      "For this study design, we assumed means of {.f2(p$mean_A)} ",
      "(SD {.f2(p$sd_A)}) in Group A and {.f2(p$mean_B)} ",
      "(SD {.f2(p$sd_B)}) in Group B, with a non-inferiority margin of ",
      "{.f2(p$margin)}. ",
      "Assuming a one-sided significance level of {alpha} and {n_phrase}, ",
      "the achieved power of a one-sided two-sample t-test was calculated ",
      "using pwr.t.test (alternative = 'greater') in the R package pwr ",
      "(version {pwr_v}). The achieved power was {pw}%.",
      "{drop_tail}",
      .sep = "\n"
    ),
    ttest_m2_ni = glue::glue(
      "For this study design, we assumed a between-group mean difference of ",
      "{.f2(p$diff)} with standard deviations of {.f2(p$sd_A)} and ",
      "{.f2(p$sd_B)} in Groups A and B, and a non-inferiority margin of ",
      "{.f2(p$margin)}. ",
      "Assuming a one-sided significance level of {alpha} and {n_phrase}, ",
      "the achieved power of a one-sided two-sample t-test was calculated ",
      "using pwr.t.test (alternative = 'greater') in the R package pwr ",
      "(version {pwr_v}). The achieved power was {pw}%.",
      "{drop_tail}",
      .sep = "\n"
    ),
    paired_ni = glue::glue(
      "For this study design, we assumed a mean paired difference of ",
      "{.f2(p$diff_mean)}, a SD of differences of {.f2(p$sd_diff)}, ",
      "and a non-inferiority margin of {.f2(p$margin)}. ",
      "Assuming a one-sided significance level of {alpha} and ",
      "{r$n_per_arm_evaluable} evaluable pairs, the achieved power of a ",
      "one-sided paired t-test was calculated using pwr.t.test in the R ",
      "package pwr (version {pwr_v}). The achieved power was {pw}%.",
      "{drop_tail_1}",
      .sep = "\n"
    ),
    binary_ni = glue::glue(
      "For this study design, we assumed event rates of ",
      "{.fmt_pct(p$p_A)}% in Group A and {.fmt_pct(p$p_B)}% in Group B, ",
      "with a non-inferiority margin of {.fmt_pct(p$margin)}% ",
      "(on the risk-difference scale). ",
      "Assuming a one-sided significance level of {alpha} and ",
      "{r$n_per_arm_evaluable} evaluable participants per arm ",
      "(total {r$n_total_evaluable}), the achieved power was calculated ",
      "using the normal approximation of Chow et al. (2018, Section 4.2) ",
      "implemented via stats::qnorm. The achieved power was {pw}%.",
      "{drop_tail}",
      .sep = "\n"
    ),
    .en_power_calc_generic(design_id, p, r, drop_tail, drop_tail_1)
  )
  as.character(body)
}

# 検出力モードの汎用テンプレート（新規デザイン用）
.jp_power_calc_generic <- function(design_id, p, r, drop_tail, drop_tail_1) {
  info <- backend_info_for(design_id)
  pw   <- .fmt_power(r$achieved_power %||% 0)
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
    glue::glue("{r$n_per_arm_evaluable} ペア")
  } else if (.is_unequal_alloc(r) ||
             (!is.null(r$n_intervention_evaluable) &&
              !is.null(r$n_control_evaluable) &&
              !is.null(r$allocation_ratio) &&
              r$allocation_ratio != 1)) {
    glue::glue(
      "対照群 {r$n_control_evaluable} 例、",
      "介入群 {r$n_intervention_evaluable} 例（合計 {r$n_total_evaluable} 例、",
      "割付比 {.f2(r$allocation_ratio)}:1）"
    )
  } else {
    glue::glue(
      "各群 {r$n_per_arm_evaluable} 例（合計 {r$n_total_evaluable} 例）"
    )
  }
  body <- glue::glue(
    "本研究の設計は「{design_label}」である。",
    "{n_line} における達成検出力を、R の {info$pkg} パッケージ（{info$fun}）を用いて算出した。",
    "その結果、達成検出力は {pw}% であった。",
    "公式の出典は {info$ref}。",
    .sep = "\n"
  )
  paste(body, tail, sep = "\n")
}

.en_power_calc_generic <- function(design_id, p, r, drop_tail, drop_tail_1) {
  info <- backend_info_for(design_id)
  pw   <- .fmt_power(r$achieved_power %||% 0)
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
    glue::glue("{r$n_per_arm_evaluable} pairs")
  } else if (!is.null(r$n_intervention_evaluable) &&
             !is.null(r$n_control_evaluable) &&
             !is.null(r$allocation_ratio) &&
             r$allocation_ratio != 1) {
    glue::glue(
      "{r$n_control_evaluable} controls and ",
      "{r$n_intervention_evaluable} in the intervention arm ",
      "(total {r$n_total_evaluable}, allocation {.f2(r$allocation_ratio)}:1)"
    )
  } else {
    glue::glue(
      "{r$n_per_arm_evaluable} participants per arm (total {r$n_total_evaluable})"
    )
  }
  body <- glue::glue(
    "The study design was {design_label}.",
    "The achieved power with {n_line} was calculated using the {info$pkg} R package ({info$fun}).",
    "The achieved power was {pw}%.",
    "The source of the formula is {info$ref}.",
    .sep = "\n"
  )
  paste(body, tail, sep = "\n")
}

# =========================================================================
# 日本語テンプレート（必要症例数モード）
# =========================================================================

.jp_ttest_m1 <- function(p, r, pw) {
  body <- glue::glue(
    "本研究の主要エンドポイントについて、介入群と 対照群の平均値の差を",
    "検出する 2 群並行群間比較試験を計画した。",
    "事前の情報（[文献を記載してください]）に基づき、",
    "介入群の平均を {.f2(p$mean_A)}、対照群の平均を {.f2(p$mean_B)}、",
    "介入群の SD を {.f2(p$sd_A)}、対照群の SD を {.f2(p$sd_B)} と想定した。",
    "両側有意水準 α={.fmt_alpha(p$alpha)}、検出力 1-β={.fmt_pct(pw)}%、",
    "最終的な脱落割合 {.fmt_pct(p$dropout)}% を",
    "考慮し、必要症例数を算出した結果、解析対象として各群 {r$n_per_arm_evaluable} 例、",
    "登録必要例数は各群 {r$n_per_arm_randomized} 例（合計 {r$n_total_randomized} 例）となった。",
    "必要症例数の公式は各群の SD を別々に用いる Welch 型の",
    "n = (z_{{1-α/2}} + z_{{1-β}})^2 × (σ_A^2 + σ_B^2) / Δ^2 に基づく。",
    .sep = "\n"
  )
  paste0(body, .cite_footer_jp("Section 3.2"))
}

.jp_ttest_m2 <- function(p, r, pw) {
  body <- glue::glue(
    "本研究の主要エンドポイントについて、介入群と 対照群の平均値の差",
    "（群間差 Δ = {.f2(p$diff)}）を検出する 2 群並行群間比較試験を計画した。",
    "事前の情報（[文献を記載してください]）に基づき、",
    "介入群の SD を {.f2(p$sd_A)}、対照群の SD を {.f2(p$sd_B)} と想定した。",
    "両側有意水準 α={.fmt_alpha(p$alpha)}、検出力 1-β={.fmt_pct(pw)}%、",
    "最終的な脱落割合 {.fmt_pct(p$dropout)}% を",
    "考慮し、必要症例数を算出した結果、解析対象として各群 {r$n_per_arm_evaluable} 例、",
    "登録必要例数は各群 {r$n_per_arm_randomized} 例（合計 {r$n_total_randomized} 例）となった。",
    "必要症例数の公式は各群の SD を別々に用いる Welch 型の",
    "n = (z_{{1-α/2}} + z_{{1-β}})^2 × (σ_A^2 + σ_B^2) / Δ^2 に基づく。",
    .sep = "\n"
  )
  paste0(body, .cite_footer_jp("Section 3.2"))
}

.jp_paired <- function(p, r, pw) {
  body <- glue::glue(
    "本研究の主要エンドポイントについて、同一被験者内の対応のある",
    "観測（例: 治療前後）の差を検出する試験を計画した。",
    "事前の情報（[文献を記載してください]）に基づき、",
    "差の平均を {.f2(p$diff_mean)}、差の SD を {.f2(p$sd_diff)} と想定した。",
    "両側有意水準 α={.fmt_alpha(p$alpha)}、検出力 1-β={.fmt_pct(pw)}%、",
    "最終的な脱落割合 {.fmt_pct(p$dropout)}% を",
    "考慮し、必要ペア数を算出した結果、解析対象として {r$n_per_arm_evaluable} ペア、",
    "登録必要ペア数は {r$n_per_arm_randomized} ペアとなった。",
    .sep = "\n"
  )
  paste0(body, .cite_footer_jp("Section 3.3"))
}

.jp_paired_corr <- function(p, r, pw) {
  body <- glue::glue(
    "本研究の主要エンドポイントについて、同一被験者内の対応のある",
    "観測（例: 治療前後）の差を検出する試験を計画した。",
    "事前の情報（[文献を記載してください]）に基づき、",
    "治療前の平均を {.f2(p$mean_1)}（SD {.f2(p$sd_1)}）、",
    "治療後の平均を {.f2(p$mean_2)}（SD {.f2(p$sd_2)}）、",
    "前後の相関係数を {.f2(p$r)} と想定した。",
    "これより差の SD は",
    "sqrt({.f2(p$sd_1)}^2 + {.f2(p$sd_2)}^2 − 2 × {.f2(p$r)} × {.f2(p$sd_1)} × {.f2(p$sd_2)}) = {.fmt_or_na(r$sd_diff)}",
    "（差の平均 {.fmt_or_na(r$diff_mean)}）と換算された。",
    "両側有意水準 α={.fmt_alpha(p$alpha)}、検出力 1-β={.fmt_pct(pw)}%、",
    "最終的な脱落割合 {.fmt_pct(p$dropout)}% を",
    "考慮し、必要ペア数を算出した結果、解析対象として {r$n_per_arm_evaluable} ペア、",
    "登録必要ペア数は {r$n_per_arm_randomized} ペアとなった。",
    .sep = "\n"
  )
  paste0(body, .cite_footer_jp("Section 3.3"))
}

.jp_binary_chisq <- function(p, r, pw) {
  body <- glue::glue(
    "本研究の主要エンドポイントについて、介入群と 対照群の発生割合の差を",
    "検出する 2 群並行群間比較試験を計画した。",
    "事前の情報（[文献を記載してください]）に基づき、",
    "介入群の割合を {.fmt_pct(p$p_A)}%、対照群の割合を {.fmt_pct(p$p_B)}% と想定した。",
    "両側有意水準 α={.fmt_alpha(p$alpha)}、検出力 1-β={.fmt_pct(pw)}%、",
    "最終的な脱落割合 {.fmt_pct(p$dropout)}% を",
    "考慮し、χ² 検定（1:1 割付）に必要な症例数を算出した結果、",
    "解析対象として各群 {r$n_per_arm_evaluable} 例、",
    "登録必要例数は各群 {r$n_per_arm_randomized} 例",
    "（合計 {r$n_total_randomized} 例）となった。",
    .sep = "\n"
  )
  paste0(body, .cite_footer_jp("Section 4.2"))
}

.jp_binary_fisher <- function(p, r, pw) {
  body <- glue::glue(
    "本研究の主要エンドポイントについて、介入群と 対照群の発生割合の差を",
    "Fisher の正確検定で検出する 2 群並行群間比較試験を計画した。",
    "事前の情報（[文献を記載してください]）に基づき、",
    "介入群の割合を {.fmt_pct(p$p_A)}%、対照群の割合を {.fmt_pct(p$p_B)}% と想定した。",
    "両側有意水準 α={.fmt_alpha(p$alpha)}、検出力 1-β={.fmt_pct(pw)}%、",
    "最終的な脱落割合 {.fmt_pct(p$dropout)}% を",
    "考慮し、必要症例数を算出した結果、解析対象として各群 {r$n_per_arm_evaluable} 例、",
    "登録必要例数は各群 {r$n_per_arm_randomized} 例（合計 {r$n_total_randomized} 例）となった。",
    "なお Fisher の正確検定の厳密な検出力計算は pwr に未搭載のため、",
    "χ² 検定ベースの近似を用いている。",
    .sep = "\n"
  )
  paste0(body, .cite_footer_jp("Section 5.2"))
}

.jp_one_mean <- function(p, r) {
  body <- glue::glue(
    "本研究の主要エンドポイントについて、平均値を 1 標本精度ベースで",
    "推定する試験を計画した。",
    "事前の情報（[文献を記載してください]）に基づき、SD を {.f2(p$sd)} と想定し、",
    "{.fmt_pct(p$conf_level)}% 信頼区間の半幅 {format(p$half_width)} の精度で推定することを目標とした。",
    "必要症例数の公式 n = (z_{{1-α/2}} × SD / E)^2 を用い、",
    "最終的な脱落割合 {.fmt_pct(p$dropout)}% を考慮して算出した結果、",
    "解析対象として {r$n_per_arm_evaluable} 例、登録必要例数は {r$n_per_arm_randomized} 例となった。",
    .sep = "\n"
  )
  paste0(body, .cite_footer_jp("Section 3.1, pp. 50-57", pwr = FALSE))
}

.jp_one_prop <- function(p, r) {
  method_jp <- switch(p$method,
    normal = "正規近似",
    wilson = "Wilson 法",
    exact  = "Exact 法（Clopper-Pearson）"
  )
  body <- glue::glue(
    "本研究の主要エンドポイントについて、発生割合を 1 標本精度ベースで",
    "推定する試験を計画した。",
    "事前の情報（[文献を記載してください]）に基づき、",
    "予想される発生割合を {.fmt_pct(p$p)}%、信頼区間の計算法は {method_jp} と設定し、",
    "{.fmt_pct(p$conf_level)}% 信頼区間の半幅 {format(p$half_width)} の精度で推定することを目標とした。",
    "最終的な脱落割合 {.fmt_pct(p$dropout)}% を考慮して算出した結果、",
    "解析対象として {r$n_per_arm_evaluable} 例、",
    "登録必要例数は {r$n_per_arm_randomized} 例となった。",
    .sep = "\n"
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
  body <- glue::glue(
    "本研究の主要エンドポイントについて、介入群（新治療）が 対照群（既存治療）",
    "に対して平均値で非劣性であることを示す 2 群並行群間比較試験を計画した。",
    "事前の情報（[文献を記載してください]）に基づき、",
    "介入群と 対照群の平均値の差（群間差 Δ）を {.f2(p$diff)}、介入群の SD を {.f2(p$sd_A)}、",
    "対照群の SD を {.f2(p$sd_B)} と想定し、非劣性マージン M を {.f2(p$margin)} と設定した。",
    "片側有意水準 α={.fmt_alpha(p$alpha)}、検出力 1-β={.fmt_pct(pw)}%、",
    "最終的な脱落割合 {.fmt_pct(p$dropout)}% を",
    "考慮し、必要症例数を算出した結果、解析対象として各群 {r$n_per_arm_evaluable} 例、",
    "登録必要例数は各群 {r$n_per_arm_randomized} 例（合計 {r$n_total_randomized} 例）となった。",
    "必要症例数の公式は各群の SD を別々に用いる",
    "n = (z_{{1-α}} + z_{{1-β}})^2 × (σ_A^2 + σ_B^2) / (Δ + M)^2 に基づく。",
    .sep = "\n"
  )
  paste0(body, .cite_footer_jp("Section 3.2"))
}

.jp_ttest_ni <- function(p, r, pw) {
  body <- glue::glue(
    "本研究の主要エンドポイントについて、介入群（新治療）が 対照群（既存治療）",
    "に対して平均値で非劣性であることを示す 2 群並行群間比較試験を計画した。",
    "事前の情報（[文献を記載してください]）に基づき、",
    "介入群の平均を {.f2(p$mean_A)}、対照群の平均を {.f2(p$mean_B)}、介入群の SD を {.f2(p$sd_A)}、",
    "対照群の SD を {.f2(p$sd_B)} と想定し、非劣性マージン M を {.f2(p$margin)} と設定した。",
    "片側有意水準 α={.fmt_alpha(p$alpha)}、検出力 1-β={.fmt_pct(pw)}%、",
    "最終的な脱落割合 {.fmt_pct(p$dropout)}% を",
    "考慮し、必要症例数を算出した結果、解析対象として各群 {r$n_per_arm_evaluable} 例、",
    "登録必要例数は各群 {r$n_per_arm_randomized} 例（合計 {r$n_total_randomized} 例）となった。",
    "必要症例数の公式は各群の SD を別々に用いる",
    "n = (z_{{1-α}} + z_{{1-β}})^2 × (σ_A^2 + σ_B^2) / (Δ + M)^2 に基づく",
    "（Δ = μ_A − μ_B）。",
    .sep = "\n"
  )
  paste0(body, .cite_footer_jp("Section 3.2"))
}

.jp_paired_ni <- function(p, r, pw) {
  body <- glue::glue(
    "本研究の主要エンドポイントについて、同一被験者内で新治療が既存治療",
    "に対して非劣性であることを示す試験を計画した。",
    "事前の情報（[文献を記載してください]）に基づき、差の平均を {.f2(p$diff_mean)}、",
    "差の SD を {.f2(p$sd_diff)} と想定し、非劣性マージン M を {.f2(p$margin)} と設定した。",
    "片側有意水準 α={.fmt_alpha(p$alpha)}、検出力 1-β={.fmt_pct(pw)}%、",
    "最終的な脱落割合 {.fmt_pct(p$dropout)}% を",
    "考慮し、必要ペア数を算出した結果、解析対象として {r$n_per_arm_evaluable} ペア、",
    "登録必要ペア数は {r$n_per_arm_randomized} ペアとなった。",
    .sep = "\n"
  )
  paste0(body, .cite_footer_jp("Section 3.3"))
}

.jp_binary_ni <- function(p, r, pw) {
  body <- glue::glue(
    "本研究の主要エンドポイントについて、介入群（新治療）が 対照群（既存治療）",
    "に対して発生割合で非劣性であることを示す 2 群並行群間比較試験を計画した。",
    "事前の情報（[文献を記載してください]）に基づき、",
    "介入群の割合を {.fmt_pct(p$p_A)}%、対照群の割合を {.fmt_pct(p$p_B)}% と想定し、",
    "非劣性マージン M を {.fmt_pct(p$margin)}%（リスク差の単位）と設定した。",
    "片側有意水準 α={.fmt_alpha(p$alpha)}、検出力 1-β={.fmt_pct(pw)}%、",
    "最終的な脱落割合 {.fmt_pct(p$dropout)}% を",
    "考慮し、リスク差ベースの正規近似に基づき必要症例数を算出した結果、",
    "解析対象として各群 {r$n_per_arm_evaluable} 例、",
    "登録必要例数は各群 {r$n_per_arm_randomized} 例",
    "（合計 {r$n_total_randomized} 例）となった。",
    "なお、より正確な計算には Farrington-Manning 法の使用を推奨する。",
    .sep = "\n"
  )
  paste0(body, .cite_footer_jp("Section 4.2", pwr = FALSE))
}

# =========================================================================
# 英語テンプレート（必要症例数モード）
# =========================================================================

.en_ttest_m1 <- function(p, r, pw) {
  body <- glue::glue(
    "For the primary endpoint, we planned a two-arm parallel-group trial",
    "to detect a difference in means between Group A and Group B.",
    "Based on prior evidence ([please insert reference]), we assumed a",
    "mean of {.f2(p$mean_A)} in Group A and {.f2(p$mean_B)} in Group B,",
    "with SDs of {.f2(p$sd_A)} and {.f2(p$sd_B)}, respectively.",
    "With a two-sided significance level of α={.fmt_alpha(p$alpha)},",
    "power 1-β={.fmt_pct(pw)}%, and an anticipated final dropout rate of",
    "{.fmt_pct(p$dropout)}%, the calculation yielded {r$n_per_arm_evaluable}",
    "evaluable participants per arm and a target enrolment of",
    "{r$n_per_arm_randomized} per arm (total {r$n_total_randomized}).",
    "The sample-size formula uses each group's SD separately:",
    "n = (z_{{1-α/2}} + z_{{1-β}})^2 × (σ_A^2 + σ_B^2) / Δ^2.",
    .sep = "\n"
  )
  paste0(body, .cite_footer_en("Section 3.2"))
}

.en_ttest_m2 <- function(p, r, pw) {
  body <- glue::glue(
    "For the primary endpoint, we planned a two-arm parallel-group trial",
    "to detect a between-group difference in means (Δ = {.f2(p$diff)}).",
    "Based on prior evidence ([please insert reference]), we assumed",
    "SDs of {.f2(p$sd_A)} and {.f2(p$sd_B)} in Groups A and B, respectively.",
    "With a two-sided significance level of α={.fmt_alpha(p$alpha)},",
    "power 1-β={.fmt_pct(pw)}%, and an anticipated final dropout rate of",
    "{.fmt_pct(p$dropout)}%, the calculation yielded",
    "{r$n_per_arm_evaluable} evaluable participants per arm and a target",
    "enrolment of {r$n_per_arm_randomized} per arm (total {r$n_total_randomized}).",
    "The sample-size formula uses each group's SD separately:",
    "n = (z_{{1-α/2}} + z_{{1-β}})^2 × (σ_A^2 + σ_B^2) / Δ^2.",
    .sep = "\n"
  )
  paste0(body, .cite_footer_en("Section 3.2"))
}

.en_paired <- function(p, r, pw) {
  body <- glue::glue(
    "For the primary endpoint, we planned a trial to detect a",
    "within-subject difference between paired observations (e.g. pre-",
    "and post-treatment). Based on prior evidence ([please insert",
    "reference]), we assumed a mean paired difference of {.f2(p$diff_mean)}",
    "and an SD of differences of {.f2(p$sd_diff)}.",
    "With a two-sided significance level of α={.fmt_alpha(p$alpha)},",
    "power 1-β={.fmt_pct(pw)}%, and an anticipated final dropout rate of",
    "{.fmt_pct(p$dropout)}%, the calculation yielded",
    "{r$n_per_arm_evaluable} evaluable pairs and a target enrolment of",
    "{r$n_per_arm_randomized} pairs.",
    .sep = "\n"
  )
  paste0(body, .cite_footer_en("Section 3.3"))
}

.en_paired_corr <- function(p, r, pw) {
  body <- glue::glue(
    "For the primary endpoint, we planned a trial to detect a",
    "within-subject difference between paired observations.",
    "Based on prior evidence ([please insert reference]), we assumed a",
    "mean of {.f2(p$mean_1)} (SD {.f2(p$sd_1)}) at baseline and",
    "{.f2(p$mean_2)} (SD {.f2(p$sd_2)}) post-treatment, with a",
    "correlation coefficient of r = {.f2(p$r)}.",
    "The SD of paired differences was calculated as",
    "sqrt({.f2(p$sd_1)}^2 + {.f2(p$sd_2)}^2 - 2 x {.f2(p$r)} x {.f2(p$sd_1)} x {.f2(p$sd_2)}) = {.fmt_or_na(r$sd_diff)}",
    "(mean difference = {.fmt_or_na(r$diff_mean)}).",
    "With α={.fmt_alpha(p$alpha)} (two-sided), power 1-β={.fmt_pct(pw)}%,",
    "and an anticipated final dropout rate of {.fmt_pct(p$dropout)}%,",
    "the calculation yielded {r$n_per_arm_evaluable} evaluable pairs and",
    "a target enrolment of {r$n_per_arm_randomized} pairs.",
    .sep = "\n"
  )
  paste0(body, .cite_footer_en("Section 3.3"))
}

.en_binary_chisq <- function(p, r, pw) {
  body <- glue::glue(
    "For the primary endpoint, we planned a two-arm parallel-group trial",
    "to detect a difference in event proportions between Group A and",
    "Group B using the chi-squared test.",
    "Based on prior evidence ([please insert reference]), we assumed",
    "event rates of {.fmt_pct(p$p_A)}% in Group A and {.fmt_pct(p$p_B)}% in Group B.",
    "With α={.fmt_alpha(p$alpha)} (two-sided), power 1-β={.fmt_pct(pw)}%,",
    "and an anticipated final dropout rate of {.fmt_pct(p$dropout)}%,",
    "the calculation yielded {r$n_per_arm_evaluable} evaluable participants per arm",
    "and a target enrolment of {r$n_per_arm_randomized} per arm (total {r$n_total_randomized}).",
    .sep = "\n"
  )
  paste0(body, .cite_footer_en("Section 4.2"))
}

.en_binary_fisher <- function(p, r, pw) {
  body <- glue::glue(
    "For the primary endpoint, we planned a two-arm parallel-group trial",
    "to detect a difference in event proportions between Group A and",
    "Group B using Fisher's exact test.",
    "Based on prior evidence ([please insert reference]), we assumed",
    "event rates of {.fmt_pct(p$p_A)}% in Group A and {.fmt_pct(p$p_B)}% in Group B.",
    "With α={.fmt_alpha(p$alpha)} (two-sided), power 1-β={.fmt_pct(pw)}%,",
    "and an anticipated final dropout rate of {.fmt_pct(p$dropout)}%,",
    "the calculation yielded {r$n_per_arm_evaluable} evaluable participants per arm",
    "and a target enrolment of {r$n_per_arm_randomized} per arm (total {r$n_total_randomized}).",
    "As an exact-test power calculation is not implemented in pwr,",
    "a chi-squared-based approximation was used.",
    .sep = "\n"
  )
  paste0(body, .cite_footer_en("Section 5.2"))
}

.en_one_mean <- function(p, r) {
  body <- glue::glue(
    "For the primary endpoint, we planned a one-sample precision-based",
    "study to estimate a mean. Based on prior evidence ([please insert",
    "reference]), we assumed an SD of {.f2(p$sd)} and targeted a",
    "{.fmt_pct(p$conf_level)}% confidence-interval half-width of {format(p$half_width)}.",
    "Using n = (z_{{1-α/2}} × SD / E)^2 and an anticipated final dropout",
    "rate of {.fmt_pct(p$dropout)}%, the calculation yielded",
    "{r$n_per_arm_evaluable} evaluable participants and a target enrolment of",
    "{r$n_per_arm_randomized}.",
    .sep = "\n"
  )
  paste0(body, .cite_footer_en("Section 3.1, pp. 50-57", pwr = FALSE))
}

.en_one_prop <- function(p, r) {
  method_en <- switch(p$method,
    normal = "the normal approximation",
    wilson = "Wilson's method",
    exact  = "the exact Clopper-Pearson method"
  )
  body <- glue::glue(
    "For the primary endpoint, we planned a one-sample precision-based",
    "study to estimate a proportion. Based on prior evidence ([please",
    "insert reference]), we assumed an expected proportion of {.fmt_pct(p$p)}%",
    "and targeted a {.fmt_pct(p$conf_level)}% confidence-interval half-width",
    "of {format(p$half_width)}, computed using {method_en}.",
    "With an anticipated final dropout rate of {.fmt_pct(p$dropout)}%,",
    "the calculation yielded {r$n_per_arm_evaluable} evaluable participants",
    "and a target enrolment of {r$n_per_arm_randomized}.",
    .sep = "\n"
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
  body <- glue::glue(
    "For the primary endpoint, we planned a two-arm parallel-group trial",
    "to demonstrate non-inferiority of Group A (new treatment) to",
    "Group B (standard of care). Based on prior evidence ([please insert",
    "reference]), we assumed a between-group mean difference of {.f2(p$diff)},",
    "SDs of {.f2(p$sd_A)} and {.f2(p$sd_B)} in Groups A and B, and",
    "pre-specified a non-inferiority margin of M = {.f2(p$margin)}",
    "(on the mean-difference scale).",
    "With a one-sided significance level of α={.fmt_alpha(p$alpha)},",
    "power 1-β={.fmt_pct(pw)}%, and an anticipated final dropout rate of",
    "{.fmt_pct(p$dropout)}%, the calculation yielded",
    "{r$n_per_arm_evaluable} evaluable participants per arm and a target",
    "enrolment of {r$n_per_arm_randomized} per arm (total {r$n_total_randomized}).",
    "The sample-size formula uses each group's SD separately:",
    "n = (z_{{1-α}} + z_{{1-β}})^2 × (σ_A^2 + σ_B^2) / (Δ + M)^2.",
    .sep = "\n"
  )
  paste0(body, .cite_footer_en("Section 3.2"))
}

.en_ttest_ni <- function(p, r, pw) {
  body <- glue::glue(
    "For the primary endpoint, we planned a two-arm parallel-group trial",
    "to demonstrate non-inferiority of Group A (new treatment) to",
    "Group B (standard of care).",
    "Based on prior evidence ([please insert reference]), we assumed",
    "means of {.f2(p$mean_A)} in Group A and {.f2(p$mean_B)} in Group B,",
    "with SDs of {.f2(p$sd_A)} and {.f2(p$sd_B)}, and pre-specified a",
    "non-inferiority margin of M = {.f2(p$margin)}.",
    "With α={.fmt_alpha(p$alpha)} (one-sided), power 1-β={.fmt_pct(pw)}%,",
    "and an anticipated final dropout rate of {.fmt_pct(p$dropout)}%,",
    "the calculation yielded {r$n_per_arm_evaluable} evaluable participants",
    "per arm and a target enrolment of {r$n_per_arm_randomized} per arm",
    "(total {r$n_total_randomized}).",
    "The sample-size formula uses each group's SD separately:",
    "n = (z_{{1-α}} + z_{{1-β}})^2 × (σ_A^2 + σ_B^2) / (Δ + M)^2",
    "(Δ = μ_A − μ_B).",
    .sep = "\n"
  )
  paste0(body, .cite_footer_en("Section 3.2"))
}

.en_paired_ni <- function(p, r, pw) {
  body <- glue::glue(
    "For the primary endpoint, we planned a within-subject comparison",
    "to demonstrate non-inferiority of the new treatment to the",
    "standard treatment. Based on prior evidence ([please insert",
    "reference]), we assumed a mean paired difference of {.f2(p$diff_mean)}",
    "with an SD of differences of {.f2(p$sd_diff)}, and pre-specified a",
    "non-inferiority margin of M = {.f2(p$margin)}.",
    "With α={.fmt_alpha(p$alpha)} (one-sided), power 1-β={.fmt_pct(pw)}%,",
    "and an anticipated final dropout rate of {.fmt_pct(p$dropout)}%,",
    "the calculation yielded {r$n_per_arm_evaluable} evaluable pairs",
    "and a target enrolment of {r$n_per_arm_randomized} pairs.",
    .sep = "\n"
  )
  paste0(body, .cite_footer_en("Section 3.3"))
}

.en_binary_ni <- function(p, r, pw) {
  body <- glue::glue(
    "For the primary endpoint, we planned a two-arm parallel-group trial",
    "to demonstrate non-inferiority of Group A (new treatment) to",
    "Group B (standard of care) in event proportions.",
    "Based on prior evidence ([please insert reference]), we assumed",
    "event rates of {.fmt_pct(p$p_A)}% in Group A and {.fmt_pct(p$p_B)}% in Group B,",
    "and pre-specified a non-inferiority margin of M = {.fmt_pct(p$margin)}%",
    "(on the risk-difference scale). With α={.fmt_alpha(p$alpha)} (one-sided),",
    "power 1-β={.fmt_pct(pw)}%, and an anticipated final dropout rate of",
    "{.fmt_pct(p$dropout)}%, the calculation using a normal-approximation",
    "risk-difference formula yielded {r$n_per_arm_evaluable} evaluable",
    "participants per arm and a target enrolment of {r$n_per_arm_randomized}",
    "per arm (total {r$n_total_randomized}).",
    "Note that a more accurate calculation using the Farrington-Manning method",
    "is recommended.",
    .sep = "\n"
  )
  paste0(body, .cite_footer_en("Section 4.2", pwr = FALSE))
}

# =========================================================================
# 新規デザイン（Phase 2 / 3）の論文テンプレート
# =========================================================================

# ---- D1 McNemar ----
.jp_mcnemar <- function(p, r, pw) {
  body <- glue::glue(
    "本研究の主要エンドポイントについて、対応のある二値アウトカムの",
    "前後（または 2 条件間）の発生割合の差を McNemar 検定で検出する",
    "試験を計画した。事前の情報（[文献を記載してください]）に基づき、",
    "不一致ペアの割合 p_disc を {.f3(p$p_disc)}、",
    "そのうち A が優位である割合 ψ を {.f2(p$psi)} と仮定した。",
    "両側有意水準 α={.fmt_alpha(p$alpha)}、検出力 1-β={.fmt_pct(pw)}%、",
    "最終的な脱落割合 {.fmt_pct(p$dropout)}% を",
    "考慮し、Connor (1987) の公式に基づき必要ペア数を算出した結果、",
    "解析対象として {r$n_per_arm_evaluable} ペア、",
    "登録必要ペア数は {r$n_per_arm_randomized} ペアとなった。",
    .sep = "\n"
  )
  paste0(body, .cite_footer_jp(
    section_text = NULL, pwr = FALSE,
    alt_text = "必要ペア数の公式は Connor, R. J. (1987), Biometrics 43: 207-211 に準拠している。"
  ))
}
.en_mcnemar <- function(p, r, pw) {
  body <- glue::glue(
    "For the primary endpoint, we planned a study to detect a difference",
    "in a paired-binary outcome between two conditions using McNemar's",
    "test. Based on prior evidence ([please insert reference]), we assumed",
    "a discordant-pair proportion of {.f3(p$p_disc)} (p_disc) and a proportion of",
    "{.f2(p$psi)} (ψ) favouring A among discordant pairs.",
    "With α={.fmt_alpha(p$alpha)} (two-sided), power 1-β={.fmt_pct(pw)}%,",
    "and an anticipated final dropout rate of {.fmt_pct(p$dropout)}%,",
    "the calculation yielded {r$n_per_arm_evaluable} evaluable pairs and",
    "a target enrolment of {r$n_per_arm_randomized} pairs.",
    .sep = "\n"
  )
  paste0(body, .cite_footer_en(
    section_text = NULL, pwr = FALSE,
    alt_text = "The sample-size formula follows Connor, R. J. (1987), Biometrics 43: 207-211."
  ))
}

# ---- D2 ANCOVA ----
.jp_ancova <- function(p, r, pw) {
  glue::glue(
    "本研究では、連続量アウトカム [アウトカム名を指定] における 2 群の",
    "平均値の差を、共変量（例: ベースライン測定値）で調整した ANCOVA",
    "モデルで検定することを主目的とした。",
    "過去の知見 [参考文献を挿入] から、",
    "介入群平均を {.f2(p$mean_A)}、対照群平均を {.f2(p$mean_B)}、",
    "共通 SD を {.f2(p$sd_common)}、",
    "共変量とアウトカムの相関係数を {.f2(p$r)} と想定した。",
    "両側有意水準 {.fmt_alpha(p$alpha)}、目標検出力 {.fmt_pct(pw)}% のもと、",
    "Borm ら (2007) の分散低減公式（SD_adj = SD × sqrt(1 - r²)）を用い、",
    "pwr パッケージ（version {get_pwr_version()}）の pwr.t.test 関数により",
    "必要症例数を計算した。",
    "その結果、解析対象として各群 {r$n_per_arm_evaluable} 例",
    "（合計 {r$n_total_evaluable} 例）が必要であった。",
    "脱落率を {.fmt_pct(p$dropout)}% と見込む場合、登録必要症例数は",
    "各群 {r$n_per_arm_randomized} 例（合計 {r$n_total_randomized} 例）となる。",
    .sep = "\n"
  )
}
.en_ancova <- function(p, r, pw) {
  glue::glue(
    "The primary objective was to compare the continuous outcome [please",
    "specify] between two groups using an ANCOVA model adjusted for a",
    "covariate (e.g. baseline measurement). Based on prior evidence [please",
    "insert reference], we assumed means of {.f2(p$mean_A)} and {.f2(p$mean_B)}",
    "in Groups A and B with a common SD of {.f2(p$sd_common)}, and a",
    "correlation of {.f2(p$r)} between the covariate and the outcome.",
    "Assuming a two-sided significance level of {.fmt_alpha(p$alpha)} and a",
    "power of {.fmt_pct(pw)}%, the required sample size was calculated using",
    "the variance-reduction formula of Borm et al. (2007) (SD_adj = SD × ",
    "sqrt(1 - r²)) and pwr.t.test in the R package pwr (version {get_pwr_version()}).",
    "The calculation indicated that {r$n_per_arm_evaluable} evaluable participants",
    "per arm (total {r$n_total_evaluable}) would be required.",
    "Accounting for an anticipated dropout rate of {.fmt_pct(p$dropout)}%,",
    "the target number of randomized participants was set to",
    "{r$n_per_arm_randomized} per arm (total {r$n_total_randomized}).",
    .sep = "\n"
  )
}

# ---- D3 log-rank ----
.jp_logrank <- function(p, r, pw) {
  HR_used <- r$HR %||% p$HR %||% NA_real_
  mode <- p$input_mode %||% (if (!is.null(p$median_T)) "mst" else "hr")
  median_T <- p$median_T %||% NA_real_
  assumption <- if (mode == "mst") {
    glue::glue(
      "対照群の中央生存期間を {.f1(p$median_C)}、",
      "治療群の中央生存期間を {.f1(median_T)} と想定",
      "（指数分布仮定により HR = {.f3(HR_used)} と換算）"
    )
  } else {
    glue::glue(
      "対照群の中央生存期間を {.f1(p$median_C)}、",
      "ハザード比を {.f2(HR_used)} と想定"
    )
  }
  alloc_str <- glue::glue("{.f2(p$allocation_ratio %||% 1)}:1（治療群:対照群）")
  events <- r$events_required %||% NA
  body <- glue::glue(
    "本研究の主要エンドポイントである生存時間アウトカムについて、2 群の",
    "生存関数の差を log-rank 検定で検出する試験を計画した。",
    "事前の情報（[文献を記載してください]）に基づき、{assumption} し、",
    "登録期間 {.f1(p$accrual)}、追跡期間 {.f1(p$followup)} を仮定（指数分布、一様リクルート）した。",
    "割付比は {alloc_str}、片側有意水準 α={.fmt_alpha(p$alpha)}、",
    "検出力 1-β={.fmt_pct(pw)}%、最終的な脱落割合 {.fmt_pct(p$dropout)}% を考慮し、",
    "必要イベント数および総症例数を算出した結果、",
    "必要イベント数 {events} 件、総症例数 {r$n_total_evaluable} 例（各群 {r$n_per_arm_evaluable} 例）、",
    "登録必要例数は各群 {r$n_per_arm_randomized} 例（合計 {r$n_total_randomized} 例）となった。",
    .sep = "\n"
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
  median_T <- p$median_T %||% NA_real_
  assumption <- if (mode == "mst") {
    glue::glue(
      "a median survival of {.f1(p$median_C)} in the control arm and ",
      "{.f1(median_T)} in the treatment arm ",
      "(giving HR = {.f3(HR_used)} under an exponential assumption)"
    )
  } else {
    glue::glue(
      "a median survival of {.f1(p$median_C)} in the control arm and ",
      "a hazard ratio of {.f2(HR_used)}"
    )
  }
  alloc_str <- glue::glue("{.f2(p$allocation_ratio %||% 1)}:1 (treatment:control)")
  events <- r$events_required %||% NA
  body <- glue::glue(
    "For the primary time-to-event endpoint, we planned a trial comparing",
    "survival between two groups using the log-rank test. Based on prior",
    "evidence ([please insert reference]), we assumed {assumption},",
    "with an enrolment period of {.f1(p$accrual)} and additional follow-up",
    "of {.f1(p$followup)} (exponential survival, uniform recruitment).",
    "The allocation ratio was {alloc_str}.",
    "With α={.fmt_alpha(p$alpha)} (one-sided), power 1-β={.fmt_pct(pw)}%,",
    "and an anticipated final dropout rate of {.fmt_pct(p$dropout)}%,",
    "the calculation yielded {events} required events and",
    "a total of {r$n_total_evaluable} evaluable participants ({r$n_per_arm_evaluable} per arm),",
    "with a target enrolment of {r$n_per_arm_randomized} per arm (total {r$n_total_randomized}).",
    .sep = "\n"
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
  glue::glue(
    "本研究では、{as.integer(p$k)} 時点の反復測定による連続量アウトカム",
    "[アウトカム名を指定] を、両群の post-baseline 平均値で比較することを主目的とした。",
    "過去の知見 [参考文献を挿入] から、",
    "介入群平均を {.f2(p$mean_A)}、対照群平均を {.f2(p$mean_B)}、",
    "1 時点の SD を {.f2(p$sd_common)}、",
    "被験者内相関（compound symmetry）を {.f2(p$rho)} と想定した。",
    "両側有意水準 {.fmt_alpha(p$alpha)}、目標検出力 {.fmt_pct(pw)}% のもと、",
    "Diggle ら (2002) による平均の有効 SD 公式（SD × sqrt((1 + (k-1)ρ)/k)）を用い、",
    "pwr パッケージ（version {get_pwr_version()}）の pwr.t.test 関数により必要症例数を",
    "計算した。",
    "その結果、解析対象として各群 {r$n_per_arm_evaluable} 例",
    "（合計 {r$n_total_evaluable} 例）が必要であった。",
    "脱落率を {.fmt_pct(p$dropout)}% と見込む場合、登録必要症例数は",
    "各群 {r$n_per_arm_randomized} 例（合計 {r$n_total_randomized} 例）となる。",
    .sep = "\n"
  )
}
.en_longitudinal <- function(p, r, pw) {
  glue::glue(
    "The primary objective was to compare post-baseline means of a",
    "continuous outcome [please specify], measured {as.integer(p$k)} times,",
    "between two groups. Based on prior evidence [please insert reference],",
    "we assumed means of {.f2(p$mean_A)} and {.f2(p$mean_B)} in Groups A and B",
    "with a common SD of {.f2(p$sd_common)} at each time point, and a",
    "within-subject correlation (compound symmetry) of {.f2(p$rho)}.",
    "Assuming a two-sided significance level of {.fmt_alpha(p$alpha)} and a",
    "power of {.fmt_pct(pw)}%, the required sample size was calculated using",
    "the effective-SD formula of Diggle et al. (2002)",
    "(SD × sqrt((1 + (k-1)ρ)/k)) and pwr.t.test in the R package pwr",
    "(version {get_pwr_version()}). The calculation indicated that",
    "{r$n_per_arm_evaluable} evaluable participants per arm",
    "(total {r$n_total_evaluable}) would be required.",
    "Accounting for an anticipated dropout rate of {.fmt_pct(p$dropout)}%,",
    "the target number of randomized participants was set to",
    "{r$n_per_arm_randomized} per arm (total {r$n_total_randomized}).",
    .sep = "\n"
  )
}

# ---- D5 group sequential ----
.jp_group_sequential <- function(p, r, pw) {
  inflation <- r$inflation %||% NA_real_
  glue::glue(
    "本研究では、連続量アウトカム [アウトカム名を指定] における 2 群比較",
    "を、{p$boundary} 境界を用いた {as.integer(p$K)} 回の解析（最終解析を含む）",
    "による群逐次デザインで実施することとした。",
    "過去の知見 [参考文献を挿入] から、",
    "介入群平均を {.f2(p$mean_A)}、対照群平均を {.f2(p$mean_B)}、",
    "共通 SD を {.f2(p$sd_common)} と想定した。",
    "両側有意水準 {.fmt_alpha(p$alpha)}、目標検出力 {.fmt_pct(pw)}% のもと、",
    "固定デザインの必要症例数を pwr パッケージ（version {get_pwr_version()}）の",
    "pwr.t.test 関数で算出し、Jennison & Turnbull (2000) による inflation factor",
    "（{p$boundary} 境界、K={as.integer(p$K)}）{.f3(inflation)} を適用した。",
    "その結果、解析対象として各群 {r$n_per_arm_evaluable} 例",
    "（合計 {r$n_total_evaluable} 例）が必要であった。",
    "脱落率を {.fmt_pct(p$dropout)}% と見込む場合、登録必要症例数は",
    "各群 {r$n_per_arm_randomized} 例（合計 {r$n_total_randomized} 例）となる。",
    .sep = "\n"
  )
}
.en_group_sequential <- function(p, r, pw) {
  inflation <- r$inflation %||% NA_real_
  glue::glue(
    "The trial used a group-sequential design with {as.integer(p$K)} analyses",
    "(including the final analysis) and {p$boundary} boundaries for a",
    "two-group comparison of a continuous outcome [please specify].",
    "Based on prior evidence [please insert reference], we assumed",
    "means of {.f2(p$mean_A)} and {.f2(p$mean_B)} with a common SD of {.f2(p$sd_common)}.",
    "Assuming a two-sided overall significance level of {.fmt_alpha(p$alpha)}",
    "and a power of {.fmt_pct(pw)}%, the fixed-design sample size was calculated",
    "via pwr.t.test in the R package pwr (version {get_pwr_version()}),",
    "and the inflation factor of Jennison & Turnbull (2000) for the",
    "{p$boundary} boundary at K={as.integer(p$K)} ({.f3(inflation)}) was applied.",
    "The calculation indicated that {r$n_per_arm_evaluable} evaluable",
    "participants per arm (total {r$n_total_evaluable}) would be required.",
    "Accounting for an anticipated dropout rate of {.fmt_pct(p$dropout)}%,",
    "the target number of randomized participants was set to",
    "{r$n_per_arm_randomized} per arm (total {r$n_total_randomized}).",
    .sep = "\n"
  )
}

# ---- D6 cluster (continuous) ----
.jp_cluster_cont <- function(p, r, pw) {
  DE <- r$DE %||% NA_real_
  K_per_arm <- as.integer(r$K_per_arm %||% 0L)
  glue::glue(
    "本研究は、連続量アウトカム [アウトカム名を指定] を主要評価項目とした",
    "クラスターランダム化比較試験として計画した。",
    "過去の知見 [参考文献を挿入] から、",
    "介入群平均を {.f2(p$mean_A)}、対照群平均を {.f2(p$mean_B)}、",
    "共通 SD を {.f2(p$sd_common)}、",
    "平均クラスターサイズを {as.integer(p$m)}、",
    "級内相関係数 ICC を {.f3(p$ICC)} と想定した。",
    "両側有意水準 {.fmt_alpha(p$alpha)}、目標検出力 {.fmt_pct(pw)}% のもと、",
    "Donner & Klar (2000) に基づき Design Effect DE = 1 + (m-1)ICC = {.f3(DE)} を適用し、",
    "pwr パッケージ（version {get_pwr_version()}）の pwr.t.test 関数による iid 必要症例数に",
    "DE を掛けて必要症例数を算出した。",
    "その結果、解析対象として各群 {r$n_per_arm_evaluable} 例、",
    "必要クラスター数は各群 {K_per_arm} と算出された。",
    "脱落率を {.fmt_pct(p$dropout)}% と見込む場合、登録必要症例数は",
    "各群 {r$n_per_arm_randomized} 例（合計 {r$n_total_randomized} 例）となる。",
    .sep = "\n"
  )
}
.en_cluster_cont <- function(p, r, pw) {
  DE <- r$DE %||% NA_real_
  K_per_arm <- as.integer(r$K_per_arm %||% 0L)
  glue::glue(
    "The trial was designed as a cluster-randomised controlled trial with a",
    "continuous primary outcome [please specify]. Based on prior evidence",
    "[please insert reference], we assumed means of {.f2(p$mean_A)} and",
    "{.f2(p$mean_B)} with a common SD of {.f2(p$sd_common)}, an average",
    "cluster size of {as.integer(p$m)}, and an intra-cluster correlation",
    "(ICC) of {.f3(p$ICC)}.",
    "Assuming a two-sided significance level of {.fmt_alpha(p$alpha)} and a",
    "power of {.fmt_pct(pw)}%, the required sample size was obtained by",
    "multiplying the iid sample size from pwr.t.test (pwr version {get_pwr_version()})",
    "by the design effect DE = 1 + (m-1)ICC = {.f3(DE)} (Donner & Klar 2000).",
    "The calculation indicated that {r$n_per_arm_evaluable} evaluable",
    "participants per arm and {K_per_arm} clusters per arm would be required.",
    "Accounting for an anticipated dropout rate of {.fmt_pct(p$dropout)}%,",
    "the target number of randomized participants was set to",
    "{r$n_per_arm_randomized} per arm (total {r$n_total_randomized}).",
    .sep = "\n"
  )
}

# ---- D6 cluster (binary) ----
.jp_cluster_bin <- function(p, r, pw) {
  DE <- r$DE %||% NA_real_
  K_per_arm <- as.integer(r$K_per_arm %||% 0L)
  glue::glue(
    "本研究は、二値アウトカム [アウトカム名を指定] を主要評価項目とした",
    "クラスターランダム化比較試験として計画した。",
    "過去の知見 [参考文献を挿入] から、",
    "介入群の発生割合を {.fmt_pct(p$p_A)}%、対照群の発生割合を {.fmt_pct(p$p_B)}%、",
    "平均クラスターサイズを {as.integer(p$m)}、",
    "級内相関係数 ICC を {.f3(p$ICC)} と想定した。",
    "両側有意水準 {.fmt_alpha(p$alpha)}、目標検出力 {.fmt_pct(pw)}% のもと、",
    "Donner & Klar (2000) に基づき Design Effect DE = 1 + (m-1)ICC = {.f3(DE)} を適用し、",
    "pwr パッケージ（version {get_pwr_version()}）の pwr.2p.test 関数による iid 必要症例数に",
    "DE を掛けて必要症例数を算出した。",
    "その結果、解析対象として各群 {r$n_per_arm_evaluable} 例、",
    "必要クラスター数は各群 {K_per_arm} と算出された。",
    "脱落率を {.fmt_pct(p$dropout)}% と見込む場合、登録必要症例数は",
    "各群 {r$n_per_arm_randomized} 例（合計 {r$n_total_randomized} 例）となる。",
    .sep = "\n"
  )
}
.en_cluster_bin <- function(p, r, pw) {
  DE <- r$DE %||% NA_real_
  K_per_arm <- as.integer(r$K_per_arm %||% 0L)
  glue::glue(
    "The trial was designed as a cluster-randomised controlled trial with a",
    "binary primary outcome [please specify]. Based on prior evidence",
    "[please insert reference], we assumed event proportions of {.fmt_pct(p$p_A)}%",
    "and {.fmt_pct(p$p_B)}% in Groups A and B, an average cluster size of",
    "{as.integer(p$m)}, and an intra-cluster correlation (ICC) of {.f3(p$ICC)}.",
    "Assuming a two-sided significance level of {.fmt_alpha(p$alpha)} and a",
    "power of {.fmt_pct(pw)}%, the required sample size was obtained by",
    "multiplying the iid sample size from pwr.2p.test (pwr version {get_pwr_version()})",
    "by the design effect DE = 1 + (m-1)ICC = {.f3(DE)} (Donner & Klar 2000).",
    "The calculation indicated that {r$n_per_arm_evaluable} evaluable participants",
    "per arm and {K_per_arm} clusters per arm would be required.",
    "Accounting for an anticipated dropout rate of {.fmt_pct(p$dropout)}%,",
    "the target number of randomized participants was set to",
    "{r$n_per_arm_randomized} per arm (total {r$n_total_randomized}).",
    .sep = "\n"
  )
}

# ---- D7 diagnostic ----
.jp_diagnostic <- function(p, r) {
  body <- glue::glue(
    "本研究の主要エンドポイントについて、診断検査の精度（感度 Se・特異度 Sp）",
    "を指定した信頼区間の半幅で推定する試験を計画した。",
    "事前の情報（[文献を記載してください]）に基づき、",
    "予想感度を {.fmt_pct(p$Se)}%、予想特異度を {.fmt_pct(p$Sp)}%、",
    "対象集団の有病率を {.fmt_pct(p$prev)}% と想定し、",
    "{.fmt_pct(p$conf_level)}% 信頼区間の目標半幅を {.f3(p$half_width)} とした。",
    "最終的な脱落割合 {.fmt_pct(p$dropout)}% を考慮し、",
    "Buderer (1996) の正規近似に基づき必要症例数を算出した結果、",
    "解析対象として {r$n_per_arm_evaluable} 例",
    "（疾患あり {r$n_dis_required %||% 0L} 例、疾患なし {r$n_non_required %||% 0L} 例の要件を満たす）、",
    "登録必要例数は {r$n_per_arm_randomized} 例となった。",
    .sep = "\n"
  )
  paste0(body, .cite_footer_jp(
    section_text = NULL, pwr = FALSE,
    alt_text = "必要症例数の公式は Buderer, N. M. (1996), Acad Emerg Med 3: 895-900 に準拠している。"
  ))
}
.en_diagnostic <- function(p, r) {
  body <- glue::glue(
    "For the primary endpoint, we planned a study to estimate the",
    "diagnostic accuracy (both sensitivity and specificity) of a test.",
    "Based on prior evidence ([please insert reference]), we assumed an",
    "expected sensitivity of {.fmt_pct(p$Se)}%, specificity of {.fmt_pct(p$Sp)}%,",
    "and prevalence of {.fmt_pct(p$prev)}%, targeting a {.fmt_pct(p$conf_level)}%",
    "confidence-interval half-width of {.f3(p$half_width)}.",
    "With an anticipated final dropout rate of {.fmt_pct(p$dropout)}%, the",
    "normal-approximation calculation yielded {r$n_per_arm_evaluable} evaluable",
    "participants (satisfying {r$n_dis_required %||% 0L} diseased and",
    "{r$n_non_required %||% 0L} non-diseased participants) and a target",
    "enrolment of {r$n_per_arm_randomized}.",
    .sep = "\n"
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
  ARE <- r$ARE %||% NA_real_
  glue::glue(
    "本研究では、連続量アウトカム [アウトカム名を指定] における 2 群の",
    "位置パラメータを Mann-Whitney U 検定（Wilcoxon ランク和検定）で",
    "比較することを主目的とした。過去の知見 [参考文献を挿入] から、",
    "介入群の平均を {.f2(p$mean_A)}、対照群の平均を {.f2(p$mean_B)}、",
    "共通 SD を {.f2(p$sd_common)}、分布形状は {dist_label} と想定した。",
    "両側有意水準 {.fmt_alpha(p$alpha)}、目標検出力 {.fmt_pct(pw)}% のもと、",
    "pwr パッケージ（version {get_pwr_version()}）の pwr.t.test 関数による t 検定必要例数を",
    "ARE = {.f3(ARE)}（Hollander & Wolfe 1999）で補正して算出した。",
    "その結果、解析対象として各群 {r$n_per_arm_evaluable} 例",
    "（合計 {r$n_total_evaluable} 例）が必要であった。",
    "脱落率を {.fmt_pct(p$dropout)}% と見込む場合、登録必要症例数は",
    "各群 {r$n_per_arm_randomized} 例（合計 {r$n_total_randomized} 例）となる。",
    .sep = "\n"
  )
}
.en_mann_whitney <- function(p, r, pw) {
  dist_label <- switch(p$distribution %||% "normal",
    normal = "normal", lognormal = "log-normal",
    exponential = "exponential", p$distribution)
  ARE <- r$ARE %||% NA_real_
  glue::glue(
    "The primary objective was to compare the location of a continuous",
    "outcome [please specify] between two groups using the Mann-Whitney U",
    "test (Wilcoxon rank-sum test). Based on prior evidence [please insert",
    "reference], we assumed means of {.f2(p$mean_A)} and {.f2(p$mean_B)} in",
    "Groups A and B with a common SD of {.f2(p$sd_common)}, and a",
    "{dist_label} distributional shape.",
    "Assuming a two-sided significance level of {.fmt_alpha(p$alpha)} and a",
    "power of {.fmt_pct(pw)}%, the required sample size was obtained by",
    "dividing the two-sample t-test sample size (computed via pwr.t.test in",
    "pwr version {get_pwr_version()}) by the asymptotic relative efficiency",
    "(ARE) of {.f3(ARE)} (Hollander & Wolfe 1999).",
    "The calculation indicated that {r$n_per_arm_evaluable} evaluable",
    "participants per arm (total {r$n_total_evaluable}) would be required.",
    "Accounting for an anticipated dropout rate of {.fmt_pct(p$dropout)}%,",
    "the target number of randomized participants was set to",
    "{r$n_per_arm_randomized} per arm (total {r$n_total_randomized}).",
    .sep = "\n"
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
  chow_pri <- function(sec) paste0("[主引用] ", chow_cite, " ", sec)
  primary <- switch(design_id,
    ttest_m1      = chow_pri("Section 3.2, pp. 57-65."),
    ttest_m2      = chow_pri("Section 3.2, pp. 57-65."),
    paired        = chow_pri("Section 3.3, pp. 65-70."),
    paired_corr   = chow_pri("Section 3.3, pp. 65-70."),
    binary_chisq  = chow_pri("Section 4.2, pp. 89-95."),
    binary_fisher = chow_pri("Section 5.2, pp. 121-124."),
    one_mean      = chow_pri("Section 3.1, pp. 50-57; Section 1.3.2, pp. 15-16."),
    one_prop      = chow_pri("Section 4.1, pp. 84-89."),
    ttest_ni      = chow_pri("Section 3.2, pp. 57-65."),
    ttest_m2_ni   = chow_pri("Section 3.2, pp. 57-65."),
    paired_ni     = chow_pri("Section 3.3, pp. 65-70."),
    binary_ni     = chow_pri("Section 4.2, pp. 89-95."),
    logrank       = chow_pri("Section 7.4, pp. 179-185."),
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
