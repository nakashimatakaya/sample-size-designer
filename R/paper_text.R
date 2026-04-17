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
    paired_ni     = .jp_paired_ni(params, result, power_target),
    binary_ni     = .jp_binary_ni(params, result, power_target),
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
    paired_ni     = .en_paired_ni(params, result, power_target),
    binary_ni     = .en_binary_ni(params, result, power_target),
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
    stop("unknown design_id: ", design_id)
  )
  body
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
    stop("unknown design_id: ", design_id)
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
    NULL
  )
  c(base, extra)
}
