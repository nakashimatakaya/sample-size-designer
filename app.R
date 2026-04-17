# SampleSizeApp
# サンプルサイズ設計ツール。
# ・計算ロジックはすべて pwr / binom / stats に委譲（自前公式は書かない）
# ・UI は bslib::page_navbar、各デザインは Shiny モジュールとして R/modules.R に分離
# ・5 つのトップタブ: 2 群比較・連続量／2 群比較・二値／1 標本／非劣性／使い方

library(shiny)
library(bslib)
library(pwr)
library(binom)

# Shinylive 対応: ggplot2 の依存パッケージを明示的に読み込む
# 実コードからは直接呼ばないが、Shinylive の静的解析が拾えないため
# library() で明示してバンドルに含めさせる。
suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
  library(munsell)
  library(tibble)
  library(rlang)
  library(cli)
  library(glue)
  library(lifecycle)
  library(vctrs)
  library(pillar)
  library(withr)
  library(magrittr)
  library(fansi)
  library(utf8)
  library(farver)
  library(labeling)
  library(RColorBrewer)
  library(viridisLite)
  library(gtable)
  library(isoband)
  library(mgcv)
  library(MASS)
  library(nlme)
  library(Matrix)
  library(lattice)
})

# 計算層（engine）
source("R/common.R",                    local = TRUE)
source("R/calc_ttest.R",                local = TRUE)
source("R/calc_paired.R",               local = TRUE)
source("R/calc_binary_chisq.R",         local = TRUE)
source("R/calc_binary_fisher.R",        local = TRUE)
source("R/calc_one_mean_precision.R",   local = TRUE)
source("R/calc_one_prop_precision.R",   local = TRUE)
source("R/calc_ttest_ni.R",             local = TRUE)
source("R/calc_paired_ni.R",            local = TRUE)
source("R/calc_binary_ni.R",            local = TRUE)
# --- 新規デザイン (Phase 2 / 3) --------------------------------------
source("R/calc_mcnemar.R",              local = TRUE)
source("R/calc_ancova.R",               local = TRUE)
source("R/calc_logrank.R",              local = TRUE)
source("R/calc_longitudinal.R",         local = TRUE)
source("R/calc_group_sequential.R",     local = TRUE)
source("R/calc_cluster.R",              local = TRUE)
source("R/calc_diagnostic.R",           local = TRUE)
source("R/calc_mann_whitney.R",         local = TRUE)

# テキスト生成・用語解説
source("R/paper_text.R", local = TRUE)
source("R/r_code_gen.R", local = TRUE)
source("R/glossary.R",   local = TRUE)

# UI ヘルパー・モジュール
source("R/ui_helpers.R", local = TRUE)
source("R/modules.R",    local = TRUE)

# ========================================================================
# Theme & CSS
# ========================================================================
app_theme <- bs_theme(
  version   = 5,
  bootswatch = "flatly",
  base_font = font_collection(
    "Hiragino Sans", "Hiragino Kaku Gothic ProN",
    "Yu Gothic", "Meiryo", "sans-serif"
  )
)

extra_css <- tags$style(HTML("
  body { letter-spacing: 0.01em; }
  .help-badge { margin-left: 4px; }
  .card-header { font-weight: 600; }
  .navbar-brand { font-weight: 700; letter-spacing: 0.02em; }
  .nav-item .nav-link { font-weight: 500; }
  pre { background: #f8f9fa; padding: 10px; border-radius: 6px; }
  .value-box-area .bslib-value-box { min-height: 110px; }

  /* ---- 計算モードのセグメントコントロール化 ---- */
  .segmented-radio { margin-bottom: 1rem; }
  .segmented-radio .control-label {
    display: block;
    font-weight: 600;
    font-size: 0.9rem;
    margin-bottom: 0.35rem;
    color: #495057;
  }
  .segmented-radio .shiny-options-group {
    display: flex;
    border: 1px solid #ced4da;
    border-radius: 0.5rem;
    overflow: hidden;
    background: #fff;
  }
  .segmented-radio .radio-inline {
    flex: 1 1 0;
    margin: 0 !important;
    padding: 0.55rem 0.75rem;
    text-align: center;
    cursor: pointer;
    font-size: 0.88rem;
    line-height: 1.3;
    transition: background-color 0.15s ease, color 0.15s ease;
    border-left: 1px solid #ced4da;
    user-select: none;
  }
  .segmented-radio .radio-inline:first-of-type { border-left: none; }
  .segmented-radio .radio-inline input[type='radio'] {
    position: absolute;
    opacity: 0;
    pointer-events: none;
  }
  .segmented-radio .radio-inline:has(input[type='radio']:checked) {
    background-color: var(--bs-primary, #2C3E50);
    color: #fff;
    font-weight: 600;
  }
  .segmented-radio .radio-inline:hover:not(:has(input:checked)) {
    background-color: #f1f3f5;
  }
"))

# ========================================================================
# UI
# 大タブ: 連続量アウトカム / 二値アウトカム / 1 標本（精度ベース）/ 使い方
# 非劣性は独立タブを廃止し、各デザイン画面内の「試験デザイン」ラジオで選択。
# ========================================================================
ui <- page_navbar(
  title = "サンプルサイズ設計ツール",
  theme = app_theme,
  header = extra_css,
  underline = TRUE,

  nav_panel(
    "連続量アウトカム",
    navset_pill(
      nav_panel("2 群比較（群別入力）",       mod_ttest_m1_ui("m1")),
      nav_panel("2 群比較（差と群別 SD）",    mod_ttest_m2_ui("m2")),
      nav_panel("対応のある t 検定",          mod_paired_ui("pr")),
      nav_panel("ANCOVA（共変量調整）",        mod_ancova_ui("anc")),
      nav_panel("反復測定（longitudinal）",    mod_longitudinal_ui("lon"))
    )
  ),

  nav_panel(
    "二値アウトカム",
    navset_pill(
      nav_panel("2 群比較（χ² 検定）",            mod_binary_chisq_ui("bin_chi")),
      nav_panel("2 群比較（Fisher の正確検定）", mod_binary_fisher_ui("bin_f")),
      nav_panel("対応のある（McNemar）",          mod_mcnemar_ui("mcn"))
    )
  ),

  nav_panel(
    "生存時間解析",
    navset_pill(
      nav_panel("log-rank 検定",               mod_logrank_ui("lgr"))
    )
  ),

  nav_panel(
    "1 標本（精度ベース）",
    navset_pill(
      nav_panel("平均（精度ベース）",   mod_one_mean_ui("om")),
      nav_panel("割合（精度ベース）",   mod_one_prop_ui("op"))
    )
  ),

  nav_panel(
    "クラスター / 特殊デザイン",
    navset_pill(
      nav_panel("群逐次デザイン",       mod_group_sequential_ui("gs")),
      nav_panel("クラスターランダム化", mod_cluster_ui("clu")),
      nav_panel("診断精度",             mod_diagnostic_ui("dgn")),
      nav_panel("Mann-Whitney",          mod_mann_whitney_ui("mw"))
    )
  ),

  nav_panel(
    "使い方",
    mod_guide_ui("guide")
  ),

  nav_spacer(),
  nav_item(
    tags$span(class = "text-muted small",
              "pwr / binom / stats を使用")
  )
)

# ========================================================================
# Server
# ========================================================================
server <- function(input, output, session) {
  mod_ttest_m1_server("m1")
  mod_ttest_m2_server("m2")
  mod_paired_server("pr")
  mod_binary_chisq_server("bin_chi")
  mod_binary_fisher_server("bin_f")
  mod_one_mean_server("om")
  mod_one_prop_server("op")
  # --- 新規デザイン (Phase 2 / 3) ----------------------------------------
  mod_mcnemar_server("mcn")
  mod_ancova_server("anc")
  mod_logrank_server("lgr")
  mod_longitudinal_server("lon")
  mod_group_sequential_server("gs")
  mod_cluster_server("clu")
  mod_diagnostic_server("dgn")
  mod_mann_whitney_server("mw")
  mod_guide_server("guide")
}

shinyApp(ui, server)
