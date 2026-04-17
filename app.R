# SampleSizeApp
# サンプルサイズ設計ツール。
# ・計算ロジックはすべて pwr / binom / stats に委譲（自前公式は書かない）
# ・UI は bslib::page_navbar、各デザインは Shiny モジュールとして R/modules.R に分離
# ・5 つのトップタブ: 2 群比較・連続量／2 群比較・二値／1 標本／非劣性／使い方

library(shiny)
library(bslib)
library(ggplot2)
library(pwr)
library(binom)

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
"))

# ========================================================================
# UI
# ========================================================================
ui <- page_navbar(
  title = "サンプルサイズ設計ツール",
  theme = app_theme,
  header = extra_css,
  underline = TRUE,

  nav_panel(
    "2 群比較・連続量",
    navset_pill(
      nav_panel("群別入力（モード1）",    mod_ttest_m1_ui("m1")),
      nav_panel("差と群別 SD（モード2）", mod_ttest_m2_ui("m2")),
      nav_panel("対応のある t 検定",       mod_paired_ui("pr"))
    )
  ),

  nav_panel(
    "2 群比較・二値",
    mod_binary_ui("bin")
  ),

  nav_panel(
    "1 標本",
    navset_pill(
      nav_panel("平均（精度ベース）",   mod_one_mean_ui("om")),
      nav_panel("割合（精度ベース）",   mod_one_prop_ui("op"))
    )
  ),

  nav_panel(
    "非劣性",
    navset_pill(
      nav_panel("連続量・2 標本 t 検定",   mod_ttest_ni_ui("ni_t")),
      nav_panel("連続量・対応のある t 検定", mod_paired_ni_ui("ni_pr")),
      nav_panel("二値（リスク差）",        mod_binary_ni_ui("ni_bin"))
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
  mod_binary_server("bin")
  mod_one_mean_server("om")
  mod_one_prop_server("op")
  mod_ttest_ni_server("ni_t")
  mod_paired_ni_server("ni_pr")
  mod_binary_ni_server("ni_bin")
  mod_guide_server("guide")
}

shinyApp(ui, server)
