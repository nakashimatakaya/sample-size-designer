# R/modules.R
# 各デザインの Shiny モジュール。
# UI は layout_sidebar（左に入力、右に navset_card_tab の 5 タブ）。
# 共通ロジックは common_main_card() / common_main_server() にまとめる。

# ------------------------------------------------------------------------
# 共通: 右側のメインカード（5 タブ）
# ------------------------------------------------------------------------
common_main_card <- function(ns, design_id) {
  has_power <- design_supports_power(design_id)

  # グラフパネルの上部コントロール
  plot_controls <- if (has_power) {
    tagList(
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        radioButtons(ns("y_axis"), "縦軸",
          choices = c("検出力" = "power", "必要症例数" = "req_n"),
          selected = "power"),
        selectInput(ns("x_var"),      "横軸に置く変数", choices = NULL),
        selectInput(ns("legend_var"), "凡例（色分け）",  choices = NULL)
      ),
      conditionalPanel(
        sprintf("input['%s'] == 'req_n'", ns("y_axis")),
        radioButtons(ns("req_n_kind"), "必要症例数の種類",
          choices = c("登録必要数（脱落考慮）" = "randomized",
                      "解析対象必要数"         = "evaluable"),
          selected = "randomized", inline = TRUE)
      )
    )
  } else {
    # 精度ベース: 縦軸は必要症例数のみ
    tagList(
      bslib::layout_columns(
        col_widths = c(6, 6),
        selectInput(ns("x_var"),      "横軸に置く変数", choices = NULL),
        selectInput(ns("legend_var"), "凡例（色分け）",  choices = NULL)
      ),
      radioButtons(ns("req_n_kind"), "必要症例数の種類",
        choices = c("登録必要数（脱落考慮）" = "randomized",
                    "解析対象必要数"         = "evaluable"),
        selected = "randomized", inline = TRUE)
    )
  }

  bslib::navset_card_tab(
    bslib::nav_panel(
      "結果",
      uiOutput(ns("result_boxes")),
      br(),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          "この結果を読み解くヒント",
          htmlOutput(ns("result_hint"))
        )
      )
    ),
    bslib::nav_panel(
      "グラフ",
      plot_controls,
      plotOutput(ns("plot"), height = "420px")
    ),
    bslib::nav_panel(
      "論文記載用テキスト",
      h5("日本語版"),
      verbatimTextOutput(ns("paper_jp")),
      br(),
      h5("English version"),
      verbatimTextOutput(ns("paper_en"))
    ),
    bslib::nav_panel(
      "R コード",
      verbatimTextOutput(ns("r_code"))
    ),
    bslib::nav_panel(
      "計算の根拠",
      htmlOutput(ns("citation"))
    )
  )
}

# ------------------------------------------------------------------------
# 共通: 右側の出力を一括で wire up する（moduleServer 内から呼ぶ）
#
# power_target / calc_mode は reactive（関数）・静的値どちらでも受ける。
#   power_target : 数値（既定 0.80）または reactive
#   calc_mode    : "sample_size"（既定）または "power_calc"、または reactive
# ------------------------------------------------------------------------
common_main_server <- function(input, output, session, design_id,
                               params_reactive,
                               power_target = NULL,
                               calc_mode    = NULL) {
  has_power <- design_supports_power(design_id)

  resolve_pt <- function() {
    if (is.null(power_target))          0.80
    else if (is.function(power_target)) power_target()
    else                                 power_target
  }
  resolve_cm <- function() {
    if (is.null(calc_mode))          "sample_size"
    else if (is.function(calc_mode)) calc_mode()
    else                              calc_mode
  }

  result <- reactive({
    compute_result_dispatch(design_id, params_reactive(),
                            resolve_pt(), resolve_cm())
  })

  # 結果ボックス（mode で表示が切り替わる）
  output$result_boxes <- renderUI({
    render_result_boxes(result())
  })
  # 読み解きヒント
  output$result_hint <- renderUI({
    txt <- result_hint_text(design_id, params_reactive(), result(),
                            resolve_pt(), resolve_cm())
    htmltools::HTML(gsub("\n", "<br>", htmltools::htmlEscape(txt)))
  })
  # 論文
  output$paper_jp <- renderText({
    gen_paper_jp(design_id, params_reactive(), result(),
                 resolve_pt(), resolve_cm())
  })
  output$paper_en <- renderText({
    gen_paper_en(design_id, params_reactive(), result(),
                 resolve_pt(), resolve_cm())
  })
  # R コード
  output$r_code <- renderText({
    gen_r_code(design_id, params_reactive(), resolve_pt(), resolve_cm())
  })
  # 計算の根拠
  output$citation <- renderUI({
    render_citation(design_id, result())
  })

  # X 軸候補の動的更新
  observe({
    vars <- design_plot_vars(design_id)
    y_axis <- if (has_power) input$y_axis else "req_n"
    if (y_axis == "req_n") vars <- vars[names(vars) != "n"]
    named <- setNames(names(vars), unname(vars))
    if (length(named) == 0) return()
    sel <- if (isTruthy(input$x_var) && input$x_var %in% named) input$x_var
           else named[[1]]
    updateSelectInput(session, "x_var", choices = named, selected = sel)
  })
  # 凡例候補の動的更新
  observe({
    vars <- design_plot_vars(design_id)
    y_axis <- if (has_power) input$y_axis else "req_n"
    if (y_axis == "req_n") vars <- vars[names(vars) != "n"]
    if (isTruthy(input$x_var)) vars <- vars[names(vars) != input$x_var]
    named <- c("凡例なし（1本だけ描画）" = "__none__",
               setNames(names(vars), unname(vars)))
    sel <- if (isTruthy(input$legend_var) && input$legend_var %in% named)
             input$legend_var
           else named[[1]]
    updateSelectInput(session, "legend_var", choices = named, selected = sel)
  })

  # プロット: sample_size モードでは n 欄が隠れているので、
  # 検出力 y 軸用の n として「計算済みの必要 n」を使う。
  output$plot <- renderPlot({
    p <- params_reactive()
    if (resolve_cm() == "sample_size") {
      p$n <- result()$n_per_arm_evaluable
    }
    y_axis <- if (has_power) (input$y_axis %||% "power") else "req_n"
    rnkind <- input$req_n_kind %||% "randomized"
    xv <- input$x_var
    lv <- input$legend_var %||% "__none__"
    if (!isTruthy(xv)) return(NULL)
    make_sensitivity_plot(design_id, p, result(),
                          xv, lv, y_axis, rnkind, resolve_pt())
  })

  invisible(result)
}

# ------------------------------------------------------------------------
# 各モジュール共通: サイドバー先頭に置く「計算モード + 目標検出力 or n」ブロック
# design_id に応じて n のラベル（「1 群あたり n」か「ペア数 n」など）を変える。
# ------------------------------------------------------------------------
.calc_mode_and_power_inputs <- function(ns, n_label = "1 群あたり n",
                                        n_default = 50) {
  # 外側の div にクラスを付けて CSS でセグメントコントロール風に装飾する
  # （CSS は app.R の extra_css に .segmented-radio として定義）
  tagList(
    tags$div(
      class = "segmented-radio",
      radioButtons(ns("calc_mode"), "計算モード",
        choices = c("必要症例数を計算"  = "sample_size",
                    "検出力を計算"      = "power_calc"),
        selected = "sample_size", inline = TRUE)
    )
  )
}
.power_target_input <- function(ns) {
  conditionalPanel(
    sprintf("input['%s'] == 'sample_size'", ns("calc_mode")),
    labeled_input(numericInput(ns("power_target"), "目標検出力",
                               value = 0.80, min = 0.70, max = 0.99,
                               step = 0.01), "power")
  )
}
.n_input <- function(ns, label, default) {
  conditionalPanel(
    sprintf("input['%s'] == 'power_calc'", ns("calc_mode")),
    labeled_input(numericInput(ns("n"), label,
                               value = default, min = 2, step = 1), NULL)
  )
}

# ------------------------------------------------------------------------
# 動的 design_id をサポートする共通出力 wiring。
# common_main_server() が静的 design_id 向けなのに対し、
# こちらは hypothesis ラジオや test ラジオで design_id が切替わる
# モジュール（mod_ttest_m1, mod_ttest_m2, mod_paired, mod_binary_*）用。
# ------------------------------------------------------------------------
.wire_common_outputs <- function(input, output, session,
                                 design_id_r, params_r,
                                 resolve_pt, resolve_cm,
                                 extra_result_mutator = NULL) {
  result <- reactive({
    r <- compute_result_dispatch(design_id_r(), params_r(),
                                 resolve_pt(), resolve_cm())
    if (is.function(extra_result_mutator)) r <- extra_result_mutator(r, params_r())
    r
  })

  output$result_boxes <- renderUI({ render_result_boxes(result()) })
  output$result_hint <- renderUI({
    txt <- result_hint_text(design_id_r(), params_r(), result(),
                            resolve_pt(), resolve_cm())
    htmltools::HTML(gsub("\n", "<br>", htmltools::htmlEscape(txt)))
  })
  output$paper_jp <- renderText({
    gen_paper_jp(design_id_r(), params_r(), result(),
                 resolve_pt(), resolve_cm())
  })
  output$paper_en <- renderText({
    gen_paper_en(design_id_r(), params_r(), result(),
                 resolve_pt(), resolve_cm())
  })
  output$r_code <- renderText({
    gen_r_code(design_id_r(), params_r(), resolve_pt(), resolve_cm())
  })
  output$citation <- renderUI({ render_citation(design_id_r(), result()) })

  observe({
    vars <- design_plot_vars(design_id_r())
    if ((input$y_axis %||% "power") == "req_n") vars <- vars[names(vars) != "n"]
    named <- setNames(names(vars), unname(vars))
    if (length(named) == 0) return()
    sel <- if (isTruthy(input$x_var) && input$x_var %in% named) input$x_var
           else named[[1]]
    updateSelectInput(session, "x_var", choices = named, selected = sel)
  })
  observe({
    vars <- design_plot_vars(design_id_r())
    if ((input$y_axis %||% "power") == "req_n") vars <- vars[names(vars) != "n"]
    if (isTruthy(input$x_var)) vars <- vars[names(vars) != input$x_var]
    named <- c("凡例なし（1本だけ描画）" = "__none__",
               setNames(names(vars), unname(vars)))
    sel <- if (isTruthy(input$legend_var) && input$legend_var %in% named)
             input$legend_var
           else named[[1]]
    updateSelectInput(session, "legend_var", choices = named, selected = sel)
  })

  output$plot <- renderPlot({
    p <- params_r()
    if (resolve_cm() == "sample_size") p$n <- result()$n_per_arm_evaluable
    y_axis <- input$y_axis %||% "power"
    rnkind <- input$req_n_kind %||% "randomized"
    xv <- input$x_var
    lv <- input$legend_var %||% "__none__"
    if (!isTruthy(xv)) return(NULL)
    make_sensitivity_plot(design_id_r(), p, result(),
                          xv, lv, y_axis, rnkind, resolve_pt())
  })

  invisible(result)
}

# ------------------------------------------------------------------------
# 試験デザイン（優越性 / 非劣性）ラジオ + 非劣性時のマージン入力。
# ブロック 3（デザインの選択）に置く。
# ------------------------------------------------------------------------
.hypothesis_inputs <- function(ns, margin_default = 2, margin_step = 0.1,
                               margin_unit = "平均差の単位",
                               margin_max = NA_real_) {
  margin_args <- list(
    inputId = ns("margin"),
    label   = "非劣性マージン M（>0）",
    value   = margin_default,
    min     = 1e-6,
    step    = margin_step
  )
  if (!is.na(margin_max)) margin_args$max <- margin_max
  tagList(
    radioButtons(ns("hypothesis"), "試験デザイン",
      choices = c("優越性" = "sup", "非劣性" = "ni"),
      selected = "sup", inline = TRUE),
    conditionalPanel(
      sprintf("input['%s'] == 'ni'", ns("hypothesis")),
      labeled_input(do.call(numericInput, margin_args), "margin"),
      tags$div(
        class = "text-muted small",
        sprintf("マージン M の単位は%sです。非劣性では片側検定になります（α は片側の値を入力してください）。",
                margin_unit)
      )
    )
  )
}

# ========================================================================
# デザイン 1: 2 標本 t 検定（群別入力、モード1）
# ========================================================================
mod_ttest_m1_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      .calc_mode_and_power_inputs(ns),
      labeled_input(numericInput(ns("mean_A"), "A の平均値",
                                 value = 10, step = 0.1), "mean_A"),
      labeled_input(numericInput(ns("sd_A"), "A の SD",
                                 value = 4, min = 0, step = 0.1), "sd_A"),
      labeled_input(numericInput(ns("mean_B"), "B の平均値",
                                 value = 8, step = 0.1), "mean_B"),
      labeled_input(numericInput(ns("sd_B"), "B の SD",
                                 value = 4, min = 0, step = 0.1), "sd_B"),
      labeled_input(numericInput(ns("alpha"), "有意水準 α",
                                 value = 0.05, min = 0, max = 1, step = 0.005),
                    "alpha"),
      .power_target_input(ns),
      .n_input(ns, "1 群あたり n", 50),
      .hypothesis_inputs(ns, margin_default = 2,
                         margin_unit = "平均差"),
      labeled_input(numericInput(ns("dropout"), "脱落率 L",
                                 value = 0.10, min = 0, max = 0.99,
                                 step = 0.05), "dropout")
    ),
    common_main_card(ns, "ttest_m1")
  )
}

mod_ttest_m1_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    resolve_pt <- function() input$power_target %||% 0.80
    resolve_cm <- function() input$calc_mode    %||% "sample_size"

    design_id <- reactive({
      if ((input$hypothesis %||% "sup") == "ni") "ttest_ni" else "ttest_m1"
    })

    params <- reactive({
      validate(
        need(isTruthy(input$sd_A) && input$sd_A > 0, "A の SD は正の値"),
        need(isTruthy(input$sd_B) && input$sd_B > 0, "B の SD は正の値"),
        need(isTruthy(input$alpha) && input$alpha > 0 && input$alpha < 1,
             "α は 0 < α < 1")
      )
      if (resolve_cm() == "power_calc") {
        validate(need(isTruthy(input$n) && input$n >= 2, "n は 2 以上"))
      }
      if (design_id() == "ttest_ni") {
        validate(need(isTruthy(input$margin) && input$margin > 0,
                      "非劣性マージン M は正の値"))
      } else {
        validate(need(input$mean_A != input$mean_B,
                      "A と B の平均値が同じでは計算できません"))
      }
      list(
        diff   = input$mean_A - input$mean_B,
        mean_A = input$mean_A, sd_A = input$sd_A,
        mean_B = input$mean_B, sd_B = input$sd_B,
        margin = input$margin,
        alpha  = input$alpha,
        n      = input$n %||% 50,
        dropout = input$dropout
      )
    })

    .wire_common_outputs(input, output, session,
                         design_id_r = design_id, params_r = params,
                         resolve_pt = resolve_pt, resolve_cm = resolve_cm)
  })
}

# ========================================================================
# デザイン 2: 2 標本 t 検定（差と群別 SD、モード2）
# ========================================================================
mod_ttest_m2_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      .calc_mode_and_power_inputs(ns),
      labeled_input(numericInput(ns("diff"), "群間差 Δ（= A − B）",
                                 value = 2, step = 0.1), "diff"),
      labeled_input(numericInput(ns("sd_A"), "A の SD",
                                 value = 4, min = 0, step = 0.1), "sd_A"),
      labeled_input(numericInput(ns("sd_B"), "B の SD",
                                 value = 4, min = 0, step = 0.1), "sd_B"),
      labeled_input(numericInput(ns("alpha"), "有意水準 α",
                                 value = 0.05, min = 0, max = 1, step = 0.005),
                    "alpha"),
      .power_target_input(ns),
      .n_input(ns, "1 群あたり n", 50),
      .hypothesis_inputs(ns, margin_default = 2,
                         margin_unit = "平均差"),
      labeled_input(numericInput(ns("dropout"), "脱落率 L",
                                 value = 0.10, min = 0, max = 0.99,
                                 step = 0.05), "dropout")
    ),
    common_main_card(ns, "ttest_m2")
  )
}

mod_ttest_m2_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    resolve_pt <- function() input$power_target %||% 0.80
    resolve_cm <- function() input$calc_mode    %||% "sample_size"

    design_id <- reactive({
      if ((input$hypothesis %||% "sup") == "ni") "ttest_m2_ni" else "ttest_m2"
    })

    params <- reactive({
      validate(
        need(isTruthy(input$sd_A) && input$sd_A > 0, "A の SD は正の値"),
        need(isTruthy(input$sd_B) && input$sd_B > 0, "B の SD は正の値"),
        need(isTruthy(input$alpha) && input$alpha > 0 && input$alpha < 1,
             "α は 0<α<1")
      )
      if (resolve_cm() == "power_calc") {
        validate(need(isTruthy(input$n) && input$n >= 2, "n は 2 以上"))
      }
      if (design_id() == "ttest_m2_ni") {
        validate(need(isTruthy(input$margin) && input$margin > 0,
                      "非劣性マージン M は正の値"))
      } else {
        validate(need(input$diff != 0, "群間差 Δ が 0 では計算できません"))
      }
      list(
        diff   = input$diff,
        sd_A   = input$sd_A, sd_B = input$sd_B,
        margin = input$margin,
        alpha  = input$alpha,
        n      = input$n %||% 50,
        dropout = input$dropout
      )
    })

    .wire_common_outputs(input, output, session,
                         design_id_r = design_id, params_r = params,
                         resolve_pt = resolve_pt, resolve_cm = resolve_cm)
  })
}

# ========================================================================
# デザイン 3: 対応のある t 検定（優越性）
# 入力モードは「差の SD を直接入力」と「相関係数から差の SD を計算」の 2 種類。
# ラジオで切り替え、design_id を動的に "paired" / "paired_corr" に変える。
# ========================================================================
mod_paired_ui <- function(id) {
  ns <- NS(id)

  direct_inputs <- tagList(
    labeled_input(numericInput(ns("diff_mean"), "差の平均",
                               value = 2, step = 0.1), "diff_mean"),
    labeled_input(numericInput(ns("sd_diff"), "差の SD",
                               value = 4, min = 0, step = 0.1), "sd_diff")
  )
  corr_inputs <- tagList(
    labeled_input(numericInput(ns("mean_1"), "治療前の平均（測定 1）",
                               value = 10, step = 0.1), "mean_1"),
    labeled_input(numericInput(ns("mean_2"), "治療後の平均（測定 2）",
                               value = 12, step = 0.1), "mean_2"),
    labeled_input(numericInput(ns("sd_1"), "治療前の SD",
                               value = 4, min = 0, step = 0.1), "sd_1"),
    labeled_input(numericInput(ns("sd_2"), "治療後の SD",
                               value = 4, min = 0, step = 0.1), "sd_2"),
    labeled_input(numericInput(ns("r"), "相関係数 r",
                               value = 0.5, min = -1, max = 1, step = 0.05),
                  "r"),
    htmltools::div(
      class = "alert alert-info small py-2",
      htmlOutput(ns("corr_calc_box"))
    )
  )

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      .calc_mode_and_power_inputs(ns),
      # 非劣性のときは corr モード不可なので、input_mode ラジオは優越性時のみ表示
      conditionalPanel(
        sprintf("input['%s'] == 'sup'", ns("hypothesis")),
        radioButtons(ns("input_mode"), "入力モード",
          choices = c("差の SD を直接入力"           = "direct",
                      "相関係数から差の SD を計算"   = "corr"),
          selected = "direct")
      ),
      conditionalPanel(
        # direct: 優越性 direct OR 非劣性（常に direct）
        sprintf("(input['%s'] == 'sup' && input['%s'] == 'direct') || input['%s'] == 'ni'",
                ns("hypothesis"), ns("input_mode"), ns("hypothesis")),
        direct_inputs
      ),
      conditionalPanel(
        sprintf("input['%s'] == 'sup' && input['%s'] == 'corr'",
                ns("hypothesis"), ns("input_mode")),
        corr_inputs
      ),
      labeled_input(numericInput(ns("alpha"), "有意水準 α",
                                 value = 0.05, min = 0, max = 1, step = 0.005),
                    "alpha"),
      .power_target_input(ns),
      .n_input(ns, "ペア数 n", 30),
      .hypothesis_inputs(ns, margin_default = 2,
                         margin_unit = "平均差"),
      conditionalPanel(
        sprintf("input['%s'] == 'ni'", ns("hypothesis")),
        tags$div(
          class = "text-muted small",
          "非劣性では相関係数モードは使えません（差の SD を直接入力してください）。"
        )
      ),
      labeled_input(numericInput(ns("dropout"), "脱落率 L",
                                 value = 0.10, min = 0, max = 0.99,
                                 step = 0.05), "dropout")
    ),
    common_main_card(ns, "paired")
  )
}

mod_paired_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    resolve_pt <- function() input$power_target %||% 0.80
    resolve_cm <- function() input$calc_mode    %||% "sample_size"

    # design_id の決定ロジック:
    #   hypothesis=ni            → paired_ni    （direct 入力のみ）
    #   hypothesis=sup, mode=corr → paired_corr
    #   hypothesis=sup, mode=direct → paired
    design_id <- reactive({
      if ((input$hypothesis %||% "sup") == "ni") {
        "paired_ni"
      } else if ((input$input_mode %||% "direct") == "corr") {
        "paired_corr"
      } else {
        "paired"
      }
    })

    params <- reactive({
      validate(
        need(isTruthy(input$alpha) && input$alpha > 0 && input$alpha < 1,
             "α は 0<α<1")
      )
      if (resolve_cm() == "power_calc") {
        validate(need(isTruthy(input$n) && input$n >= 2, "n は 2 以上"))
      }
      did <- design_id()

      if (did == "paired_corr") {
        validate(
          need(isTruthy(input$sd_1) && input$sd_1 > 0, "治療前の SD は正の値"),
          need(isTruthy(input$sd_2) && input$sd_2 > 0, "治療後の SD は正の値"),
          need(isTruthy(input$r) && input$r >= -1 && input$r <= 1,
               "相関係数 r は −1〜1"),
          need(isTruthy(input$mean_1) && isTruthy(input$mean_2) &&
                 input$mean_1 != input$mean_2,
               "治療前と治療後の平均が同じでは計算できません")
        )
        return(list(
          mean_1 = input$mean_1, mean_2 = input$mean_2,
          sd_1   = input$sd_1,   sd_2   = input$sd_2,
          r      = input$r,
          alpha = input$alpha, n = input$n %||% 30, dropout = input$dropout
        ))
      }

      # paired / paired_ni は direct 入力を使う
      validate(
        need(isTruthy(input$sd_diff) && input$sd_diff > 0, "差の SD は正の値")
      )
      if (did == "paired_ni") {
        validate(need(isTruthy(input$margin) && input$margin > 0,
                      "非劣性マージン M は正の値"))
      } else {
        validate(need(isTruthy(input$diff_mean) && input$diff_mean != 0,
                      "差の平均が 0 では計算できません"))
      }
      list(
        diff_mean = input$diff_mean %||% 0,
        sd_diff   = input$sd_diff,
        margin    = input$margin,
        alpha = input$alpha, n = input$n %||% 30, dropout = input$dropout
      )
    })

    result <- reactive({
      compute_result_dispatch(design_id(), params(), resolve_pt(), resolve_cm())
    })

    # 相関モードのとき、換算された差の SD と差の平均を脇に表示
    output$corr_calc_box <- renderUI({
      req(input$input_mode == "corr")
      r <- result()
      htmltools::HTML(sprintf(
        paste0(
          "相関係数から計算された差の SD: <b>%.2f</b><br>",
          "差の平均（= 治療後 − 治療前）: <b>%.2f</b>"
        ),
        r$sd_diff, r$diff_mean
      ))
    })

    # 共通出力（結果／ヒント／論文／R コード／計算の根拠）
    output$result_boxes <- renderUI({ render_result_boxes(result()) })
    output$result_hint <- renderUI({
      txt <- result_hint_text(design_id(), params(), result(),
                              resolve_pt(), resolve_cm())
      htmltools::HTML(gsub("\n", "<br>", htmltools::htmlEscape(txt)))
    })
    output$paper_jp <- renderText({
      gen_paper_jp(design_id(), params(), result(), resolve_pt(), resolve_cm())
    })
    output$paper_en <- renderText({
      gen_paper_en(design_id(), params(), result(), resolve_pt(), resolve_cm())
    })
    output$r_code   <- renderText({
      gen_r_code(design_id(), params(), resolve_pt(), resolve_cm())
    })
    output$citation <- renderUI({ render_citation(design_id(), result()) })

    # プロットの X 軸・凡例候補を動的更新
    observe({
      vars <- design_plot_vars(design_id())
      if ((input$y_axis %||% "power") == "req_n") vars <- vars[names(vars) != "n"]
      named <- setNames(names(vars), unname(vars))
      sel <- if (isTruthy(input$x_var) && input$x_var %in% named) input$x_var
             else named[[1]]
      updateSelectInput(session, "x_var", choices = named, selected = sel)
    })
    observe({
      vars <- design_plot_vars(design_id())
      if ((input$y_axis %||% "power") == "req_n") vars <- vars[names(vars) != "n"]
      if (isTruthy(input$x_var)) vars <- vars[names(vars) != input$x_var]
      named <- c("凡例なし（1本だけ描画）" = "__none__",
                 setNames(names(vars), unname(vars)))
      sel <- if (isTruthy(input$legend_var) && input$legend_var %in% named)
               input$legend_var
             else named[[1]]
      updateSelectInput(session, "legend_var", choices = named, selected = sel)
    })
    output$plot <- renderPlot({
      p <- params()
      if (resolve_cm() == "sample_size") p$n <- result()$n_per_arm_evaluable
      y_axis <- input$y_axis %||% "power"
      rnkind <- input$req_n_kind %||% "randomized"
      xv <- input$x_var
      lv <- input$legend_var %||% "__none__"
      if (!isTruthy(xv)) return(NULL)
      make_sensitivity_plot(design_id(), p, result(),
                            xv, lv, y_axis, rnkind, resolve_pt())
    })
  })
}

# ========================================================================
# デザイン 4: 二値・χ² / Fisher（統合モジュール、ラジオで切替）
# ========================================================================
# χ² 検定: 優越性 / 非劣性 を画面内で切替
mod_binary_chisq_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      .calc_mode_and_power_inputs(ns),
      labeled_input(numericInput(ns("p_A"), "A 群の割合 p_A",
                                 value = 0.30, min = 0, max = 1, step = 0.01),
                    "p_A"),
      labeled_input(numericInput(ns("p_B"), "B 群の割合 p_B",
                                 value = 0.50, min = 0, max = 1, step = 0.01),
                    "p_B"),
      labeled_input(numericInput(ns("alpha"), "有意水準 α",
                                 value = 0.05, min = 0, max = 1, step = 0.005),
                    "alpha"),
      .power_target_input(ns),
      .n_input(ns, "1 群あたり n", 100),
      .hypothesis_inputs(ns, margin_default = 0.10,
                         margin_step = 0.01,
                         margin_unit = "リスク差",
                         margin_max = 1),
      labeled_input(numericInput(ns("dropout"), "脱落率 L",
                                 value = 0.10, min = 0, max = 0.99,
                                 step = 0.05), "dropout")
    ),
    common_main_card(ns, "binary_chisq")
  )
}

mod_binary_chisq_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    resolve_pt <- function() input$power_target %||% 0.80
    resolve_cm <- function() input$calc_mode    %||% "sample_size"

    design_id <- reactive({
      if ((input$hypothesis %||% "sup") == "ni") "binary_ni" else "binary_chisq"
    })

    params <- reactive({
      validate(
        need(isTruthy(input$p_A) && input$p_A >= 0 && input$p_A <= 1, "p_A は [0,1]"),
        need(isTruthy(input$p_B) && input$p_B >= 0 && input$p_B <= 1, "p_B は [0,1]"),
        need(isTruthy(input$alpha) && input$alpha > 0 && input$alpha < 1, "α は 0<α<1")
      )
      if (resolve_cm() == "power_calc") {
        validate(need(isTruthy(input$n) && input$n >= 2, "n は 2 以上"))
      }
      if (design_id() == "binary_ni") {
        validate(need(isTruthy(input$margin) && input$margin > 0,
                      "非劣性マージン M は正の値"))
      } else {
        validate(need(input$p_A != input$p_B,
                      "p_A と p_B が同じでは計算できません"))
      }
      list(p_A = input$p_A, p_B = input$p_B,
           margin = input$margin,
           alpha = input$alpha, n = input$n %||% 100, dropout = input$dropout)
    })

    .wire_common_outputs(input, output, session,
                         design_id_r = design_id, params_r = params,
                         resolve_pt = resolve_pt, resolve_cm = resolve_cm)
  })
}

# Fisher の正確検定: 優越性のみ対応（NI は pwr に未搭載）
mod_binary_fisher_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      .calc_mode_and_power_inputs(ns),
      labeled_input(numericInput(ns("p_A"), "A 群の割合 p_A",
                                 value = 0.30, min = 0, max = 1, step = 0.01),
                    "p_A"),
      labeled_input(numericInput(ns("p_B"), "B 群の割合 p_B",
                                 value = 0.50, min = 0, max = 1, step = 0.01),
                    "p_B"),
      labeled_input(numericInput(ns("alpha"), "有意水準 α（両側）",
                                 value = 0.05, min = 0, max = 1, step = 0.005),
                    "alpha"),
      .power_target_input(ns),
      .n_input(ns, "1 群あたり n", 100),
      tags$div(
        class = "text-muted small",
        "このデザインの非劣性は pwr に未対応のため、",
        "優越性（両側）のみ対応しています。",
        "非劣性の場合は「2 群比較（χ² 検定）」タブをご利用ください。"
      ),
      labeled_input(numericInput(ns("dropout"), "脱落率 L",
                                 value = 0.10, min = 0, max = 0.99,
                                 step = 0.05), "dropout")
    ),
    common_main_card(ns, "binary_fisher")
  )
}

mod_binary_fisher_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    params <- reactive({
      validate(
        need(isTruthy(input$p_A) && input$p_A >= 0 && input$p_A <= 1, "p_A は [0,1]"),
        need(isTruthy(input$p_B) && input$p_B >= 0 && input$p_B <= 1, "p_B は [0,1]"),
        need(isTruthy(input$alpha) && input$alpha > 0 && input$alpha < 1, "α は 0<α<1"),
        need(input$p_A != input$p_B, "p_A と p_B が同じでは計算できません")
      )
      if ((input$calc_mode %||% "sample_size") == "power_calc") {
        validate(need(isTruthy(input$n) && input$n >= 2, "n は 2 以上"))
      }
      list(p_A = input$p_A, p_B = input$p_B,
           alpha = input$alpha, n = input$n %||% 100, dropout = input$dropout)
    })
    common_main_server(
      input, output, session, "binary_fisher", params,
      power_target = reactive(input$power_target %||% 0.80),
      calc_mode    = reactive(input$calc_mode    %||% "sample_size")
    )
  })
}

# ========================================================================
# デザイン 5: 1 標本・平均・精度ベース
# ========================================================================
mod_one_mean_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      labeled_input(numericInput(ns("sd"), "予想される SD",
                                 value = 10, min = 0, step = 0.1), "sd"),
      labeled_input(numericInput(ns("half_width"), "目標 CI 半幅 E",
                                 value = 2, min = 0, step = 0.1),
                    "half_width"),
      labeled_input(numericInput(ns("conf_level"), "信頼水準",
                                 value = 0.95, min = 0.5, max = 0.999,
                                 step = 0.01), "conf_level"),
      labeled_input(numericInput(ns("dropout"), "脱落率 L",
                                 value = 0.10, min = 0, max = 0.99,
                                 step = 0.05), "dropout")
    ),
    common_main_card(ns, "one_mean")
  )
}

mod_one_mean_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    params <- reactive({
      validate(
        need(isTruthy(input$sd) && input$sd > 0, "SD は正の値"),
        need(isTruthy(input$half_width) && input$half_width > 0, "半幅 E は正の値"),
        need(isTruthy(input$conf_level) && input$conf_level > 0 &&
               input$conf_level < 1, "信頼水準は 0〜1")
      )
      list(sd = input$sd, half_width = input$half_width,
           conf_level = input$conf_level, dropout = input$dropout)
    })
    common_main_server(input, output, session, "one_mean", params)
  })
}

# ========================================================================
# デザイン 6: 1 標本・割合・精度ベース
# ========================================================================
mod_one_prop_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      labeled_input(numericInput(ns("p"), "予想される割合 p",
                                 value = 0.30, min = 0, max = 1, step = 0.01),
                    "p_A"),
      labeled_input(numericInput(ns("half_width"), "目標 CI 半幅 E",
                                 value = 0.05, min = 0.001, max = 0.5,
                                 step = 0.005), "half_width"),
      labeled_input(numericInput(ns("conf_level"), "信頼水準",
                                 value = 0.95, min = 0.5, max = 0.999,
                                 step = 0.01), "conf_level"),
      radioButtons(ns("method"), "CI の方法",
        choices = c("Wilson" = "wilson", "Exact (Clopper-Pearson)" = "exact",
                    "正規近似" = "normal"),
        selected = "wilson"),
      labeled_input(numericInput(ns("dropout"), "脱落率 L",
                                 value = 0.10, min = 0, max = 0.99,
                                 step = 0.05), "dropout")
    ),
    common_main_card(ns, "one_prop")
  )
}

mod_one_prop_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    params <- reactive({
      validate(
        need(isTruthy(input$p) && input$p > 0 && input$p < 1,
             "p は 0〜1 の範囲で"),
        need(isTruthy(input$half_width) && input$half_width > 0, "半幅 E は正の値"),
        need(isTruthy(input$conf_level) && input$conf_level > 0 &&
               input$conf_level < 1, "信頼水準は 0〜1")
      )
      list(p = input$p, half_width = input$half_width,
           conf_level = input$conf_level, method = input$method,
           dropout = input$dropout)
    })
    common_main_server(input, output, session, "one_prop", params)
  })
}

# ========================================================================
# デザイン 7: 非劣性・2 標本 t 検定
# ========================================================================
mod_ttest_ni_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      .calc_mode_and_power_inputs(ns),
      bslib::card_body(
        tags$div(
          class = "text-muted small",
          "片側検定。仮説: H0: μ_A − μ_B ≤ −M, H1: μ_A − μ_B > −M。",
          "マージン M は平均差の単位。"
        )
      ),
      labeled_input(numericInput(ns("mean_A"), "A の平均値（新治療）",
                                 value = 10, step = 0.1), "mean_A"),
      labeled_input(numericInput(ns("sd_A"), "A の SD",
                                 value = 4, min = 0, step = 0.1), "sd_A"),
      labeled_input(numericInput(ns("mean_B"), "B の平均値（既存治療）",
                                 value = 10, step = 0.1), "mean_B"),
      labeled_input(numericInput(ns("sd_B"), "B の SD",
                                 value = 4, min = 0, step = 0.1), "sd_B"),
      labeled_input(numericInput(ns("margin"), "非劣性マージン M（>0）",
                                 value = 2, min = 0.001, step = 0.1), "margin"),
      labeled_input(numericInput(ns("alpha"), "有意水準 α（片側）",
                                 value = 0.025, min = 0, max = 1, step = 0.005),
                    "alpha"),
      .power_target_input(ns),
      .n_input(ns, "1 群あたり n", 50),
      labeled_input(numericInput(ns("dropout"), "脱落率 L",
                                 value = 0.10, min = 0, max = 0.99,
                                 step = 0.05), "dropout")
    ),
    common_main_card(ns, "ttest_ni")
  )
}

mod_ttest_ni_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    params <- reactive({
      validate(
        need(isTruthy(input$sd_A) && input$sd_A > 0, "A の SD は正の値"),
        need(isTruthy(input$sd_B) && input$sd_B > 0, "B の SD は正の値"),
        need(isTruthy(input$margin) && input$margin > 0, "マージン M は正の値"),
        need(isTruthy(input$alpha) && input$alpha > 0 && input$alpha < 1, "α は 0<α<1")
      )
      if ((input$calc_mode %||% "sample_size") == "power_calc") {
        validate(need(isTruthy(input$n) && input$n >= 2, "n は 2 以上"))
      }
      list(diff = input$mean_A - input$mean_B,
           mean_A = input$mean_A, sd_A = input$sd_A,
           mean_B = input$mean_B, sd_B = input$sd_B,
           margin = input$margin,
           alpha = input$alpha, n = input$n %||% 50,
           dropout = input$dropout)
    })
    common_main_server(
      input, output, session, "ttest_ni", params,
      power_target = reactive(input$power_target %||% 0.80),
      calc_mode    = reactive(input$calc_mode    %||% "sample_size")
    )
  })
}

# ========================================================================
# デザイン 8: 非劣性・対応のある t 検定
# ========================================================================
mod_paired_ni_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      .calc_mode_and_power_inputs(ns),
      bslib::card_body(
        tags$div(
          class = "text-muted small",
          "片側検定。対応のある差が −M を超えることを示す。"
        )
      ),
      labeled_input(numericInput(ns("diff_mean"), "差の平均",
                                 value = 0, step = 0.1), "diff_mean"),
      labeled_input(numericInput(ns("sd_diff"), "差の SD",
                                 value = 4, min = 0, step = 0.1), "sd_diff"),
      labeled_input(numericInput(ns("margin"), "非劣性マージン M（>0）",
                                 value = 2, min = 0.001, step = 0.1), "margin"),
      labeled_input(numericInput(ns("alpha"), "有意水準 α（片側）",
                                 value = 0.025, min = 0, max = 1, step = 0.005),
                    "alpha"),
      .power_target_input(ns),
      .n_input(ns, "ペア数 n", 30),
      labeled_input(numericInput(ns("dropout"), "脱落率 L",
                                 value = 0.10, min = 0, max = 0.99,
                                 step = 0.05), "dropout")
    ),
    common_main_card(ns, "paired_ni")
  )
}

mod_paired_ni_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    params <- reactive({
      validate(
        need(isTruthy(input$sd_diff) && input$sd_diff > 0, "差の SD は正の値"),
        need(isTruthy(input$margin) && input$margin > 0, "マージン M は正の値"),
        need(isTruthy(input$alpha) && input$alpha > 0 && input$alpha < 1, "α は 0<α<1")
      )
      if ((input$calc_mode %||% "sample_size") == "power_calc") {
        validate(need(isTruthy(input$n) && input$n >= 2, "n は 2 以上"))
      }
      list(diff_mean = input$diff_mean, sd_diff = input$sd_diff,
           margin = input$margin,
           alpha = input$alpha, n = input$n %||% 30, dropout = input$dropout)
    })
    common_main_server(
      input, output, session, "paired_ni", params,
      power_target = reactive(input$power_target %||% 0.80),
      calc_mode    = reactive(input$calc_mode    %||% "sample_size")
    )
  })
}

# ========================================================================
# デザイン 9: 非劣性・二値（Chow 2018 Sec 4.2 正規近似）
# ========================================================================
mod_binary_ni_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      .calc_mode_and_power_inputs(ns),
      bslib::card_body(
        tags$div(
          class = "text-muted small",
          "片側・リスク差ベースの正規近似（Chow 2018）。",
          "M はリスク差の単位（0.05 = 5%）。"
        )
      ),
      labeled_input(numericInput(ns("p_A"), "A 群の割合（新治療）",
                                 value = 0.70, min = 0, max = 1, step = 0.01),
                    "p_A"),
      labeled_input(numericInput(ns("p_B"), "B 群の割合（既存治療）",
                                 value = 0.70, min = 0, max = 1, step = 0.01),
                    "p_B"),
      labeled_input(numericInput(ns("margin"), "非劣性マージン M",
                                 value = 0.10, min = 0.001, max = 1,
                                 step = 0.01), "margin"),
      labeled_input(numericInput(ns("alpha"), "有意水準 α（片側）",
                                 value = 0.025, min = 0, max = 1, step = 0.005),
                    "alpha"),
      .power_target_input(ns),
      .n_input(ns, "1 群あたり n", 300),
      labeled_input(numericInput(ns("dropout"), "脱落率 L",
                                 value = 0.10, min = 0, max = 0.99,
                                 step = 0.05), "dropout")
    ),
    common_main_card(ns, "binary_ni")
  )
}

mod_binary_ni_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    params <- reactive({
      validate(
        need(isTruthy(input$p_A) && input$p_A >= 0 && input$p_A <= 1, "p_A は [0,1]"),
        need(isTruthy(input$p_B) && input$p_B >= 0 && input$p_B <= 1, "p_B は [0,1]"),
        need(isTruthy(input$margin) && input$margin > 0, "マージン M は正の値"),
        need(isTruthy(input$alpha) && input$alpha > 0 && input$alpha < 1, "α は 0<α<1")
      )
      if ((input$calc_mode %||% "sample_size") == "power_calc") {
        validate(need(isTruthy(input$n) && input$n >= 2, "n は 2 以上"))
      }
      list(p_A = input$p_A, p_B = input$p_B,
           margin = input$margin,
           alpha = input$alpha, n = input$n %||% 300, dropout = input$dropout)
    })
    common_main_server(
      input, output, session, "binary_ni", params,
      power_target = reactive(input$power_target %||% 0.80),
      calc_mode    = reactive(input$calc_mode    %||% "sample_size")
    )
  })
}

# ========================================================================
# 「使い方」タブ
# ========================================================================
mod_guide_ui <- function(id) {
  ns <- NS(id)
  # 用語集の accordion パネル
  glossary_panels <- lapply(glossary_entries, function(e) {
    bslib::accordion_panel(
      e$title,
      htmltools::HTML(gsub("\n", "<br>", htmltools::htmlEscape(e$body)))
    )
  })

  bslib::layout_column_wrap(
    width = 1,
    bslib::card(
      bslib::card_header("はじめに"),
      bslib::card_body(
        htmltools::HTML(paste(
          "<p>このツールは、臨床研究で必要な症例数を、",
          "スライダー感覚で試算できるアプリです。",
          "計算はすべて R の <code>pwr</code> / <code>binom</code> / <code>stats</code> ",
          "パッケージに委ねています。</p>",
          "<p>各タブで入力を変えると、右側の「結果」「グラフ」「論文記載用テキスト」",
          "「R コード」「計算の根拠」がリアルタイムに更新されます。</p>",
          "<p>画面右側の R コードをコピーすれば、手元の R でも同じ計算が",
          "再現できます。</p>",
          sep = "\n"
        ))
      )
    ),
    bslib::card(
      bslib::card_header(design_selector_text$title),
      bslib::card_body(
        htmltools::HTML(paste0(
          "<pre style='white-space:pre-wrap;background:transparent;border:0;padding:0;font-family:inherit;'>",
          htmltools::htmlEscape(design_selector_text$body),
          "</pre>"
        ))
      )
    ),
    bslib::card(
      bslib::card_header(result_reading_text$title),
      bslib::card_body(
        htmltools::HTML(paste0(
          "<pre style='white-space:pre-wrap;background:transparent;border:0;padding:0;font-family:inherit;'>",
          htmltools::htmlEscape(result_reading_text$body),
          "</pre>"
        ))
      )
    ),
    bslib::card(
      bslib::card_header(case_study_text$title),
      bslib::card_body(
        htmltools::HTML(paste0(
          "<pre style='white-space:pre-wrap;background:transparent;border:0;padding:0;font-family:inherit;'>",
          htmltools::htmlEscape(case_study_text$body),
          "</pre>"
        ))
      )
    ),
    bslib::card(
      bslib::card_header(faq_text$title),
      bslib::card_body(
        htmltools::HTML(paste0(
          "<pre style='white-space:pre-wrap;background:transparent;border:0;padding:0;font-family:inherit;'>",
          htmltools::htmlEscape(faq_text$body),
          "</pre>"
        ))
      )
    ),
    bslib::card(
      bslib::card_header("用語集"),
      bslib::card_body(
        do.call(bslib::accordion,
                c(list(open = FALSE), glossary_panels))
      )
    )
  )
}

mod_guide_server <- function(id) {
  moduleServer(id, function(input, output, session) { })
}

# ========================================================================
# 新規デザイン（Phase 2 / 3）のモジュール
# ------------------------------------------------------------------------
# すべて共通して：
#  - サイドバーに .calc_mode_and_power_inputs / 目標検出力 or n /
#    脱落率 と、デザイン固有の入力
#  - 右側は common_main_card(ns, design_id) で同じ 5 タブ
#  - サーバーは common_main_server() で結線（design_id 固定）
# ========================================================================

# ---- D1. McNemar（対応のある二値）----
mod_mcnemar_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      .calc_mode_and_power_inputs(ns),
      numericInput(ns("p_disc"), "不一致ペア割合 p_disc（0〜1）",
                   value = 0.30, min = 0.01, max = 1, step = 0.05),
      numericInput(ns("psi"), "不一致のうち A が優位な割合 ψ",
                   value = 0.60, min = 0.51, max = 0.99, step = 0.05),
      numericInput(ns("alpha"), "有意水準 α（両側）",
                   value = 0.05, min = 0, max = 1, step = 0.005),
      .power_target_input(ns),
      .n_input(ns, "ペア数 n", 100),
      numericInput(ns("dropout"), "脱落率 L",
                   value = 0.10, min = 0, max = 0.99, step = 0.05)
    ),
    common_main_card(ns, "mcnemar")
  )
}

mod_mcnemar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    pt <- function() input$power_target %||% 0.80
    cm <- function() input$calc_mode    %||% "sample_size"
    params <- reactive({
      validate(
        need(isTruthy(input$p_disc) && input$p_disc > 0 && input$p_disc <= 1,
             "p_disc は 0 より大きく 1 以下"),
        need(isTruthy(input$psi) && input$psi > 0.5 && input$psi < 1,
             "ψ は 0.5 より大（0.5 では計算不能）"),
        need(isTruthy(input$alpha) && input$alpha > 0 && input$alpha < 1,
             "α は 0〜1")
      )
      list(p_disc = input$p_disc, psi = input$psi,
           alpha = input$alpha,
           n = input$n %||% 100,
           dropout = input$dropout)
    })
    common_main_server(input, output, session, "mcnemar", params,
                       power_target = pt, calc_mode = cm)
  })
}

# ---- D2. ANCOVA ----
mod_ancova_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      .calc_mode_and_power_inputs(ns),
      numericInput(ns("mean_A"), "A の平均値", value = 10, step = 0.1),
      numericInput(ns("mean_B"), "B の平均値", value = 8,  step = 0.1),
      numericInput(ns("sd_common"), "共通 SD",
                   value = 4, min = 0, step = 0.1),
      numericInput(ns("r"), "共変量とアウトカムの相関 r（−1〜1）",
                   value = 0.5, min = -0.95, max = 0.95, step = 0.05),
      numericInput(ns("alpha"), "有意水準 α（両側）",
                   value = 0.05, min = 0, max = 1, step = 0.005),
      .power_target_input(ns),
      .n_input(ns, "1 群あたり n", 50),
      numericInput(ns("dropout"), "脱落率 L",
                   value = 0.10, min = 0, max = 0.99, step = 0.05)
    ),
    common_main_card(ns, "ancova")
  )
}

mod_ancova_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    pt <- function() input$power_target %||% 0.80
    cm <- function() input$calc_mode    %||% "sample_size"
    params <- reactive({
      validate(
        need(isTruthy(input$sd_common) && input$sd_common > 0,
             "共通 SD は正の値"),
        need(isTruthy(input$r) && abs(input$r) < 1,
             "相関 r は −1〜1 の開区間"),
        need(isTruthy(input$mean_A) && isTruthy(input$mean_B) &&
               input$mean_A != input$mean_B,
             "A と B の平均が同じでは計算できません")
      )
      list(mean_A = input$mean_A, mean_B = input$mean_B,
           sd_common = input$sd_common, r = input$r,
           alpha = input$alpha,
           n = input$n %||% 50,
           dropout = input$dropout)
    })
    common_main_server(input, output, session, "ancova", params,
                       power_target = pt, calc_mode = cm)
  })
}

# ---- D3. log-rank ----
mod_logrank_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      .calc_mode_and_power_inputs(ns),
      numericInput(ns("median_C"), "対照群の中央生存期間 m_C（月）",
                   value = 12, min = 0.1, step = 0.5),
      numericInput(ns("HR"), "ハザード比 HR（1 以外）",
                   value = 0.75, min = 0.05, max = 5, step = 0.05),
      numericInput(ns("accrual"), "アクルー期間 a（月）",
                   value = 24, min = 0.5, step = 1),
      numericInput(ns("followup"), "追加フォローアップ f（月）",
                   value = 12, min = 0, step = 1),
      numericInput(ns("p_alloc"), "治療群への割付比（0〜1）",
                   value = 0.5, min = 0.05, max = 0.95, step = 0.05),
      numericInput(ns("alpha"), "有意水準 α（片側）",
                   value = 0.025, min = 0, max = 1, step = 0.005),
      .power_target_input(ns),
      .n_input(ns, "総症例数 N", 300),
      numericInput(ns("dropout"), "脱落率 L",
                   value = 0.10, min = 0, max = 0.99, step = 0.05)
    ),
    common_main_card(ns, "logrank")
  )
}

mod_logrank_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    pt <- function() input$power_target %||% 0.80
    cm <- function() input$calc_mode    %||% "sample_size"
    params <- reactive({
      validate(
        need(isTruthy(input$median_C) && input$median_C > 0,
             "中央生存期間は正の値"),
        need(isTruthy(input$HR) && input$HR > 0 && input$HR != 1,
             "HR は正かつ 1 以外"),
        need(isTruthy(input$accrual) && input$accrual > 0, "a > 0"),
        need(isTruthy(input$followup) && input$followup >= 0, "f ≥ 0")
      )
      list(median_C = input$median_C, HR = input$HR,
           accrual = input$accrual, followup = input$followup,
           p_alloc = input$p_alloc, alpha = input$alpha,
           n = input$n %||% 300,
           dropout = input$dropout)
    })
    common_main_server(input, output, session, "logrank", params,
                       power_target = pt, calc_mode = cm)
  })
}

# ---- D4. longitudinal ----
mod_longitudinal_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      .calc_mode_and_power_inputs(ns),
      numericInput(ns("mean_A"), "A 群 post-baseline 平均",
                   value = 10, step = 0.1),
      numericInput(ns("mean_B"), "B 群 post-baseline 平均",
                   value = 8,  step = 0.1),
      numericInput(ns("sd_common"), "1 時点の共通 SD",
                   value = 4, min = 0, step = 0.1),
      numericInput(ns("k"), "測定時点数 k（>=2）",
                   value = 3, min = 1, max = 20, step = 1),
      numericInput(ns("rho"), "被験者内相関 ρ（0〜1 未満）",
                   value = 0.5, min = 0, max = 0.95, step = 0.05),
      numericInput(ns("alpha"), "有意水準 α（両側）",
                   value = 0.05, min = 0, max = 1, step = 0.005),
      .power_target_input(ns),
      .n_input(ns, "1 群あたり n", 50),
      numericInput(ns("dropout"), "脱落率 L",
                   value = 0.10, min = 0, max = 0.99, step = 0.05)
    ),
    common_main_card(ns, "longitudinal")
  )
}

mod_longitudinal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    pt <- function() input$power_target %||% 0.80
    cm <- function() input$calc_mode    %||% "sample_size"
    params <- reactive({
      validate(
        need(isTruthy(input$sd_common) && input$sd_common > 0, "SD > 0"),
        need(isTruthy(input$k) && input$k >= 1, "k は 1 以上"),
        need(isTruthy(input$rho) && input$rho >= 0 && input$rho < 1,
             "ρ は 0 以上 1 未満"),
        need(isTruthy(input$mean_A) && isTruthy(input$mean_B) &&
               input$mean_A != input$mean_B,
             "A と B の平均が同じでは計算できません")
      )
      list(mean_A = input$mean_A, mean_B = input$mean_B,
           sd_common = input$sd_common,
           k = as.integer(input$k), rho = input$rho,
           alpha = input$alpha,
           n = input$n %||% 50,
           dropout = input$dropout)
    })
    common_main_server(input, output, session, "longitudinal", params,
                       power_target = pt, calc_mode = cm)
  })
}

# ---- D5. group sequential ----
mod_group_sequential_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      .calc_mode_and_power_inputs(ns),
      numericInput(ns("mean_A"), "A の平均", value = 10, step = 0.1),
      numericInput(ns("mean_B"), "B の平均", value = 8,  step = 0.1),
      numericInput(ns("sd_common"), "共通 SD",
                   value = 4, min = 0, step = 0.1),
      numericInput(ns("K"), "解析回数 K（2〜5）",
                   value = 3, min = 1, max = 5, step = 1),
      radioButtons(ns("boundary"), "境界タイプ",
                   choices = c("O'Brien-Fleming" = "OBF",
                               "Pocock"          = "Pocock"),
                   selected = "OBF", inline = TRUE),
      numericInput(ns("alpha"), "全体 α（両側）",
                   value = 0.05, min = 0, max = 1, step = 0.005),
      .power_target_input(ns),
      .n_input(ns, "1 群あたり n", 80),
      numericInput(ns("dropout"), "脱落率 L",
                   value = 0.10, min = 0, max = 0.99, step = 0.05)
    ),
    common_main_card(ns, "group_sequential")
  )
}

mod_group_sequential_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    pt <- function() input$power_target %||% 0.80
    cm <- function() input$calc_mode    %||% "sample_size"
    params <- reactive({
      validate(
        need(isTruthy(input$sd_common) && input$sd_common > 0, "SD > 0"),
        need(isTruthy(input$K) && input$K %in% 1:5, "K は 1〜5"),
        need(input$mean_A != input$mean_B,
             "A と B の平均が同じでは計算できません")
      )
      list(mean_A = input$mean_A, mean_B = input$mean_B,
           sd_common = input$sd_common,
           K = as.integer(input$K), boundary = input$boundary,
           alpha = input$alpha,
           n = input$n %||% 80,
           dropout = input$dropout)
    })
    common_main_server(input, output, session, "group_sequential", params,
                       power_target = pt, calc_mode = cm)
  })
}

# ---- D6. cluster（連続量 / 二値 を 1 モジュール内ラジオで切替）----
mod_cluster_ui <- function(id) {
  ns <- NS(id)
  cont_inputs <- tagList(
    numericInput(ns("mean_A"), "A の平均", value = 10, step = 0.1),
    numericInput(ns("mean_B"), "B の平均", value = 8,  step = 0.1),
    numericInput(ns("sd_common"), "共通 SD",
                 value = 4, min = 0, step = 0.1)
  )
  bin_inputs <- tagList(
    numericInput(ns("p_A"), "A 群の発生割合",
                 value = 0.5, min = 0, max = 1, step = 0.05),
    numericInput(ns("p_B"), "B 群の発生割合",
                 value = 0.3, min = 0, max = 1, step = 0.05)
  )
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      .calc_mode_and_power_inputs(ns),
      radioButtons(ns("outcome_type"), "アウトカムタイプ",
                   choices = c("連続量" = "cont", "二値" = "bin"),
                   selected = "cont", inline = TRUE),
      conditionalPanel(
        sprintf("input['%s'] == 'cont'", ns("outcome_type")), cont_inputs),
      conditionalPanel(
        sprintf("input['%s'] == 'bin'", ns("outcome_type")),  bin_inputs),
      numericInput(ns("m"), "平均クラスターサイズ m",
                   value = 20, min = 2, step = 1),
      numericInput(ns("ICC"), "級内相関係数 ICC（0〜1 未満）",
                   value = 0.05, min = 0, max = 0.95, step = 0.01),
      numericInput(ns("alpha"), "有意水準 α（両側）",
                   value = 0.05, min = 0, max = 1, step = 0.005),
      .power_target_input(ns),
      .n_input(ns, "1 群あたり n", 200),
      numericInput(ns("dropout"), "脱落率 L",
                   value = 0.10, min = 0, max = 0.99, step = 0.05)
    ),
    common_main_card(ns, "cluster_cont")
  )
}

mod_cluster_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    pt <- function() input$power_target %||% 0.80
    cm <- function() input$calc_mode    %||% "sample_size"
    design_id_r <- reactive({
      if ((input$outcome_type %||% "cont") == "bin") "cluster_bin"
      else "cluster_cont"
    })
    params <- reactive({
      validate(
        need(isTruthy(input$m) && input$m >= 2,
             "クラスターサイズ m は 2 以上"),
        need(isTruthy(input$ICC) && input$ICC >= 0 && input$ICC < 1,
             "ICC は 0〜1 未満")
      )
      if (design_id_r() == "cluster_bin") {
        validate(
          need(isTruthy(input$p_A) && input$p_A >= 0 && input$p_A <= 1,
               "p_A は 0〜1"),
          need(isTruthy(input$p_B) && input$p_B >= 0 && input$p_B <= 1,
               "p_B は 0〜1"),
          need(input$p_A != input$p_B, "p_A と p_B が同じでは計算できません")
        )
        return(list(p_A = input$p_A, p_B = input$p_B,
                    m = as.integer(input$m), ICC = input$ICC,
                    alpha = input$alpha,
                    n = input$n %||% 200,
                    dropout = input$dropout))
      }
      validate(
        need(isTruthy(input$sd_common) && input$sd_common > 0, "SD > 0"),
        need(input$mean_A != input$mean_B,
             "A と B の平均が同じでは計算できません")
      )
      list(mean_A = input$mean_A, mean_B = input$mean_B,
           sd_common = input$sd_common,
           m = as.integer(input$m), ICC = input$ICC,
           alpha = input$alpha,
           n = input$n %||% 200,
           dropout = input$dropout)
    })
    # design_id が動的なので .wire_common_outputs を使う
    .wire_common_outputs(input, output, session,
                         design_id_r = design_id_r, params_r = params,
                         resolve_pt = pt, resolve_cm = cm)
  })
}

# ---- D7. diagnostic ----
mod_diagnostic_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      # 診断精度は「必要症例数」モードのみ（検出力という概念なし）
      numericInput(ns("Se"), "予想感度 Se（0〜1）",
                   value = 0.85, min = 0, max = 1, step = 0.05),
      numericInput(ns("Sp"), "予想特異度 Sp（0〜1）",
                   value = 0.85, min = 0, max = 1, step = 0.05),
      numericInput(ns("prev"), "有病率 prev（0〜1）",
                   value = 0.10, min = 0.001, max = 0.99, step = 0.01),
      numericInput(ns("half_width"), "目標 CI 半幅 E",
                   value = 0.05, min = 0.001, max = 0.5, step = 0.01),
      numericInput(ns("conf_level"), "信頼水準",
                   value = 0.95, min = 0.5, max = 0.999, step = 0.01),
      numericInput(ns("dropout"), "脱落率 L",
                   value = 0.10, min = 0, max = 0.99, step = 0.05)
    ),
    common_main_card(ns, "diagnostic")
  )
}

mod_diagnostic_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    params <- reactive({
      validate(
        need(isTruthy(input$Se) && input$Se > 0 && input$Se < 1,
             "Se は 0〜1 の開区間"),
        need(isTruthy(input$Sp) && input$Sp > 0 && input$Sp < 1,
             "Sp は 0〜1 の開区間"),
        need(isTruthy(input$prev) && input$prev > 0 && input$prev < 1,
             "有病率は 0〜1 の開区間"),
        need(isTruthy(input$half_width) && input$half_width > 0 &&
               input$half_width < 1, "CI 半幅 E は 0 より大きく 1 未満")
      )
      list(Se = input$Se, Sp = input$Sp, prev = input$prev,
           half_width = input$half_width, conf_level = input$conf_level,
           dropout = input$dropout)
    })
    common_main_server(input, output, session, "diagnostic", params)
  })
}

# ---- D8. Mann-Whitney ----
mod_mann_whitney_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      .calc_mode_and_power_inputs(ns),
      numericInput(ns("mean_A"), "A の想定位置（平均）",
                   value = 10, step = 0.1),
      numericInput(ns("mean_B"), "B の想定位置（平均）",
                   value = 8, step = 0.1),
      numericInput(ns("sd_common"), "共通 SD（正規スケール）",
                   value = 4, min = 0, step = 0.1),
      radioButtons(ns("distribution"), "分布の仮定",
                   choices = c("正規"      = "normal",
                               "対数正規"  = "lognormal",
                               "指数"      = "exponential"),
                   selected = "normal", inline = TRUE),
      numericInput(ns("alpha"), "有意水準 α（両側）",
                   value = 0.05, min = 0, max = 1, step = 0.005),
      .power_target_input(ns),
      .n_input(ns, "1 群あたり n", 50),
      numericInput(ns("dropout"), "脱落率 L",
                   value = 0.10, min = 0, max = 0.99, step = 0.05)
    ),
    common_main_card(ns, "mann_whitney")
  )
}

mod_mann_whitney_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    pt <- function() input$power_target %||% 0.80
    cm <- function() input$calc_mode    %||% "sample_size"
    params <- reactive({
      validate(
        need(isTruthy(input$sd_common) && input$sd_common > 0, "SD > 0"),
        need(input$mean_A != input$mean_B,
             "A と B の平均が同じでは計算できません")
      )
      list(mean_A = input$mean_A, mean_B = input$mean_B,
           sd_common = input$sd_common,
           distribution = input$distribution,
           alpha = input$alpha,
           n = input$n %||% 50,
           dropout = input$dropout)
    })
    common_main_server(input, output, session, "mann_whitney", params,
                       power_target = pt, calc_mode = cm)
  })
}
