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
  tagList(
    radioButtons(ns("calc_mode"), "計算モード",
      choices = c("必要症例数を計算する"               = "sample_size",
                  "与えられた n での検出力を計算する" = "power_calc"),
      selected = "sample_size")
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
      labeled_input(numericInput(ns("alpha"), "有意水準 α（両側）",
                                 value = 0.05, min = 0, max = 1, step = 0.005),
                    "alpha"),
      .power_target_input(ns),
      .n_input(ns, "1 群あたり n", 50),
      labeled_input(numericInput(ns("dropout"), "脱落率 L",
                                 value = 0.10, min = 0, max = 0.99,
                                 step = 0.05), "dropout")
    ),
    common_main_card(ns, "ttest_m1")
  )
}

mod_ttest_m1_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    params <- reactive({
      validate(
        need(isTruthy(input$sd_A) && input$sd_A > 0, "A の SD は正の値"),
        need(isTruthy(input$sd_B) && input$sd_B > 0, "B の SD は正の値"),
        need(isTruthy(input$alpha) && input$alpha > 0 && input$alpha < 1,
             "α は 0 < α < 1"),
        need(input$mean_A != input$mean_B,
             "A と B の平均値が同じでは計算できません")
      )
      if ((input$calc_mode %||% "sample_size") == "power_calc") {
        validate(need(isTruthy(input$n) && input$n >= 2, "n は 2 以上"))
      }
      list(mean_A = input$mean_A, sd_A = input$sd_A,
           mean_B = input$mean_B, sd_B = input$sd_B,
           alpha  = input$alpha,
           n      = input$n %||% 50,
           dropout = input$dropout)
    })
    common_main_server(
      input, output, session, "ttest_m1", params,
      power_target = reactive(input$power_target %||% 0.80),
      calc_mode    = reactive(input$calc_mode    %||% "sample_size")
    )
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
      labeled_input(numericInput(ns("alpha"), "有意水準 α（両側）",
                                 value = 0.05, min = 0, max = 1, step = 0.005),
                    "alpha"),
      .power_target_input(ns),
      .n_input(ns, "1 群あたり n", 50),
      labeled_input(numericInput(ns("dropout"), "脱落率 L",
                                 value = 0.10, min = 0, max = 0.99,
                                 step = 0.05), "dropout")
    ),
    common_main_card(ns, "ttest_m2")
  )
}

mod_ttest_m2_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    params <- reactive({
      validate(
        need(isTruthy(input$sd_A) && input$sd_A > 0, "A の SD は正の値"),
        need(isTruthy(input$sd_B) && input$sd_B > 0, "B の SD は正の値"),
        need(isTruthy(input$alpha) && input$alpha > 0 && input$alpha < 1, "α は 0<α<1"),
        need(input$diff != 0, "群間差 Δ が 0 では計算できません")
      )
      if ((input$calc_mode %||% "sample_size") == "power_calc") {
        validate(need(isTruthy(input$n) && input$n >= 2, "n は 2 以上"))
      }
      list(diff = input$diff, sd_A = input$sd_A, sd_B = input$sd_B,
           alpha = input$alpha, n = input$n %||% 50, dropout = input$dropout)
    })
    common_main_server(
      input, output, session, "ttest_m2", params,
      power_target = reactive(input$power_target %||% 0.80),
      calc_mode    = reactive(input$calc_mode    %||% "sample_size")
    )
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
      radioButtons(ns("input_mode"), "入力モード",
        choices = c("差の SD を直接入力"           = "direct",
                    "相関係数から差の SD を計算"   = "corr"),
        selected = "direct"),
      conditionalPanel(
        sprintf("input['%s'] == 'direct'", ns("input_mode")),
        direct_inputs
      ),
      conditionalPanel(
        sprintf("input['%s'] == 'corr'", ns("input_mode")),
        corr_inputs
      ),
      labeled_input(numericInput(ns("alpha"), "有意水準 α（両側）",
                                 value = 0.05, min = 0, max = 1, step = 0.005),
                    "alpha"),
      .power_target_input(ns),
      .n_input(ns, "ペア数 n", 30),
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

    # パラメータリストとデザイン ID をモードで切り替える
    params <- reactive({
      validate(
        need(isTruthy(input$alpha) && input$alpha > 0 && input$alpha < 1,
             "α は 0<α<1")
      )
      if (resolve_cm() == "power_calc") {
        validate(need(isTruthy(input$n) && input$n >= 2, "n は 2 以上"))
      }
      if ((input$input_mode %||% "direct") == "corr") {
        validate(
          need(isTruthy(input$sd_1) && input$sd_1 > 0, "治療前の SD は正の値"),
          need(isTruthy(input$sd_2) && input$sd_2 > 0, "治療後の SD は正の値"),
          need(isTruthy(input$r) && input$r >= -1 && input$r <= 1,
               "相関係数 r は −1〜1"),
          need(isTruthy(input$mean_1) && isTruthy(input$mean_2) &&
                 input$mean_1 != input$mean_2,
               "治療前と治療後の平均が同じでは計算できません")
        )
        list(
          mean_1 = input$mean_1, mean_2 = input$mean_2,
          sd_1   = input$sd_1,   sd_2   = input$sd_2,
          r      = input$r,
          alpha = input$alpha,
          n = input$n %||% 30,
          dropout = input$dropout
        )
      } else {
        validate(
          need(isTruthy(input$sd_diff) && input$sd_diff > 0, "差の SD は正の値"),
          need(isTruthy(input$diff_mean) && input$diff_mean != 0,
               "差の平均が 0 では計算できません")
        )
        list(
          diff_mean = input$diff_mean, sd_diff = input$sd_diff,
          alpha = input$alpha,
          n = input$n %||% 30,
          dropout = input$dropout
        )
      }
    })
    design_id <- reactive({
      if ((input$input_mode %||% "direct") == "corr") "paired_corr" else "paired"
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
mod_binary_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      .calc_mode_and_power_inputs(ns),
      radioButtons(ns("test"), "検定方法",
        choices = c("χ² 検定" = "chisq", "Fisher の正確検定" = "fisher"),
        selected = "chisq"),
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
      labeled_input(numericInput(ns("dropout"), "脱落率 L",
                                 value = 0.10, min = 0, max = 0.99,
                                 step = 0.05), "dropout")
    ),
    common_main_card(ns, "binary_chisq")   # design_id は server 側で上書き
  )
}

mod_binary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    resolve_pt <- function() input$power_target %||% 0.80
    resolve_cm <- function() input$calc_mode    %||% "sample_size"

    params <- reactive({
      validate(
        need(isTruthy(input$p_A) && input$p_A >= 0 && input$p_A <= 1, "p_A は [0,1]"),
        need(isTruthy(input$p_B) && input$p_B >= 0 && input$p_B <= 1, "p_B は [0,1]"),
        need(isTruthy(input$alpha) && input$alpha > 0 && input$alpha < 1, "α は 0<α<1"),
        need(input$p_A != input$p_B, "p_A と p_B が同じでは計算できません")
      )
      if (resolve_cm() == "power_calc") {
        validate(need(isTruthy(input$n) && input$n >= 2, "n は 2 以上"))
      }
      list(p_A = input$p_A, p_B = input$p_B,
           alpha = input$alpha, n = input$n %||% 100, dropout = input$dropout)
    })
    design_id <- reactive({
      if (input$test == "fisher") "binary_fisher" else "binary_chisq"
    })

    result <- reactive({
      compute_result_dispatch(design_id(), params(), resolve_pt(), resolve_cm())
    })

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
    output$r_code <- renderText({
      gen_r_code(design_id(), params(), resolve_pt(), resolve_cm())
    })
    output$citation <- renderUI({ render_citation(design_id(), result()) })

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
