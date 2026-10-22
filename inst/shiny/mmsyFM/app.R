# mmsyFM Shiny app (dev-shiny): set-up, equilibrium explorer, yield curves,
# optimiser, harvest strategies and trigger tuning.
# Launch with mmsyFM::run_app().

library(shiny)
library(mmsyFM)

fmt <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")
state_table <- function(st, sys) data.frame(
  Stock = sys$species, Code = sys$code, `B/Bmsy` = round(unname(st$BBmsy), 2),
  `F/Fmsy` = round(unname(st$FFmsy), 2), check.names = FALSE)
summary_table <- function(st, label) data.frame(
  Scenario = label, Days = fmt(sum(st$E_seg)), Yield = fmt(st$summary$yield),
  Profit = fmt(st$summary$profit), `Lowest B/Bmsy` = round(min(st$BBmsy), 2),
  `Stocks < Bpa` = sum(st$BBmsy < 0.5), check.names = FALSE)
eq_state <- function(sys, caps, years, ...) {
  x <- ms_simulate(sys, matrix(caps, 1), years, capacity = matrix(caps, 1), avg_years = 20, ...)
  s <- ms_summary(x)
  list(E_seg = setNames(colMeans(x$E_seg[, 1, , drop = FALSE][, 1, ]), names(sys$segments)),
       B = colMeans(x$B[, 1, ]), summary = s, BBmsy = attr(s, "BBmsy")[1, ], FFmsy = attr(s, "FFmsy")[1, ],
       E_metier = colMeans(x$E_metier[, 1, ]))
}
proc_args <- function(input, sys) list(
  proc_sd = proc_sd_from_r(sys$r) * input$proc_scale,
  proc_dep = if (input$proc_dep) "jabba" else "none",
  proc_center = input$proc_center, rho = input$proc_rho)

ui <- navbarPage(
  "mmsyFM", id = "tabs",
  header = tags$head(tags$style(HTML(
    "body { font-size: 14px; } .well { background: #f7f7f5; } h4 { margin-top: 18px; }
     .note { color: #52514e; font-size: 13px; }"))),

  # ---- 1. Set-up -------------------------------------------------------------
  tabPanel("1. Set-up",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("preset", "System", c("Atlantic example" = "atlantic", "Upload tables" = "upload")),
        conditionalPanel("input.preset == 'upload'",
          fileInput("f_species", "species.csv"), fileInput("f_metiers", "metiers.csv"),
          fileInput("f_q", "catchability.csv"), fileInput("f_segments", "segments.csv"),
          numericInput("season_amp", "Effort seasonality", 0, 0, 1, 0.1)),
        numericInput("capacity", "Capacity (days per segment)", 1000, 1, step = 100),
        sliderInput("gamma", "Preference strength (gamma)", 0, 3, 1.5, 0.25),
        hr(),
        h5("Tune the status quo (open access)"),
        uiOutput("share_inputs"),
        numericInput("tune_min", "Weakest stock at open access (B/Bmsy)", 0.4, 0.05, 1.5, 0.05),
        actionButton("tune", "Tune daily costs", class = "btn-primary"),
        p(class = "note", "Tuning solves the daily cost of each segment (about 45 s).")),
      mainPanel(width = 9,
        h4("Status quo"), tableOutput("sq_summary"),
        fluidRow(column(6, h4("Stocks"), tableOutput("sq_stocks")),
                 column(6, h4("Segments"), tableOutput("segments"))),
        h4("Catchability by metier (relative to each stock's maximum)"),
        plotOutput("q_plot", height = "320px")))),

  # ---- 2. Equilibrium explorer ----------------------------------------------
  tabPanel("2. Equilibrium",
    sidebarLayout(
      sidebarPanel(width = 3,
        uiOutput("day_sliders"),
        uiOutput("open_boxes"),
        actionButton("run_eq", "Run equilibrium", class = "btn-primary"),
        p(class = "note", "Days are caps; the days fished are those of the vessels that break even.")),
      mainPanel(width = 9,
        tableOutput("eq_summary"),
        plotOutput("eq_kobe", height = "380px"),
        h4("Days by metier"), tableOutput("eq_metier")))),

  # ---- 3. Yield curves -------------------------------------------------------
  tabPanel("3. Yield curves",
    sidebarLayout(
      sidebarPanel(width = 3,
        checkboxGroupInput("yc_treat", "Treatments",
          c("Shared q (mse.SEA)" = "sharedq", "Metiers, profit-blind" = "blind",
            "Metiers, profit-seeking" = "seek", "Profit-seeking + closure" = "closed"),
          c("sharedq", "seek", "closed")),
        uiOutput("yc_close"),
        actionButton("run_yc", "Compute curves", class = "btn-primary"),
        p(class = "note", "Days scaled from the status quo; about 10-20 s.")),
      mainPanel(width = 9, plotOutput("yc_plot", height = "480px"), tableOutput("yc_points")))),

  # ---- 4. Optimiser ----------------------------------------------------------
  tabPanel("4. Safeguarded MMSY",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("opt_obj", "Objective", c("yield", "revenue", "profit")),
        radioButtons("opt_levers", "Levers", c("One global cap" = "total", "Cap by segment" = "segments")),
        sliderInput("opt_minshare", "Viability floor (minimum share per segment)", 0, 0.33, 0, 0.01),
        numericInput("opt_bpa", "Safeguard Bpa (B/Bmsy)", 0.5, 0.1, 1, 0.05),
        uiOutput("opt_close"),
        actionButton("run_opt", "Optimise", class = "btn-primary"),
        p(class = "note", "One cap: about 10 s; caps by segment: about 1 min.")),
      mainPanel(width = 9, tableOutput("opt_summary"), plotOutput("opt_kobe", height = "380px")))),

  # ---- 5. Harvest strategies -------------------------------------------------
  tabPanel("5. Harvest strategy",
    sidebarLayout(
      sidebarPanel(width = 3,
        h5("Rule"),
        sliderInput("hs_btrig", "Effort trigger (B/Bmsy)", 0.3, 1.5, 0.6, 0.05),
        uiOutput("hs_close"),
        sliderInput("hs_btrig_close", "Closure trigger (B/Bmsy)", 0.5, 5, 3, 0.25),
        sliderInput("hs_maxchange", "Maximum annual change (Inf = none)", 0.05, 1, 1, 0.05),
        h5("Uncertainty"),
        sliderInput("hs_lag", "Data lag (years)", 0, 3, 2, 1),
        sliderInput("hs_assess", "Assessment error CV", 0, 0.5, 0.2, 0.05),
        sliderInput("hs_impl", "Implementation error CV", 0, 0.5, 0.1, 0.05),
        sliderInput("hs_effort", "Effort noise CV", 0, 0.5, 0.1, 0.05),
        sliderInput("proc_scale", "Process SD (x r-based default)", 0, 2, 1, 0.1),
        checkboxInput("proc_dep", "JABBA proc.dyn (volatile at low B)", TRUE),
        radioButtons("proc_center", "Process error centred on", c("median", "mean"), inline = TRUE),
        sliderInput("proc_rho", "Process AR(1)", 0, 0.9, 0.3, 0.1),
        h5("Simulation"),
        numericInput("hs_nsim", "Simulations", 100, 10, 1000, 10),
        numericInput("hs_years", "Years", 40, 10, 100, 5),
        actionButton("run_hs", "Run", class = "btn-primary")),
      mainPanel(width = 9, tableOutput("hs_perf"),
                plotOutput("hs_status", height = "420px"), plotOutput("hs_fleet", height = "300px")))),

  # ---- 6. Tuning -------------------------------------------------------------
  tabPanel("6. Tune triggers",
    sidebarLayout(
      sidebarPanel(width = 3,
        sliderInput("tu_eff", "Effort triggers", 0.3, 1.5, c(0.4, 1.0), 0.1),
        textInput("tu_close", "Closure triggers (comma-separated)", "1, 1.5, 2, 3, 5"),
        numericInput("tu_risk", "Risk limit: worst stock P(B < Bpa)", 0.05, 0.01, 0.5, 0.01),
        numericInput("tu_nsim", "Simulations per grid point", 50, 10, 500, 10),
        p(class = "note", "Uses the rule and uncertainty settings of tab 5. Each grid point sees the same random errors."),
        actionButton("run_tu", "Tune", class = "btn-primary")),
      mainPanel(width = 9, h4("Tuned rule"), tableOutput("tu_best"), h4("Grid"), tableOutput("tu_table"))))
)

server <- function(input, output, session) {
  rv <- reactiveValues(sys = NULL, sq = NULL)

  # system and status quo
  # base system: changes only with the preset or uploaded tables (not with gamma or
  # capacity, so a tuned system is kept)
  observeEvent(list(input$preset, input$f_species, input$f_metiers, input$f_q, input$f_segments,
                    input$season_amp), {
    sys <- if (input$preset == "atlantic") atlantic_system() else {
      req(input$f_species, input$f_metiers, input$f_q, input$f_segments)
      rd <- function(f) utils::read.csv(f$datapath, check.names = FALSE, stringsAsFactors = FALSE)
      tryCatch(ms_system(rd(input$f_species), rd(input$f_metiers), rd(input$f_q), rd(input$f_segments),
                         effort_season_amp = input$season_amp),
               error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    }
    req(sys)
    rv$sys <- sys
  })
  # status quo: open access at the current capacity and preference strength
  observe({
    req(rv$sys)
    rv$sq <- ms_status_quo(rv$sys, input$capacity, gamma = input$gamma, years = 300)
  })
  observeEvent(input$tune, {
    sys <- rv$sys; req(sys)
    sh <- vapply(names(sys$segments), function(g) input[[paste0("share_", g)]] %||% sys$segments[[g]], 0)
    withProgress(message = "Tuning daily costs", value = 0.3, {
      tu <- ms_tune(sys, shares = sh / sum(sh), status = c(min = input$tune_min), cap = input$capacity,
                    gamma = input$gamma, verbose = FALSE)
    })
    rv$sys <- tu
    showNotification(sprintf("Tuned (max error %.3f)", attr(tu, "max_error")))
  })
  output$share_inputs <- renderUI({
    sys <- rv$sys; req(sys)
    lapply(names(sys$segments), function(g)
      numericInput(paste0("share_", g), paste("Target share of days:", g), round(sys$segments[[g]], 2), 0, 1, 0.05))
  })
  output$sq_summary <- renderTable(summary_table(rv$sq, "Status quo"), striped = TRUE)
  output$sq_stocks <- renderTable(state_table(rv$sq, rv$sys), striped = TRUE)
  output$segments <- renderTable({
    sys <- rv$sys; req(sys)
    data.frame(Segment = names(sys$segments), Loyalty = sys$loyalty, Efficiency = apply(sys$eff, 1, max),
               `Daily cost x` = round(sys$cost_seg, 3), `Status quo days` = fmt(rv$sq$E_seg),
               check.names = FALSE)
  }, striped = TRUE)
  output$q_plot <- renderPlot(plot_catchability(req(rv$sys)), res = 110)

  # 2. equilibrium explorer
  output$day_sliders <- renderUI({
    sys <- rv$sys; sq <- rv$sq; req(sys, sq)
    lapply(names(sys$segments), function(g)
      sliderInput(paste0("days_", g), paste("Days:", g), 0, input$capacity, round(sq$E_seg[[g]]), 10))
  })
  output$open_boxes <- renderUI({
    sys <- rv$sys; req(sys)
    checkboxGroupInput("open_m", "Open metiers", sys$metiers$name, sys$metiers$name)
  })
  eq <- eventReactive(input$run_eq, {
    sys <- rv$sys
    caps <- vapply(names(sys$segments), function(g) input[[paste0("days_", g)]], 0)
    eq_state(sys, caps, 250, gamma = input$gamma, open = as.numeric(sys$metiers$name %in% input$open_m))
  })
  output$eq_summary <- renderTable(rbind(summary_table(rv$sq, "Status quo"), summary_table(eq(), "Scenario")), striped = TRUE)
  output$eq_kobe <- renderPlot(plot_kobe(list("Status quo" = rv$sq, "Scenario" = eq()), rv$sys), res = 110)
  output$eq_metier <- renderTable({
    e <- eq(); data.frame(Metier = rv$sys$metiers$name, Days = fmt(e$E_metier),
                          Share = sprintf("%.0f%%", 100 * e$E_metier / sum(e$E_metier)))
  }, striped = TRUE)

  # 3. yield curves
  output$yc_close <- renderUI(selectInput("yc_close_m", "Metier to close", req(rv$sys)$metiers$name))
  yc <- eventReactive(input$run_yc, {
    sys <- rv$sys; sq <- rv$sq; mult <- c(0, 10^seq(-1.5, log10(6), length.out = 35))
    op <- as.numeric(sys$metiers$name != input$yc_close_m)
    withProgress(message = "Computing yield curves", value = 0.2, {
      cv <- list()
      if ("sharedq" %in% input$yc_treat) cv[["Shared q (mse.SEA)"]] <- ms_shared_q_curve(sys, sq)
      if ("blind" %in% input$yc_treat) cv[["Profit-blind"]] <- ms_yield_curve(sys, sq$E_seg, mult, 200, gamma = 0)
      if ("seek" %in% input$yc_treat) cv[["Profit-seeking"]] <- ms_yield_curve(sys, sq$E_seg, mult, 200, gamma = input$gamma)
      if ("closed" %in% input$yc_treat) cv[[paste("Profit-seeking,", input$yc_close_m, "closed")]] <-
        ms_yield_curve(sys, sq$E_seg, mult, 200, gamma = input$gamma, open = op)
      cv
    })
  })
  output$yc_plot <- renderPlot(plot_yield_curves(yc(), ref = rv$sq, sys = rv$sys), res = 110)
  output$yc_points <- renderTable({
    do.call(rbind, lapply(names(yc()), function(nm) { x <- yc()[[nm]]; ok <- which(x$min_BBmsy >= 0.5 & x$E > 0)
      data.frame(Treatment = nm, `MMSY` = fmt(max(x$yield)), `Days at MMSY` = fmt(x$E[which.max(x$yield)]),
                 `Safeguarded yield` = if (length(ok)) fmt(max(x$yield[ok])) else "none",
                 check.names = FALSE) }))
  }, striped = TRUE)

  # 4. optimiser
  output$opt_close <- renderUI(checkboxGroupInput("opt_closed", "Closed metiers", req(rv$sys)$metiers$name))
  opt <- eventReactive(input$run_opt, {
    sys <- rv$sys
    lev <- if (input$opt_levers == "total") "total" else names(sys$segments)
    withProgress(message = "Optimising", value = 0.2,
      ms_optimise(sys, input$opt_obj, levers = lev, upper = if (identical(lev, "total")) 3 * input$capacity else input$capacity,
                  Bpa = input$opt_bpa, min_share = input$opt_minshare, gamma = input$gamma, years = 150,
                  open = as.numeric(!sys$metiers$name %in% input$opt_closed)))
  })
  output$opt_summary <- renderTable({
    o <- opt(); validate(need(isTRUE(o$feasible), "No feasible caps: every stock cannot be kept above Bpa."))
    rbind(summary_table(rv$sq, "Status quo"), cbind(summary_table(o, "Safeguarded optimum")))
  }, striped = TRUE)
  output$opt_kobe <- renderPlot({ o <- opt(); req(isTRUE(o$feasible))
    plot_kobe(list("Status quo" = rv$sq, "Safeguarded optimum" = o), rv$sys) }, res = 110)

  # 5. harvest strategy
  output$hs_close <- renderUI(selectInput("hs_close_s", "Closure for stock", c("none", req(rv$sys)$species)))
  rule_args <- reactive(list(Btrig = input$hs_btrig, Btrig_close = input$hs_btrig_close,
                             close = if (input$hs_close_s == "none") NULL else input$hs_close_s,
                             assess_cv = input$hs_assess,
                             max_change = if (input$hs_maxchange >= 1) Inf else input$hs_maxchange))
  sim_args <- reactive(c(list(gamma = input$gamma, capacity = input$capacity, impl_cv = input$hs_impl,
                              effort_cv = input$hs_effort, effort_rho = 0.5, hcr_lag = input$hs_lag),
                         proc_args(input, rv$sys)))
  hs <- eventReactive(input$run_hs, {
    sys <- rv$sys; sq <- rv$sq; n <- input$hs_nsim
    B0 <- matrix(sq$B, n, sys$S, byrow = TRUE); E0 <- matrix(sq$E_seg, n, sys$G, byrow = TRUE)
    run <- function(caps, hcr = NULL) do.call(ms_simulate, c(list(sys, caps, input$hs_years, B_init = B0,
      E_init = E0, hcr = hcr, seed = 1, keep = "all"), sim_args()))
    withProgress(message = "Simulating", value = 0.3, list(
      "No control" = run(matrix(input$capacity, n, sys$G)),
      "Rule" = run(E0, do.call(hcr_effort, c(list(sys), rule_args())))))
  })
  output$hs_perf <- renderTable({
    r <- hs(); ny <- input$hs_years
    p <- ms_performance(r, years = max(1, ny - 19):ny)
    data.frame(Run = p$run, Yield = fmt(p$yield), Profit = fmt(p$profit), Days = fmt(p$days),
               `P(any stock < Blim)` = sprintf("%.2f", p$P_any_below_Blim),
               `Worst stock P(B < Bpa)` = sprintf("%.2f", p$worst_P_below_Bpa), `Worst stock` = p$worst_stock,
               `Effort variability` = sprintf("%.2f", p$AAV_effort), check.names = FALSE)
  }, striped = TRUE)
  output$hs_status <- renderPlot(plot_projection(hs(), "BBmsy"), res = 100)
  output$hs_fleet <- renderPlot(plot_fleet(hs()), res = 100)

  # 6. tuning
  tu <- eventReactive(input$run_tu, {
    sys <- rv$sys; sq <- rv$sq; ra <- rule_args()
    ct <- as.numeric(strsplit(input$tu_close, ",")[[1]])
    withProgress(message = "Tuning triggers", value = 0.2,
      do.call(ms_tune_hcr, c(list(sys, triggers = seq(input$tu_eff[1], input$tu_eff[2], by = 0.1),
        close = ra$close, close_triggers = if (is.null(ra$close)) NULL else ct,
        risk = c(worst_P_below_Bpa = input$tu_risk), E_ref = sq$E_seg, B_init = sq$B,
        nsim = input$tu_nsim, years = input$hs_years, eval_years = max(1, input$hs_years - 19):input$hs_years,
        assess_cv = input$hs_assess, max_change = ra$max_change), sim_args())))
  })
  output$tu_best <- renderTable({ b <- tu()$best; validate(need(!is.null(b), "No grid point meets the risk limit.")); b }, striped = TRUE)
  output$tu_table <- renderTable(tu()$table, striped = TRUE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

shinyApp(ui, server)
