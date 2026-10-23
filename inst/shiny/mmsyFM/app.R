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

tips <- list(
  preset = "A ready-made system, or upload your own four tables: species, metiers, catchability and segments.",
  season_amp = "How much fishing days vary over the year: 0 = the same every month, 0.3 = plus or minus 30%, peaking in July.",
  capacity = "Registered fleet capacity by segment: the most days the segment could fish in a year. The days actually fished are those of the vessels that break even, so they can be fewer.",
  gamma = "0 = days spread evenly over the metiers, whatever they pay. Higher = days concentrate on the metiers with the best expected profit per day.",
  tune_min = "Target status of the weakest stock at open access (0.05 = the floor set by the F cap; Atlantic example 0.2). Together with the target shares it sets how heavily fished the starting system is. Tuning adjusts the daily cost of each segment until both are met.",
  yc_treat = "<b>Shared q</b>: one effort, fixed catchability, no fleet behaviour.<br><b>Profit-blind</b>: days spread evenly over each segment's metiers.<br><b>Profit-seeking</b>: days follow expected profit.<br><b>+ closure</b>: profit-seeking with the metier below closed all year.<br>Every curve scales the status-quo days of all segments by the same factor and runs to equilibrium.",
  opt_obj = "What is maximised at equilibrium: <b>total yield</b> (total catch), <b>revenue</b> (catch x price), <b>profit</b> (revenue minus fishing costs; usually much lower effort, i.e. MEY), or the <b>catch of one stock</b> (e.g. anchovy). Every stock is still kept at or above Bpa.",
  opt_levers = "<b>One global cap</b>: a single limit on total days, split across segments in their status-quo proportions.<br><b>Cap by segment</b>: a separate days limit for each segment, so cuts can target the fleet that catches the choke stock.",
  opt_minshare = "Used with <b>Cap by segment</b>: every segment keeps at least this share of the total days, so the optimum cannot shut a segment down. The maximum is 1/3 with three segments.",
  opt_bpa = "Safeguard: a solution is accepted only if <b>every</b> stock is at or above this B/Bmsy at equilibrium. The stock at the limit is the choke stock. 0 = plain (unsafeguarded) MMSY.",
  hs_btrig = "Below this B/Bmsy a segment's days are cut: full days at the trigger, a linear ramp down to the minimum effort at Blim (0.25 Bmsy). Default 0.5 Bmsy (the trigger at Bpa, as in ICES and GFCM advice).",
  hs_btrig_close = "Used with a closure stock. The metiers that drive that stock are closed in proportion to how depleted it is: fully closed at or below Blim (0.25 Bmsy), open for part of the time (e.g. a shorter season) between Blim and this trigger, and fully open at or above it. Default 1: the closure is lifted once the stock is rebuilt to Bmsy.",
  hs_emin = "The rule never sets a segment below this share of its full effort (at and below Blim): a minimum effort instead of a closure. F scales with days, so 0.1 puts about 0.1 x the F of the full effort on each stock. 0 allows a full closure.",
  hs_rules = "Each ticked rule is a run with the same random draws (process, assessment and implementation error), shown in its own colour.<br><b>No control</b>: the fleet at capacity, limited only by profitability.<br><b>Fixed MMSY effort</b>: each segment capped at its safeguarded-MMSY days, no feedback.<br><b>HCR</b>: each segment's days ramp from the minimum effort at Blim to its full effort at the trigger. <b>Value-weighted</b>: the status is the mean B/Bmsy of the stocks the segment fishes, weighted by their value to it. <b>Weakest link</b>: the most depleted stock decides (most precautionary). <b>Full effort</b> = the MMSY days, or today's days. <b>+ closure</b>: the stock chosen below is protected by closing the metiers that drive it.",
  hs_trig_seg = "Set a different trigger for each segment, e.g. a higher trigger for the purse seine fishing short-lived small pelagics.",
  hs_bycatch = "Off (default): a segment answers only for stocks it catches as a main fishery (catchability at least half the stock's highest). On: by-catch counts too, so more stocks can cut a segment's days.",
  hs_qstat = "Statistic shown for every stock (rows) and rule (columns). Colours rank the rules within each stock, or use the risk limit for probabilities of being below Bpa or Blim.",
  hs_qrisk = "For P(B < Bpa) and P(B < Blim): green at or below this limit, amber up to twice the limit, red above.",
  hs_bstock = "Stocks shown in the lower boxplots (one row per stock). Default: the three most depleted at the status quo.",
  hs_mclose = "Metiers closed by management (fixed, not triggered by stock status), used by the rules 'Metier closures only' and 'HCR + metier closures'. Their days move to the open metiers by expected profit.",
  hs_mclose_months = "Months of the closure (1 = January). The full range is a year-round closure; e.g. 2 to 5 closes February to May.",
  hs_mclose_share = "Share of each closed metier's ground closed in those months: 1 = fully closed, 0.5 = half of the ground (an area closure).",
  hs_maxchange = "Largest change in a segment's days from one year to the next, relative to last year (0.2 = at most 20% up or down), as for TAC changes: a cut from the status quo to the 10% floor takes about 10 years. 1 = no limit.",
  hs_lag = "Years between the data and the management year: with 2, the days for year t are set from the stock status assessed for year t-2.",
  hs_assess = "Assessment error: assessed B/Bmsy = true value x lognormal error with this CV.",
  hs_impl = "Implementation error: the days fished differ from the days allowed by a lognormal error with this CV.",
  hs_effort = "Year-to-year noise in fishing effort and catch rates by segment (autocorrelated), in every run, regulated or not.",
  proc_scale = "Process error on stock growth, as a multiple of the default SD, which increases with r (about 0.05 for sharks, 0.4 for anchovy).",
  proc_dep = "JABBA proc.dyn: process error increases at low biomass (about 1.5x at 0.5 Bmsy, 4x at 0.05 Bmsy), so depleted stocks fluctuate more.",
  proc_center = "<b>median</b>: no drift, as JABBA's dlnorm; depleted stocks can bounce back.<br><b>mean</b>: mean-unbiased, so the median drifts down when the SD is large (stochastic depensation).",
  proc_rho = "Autocorrelation of process error between years: 0 = independent years; higher = runs of good and bad years.",
  hs_nsim = "Number of stochastic replicates.",
  hs_years = "Years projected from the status quo.",
  tu_eff = "Range of effort triggers (B/Bmsy) to try, in steps of 0.1.",
  tu_close = "Closure triggers (B/Bmsy) to try for the closure stock set in tab 5.",
  tu_risk = "Highest acceptable probability that the worst stock is below Bpa over the evaluation years. The trigger with the best yield that meets it is chosen.",
  tu_nsim = "Replicates per trigger. The same random numbers are used for every trigger, so differences between triggers are not noise.",
  share = "Target share of all days fished by this segment at open access. Tuning adjusts the daily costs until the fleet settles at these shares.",
  days = "Days fished by this segment in the scenario (its cap). Fewer are used if the vessels don't break even.",
  open_m = "Untick to close a metier all year. Its days move to the open metiers by profit (displacement).",
  yc_close_m = "Used only by the 'Profit-seeking + closure' curve: this one metier is closed all year and its days move to the other metiers. The other curves keep every metier open.",
  opt_closed = "Metiers kept closed all year during the optimisation, e.g. a protected reef.",
  hs_close_s = "A choke stock protected by a closure of the metiers that drive it. 'none' = effort rule only.")
info <- function(id, label) tagList(label, tags$span(class = "info", `data-toggle` = "tooltip",
  `data-placement` = "right", `data-html` = "true", title = tips[[id]], HTML("&#9432;")))

guide_html <- function() {
  f <- system.file("guide", "how-it-works.md", package = "mmsyFM")
  if (!nzchar(f) || !requireNamespace("commonmark", quietly = TRUE))
    return(p("The guide needs the commonmark package; see also vignette('how-it-works', 'mmsyFM')."))
  md <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  # protect LaTeX ($$...$$ and $...$) from the Markdown renderer, then restore it for MathJax
  rx <- "\\$\\$[^$]+\\$\\$|\\$[^$\n]+\\$"
  math <- regmatches(md, gregexpr(rx, md))[[1]]
  key <- sprintf("MATHXX%03dXX", seq_along(math))
  regmatches(md, gregexpr(rx, md)) <- list(key)
  h <- commonmark::markdown_html(md, extensions = TRUE)
  esc <- function(x) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  for (i in seq_along(math)) h <- sub(key[i], esc(math[i]), h, fixed = TRUE)
  HTML(h)
}

ui <- navbarPage(
  "mmsyFM", id = "tabs",
  header = tags$head(tags$style(HTML(
    "body { font-size: 14px; } .well { background: #f7f7f5; } h4 { margin-top: 18px; }
     .note { color: #52514e; font-size: 13px; }
     .draft { background: #fff6e0; border-left: 3px solid #e0a020; padding: 6px 8px; }
     .guide { font-size: 15px; line-height: 1.55; max-width: 900px; padding-bottom: 40px; }
     .guide pre { background: #f7f7f5; border: none; font-size: 13px; }
     .guide table { margin: 10px 0; } .guide td, .guide th { padding: 4px 10px; border-bottom: 1px solid #ddd; }
     .info { color: #2a5db0; cursor: help; margin-left: 5px; font-weight: normal; }
     .tooltip-inner { max-width: 340px; text-align: left; font-size: 13px; padding: 8px 10px; }")),
    tags$script(HTML("$(function() { $('body').tooltip({ selector: '[data-toggle=\\'tooltip\\']', container: 'body' }); });"))),

  # ---- 1. Set-up -------------------------------------------------------------
  tabPanel("1. Set-up",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("preset", info("preset", "System"), c("Atlantic example" = "atlantic",
          "Gulf of Thailand (draft)" = "got", "Upload tables" = "upload")),
        conditionalPanel("input.preset == 'got'", p(class = "note draft",
          "Draft: 20 stock groups (from mmsyapp), trawl / purse seine / small-scale with coastal zoning.",
          "Prices, gear overlaps and costs are indicative; the stocks are conditioned on the 2024 depletions where available.")),
        conditionalPanel("input.preset == 'upload'",
          fileInput("f_species", "species.csv"), fileInput("f_metiers", "metiers.csv"),
          fileInput("f_q", "catchability.csv"), fileInput("f_segments", "segments.csv"),
          numericInput("season_amp", info("season_amp", "Effort seasonality"), 0, 0, 1, 0.1)),
        uiOutput("cap_inputs"),
        sliderInput("gamma", info("gamma", "How strongly fishers chase profit"), 0, 3, 1.5, 0.25),
        uiOutput("gamma_help"),
        hr(),
        h5("Tune the status quo (open access)"),
        uiOutput("share_inputs"),
        numericInput("tune_min", info("tune_min", "Weakest stock at open access (B/Bmsy)"), 0.2, 0.05, 1.5, 0.05),
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
        checkboxGroupInput("yc_treat", info("yc_treat", "Treatments"),
          c("Shared q" = "sharedq", "Metiers, profit-blind" = "blind",
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
        selectInput("opt_obj", info("opt_obj", "Objective"), c("Total yield" = "yield", "Revenue" = "revenue", "Profit" = "profit")),
        radioButtons("opt_levers", info("opt_levers", "Levers"), c("One global cap" = "total", "Cap by segment" = "segments")),
        p(class = "note", "The viability floor applies only to caps by segment: with one global cap the split across segments is fixed at the status-quo shares."),
        conditionalPanel("input.opt_levers == 'segments'",
          sliderInput("opt_minshare", info("opt_minshare", "Viability floor (minimum share per segment)"), 0, 0.33, 0, 0.01)),
        numericInput("opt_bpa", info("opt_bpa", "Safeguard Bpa (B/Bmsy)"), 0.5, 0.1, 1, 0.05),
        uiOutput("opt_close"),
        actionButton("run_opt", "Optimise", class = "btn-primary"),
        p(class = "note", "One cap: about 10 s; caps by segment: about 1 min.")),
      mainPanel(width = 9, tableOutput("opt_summary"), plotOutput("opt_kobe", height = "380px")))),

  # ---- 5. Harvest strategies -------------------------------------------------
  tabPanel("5. Harvest strategy",
    sidebarLayout(
      sidebarPanel(width = 3,
        checkboxGroupInput("hs_rules", info("hs_rules", "Rules to compare"),
          c("No control (status-quo days)" = "none", "Fixed MMSY effort" = "mmsy",
            "HCR, value-weighted (full = MMSY)" = "hcr_w", "HCR, weakest link (full = MMSY)" = "hcr_min",
            "HCR, value-weighted (full = status quo)" = "hcr_sq", "HCR + closure for a stock (full = MMSY)" = "hcr_close",
            "Metier closures only" = "mclose_only", "HCR + metier closures (full = MMSY)" = "mclose_hcr"),
          c("none", "mmsy", "hcr_w", "hcr_min")),
        p(class = "note", "Full effort = the safeguarded-MMSY days from tab 4, with its settings and closures (computed here with defaults if tab 4 has not been run: 1-3 min)."),
        uiOutput("hs_ref"),
        h5("Metier closures"),
        uiOutput("hs_mclose_ui"),
        sliderInput("hs_mclose_months", info("hs_mclose_months", "Closed months"), 1, 12, c(1, 12), 1),
        sliderInput("hs_mclose_share", info("hs_mclose_share", "Share of the ground closed"), 0.1, 1, 1, 0.05),
        h5("Rule"),
        sliderInput("hs_btrig", info("hs_btrig", "Effort trigger (B/Bmsy)"), 0.3, 1.5, 0.5, 0.05),
        checkboxInput("hs_trig_seg", info("hs_trig_seg", "Triggers by segment"), FALSE),
        conditionalPanel("input.hs_trig_seg", uiOutput("hs_trig_inputs")),
        checkboxInput("hs_bycatch", info("hs_bycatch", "By-catch counts for responsibility"), FALSE),
        uiOutput("hs_close"),
        conditionalPanel("input.hs_close_s && input.hs_close_s != 'none'",
          sliderInput("hs_btrig_close", info("hs_btrig_close", "Closure trigger (B/Bmsy)"), 0.3, 2, 1, 0.05)),
        sliderInput("hs_emin", info("hs_emin", "Minimum effort (share of full effort)"), 0, 0.5, 0.1, 0.05),
        sliderInput("hs_maxchange", info("hs_maxchange", "Maximum annual change in days (1 = no limit)"), 0.05, 1, 0.2, 0.05),
        h5("Uncertainty"),
        sliderInput("hs_lag", info("hs_lag", "Data lag (years)"), 0, 3, 2, 1),
        sliderInput("hs_assess", info("hs_assess", "Assessment error CV"), 0, 0.5, 0.2, 0.05),
        sliderInput("hs_impl", info("hs_impl", "Implementation error CV"), 0, 0.5, 0.1, 0.05),
        sliderInput("hs_effort", info("hs_effort", "Effort noise CV"), 0, 0.5, 0.1, 0.05),
        sliderInput("proc_scale", info("proc_scale", "Process SD (x r-based default)"), 0, 2, 1, 0.1),
        checkboxInput("proc_dep", info("proc_dep", "JABBA proc.dyn (volatile at low B)"), TRUE),
        radioButtons("proc_center", info("proc_center", "Process error centred on"), c("median", "mean"), inline = TRUE),
        sliderInput("proc_rho", info("proc_rho", "Process AR(1)"), 0, 0.9, 0.3, 0.1),
        h5("Simulation"),
        numericInput("hs_nsim", info("hs_nsim", "Simulations"), 100, 10, 1000, 10),
        numericInput("hs_years", info("hs_years", "Years"), 40, 10, 100, 5),
        actionButton("run_hs", "Run", class = "btn-primary")),
      mainPanel(width = 9,
        tabsetPanel(id = "hs_tabs",
          tabPanel("1. Table and projections",
            tableOutput("hs_perf"),
            downloadButton("hs_download", "Download statistics (CSV, mseviz format)"),
            plotOutput("hs_status", height = "520px"), plotOutput("hs_fleet", height = "300px")),
          tabPanel("2. Boxplots",
            selectInput("hs_bstock", info("hs_bstock", "Stocks (rows of panels)"), NULL, multiple = TRUE),
            plotOutput("hs_bps", height = "460px"), plotOutput("hs_bps_stock", height = "620px")),
          tabPanel("3. Trade-offs",
            selectInput("hs_tox", "x axis", c("Catch" = "C", "Revenue" = "Rev", "Profit" = "Profit", "Days" = "Days")),
            plotOutput("hs_to", height = "560px")),
          tabPanel("4. Decision table",
            p(class = "note", "Medians (means for probabilities) over the last 20 years. Colours rank the rules within each column: green best, red worst; days have no preferred direction (grey)."),
            plotOutput("hs_quilt", height = "300px"),
            fluidRow(
              column(4, selectInput("hs_qstat", info("hs_qstat", "By stock: statistic"),
                c("P(B < Bpa)" = "PBpa", "P(B < Blim)" = "PBlim", "B/Bmsy" = "BBmsy", "F/Fmsy" = "FFmsy",
                  "Catch/MSY" = "CMSY", "P(B > 0.8 Bmsy)" = "PB80", "P(Kobe green)" = "green"))),
              column(4, numericInput("hs_qrisk", info("hs_qrisk", "Risk limit for probabilities"), 0.1, 0, 1, 0.05))),
            plotOutput("hs_quilt_stocks", height = "560px")))))),

  # ---- 6. Tuning -------------------------------------------------------------
  tabPanel("6. Tune triggers",
    sidebarLayout(
      sidebarPanel(width = 3,
        sliderInput("tu_eff", info("tu_eff", "Effort triggers"), 0.3, 1.5, c(0.4, 1.0), 0.1),
        textInput("tu_close", info("tu_close", "Closure triggers (comma-separated)"), "0.5, 0.75, 1, 1.25, 1.5"),
        numericInput("tu_risk", info("tu_risk", "Risk limit: worst stock P(B < Bpa)"), 0.05, 0.01, 0.5, 0.01),
        numericInput("tu_nsim", info("tu_nsim", "Simulations per grid point"), 50, 10, 500, 10),
        p(class = "note", "Uses the rule and uncertainty settings of tab 5. Each grid point sees the same random errors."),
        actionButton("run_tu", "Tune", class = "btn-primary")),
      mainPanel(width = 9, h4("Tuned rule"), tableOutput("tu_best"), h4("Grid"), tableOutput("tu_table")))),

  # ---- Guide -------------------------------------------------------------------
  tabPanel("Guide",
    tags$script(type = "text/x-mathjax-config",
      "MathJax.Hub.Config({ tex2jax: { inlineMath: [['$', '$']], displayMath: [['$$', '$$']], processEscapes: true } });"),
    withMathJax(),
    tags$script(HTML("$(document).on('shown.bs.tab', 'a[data-value=\\'Guide\\']', function() {
      if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]); });")),
    fluidRow(column(8, offset = 1, class = "guide", guide_html())))
)

server <- function(input, output, session) {
  # plain-language reading of the profit preference (gamma)
  output$gamma_help <- renderUI({
    g <- input$gamma
    shareA <- round(100 * 2^g / (1 + 2^g))
    p(class = "note",
      "Each segment spreads its days over its metiers in proportion to expected profit per day, raised to this power.",
      tags$br(), "0 = profit-blind (days spread evenly); higher = days concentrate on the best-paying metiers.",
      tags$br(), tags$b(sprintf("At %s: if metier A pays twice the profit of B, A gets %d%% of the days", g, shareA)),
      sprintf("(0: 50%%, 1: 67%%, 3: 89%%)."))
  })

  rv <- reactiveValues(sys = NULL, sq = NULL)

  # system and status quo
  # base system: changes only with the preset or uploaded tables (not with gamma or
  # capacity, so a tuned system is kept)
  observeEvent(list(input$preset, input$f_species, input$f_metiers, input$f_q, input$f_segments,
                    input$season_amp), {
    sys <- if (input$preset == "atlantic") atlantic_system() else if (input$preset == "got") got_system_draft() else {
      req(input$f_species, input$f_metiers, input$f_q, input$f_segments)
      rd <- function(f) utils::read.csv(f$datapath, check.names = FALSE, stringsAsFactors = FALSE)
      tryCatch(ms_system(rd(input$f_species), rd(input$f_metiers), rd(input$f_q), rd(input$f_segments),
                         effort_season_amp = input$season_amp),
               error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    }
    req(sys)
    rv$sys <- sys
    # weakest-stock tuning target: 0.2 Bmsy for the Atlantic example; the
    # Gulf (and uploads) to the 0.05 Bmsy floor of the F cap
    updateNumericInput(session, "tune_min", value = if (input$preset == "atlantic") 0.2 else 0.05)
  })
  # capacity by segment: inputs default to the system's capacity (else 1000 days)
  cap_default <- function(sys) {
    d <- sys$capacity %||% setNames(rep(1000, sys$G), names(sys$segments))
    d[names(sys$segments)]
  }
  output$cap_inputs <- renderUI({
    sys <- rv$sys; req(sys); d <- cap_default(sys)
    tagList(tags$label(info("capacity", "Capacity (registered days)")),
      lapply(names(sys$segments), function(g)
        numericInput(paste0("cap_", g), g, d[[g]], 1, step = 50)))
  })
  cap <- reactive({
    sys <- rv$sys; req(sys); d <- cap_default(sys)
    v <- vapply(names(sys$segments), function(g) {
      x <- input[[paste0("cap_", g)]]; if (is.null(x) || is.na(x) || x <= 0) d[[g]] else x }, 0)
    setNames(v, names(sys$segments))
  })
  # status quo: open access at the current capacity and preference strength
  observe({
    req(rv$sys)
    rv$sq <- ms_status_quo(rv$sys, cap(), gamma = input$gamma, years = 300)
  })
  observeEvent(input$tune, {
    sys <- rv$sys; req(sys)
    sh <- vapply(names(sys$segments), function(g) input[[paste0("share_", g)]] %||% sys$segments[[g]], 0)
    withProgress(message = "Tuning daily costs", value = 0.3, {
      tu <- ms_tune(sys, shares = sh / sum(sh), status = c(min = input$tune_min), cap = cap(),
                    gamma = input$gamma, verbose = FALSE)
    })
    rv$sys <- tu
    showNotification(sprintf("Tuned (max error %.3f)", attr(tu, "max_error")))
  })
  output$share_inputs <- renderUI({
    sys <- rv$sys; req(sys)
    lapply(names(sys$segments), function(g)
      numericInput(paste0("share_", g), info("share", paste("Target share of days:", g)), round(sys$segments[[g]], 2), 0, 1, 0.05))
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
      sliderInput(paste0("days_", g), info("days", paste("Days:", g)), 0, cap()[[g]], round(sq$E_seg[[g]]),
                  max(1, round(cap()[[g]] / 100))))
  })
  output$open_boxes <- renderUI({
    sys <- rv$sys; req(sys)
    checkboxGroupInput("open_m", info("open_m", "Open metiers"), sys$metiers$name, sys$metiers$name)
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
  output$yc_close <- renderUI(selectInput("yc_close_m", info("yc_close_m", "Metier to close"), req(rv$sys)$metiers$name))
  yc <- eventReactive(input$run_yc, {
    sys <- rv$sys; sq <- rv$sq; mult <- c(0, 10^seq(-1.5, log10(6), length.out = 35))
    op <- as.numeric(sys$metiers$name != input$yc_close_m)
    withProgress(message = "Computing yield curves", value = 0.2, {
      cv <- list()
      if ("sharedq" %in% input$yc_treat) cv[["Shared q"]] <- ms_shared_q_curve(sys, sq)
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
  output$opt_close <- renderUI(checkboxGroupInput("opt_closed", info("opt_closed", "Closed metiers"), req(rv$sys)$metiers$name))
  opt <- eventReactive(input$run_opt, {
    sys <- rv$sys
    lev <- if (input$opt_levers == "total") "total" else names(sys$segments)
    withProgress(message = "Optimising", value = 0.2,
      ms_optimise(sys, input$opt_obj, levers = lev, upper = if (identical(lev, "total")) sum(cap()) else cap(),
                  ratio = rv$sq$E_seg / sum(rv$sq$E_seg),
                  Bpa = input$opt_bpa, min_share = if (input$opt_levers == "segments") input$opt_minshare else 0, gamma = input$gamma, years = 150,
                  open = as.numeric(!sys$metiers$name %in% input$opt_closed)))
  })
  # objective choices: the totals and the catch of each stock of the loaded system
  observe({ sys <- rv$sys; req(sys)
    updateSelectInput(session, "opt_obj", choices = c("Total yield" = "yield", "Revenue" = "revenue", "Profit" = "profit",
      setNames(sys$code, paste("Catch:", sys$species))), selected = isolate(input$opt_obj) %||% "yield") })
  output$opt_summary <- renderTable({
    o <- opt(); validate(need(isTRUE(o$feasible), paste0("No safeguarded optimum: ", o$reason %||% "no feasible caps", ".")))
    tab <- rbind(summary_table(rv$sq, "Status quo"), summary_table(o, "Safeguarded optimum"))
    for (k in match(o$objective_stocks, rv$sys$code))
      tab[[paste("Catch of", rv$sys$species[k])]] <- fmt(c(attr(rv$sq$summary, "catch")[1, k], o$catch[[k]]))
    tab
  }, striped = TRUE)
  output$opt_kobe <- renderPlot({ o <- opt(); req(isTRUE(o$feasible))
    plot_kobe(list("Status quo" = rv$sq, "Safeguarded optimum" = o), rv$sys) }, res = 110)

  # 5. harvest strategy
  output$hs_close <- renderUI(selectInput("hs_close_s", info("hs_close_s", "Closure for stock"), c("none", req(rv$sys)$species)))
  output$hs_trig_inputs <- renderUI({
    sys <- rv$sys; req(sys)
    lapply(names(sys$segments), function(g)
      numericInput(paste0("trig_", g), paste("Trigger:", g), input$hs_btrig %||% 0.5, 0.1, 2, 0.05))
  })
  btrig <- reactive({
    if (!isTRUE(input$hs_trig_seg)) return(input$hs_btrig)
    g <- names(rv$sys$segments)
    setNames(vapply(g, function(x) input[[paste0("trig_", x)]] %||% input$hs_btrig, 0), g)
  })
  output$hs_mclose_ui <- renderUI(checkboxGroupInput("hs_mclose", info("hs_mclose", "Metiers to close"),
    req(rv$sys)$metiers$name, inline = TRUE))
  mclose_open <- function() {
    validate(need(length(input$hs_mclose) > 0, "Tick at least one metier to close for the metier-closure rules."))
    ms_closure(rv$sys, input$hs_mclose, seq(input$hs_mclose_months[1], input$hs_mclose_months[2]), input$hs_mclose_share)
  }
  rule_args <- reactive(list(Btrig = btrig(), Btrig_close = input$hs_btrig_close,
                             close = if (input$hs_close_s == "none") NULL else input$hs_close_s,
                             assess_cv = input$hs_assess, own_q = if (isTRUE(input$hs_bycatch)) 0.2 else 0.5,
                             max_change = if (input$hs_maxchange >= 1) Inf else input$hs_maxchange, Emin = input$hs_emin))
  sim_args <- reactive(c(list(gamma = input$gamma, capacity = cap(), impl_cv = input$hs_impl,
                              effort_cv = input$hs_effort, effort_rho = 0.5, hcr_lag = input$hs_lag),
                         proc_args(input, rv$sys)))
  # safeguarded-MMSY effort: from tab 4 when it has been run for this system, else computed once
  mmsy_cache <- reactiveVal(NULL)
  ref_key <- function() list(rv$sys$Q, rv$sys$cost_seg, input$gamma, cap())
  observeEvent(opt(), { o <- opt(); if (isTRUE(o$feasible)) {
    closed <- input$opt_closed
    mmsy_cache(list(key = ref_key(), caps = o$caps,
      open = as.numeric(!rv$sys$metiers$name %in% closed),
      desc = paste0("tab 4 (", input$opt_obj, ", ", if (input$opt_levers == "total") "one global cap" else "caps by segment",
                    if (input$opt_levers == "segments" && input$opt_minshare > 0) paste0(", floor ", input$opt_minshare) else "",
                    ", Bpa ", input$opt_bpa, ", closed: ", if (length(closed)) paste(closed, collapse = ", ") else "none", ")")))
  } })
  # the full (reference) effort of the MMSY-based rules: caps, open metiers and a description
  mmsy_ref <- function() {
    m <- mmsy_cache()
    if (!is.null(m) && identical(m$key, ref_key())) return(m)
    o <- withProgress(message = "Computing the safeguarded MMSY effort (once)", value = 0.2,
      ms_optimise(rv$sys, "yield", levers = names(rv$sys$segments), upper = cap(), Bpa = 0.5,
                  gamma = input$gamma, years = 150, capacity = cap()))
    validate(need(isTRUE(o$feasible), "No safeguarded MMSY solution: every stock cannot be kept above Bpa."))
    m <- list(key = ref_key(), caps = o$caps, open = rep(1, rv$sys$M),
              desc = "defaults (yield, caps by segment, Bpa 0.5, no floor, no closures)")
    mmsy_cache(m); m
  }
  output$hs_ref <- renderUI({
    m <- mmsy_cache(); req(rv$sys)
    txt <- if (!is.null(m) && identical(m$key, ref_key()))
      paste0("Full effort in use: ", m$desc, ": ", paste(names(m$caps), round(m$caps), sep = " ", collapse = ", "), " days.")
      else "Full effort: not computed yet (tab 4 result, or defaults when the rules are run)."
    p(class = "note draft", txt)
  })
  hs <- eventReactive(input$run_hs, {
    sys <- rv$sys; sq <- rv$sq; n <- input$hs_nsim; sel <- input$hs_rules
    validate(need(length(sel) > 0, "Tick at least one rule."))
    B0 <- matrix(sq$B, n, sys$S, byrow = TRUE); E0 <- matrix(sq$E_seg, n, sys$G, byrow = TRUE)
    ref <- if (any(c("mmsy", "hcr_w", "hcr_min", "hcr_close", "mclose_hcr") %in% sel)) mmsy_ref()
    opc <- if (any(c("mclose_only", "mclose_hcr") %in% sel)) mclose_open()
    Em <- if (!is.null(ref)) matrix(ref$caps, n, sys$G, byrow = TRUE)
    run <- function(caps, hcr = NULL, open = 1) do.call(ms_simulate, c(list(sys, caps, input$hs_years, B_init = B0,
      E_init = E0, hcr = hcr, open = open, seed = 1, keep = "all"), sim_args()))
    ra <- rule_args(); ra$close <- NULL
    rule <- function(...) do.call(hcr_effort, c(list(sys), utils::modifyList(ra, list(...))))
    labs <- c(none = "No control", mmsy = "Fixed MMSY effort", hcr_w = "HCR value-weighted",
              hcr_min = "HCR weakest link", hcr_sq = "HCR value-weighted, full = status quo",
              hcr_close = "HCR + stock closure", mclose_only = "Metier closures only",
              mclose_hcr = "HCR + metier closures")
    out <- list()
    withProgress(message = "Simulating", value = 0, {
      for (k in sel) {
        incProgress(1 / length(sel), detail = labs[[k]])
        out[[labs[[k]]]] <- switch(k,
          none = run(matrix(cap(), n, sys$G, byrow = TRUE)),
          mmsy = run(Em, open = ref$open),
          hcr_w = run(Em, rule(aggregate = "weighted"), open = ref$open),
          hcr_min = run(Em, rule(aggregate = "min"), open = ref$open),
          hcr_sq = run(E0, rule(aggregate = "weighted")),
          hcr_close = { validate(need(input$hs_close_s != "none", "Choose a closure stock for 'HCR + closure'."))
                        run(Em, rule(aggregate = "weighted", close = input$hs_close_s), open = ref$open) },
          mclose_only = run(matrix(cap(), n, sys$G, byrow = TRUE), open = opc),
          mclose_hcr = run(Em, rule(aggregate = "weighted"), open = opc * matrix(ref$open, 12, sys$M, byrow = TRUE)))
      }
    })
    out
  })
  perf <- reactive({ ny <- input$hs_years; ms_perf(hs(), years = max(1, ny - 19):ny) })
  sys_stats <- c("C", "Rev", "Profit", "Days", "AAVC", "NBpa", "PanyBlim", "minBBmsy")
  observe({ sys <- rv$sys; req(sys)
    updateSelectInput(session, "hs_bstock", choices = setNames(sys$code, paste(sys$code, "-", sys$species)),
                      selected = head(sys$code[order(rv$sq$BBmsy)], 3)) })
  output$hs_quilt <- renderPlot(plot_quilt(perf(), sys_stats), res = 100)
  output$hs_quilt_stocks <- renderPlot(plot_quilt(perf(), layout = "stocks", statistic = input$hs_qstat,
    risk = if (input$hs_qstat %in% c("PBpa", "PBlim")) input$hs_qrisk), res = 100)
  output$hs_bps <- renderPlot(plot_bps(perf(), c("C", "Profit", "Days", "AAVC", "NBpa", "PanyBlim")), res = 100)
  output$hs_bps_stock <- renderPlot({ req(input$hs_bstock)
    plot_bps(perf(), c("BBmsy", "FFmsy", "CMSY", "PBlim", "PB80"), biol = input$hs_bstock) }, res = 100)
  output$hs_to <- renderPlot(plot_tradeoffs(perf(), x = input$hs_tox,
    y = setdiff(c("C", "NBpa", "PanyBlim", "Profit", "AAVC", "minBBmsy"), input$hs_tox)), res = 100)
  output$hs_perf <- renderTable(ms_perf_table(perf(), sys_stats), striped = TRUE)
  output$hs_download <- downloadHandler(filename = function() "mmsyFM_performance.csv",
    content = function(file) utils::write.csv(perf(), file, row.names = FALSE))
  output$hs_status <- renderPlot(plot_projection(hs(), "BBmsy"), res = 100)
  output$hs_fleet <- renderPlot(plot_fleet(hs()), res = 100)

  # 6. tuning
  tu <- eventReactive(input$run_tu, {
    sys <- rv$sys; sq <- rv$sq; ra <- rule_args()
    ct <- as.numeric(strsplit(input$tu_close, ",")[[1]])
    withProgress(message = "Tuning triggers", value = 0.2,
      do.call(ms_tune_hcr, c(list(sys, triggers = seq(input$tu_eff[1], input$tu_eff[2], by = 0.1),
        close = ra$close, close_triggers = if (is.null(ra$close)) NULL else ct,
        risk = c(worst_P_below_Bpa = input$tu_risk), E_ref = mmsy_ref()$caps, open = mmsy_ref()$open,
        E_init = sq$E_seg, B_init = sq$B,
        nsim = input$tu_nsim, years = input$hs_years, eval_years = max(1, input$hs_years - 19):input$hs_years,
        assess_cv = input$hs_assess, max_change = ra$max_change, Emin = ra$Emin, own_q = ra$own_q), sim_args())))
  })
  output$tu_best <- renderTable({ b <- tu()$best; validate(need(!is.null(b), "No grid point meets the risk limit.")); b }, striped = TRUE)
  output$tu_table <- renderTable(tu()$table, striped = TRUE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

shinyApp(ui, server)
