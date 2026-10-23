test_that("the Shiny app server computes the status quo and an equilibrium", {
  skip_if_not_installed("shiny")
  skip_on_cran()
  app <- new.env()
  src <- readLines(system.file("shiny", "mmsyFM", "app.R", package = "mmsyFM"))
  eval(parse(text = src[!grepl("^shinyApp\\(", src)]), envir = app)
  shiny::testServer(app$server, {
    session$setInputs(preset = "atlantic", gamma = 1.5, season_amp = 0)
    expect_equal(unname(round(rv$sq$E_seg)), c(549, 528, 459), tolerance = 0.02)
    session$setInputs(days_demersal = 300, days_pelagic = 500, days_mixed = 400,
                      open_m = rv$sys$metiers$name, run_eq = 1)
    expect_gt(eq()$summary$yield, 0)
    expect_type(output$eq_metier, "character")
  })
})

test_that("the app loads the Gulf of Thailand preset with capacity by segment", {
  skip_if_not_installed("shiny")
  skip_on_cran()
  app <- new.env()
  src <- readLines(system.file("shiny", "mmsyFM", "app.R", package = "mmsyFM"))
  eval(parse(text = src[!grepl("^shinyApp\\(", src)]), envir = app)
  shiny::testServer(app$server, {
    session$setInputs(preset = "got", gamma = 1.5, season_amp = 0)
    expect_equal(rv$sys$S, 20)
    expect_equal(unname(cap()), c(450, 225, 3000))
    sh <- rv$sq$E_seg / sum(rv$sq$E_seg)
    expect_equal(unname(round(sh, 2)), c(0.12, 0.06, 0.82), tolerance = 0.03)
    session$setInputs(cap_small_scale = 2000)
    expect_equal(cap()[["small_scale"]], 2000)
  })
})

test_that("the app compares several rules, with full effort from the safeguarded MMSY", {
  skip_if_not_installed("shiny")
  skip_on_cran()
  app <- new.env()
  src <- readLines(system.file("shiny", "mmsyFM", "app.R", package = "mmsyFM"))
  eval(parse(text = src[!grepl("^shinyApp\\(", src)]), envir = app)
  shiny::testServer(app$server, {
    session$setInputs(preset = "atlantic", gamma = 1.5, season_amp = 0, hs_close_s = "none",
                      hs_rules = c("none", "mmsy", "hcr_w", "hcr_min"), hs_btrig = 0.5, hs_trig_seg = FALSE,
                      hs_bycatch = FALSE, hs_btrig_close = 3, hs_emin = 0.1, hs_maxchange = 0.2, hs_lag = 2,
                      hs_assess = 0.2, hs_impl = 0.1, hs_effort = 0.1, proc_scale = 1, proc_dep = TRUE,
                      proc_center = "median", proc_rho = 0.3, hs_nsim = 4, hs_years = 10, run_hs = 1)
    r <- hs()
    expect_equal(names(r), c("No control", "Fixed MMSY effort", "HCR value-weighted", "HCR weakest link"))
    expect_true(all(vapply(r, function(x) dim(x$B)[1], 0) == 10))
    expect_s3_class(perf(), "data.table")
    session$setInputs(hs_rules = c("none", "mclose_only", "mclose_hcr"), hs_mclose = rv$sys$metiers$name[1],
                      hs_mclose_months = c(2, 5), hs_mclose_share = 1, run_hs = 2)
    expect_equal(names(hs()), c("No control", "Metier closures only", "HCR + metier closures"))
    expect_type(output$hs_perf, "character")
    # assessment strategies (defaults from ms_assessment_presets(), then edited)
    session$setInputs(hs_rules = c("hcr_w", "hcr_ind", "hcr_cplx", "hcr_asg"), hs_cindex = "cpue", hs_assess_agg = "weighted", run_hs = 3)
    expect_equal(names(hs()), c("HCR value-weighted", "HCR indicator species", "HCR species complexes",
                                "HCR nominal assignment"))
    session$setInputs(hs_ind = rv$sys$species[1], cplx_1 = rv$sys$species[1:3], cplx_2 = character(0),
                      cplx_3 = character(0), hs_rules = c("hcr_ind", "hcr_cplx"), run_hs = 4)
    expect_equal(length(hs()), 2)
    expect_type(output$hs_perf, "character")
  })
})

test_that("the app runs the Adriatic management-plan rules from the assessed status", {
  skip_if_not_installed("shiny")
  skip_on_cran()
  app <- new.env()
  src <- readLines(system.file("shiny", "mmsyFM", "app.R", package = "mmsyFM"))
  eval(parse(text = src[!grepl("^shinyApp\\(", src)]), envir = app)
  shiny::testServer(app$server, {
    session$setInputs(preset = "adriatic", gamma = 1.5, season_amp = 0)
    expect_equal(rv$sys$S, 10)
    session$setInputs(hs_close_s = "none", hs_rules = c("none", "hcr_plan", "hcr_plan_pool"), hs_btrig = 0.5,
                      hs_trig_seg = FALSE, hs_bycatch = FALSE, hs_btrig_close = 1, hs_emin = 0.1, hs_maxchange = 0.2,
                      hs_lag = 2, hs_assess = 0.2, hs_impl = 0.1, hs_effort = 0.1, proc_scale = 1, proc_dep = TRUE,
                      proc_center = "median", proc_rho = 0.3, hs_nsim = 3, hs_years = 6, hs_start = "assessed",
                      hs_assess_agg = "min", run_hs = 1)
    r <- hs()
    expect_equal(names(r), c("No control", "HCR plan stocks", "HCR plan stocks, pooled units"))
    expect_equal(unname(r[[1]]$init$B[1, ] / rv$sys$K), rv$sys$dep_now, tolerance = 1e-6)
  })
})
