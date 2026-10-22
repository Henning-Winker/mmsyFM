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
