test_that("the Shiny app server computes the status quo and an equilibrium", {
  skip_if_not_installed("shiny")
  skip_on_cran()
  app <- new.env()
  src <- readLines(system.file("shiny", "mmsyFM", "app.R", package = "mmsyFM"))
  eval(parse(text = src[!grepl("^shinyApp\\(", src)]), envir = app)
  shiny::testServer(app$server, {
    session$setInputs(preset = "atlantic", capacity = 1000, gamma = 1.5, season_amp = 0)
    expect_equal(unname(round(rv$sq$E_seg)), c(549, 528, 459), tolerance = 0.02)
    session$setInputs(days_demersal = 300, days_pelagic = 500, days_mixed = 400,
                      open_m = rv$sys$metiers$name, run_eq = 1)
    expect_gt(eq()$summary$yield, 0)
    expect_type(output$eq_metier, "character")
  })
})
