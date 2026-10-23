test_that("ms_perf returns mseviz-format statistics, and the plots build", {
  sys <- atlantic_system()
  sq <- ms_status_quo(sys, capacity = 1000, gamma = 1.5, years = 150)
  n <- 6; B0 <- matrix(sq$B, n, sys$S, byrow = TRUE); E0 <- matrix(sq$E_seg, n, sys$G, byrow = TRUE)
  run <- function(caps, hcr = NULL) ms_simulate(sys, caps, 12, B_init = B0, E_init = E0, hcr = hcr, gamma = 1.5,
                                               capacity = 1000, proc_sd = proc_sd_from_r(sys$r), seed = 1, keep = "all")
  runs <- list("No control" = run(matrix(1000, n, sys$G)), "HCR" = run(E0, hcr_effort(sys)))
  pf <- ms_perf(runs, years = 3:12)
  expect_s3_class(pf, "data.table")
  expect_true(all(c("mp", "biol", "statistic", "name", "desc", "year", "iter", "data") %in% names(pf)))
  expect_equal(nrow(pf), 2 * n * (7 * sys$S + sys$G + 10))
  pr <- pf[pf$type == "prob", ]
  expect_true(all(pr$data >= 0 & pr$data <= 1))
  expect_equal(levels(pf$mp), names(runs))
  tab <- ms_perf_table(pf, c("C", "PanyBlim"))
  expect_equal(dim(tab), c(2, 3))
  for (p in list(plot_bps(pf, c("C", "PanyBlim")), plot_tradeoffs(pf),
                 plot_quilt(pf, c("C", "Days", "NBpa")),
                 plot_quilt(pf, layout = "stocks", statistic = "PBpa", risk = 0.1),
                 plot_bps(pf, c("BBmsy", "PBlim"), biol = sys$code[1:2])))
    expect_silent(ggplot2::ggplot_build(p))
})
