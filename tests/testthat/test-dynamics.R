sys <- atlantic_system()

test_that("profit-blind allocation does not depend on the behaviour mode", {
  caps <- matrix(c(100, 100, 100), 1)
  a <- ms_summary(ms_simulate(sys, caps, 60, gamma = 0, behaviour = "fixed", phi = 0))
  b <- ms_summary(ms_simulate(sys, caps, 60, gamma = 0, behaviour = "dynamic", phi = 0))
  expect_equal(a$yield, b$yield)
})

test_that("equilibrium matches the Schaefer solution B/K = 1 - F/r", {
  x <- ms_simulate(sys, matrix(c(20, 20, 20), 1), 400, gamma = 0, behaviour = "fixed", phi = 0)
  BK <- apply(x$B, c(2, 3), mean)[1, ] / sys$K
  Fi <- apply(x$F, c(2, 3), mean)[1, ]
  expect_lt(max(abs(BK - pmax(1 - Fi / sys$r, 0))), 0.01)
})

test_that("the preset reproduces the tuned status quo", {
  sq <- ms_status_quo(sys, 1000, gamma = 1.5, avg_years = 30)
  expect_equal(unname(round(sq$E_seg)), c(549, 528, 459), tolerance = 0.01)
  expect_equal(round(sq$summary$yield), 1745, tolerance = 0.01)
  expect_equal(unname(round(min(sq$BBmsy), 2)), 0.40, tolerance = 0.02)
})

test_that("common random numbers give identical rows", {
  n <- 4
  x <- ms_simulate(sys, matrix(300, 2 * n, 3), 20, proc_cv = 0.1, crn = n, seed = 3, keep = "all")
  expect_equal(x$B[, 1:n, ], x$B[, n + 1:n, ])
})

test_that("process-error helpers match their anchors", {
  expect_equal(proc_sd_from_r(c(0.057, 1.2)), c(0.05, 0.4))
  expect_equal(relsig_jabba(1), 1)
  expect_gt(relsig_jabba(0.25), relsig_jabba(0.5))
})

test_that("F/Fmsy is exact for the discrete Schaefer model (B/Bmsy + F/Fmsy = 2)", {
  sys <- got_system_draft()   # r from 0.05 to 1.4
  x <- ms_simulate(sys, matrix(c(450, 225, 3000), 1, 3), years = 600, F_cap = Inf,
                   cost_seg = c(0.35, 0.2, 0.015))
  s <- ms_summary(x)
  tot <- attr(s, "BBmsy")[1, ] + attr(s, "FFmsy")[1, ]
  ok <- attr(s, "BBmsy")[1, ] > 0.01                 # extinct stocks: F/Fmsy unconstrained
  expect_equal(unname(tot[ok]), rep(2, sum(ok)), tolerance = 0.01)
  expect_equal(ffmsy(-log(1 - 0.7), 1.4), 1)
})
