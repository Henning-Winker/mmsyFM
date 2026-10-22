sys <- atlantic_system()

test_that("the effort rule returns multipliers and closures of the right shape", {
  r <- hcr_effort(sys, close = "Dentex dentex", assess_cv = 0)
  st <- list(B = matrix(sys$K * 0.3, 2, sys$S, byrow = TRUE), K = matrix(sys$K, 2, sys$S, byrow = TRUE), year = 1)
  out <- r(st)
  expect_equal(dim(out$mult), c(2, sys$G))
  expect_equal(dim(out$open), c(2, sys$M))
  expect_true(all(out$mult >= 0 & out$mult <= 1))
  expect_error(hcr_effort(sys, close = "nope"), "close")
  expect_error(hcr_effort(sys, close = "Dentex dentex", assessed = "Pagellus bogaraveo"), "assessed")
})

test_that("complexes define pooled assessment units", {
  r <- hcr_effort(sys, complexes = list(reef = c("Dentex dentex", "Conger conger")))
  expect_equal(length(attr(r, "units")), sys$S - 1)
})

test_that("a lag of 0 and 2 years both run, and ms_tune_hcr returns a table", {
  sq <- ms_status_quo(sys, 1000, gamma = 1.5, years = 150)
  tu <- ms_tune_hcr(sys, triggers = c(0.6, 1), close = "Dentex dentex", close_triggers = 3,
                    E_ref = sq$E_seg, B_init = sq$B, nsim = 10, years = 20, eval_years = 11:20,
                    gamma = 1.5, capacity = 1000, proc_cv = 0.1, hcr_lag = 2)
  expect_equal(nrow(tu$table), 2)
  expect_true(all(c("yield", "worst_P_below_Bpa") %in% names(tu$table)))
})

test_that("minimum effort floor and phased change limit", {
  sys <- got_system_draft(); cap <- c(450, 225, 3000)
  sq <- ms_status_quo(sys, capacity = cap, gamma = 1.5, years = 150)
  E0 <- matrix(sq$E_seg, 1, 3)
  run <- function(...) ms_simulate(sys, E0, 30, B_init = matrix(sq$B, 1), E_init = E0, capacity = cap,
                                   gamma = 1.5, keep = "all", phi = 0, hcr = hcr_effort(sys, Btrig = 0.6, ...))
  x <- run(Emin = 0.1, max_change = 0.2)
  frac <- sweep(x$E_seg[, 1, ], 2, sq$E_seg, "/")
  expect_gt(min(frac), 0.1 - 1e-6)                    # never below the floor
  expect_gt(min(frac[1, ]), 0.8 - 1e-6)               # year 1: at most 20% below the status quo
  ch <- frac[-1, ] / frac[-nrow(frac), ]
  expect_lt(max(abs(ch - 1)), 0.2 + 1e-6)             # at most 20% change on last year
  z <- run(Emin = 0, max_change = Inf)
  expect_lt(min(z$E_seg[, 1, ]), 1e-6)                # without a floor the rule can close a segment
})
