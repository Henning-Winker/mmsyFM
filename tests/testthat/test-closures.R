sys <- atlantic_system()

test_that("a closure in every month equals an annual closure", {
  caps <- matrix(c(400, 400, 400), 1)
  ann <- replace(rep(1, sys$M), 1, 0)
  mon <- matrix(1, 12, sys$M); mon[, 1] <- 0
  a <- ms_summary(ms_simulate(sys, caps, 60, gamma = 1.5, open = ann))
  b <- ms_summary(ms_simulate(sys, caps, 60, gamma = 1.5, open = mon))
  expect_equal(a$yield, b$yield)
})

test_that("a seasonal closure lowers F on the closed metier's main stock", {
  caps <- matrix(c(400, 400, 400), 1)
  mon <- matrix(1, 12, sys$M); mon[2:5, sys$metiers$name == "Reef_shallow"] <- 0
  f0 <- attr(ms_summary(ms_simulate(sys, caps, 80, gamma = 1.5)), "FFmsy")[1, "Dentex dentex"]
  f1 <- attr(ms_summary(ms_simulate(sys, caps, 80, gamma = 1.5, open = mon)), "FFmsy")[1, "Dentex dentex"]
  expect_lt(f1, f0)
})

test_that("ms_closure builds a monthly open matrix and moves days to the open metiers", {
  sys <- atlantic_system()
  m <- sys$metiers$name[1]
  op <- ms_closure(sys, m, months = 2:5, share = 0.5)
  expect_equal(dim(op), c(12, sys$M))
  expect_equal(unname(op[2:5, m]), rep(0.5, 4)); expect_equal(unname(op[1, m]), 1)
  expect_true(all(op[, -1] == 1))
  expect_error(ms_closure(sys, "nope"), "unknown")
  cap <- matrix(1000, 1, sys$G)
  a <- ms_simulate(sys, cap, 150, capacity = cap, gamma = 1.5, avg_years = 20)
  b <- ms_simulate(sys, cap, 150, capacity = cap, gamma = 1.5, avg_years = 20, open = ms_closure(sys, m))
  expect_lt(mean(b$E_metier[, 1, 1]), 1e-6 + 0.01 * mean(a$E_metier[, 1, 1]))   # closed all year
})
