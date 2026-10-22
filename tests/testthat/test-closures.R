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
