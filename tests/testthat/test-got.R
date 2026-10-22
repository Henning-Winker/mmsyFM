test_that("Gulf of Thailand draft reproduces its tuned status quo", {
  sys <- got_system_draft()
  expect_equal(c(sys$S, sys$M, sys$G), c(20, 10, 3))
  expect_equal(c(sys$S, sys$M, sys$G), c(20, 10, 3))
  # coastal zoning: no segment fishes outside its home metiers
  expect_true(all(sys$loyalty == 1))
  sq <- ms_status_quo(sys, capacity = c(450, 225, 3000), gamma = 1.5, years = 300)
  sh <- sq$E_seg / sum(sq$E_seg)
  expect_equal(unname(round(sh, 2)), c(0.12, 0.06, 0.82), tolerance = 0.015)
  expect_equal(stats::median(sq$BBmsy), 0.57, tolerance = 0.03)
  expect_gt(min(sq$BBmsy), 0.05)
})

test_that("F_cap keeps stocks at or above the Schaefer floor", {
  sys <- got_system_draft()
  heavy <- matrix(c(450, 225, 3000) * 5, 1, 3)
  cap <- ms_simulate(sys, heavy, years = 300, cost_seg = rep(0, 3))
  nocap <- ms_simulate(sys, heavy, years = 300, cost_seg = rep(0, 3), F_cap = Inf)
  b <- function(x) attr(ms_summary(x), "BBmsy")[1, ]
  expect_true(all(b(cap) > 0.045))
  expect_true(min(b(nocap)) < 0.01)
})

test_that("smooth cap: close to the hard cap, used by default with process error", {
  sys <- got_system_draft()
  E <- matrix(c(450, 225, 3000), 1, 3)
  hard <- ms_status_quo(sys, capacity = c(450, 225, 3000), gamma = 1.5, years = 300)
  soft <- ms_simulate(sys, E, years = 300, gamma = 1.5, F_cap_p = 8)
  expect_equal(stats::median(attr(ms_summary(soft), "BBmsy")[1, ]), stats::median(hard$BBmsy), tolerance = 0.1)
  sto <- ms_simulate(sys, E[rep(1, 4), ], years = 60, gamma = 1.5, proc_dep = "jabba",
                     proc_center = "median", seed = 1)
  expect_true(all(is.finite(sto$B)) && all(sto$B > 0))
})
