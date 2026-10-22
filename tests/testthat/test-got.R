test_that("Gulf of Thailand draft reproduces its tuned status quo", {
  sys <- got_system_draft()
  expect_equal(c(sys$S, sys$M, sys$G), c(20, 10, 3))
  # coastal zoning: no segment fishes outside its home metiers
  expect_true(all(sys$loyalty == 1))
  sq <- ms_status_quo(sys, capacity = c(450, 225, 3000), gamma = 1.5, years = 300)
  sh <- sq$E_seg / sum(sq$E_seg)
  expect_equal(unname(round(sh, 2)), c(0.12, 0.06, 0.82), tolerance = 0.015)
  expect_equal(stats::median(sq$BBmsy), 0.5, tolerance = 0.1)
})
