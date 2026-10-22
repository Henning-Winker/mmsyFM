test_that("ms_landscape: uniform stock gives q0 x selectivity; concentration raises q", {
  cells <- expand.grid(depth = seq(2.5, 77.5, 5), substrate = c("mud", "sand"))
  cells$area <- 1
  sp <- data.frame(species = c("even", "shallow"), depth_mean = c(40, 10), depth_sd = c(1e4, 5))
  mt <- data.frame(metier = c("inner", "outer"), depth_min = c(0, 40), depth_max = c(40, 80))
  sel <- matrix(c(1, 0.5, 1, 1), 2, 2, dimnames = list(mt$metier, sp$species))
  Q <- ms_landscape(cells, sp, mt, sel, q0 = 1e-3)
  expect_equal(unname(Q[, "even"]), c(1e-3, 0.5e-3), tolerance = 1e-3)
  expect_gt(Q["inner", "shallow"], 1.9e-3)                 # almost all of it on half the area
  expect_lt(Q["outer", "shallow"], 1e-6)
  expect_equal(unname(colSums(attr(Q, "distribution"))), c(1, 1))
})

test_that("the Gulf landscape tables build a valid system", {
  ext <- function(f) utils::read.csv(system.file("extdata", f, package = "mmsyFM"), check.names = FALSE)
  Q <- ms_landscape(ext("got_cells.csv"), ext("got_habitat.csv"), ext("got_grounds.csv"), ext("got_selectivity.csv"))
  expect_equal(dim(Q), c(10, 20))
  expect_true(all(apply(Q, 2, max) > 0))                   # every stock is caught somewhere
  sys <- ms_system(ext("got_species.csv"), ext("got_metiers.csv")[, 1:3], Q, ext("got_segments.csv"))
  expect_s3_class(sys, "ms_system")
})
