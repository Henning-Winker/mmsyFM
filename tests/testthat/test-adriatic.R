test_that("Adriatic draft loads with plan stocks, units and fleets", {
  sys <- adriatic_system_draft()
  expect_equal(sys$S, 10); expect_equal(sys$G, 5)
  p <- ms_assessment_presets(sys)
  expect_setequal(p$plan, sys$species[sys$code %in% c("HKE", "MUT", "SOL", "DPS", "NEP17C", "NEP17NW", "NEP17NE", "NEP18")])
  expect_equal(names(p$plan_units), "NEP")
  expect_length(p$plan_units$NEP, 4)
  expect_setequal(p$plan_segments, c("OTB", "TBB_sole"))
  expect_true(all(!is.na(sys$dep_now)))
})

test_that("Adriatic status quo reproduces the assessed F/Fmsy", {
  skip_on_cran()
  sys <- adriatic_system_draft()
  sp <- utils::read.csv(system.file("extdata", "adriatic_species.csv", package = "mmsyFM"))
  sq <- ms_status_quo(sys, capacity = sys$capacity, gamma = 1.5, years = 300)
  expect_lt(max(abs(sq$FFmsy - sp$FFmsy_now)), 0.06)
  B0 <- ms_start(sys, sq)
  expect_equal(unname(B0 / sys$K), sys$dep_now)
})

test_that("plan rule regulates only the plan fleets", {
  sys <- adriatic_system_draft()
  p <- ms_assessment_presets(sys)
  r <- hcr_effort(sys, assessed = c(setdiff(p$plan, p$plan_units$NEP), "NEP"), complexes = p$plan_units,
                  segments = p$plan_segments)
  own <- attr(r, "own")
  g <- match(c("gillnet", "LLS_18deep", "LLS_17E"), names(sys$segments))
  expect_true(all(!own[, g]))
  expect_true(any(own[, match("OTB", names(sys$segments))]))
  expect_true("NEP" %in% names(attr(r, "units")))
})
