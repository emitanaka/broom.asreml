test_that("tidy works", {
  skip()
  expect_equal(
    tidy(fit_besag_met, "all"),
    readRDS(test_path("fixtures", "tidy_besag_met_all.rds"))
  )
  expect_equal(
    tidy(fit_besag_met, "fixed"),
    readRDS(test_path("fixtures", "tidy_besag_met_fixed.rds"))
  )
  expect_equal(
    tidy(fit_besag_met, "random"),
    readRDS(test_path("fixtures", "tidy_besag_met_random.rds"))
  )
  expect_equal(
    tidy(fit_besag_met, "vcomp"),
    readRDS(test_path("fixtures", "tidy_besag_met_vcomp.rds"))
  )
  expect_equal(
    tidy(fit_besag_met, "varcomp"),
    readRDS(test_path("fixtures", "tidy_besag_met_vcomp.rds"))
  )
})
