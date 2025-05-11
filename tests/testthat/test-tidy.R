test_that("tidy works", {
  expect_equal(tidy(fit_besag_met, "all"), readRDS("tidy_besag_met_all.rds"))
  expect_equal(tidy(fit_besag_met, "fixed"), readRDS("tidy_besag_met_fixed.rds"))
  expect_equal(tidy(fit_besag_met, "random"), readRDS("tidy_besag_met_random.rds"))
  expect_equal(tidy(fit_besag_met, "vcomp"), readRDS("tidy_besag_met_vcomp.rds"))
  expect_equal(tidy(fit_besag_met, "varcomp"), readRDS("tidy_besag_met_vcomp.rds"))
})
