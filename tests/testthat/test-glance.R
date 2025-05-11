test_that("glance works", {
  expect_equal(glance(fit_besag_met), readRDS("glance_besag_met.rds"))
})
