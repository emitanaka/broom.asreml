test_that("augment works", {
  skip_if_not_installed("asreml")
  skip_on_cran()
  library(asreml)
  expect_equal(
    augment(fit_besag_met),
    readRDS(test_path("fixtures", "augment_besag_met.rds"))
  )
})
