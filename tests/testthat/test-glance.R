test_that("glance works", {
  skip_if_not_installed("asreml")
  expect_equal(
    glance(fit_besag_met),
    readRDS(test_path("fixtures", "glance_besag_met.rds"))
  )
})
