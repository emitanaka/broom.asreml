test_that("glance works", {
  skip()
  expect_equal(
    glance(fit_besag_met),
    readRDS(test_path("fixtures", "glance_besag_met.rds"))
  )
})
