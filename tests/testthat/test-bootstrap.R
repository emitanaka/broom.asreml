test_that("bootstrap works", {
  skip_if_not_installed("asreml")
  skip_on_cran()
  library(asreml)
  bootstrap_data(fit_chickweight, nsim = 2)
})
