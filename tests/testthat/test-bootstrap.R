test_that("bootstrap works", {
  skip_if_not_installed("asreml")
  bootstrap_data(fit_chickweight, nsim = 2)
})
