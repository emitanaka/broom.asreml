test_that("bootstrap works", {
  skip_if_not_installed("asreml")
  skip()
  bootstrap_asreml(fit_chickweight, sigma, nsim = 10)
})
