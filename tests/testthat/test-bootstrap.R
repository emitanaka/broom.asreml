test_that("bootstrap works", {
  bootstrap_asreml(fit_chickweight, sigma, nsim = 10)
})
