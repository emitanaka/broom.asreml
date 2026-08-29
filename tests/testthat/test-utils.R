test_that("adding model matrices works", {
  skip_if_not_installed("asreml")
  fit_chickweight_add <- asreml_model_matrix(fit_chickweight)
})


test_that("diag_ABC works", {
  A <- matrix(1:6, nrow = 2)
  B <- matrix(1:9, nrow = 3)
  C <- matrix(1:6, nrow = 3)
  expect_equal(
    diag(A %*% B %*% C),
    diag_ABC(A, B, C)
  )
})
