#' Parametric bootstrap for an asreml model
#'
#' Simulate the data for a given asreml model
#'
#' @param model An object of class `asreml`.
#' @param nsim The number of simulations to perform.
#' @param resample The type of resampling to perform. Either "random" or "residual".
#' @export
bootstrap_data <- function(
  model,
  nsim = 1,
  resample = c("random", "residual")
) {
  stop_if_not_asreml(model)
  resample <- match.arg(resample)
  model <- asreml_model_matrix(model, which = c("data", "V", "R"))
  data <- as.data.frame(model$matrix$data)
  response <- deparse(model$formulae$fixed[[2]])
  n <- nrow(data)
  res <- list()
  for (isim in seq_len(nsim)) {
    if (resample == "residual") {
      L <- t(Matrix::chol(model$matrix$R))
      data[[response]] <- as.vector(
        model$linear.predictors + L %*% stats::rnorm(n)
      )
    } else if (resample == "random") {
      L <- t(Matrix::chol(model$matrix$V))
      data[[response]] <- as.vector(
        get_fixed_fit_asreml(model) + L %*% stats::rnorm(n)
      )
    }
    data[[".sim"]] <- isim
    res[[isim]] <- data
  }
  dplyr::bind_rows(res)
}

#' Fixed-effects-only fitted values from an asreml model
#'
#' @description
#' Returns the fitted values based on fixed effects only
#' (\eqn{\hat{y} = X\hat{\beta}}), excluding all random effects.
#'
#' @param model An object of class \code{"asreml"}, fitted with
#' \code{model.frame = TRUE}.
#'
#' @return
#' A numeric vector of length \eqn{N}, giving the fixed-effects-only
#' fitted value for each observation.
#'
#' @details
#' This function reconstructs the fixed-effect design matrix from the
#' stored model frame and multiplies it by the estimated fixed
#' coefficients. Sparse fixed terms (if any) are included.
#'
#' Random effects (BLUPs) are not included.
#'
#' @keywords internal
get_fixed_fit_asreml <- function(model) {
  stop_if_not_asreml(model)

  # Get model frame
  model <- asreml_model_matrix(model, which = c("X", "Z", "data"))

  # Fixed and sparse term labels
  fixed_terms <- attr(model$formulae$fixed, "term.labels")
  sparse_terms <- attr(model$formulae$sparse, "term.labels")

  # Get estimates
  term_names <- c(
    rownames(model$coefficients$fixed),
    rownames(model$coefficients$sparse)
  )
  b <- c(
    model$coefficients$fixed,
    model$coefficients$sparse
  )

  as.numeric(model$design[, term_names, drop = FALSE] %*% b)
}
