
#' @importFrom generics glance
#' @export
generics::glance

#' Glance at an asreml object
#'
#' Provides a single row of model summaries
#'
#' @param x An asreml object.
#' @param ... Currently does nothing.
#'
#' @export
glance.asreml <- function(x, ...) {
  k <- x$nwv + nrow(x$coefficients$fixed)
  n <- nrow(x$mf)
  tibble::tibble(sigma = sqrt(x$sigma2),
                 logLik = x$loglik,
                 deviance = x$deviance,
                 df.residual = x$nedf,
                 AIC = -2 * x$loglik + 2 * k,
                 BIC = -2 * x$loglik + k * log(n),
                 nobs = n)
}
