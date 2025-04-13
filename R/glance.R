
#' @importFrom generics glance
#' @export
generics::glance


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
