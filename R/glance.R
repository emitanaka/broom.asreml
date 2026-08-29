#' Glance at an asreml object
#'
#' Provides a single row of model summaries
#'
#' @param x An asreml object.
#' @param ... Currently does nothing.
#' @importFrom generics glance
#' @examples
#' \dontrun{
#' glance(fit_besag_met)
#' }
#'
#' @return A tibble with the following columns:
#'
#' - `sigma`: the residual standard error
#' - `logLik`: the residual log likelihood
#' - `deviance`: the model deviance
#' - `df.residual`: the residual degrees of freedom
#' - `AIC`: the Akaike information criterion
#' - `BIC`: the Bayesian information criterion
#' - `nobs`: the number of observations
#' @export
glance.asreml <- function(x, ...) {
  k <- x$nwv + nrow(x$coefficients$fixed)
  n <- nrow(x$mf)
  tibble::tibble(
    sigma = sqrt(x$sigma2),
    logLik = x$loglik,
    deviance = x$deviance,
    df.residual = x$nedf,
    AIC = -2 * x$loglik + 2 * k,
    BIC = -2 * x$loglik + k * log(n),
    nobs = n
  )
}
