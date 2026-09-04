#' Identify outliers in an asreml object
#'
#' @param x An object of class \code{asreml}.
#' @param threshold The significance threshold for identifying outliers.
#' @return A logical vector indicating which observations are outliers.
#' @examples
#' \donttest{
#' is_outlier(fit_besag_met)
#' }
#' @export
is_outlier <- function(x, threshold = 0.05) {
  stopifnot(inherits(x, "asreml"))
  if (is.null(x$aom)) {
    stop(
      "The asreml object does not have an aom component. Please set aom = TRUE in the asreml() call."
    )
  }
  df <- augment(x)
  pval <- 1 - pchisq(df$.std.resid.conditional^2, 1)
  pval <- p.adjust(pval, method = "holm")
  pval < threshold
}
