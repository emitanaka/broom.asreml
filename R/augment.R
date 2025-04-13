#' @importFrom generics augment
#' @export
generics::augment


#' @export
augment.asreml <- function(x, ...) {
  res <- tibble::as_tibble(x$mf)
  res$.fitted <- x$linear.predictors
  res$.resid <- x$residuals[, 1]
  res$.hat <- x$hat
  if(!is.null(x$aom)) {
    res$.std.cond.resid <- x$aom$R[, 2, drop = TRUE]
  }
  res
}
