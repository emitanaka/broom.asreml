
#' Update the model until it converges
#'
#' This function updates the model until it converges or reaches the maximum number of iterations.
#'
#' @param object An `asreml` object.
#' @param iter_max The maximum number of iterations to perform.
#' @param step The step size for the update.
#' @param trace Whehther to show the model update information.
#' @return An updated `asreml` object.
#' @export
converge_asreml <- function(object, iter_max = 20, step = 0.0001, trace = TRUE) {
  iter <- 1
  while(!object$conv) {
    object <- asreml::update.asreml(object, trace = trace, stepsize = step)
    iter <- iter + 1
    if(iter > iter_max) {
      warning("Model did not converge after ", iter_max, " iterations.")
      break
    }
  }
  object
}
