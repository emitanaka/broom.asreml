
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


asreml_converge <- function(..., .iter_max = 10) {
  fit <- asreml(...)
  iter <- 1
  while(!fit$converge && iter < .iter_max ) {
    fit <- update(fit)
  }
  if(!fit$converge) cli::cli_alert_danger("The model didn't converge!")
  if(fit$converge) cli::cli_alert_success("The model converged.")
  fit
}

#' Extract the model frame
#'
#' @param x An asreml object
#'
#' @export
model.frame.asreml <- function(x, ...) {
  tibble::as_tibble(x$mf)
}
