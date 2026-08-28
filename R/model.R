
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
    object <- suppressWarnings(update(object, trace = trace, stepsize = step))
    iter <- iter + 1
    if(iter > iter_max) {
      cli::cli_alert_danger("Model did not converge after ", iter_max, " iterations.")
      break
    }
  }
  if(object$conv) {
    cli::cli_alert_success("The model converged.")
  }
  object
}

# Do not use below as asreml call not allowed for CRAN
#
# Fit the asreml model until it converges
#
# @param ... All the arguments for asreml::asreml
# @param .iter_max The maximum number of iterations to perform.
#
# @export
# asreml_converge <- function(..., .iter_max = 20, .step = 0.0001, .trace = TRUE) {
#   fit <- asreml::asreml(...)
#   converge_asreml(fit, iter_max = .iter_max, step = .step, trace = .trace)
# }

#' Extract the model frame
#'
#' @param x An asreml object
#'
#' @export
model.frame.asreml <- function(x, ...) {
  tibble::as_tibble(x$mf)
}




