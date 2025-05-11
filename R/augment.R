#' @importFrom generics augment
#' @export
generics::augment


#' Augment data with information from an asreml object
#'
#' If the fitted model includes `aom = TRUE` then also computes the
#' studentised conditional residual.
#'
#' @param x An asreml object.
#' @param data The original data used to produce `x`. Do not pass new data here.
#' @param newdata A new data containing all the variables in the original data except the response
#'       variables.
#' @param se_fit Whether to get the .se.fit column or not.
#' @param interval The type of confidence interval.
#' @param conf.level The confidence level to use for the interval created.
#' @param ... Does nothing yet.
#'
#' @export
augment.asreml <- function(x,
                           data = model.frame(x),
                           newdata = NULL,
                           se_fit = FALSE,
                           interval = c("none", "confidence", "prediction"),
                           conf.level = 0.95,
                           ...) {
  if(is.null(newdata)) {
    res <- data
    res$.fitted <- x$linear.predictors
    # TODO
    # res$.fitted.marginal
    # res$.fitted.conditional
    # res$.resid.marginal
    # res$.resid.conditional
    res$.resid <- x$residuals[, 1]
    res$.hat <- x$hat
    if(!is.null(x$aom)) {
      res$.std.resid.conditional <- x$aom$R[, 2, drop = TRUE]
    }
  } else {
    cli::cli_alert_danger("Not yet implemented")
  }
  res
}


update_fix <- function (x, data, ...) {
  if (is.null(newcall <- x$call) && is.null(newcall <- attr(x, "call")))
    stop("need an object with call component or attribute")
  tempcall <- list(...)
  if (!is.null(tempcall$step.size)) {
    newcall$step.size <- tempcall$step.size
    tempcall$step.size <- NULL
  }
  else if (asreml.options()$update.step.size != 0.316) {
    newcall$step.size <- asreml.options()$update.step.size
  }
  if (length(tempcall)) {
    what <- !is.na(match(names(tempcall), names(newcall)))
    for (z in names(tempcall)[what]) newcall[[z]] <- tempcall[[z]]
    if (any(!what)) {
      newcall <- c(as.list(newcall), tempcall[!what])
      newcall <- as.call(newcall)
    }
  }
  con_fix <- function(sv) {
    lapply(sv, function(obj) {
      lapply(obj, function(ele) {
        ele$con <- rep("F", length(ele$con))
        ele
      })
    })
  }

  newcall$R.param <- con_fix(x$R.param)
  newcall$G.param <- con_fix(x$G.param)
  newcall$data <- data

  # make con all fixed
  eval(newcall, sys.parent())
}


