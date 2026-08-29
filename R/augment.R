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
#' @return
#' Returns a tibble object with each row corresponding to one observation and columns as below. The first 4 columns adopt the convention from broom.mixed and broom packages.
#'
#' - `.fitted`: the linear predictor \eqn{\mathbf{X}\hat{\boldsymbol{\beta}} + \mathbf  {Z} \tilde{\boldsymbol{u}}}
#' - `.resid`: the residuals \eqn{\boldsymbol{y} - \mathbf{X}\hat{\boldsymbol{\beta}} - \mathbf{Z} \tilde{\boldsymbol{u}}}
#' - `.hat`: the hat values
#' - `.fixed`: the predicted value for fixed effect only \eqn{\mathbf{X}\hat{\boldsymbol{\beta}}}
#'
#' The columns with marginal and conditional suffixes are added to distinguish the two types of fitted values and residuals.
#'
#' - `.fitted.marginal`: the predicted value for marginal model \eqn{\mathbf{X}\hat{\boldsymbol{\beta}}}
#' - `.fitted.conditional`: the predicted value for conditional model \eqn{\mathbf{X}\hat{\boldsymbol{\beta}} + \mathbf  {Z} \tilde{\boldsymbol{u}}}
#' - `.resid.marginal`: the residuals for marginal model \eqn{\boldsymbol{y} - \mathbf{X}\hat{\boldsymbol{\beta}}}
#' - `.resid.conditional`: the residuals for conditional model \eqn{\boldsymbol{y} - \mathbf{X}\hat{\boldsymbol{\beta}} - \mathbf{Z} \tilde{\boldsymbol{u}}}
#' - `.std.resid.conditional`: the studentised conditional residual
#'
#' @importFrom generics augment
#' @export
augment.asreml <- function(
  x,
  data = model.frame(x),
  newdata = NULL,
  se_fit = FALSE,
  interval = c("none", "confidence", "prediction"),
  conf_level = 0.95,
  ...
) {
  if (!is.null(x$call$family)) {
    cli::cli_alert_danger(
      "`augment.asreml` may not return correct results for models with a family."
    )
  }
  interval <- match.arg(interval)

  if (is.null(newdata)) {
    res <- data
    res$.fitted <- x$linear.predictors
    res$.resid <- x$residuals[, 1]
    res$.hat <- x$hat
    res$.fixed <- get_fixed_fit_asreml(x)

    res$.fitted.marginal <- res$.fixed
    res$.fitted.conditional <- res$.fitted
    res$.resid.marginal <- res$.fitted + res$.resid - res$.fixed
    res$.resid.conditional <- res$.resid
    if (!is.null(x$aom)) {
      res$.std.resid.conditional <- x$aom$R[, 2, drop = TRUE]
    }
  } else {
    cli::cli_alert_danger("Not yet implemented")
  }
  if (se_fit | !"none" %in% interval) {
    x <- asreml_model_matrix(x)
    CinvXX <- PEV_XX(x)
    CinvXZ <- PEV_XZ(x)
    CinvZZ <- PEV_ZZ(x)
    diag_DCinvD <- diag_ABC(x$matrix$X, CinvXX, t(as.matrix(x$matrix$X))) +
      diag_ABC(x$matrix$Z, CinvZZ, t(as.matrix(x$matrix$Z))) +
      2 * diag_ABC(x$matrix$X, CinvXZ, t(as.matrix(x$matrix$Z)))
    se_fit_vals <- sqrt(diag_DCinvD)
  }

  if (se_fit) {
    res$.se.fit <- se_fit_vals
  }

  if (interval == "confidence") {
    res$.lower <- res$.fitted - qnorm(1 - (1 - conf_level) / 2) * se_fit_vals
    res$.upper <- res$.fitted + qnorm(1 - (1 - conf_level) / 2) * se_fit_vals
  }

  if (interval == "prediction") {
    res$.lower <- res$.fitted -
      qnorm(1 - (1 - conf_level) / 2) * sqrt(se_fit_vals^2 + x$sigma2)
    res$.upper <- res$.fitted +
      qnorm(1 - (1 - conf_level) / 2) * sqrt(se_fit_vals^2 + x$sigma2)
  }

  res
}

## this doesn't work as expected
# update_fix <- function(x, data, ...) {
#   if (is.null(newcall <- x$call) && is.null(newcall <- attr(x, "call"))) {
#     stop("need an object with call component or attribute")
#   }
#   tempcall <- list(...)
#   if (!is.null(tempcall$step.size)) {
#     newcall$step.size <- tempcall$step.size
#     tempcall$step.size <- NULL
#   } else if (asreml::asreml.options()$update.step.size != 0.316) {
#     newcall$step.size <- asreml::asreml.options()$update.step.size
#   }
#   if (length(tempcall)) {
#     what <- !is.na(match(names(tempcall), names(newcall)))
#     for (z in names(tempcall)[what]) {
#       newcall[[z]] <- tempcall[[z]]
#     }
#     if (any(!what)) {
#       newcall <- c(as.list(newcall), tempcall[!what])
#       newcall <- as.call(newcall)
#     }
#   }
#   con_fix <- function(sv) {
#     lapply(sv, function(obj) {
#       lapply(obj, function(ele) {
#         ele$con <- rep("F", length(ele$con))
#         ele
#       })
#     })
#   }

#   newcall$R.param <- con_fix(x$R.param)
#   newcall$G.param <- con_fix(x$G.param)
#   newcall$data <- data
#   newcall$trace <- FALSE

#   # make con all fixed
#   eval(newcall, sys.parent())
# }
