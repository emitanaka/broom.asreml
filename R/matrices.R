#' Get model matrices from an asreml object
#'
#' @param model An asreml object.
#' @param which A character vector specifying which matrices to return.
#'   Options include "X", "Z", "G", "R", "V", "W", "Vinv", "Ginv", "Rinv", "P", and "data".
#' @return A list containing the requested model matrices.
#' @export
asreml_model_matrix <- function(
  model,
  which = c(
    "X",
    "Z",
    "G",
    "R",
    "V",
    "W",
    "Vinv",
    "Ginv",
    "Rinv",
    "P",
    "data"
  )
) {
  stop_if_not_asreml(model)
  default_design <- asreml::asreml.options()$design
  model$matrix <- list()
  if ("P" %in% which && !"Vinv" %in% which) {
    which <- c(which, "Vinv")
  }
  if ("Ginv" %in% which && !"G" %in% which) {
    which <- c(which, "G")
    if (!"Z" %in% which) {
      which <- c(which, "Z")
    }
  }
  if ("Rinv" %in% which && !"R" %in% which) {
    which <- c(which, "R")
  }
  if ("Vinv" %in% which && !"V" %in% which) {
    which <- c(which, "V")
  }
  if (
    any(c("X", "Z", "data", "P", "V", "G") %in% which) &&
      (is.null(model$design) || is.null(model$mf))
  ) {
    asreml::asreml.options(design = TRUE)
    model <- asreml::update.asreml(model, model.frame = TRUE, trace = FALSE)
    asreml::asreml.options(design = default_design)
  }
  if ("X" %in% which) {
    model$matrix$X <- model$design[,
      1:length(model$coefficients$fixed),
      drop = FALSE
    ]
  }
  if ("Z" %in% which) {
    model$matrix$Z <- model$design[,
      (length(model$coefficients$fixed) + 1):ncol(model$design),
      drop = FALSE
    ]
  }
  if ("data" %in% which) {
    model$matrix$data <- model$mf
  }
  if ("V" %in% which && is.null(model$matrix$V)) {
    model$matrix$V <- asremlPlus::estimateV(model, which.matrix = "V")
  }
  if ("G" %in% which && is.null(model$matrix$G)) {
    ZGZt <- asremlPlus::estimateV(model, which.matrix = "G")
    ZtZGZtZ <- t(as.matrix(model$matrix$Z)) %*% ZGZt %*% model$matrix$Z
    ZtZinv <- Matrix::solve(crossprod(model$matrix$Z))
    model$matrix$G <- ZtZinv %*% ZtZGZtZ %*% ZtZinv
  }
  if ("R" %in% which && is.null(model$matrix$R)) {
    model$matrix$R <- asremlPlus::estimateV(model, which.matrix = "R")
  }
  if ("Vinv" %in% which && is.null(model$matrix$Vinv)) {
    model$matrix$Vinv <- solve(model$matrix$V)
  }
  if ("Ginv" %in% which && is.null(model$matrix$Ginv)) {
    model$matrix$Ginv <- solve(model$matrix$G)
  }
  if ("Rinv" %in% which && is.null(model$matrix$Rinv)) {
    model$matrix$Rinv <- solve(model$matrix$R)
  }
  if ("P" %in% which && is.null(model$matrix$P)) {
    model$matrix$P <- model$matrix$Vinv -
      model$matrix$Vinv %*%
        model$matrix$X %*%
        PEV_XX(model) %*%
        t(as.matrix(model$matrix$X)) %*%
        model$matrix$Vinv
  }

  model
}

#' Prediction error variance matrix
#'
#' The prediction error variance (PEV) matrix is the variance-covariance matrix of the estimated fixed and random effects in a linear mixed model. It is used to quantify the uncertainty associated with the estimated effects.
#'
#'
#' @param model An asreml object.
#' @name PEV
#' @export
PEV_XX <- function(model) {
  if (is.null(model$Cfixed)) {
    model <- asreml_model_matrix(model, which = c("X", "Vinv"))
    select_non_zero <- which(!model$coefficients$fixed[, 1] == 0)
    X <- as.matrix(model$matrix$X[, names(select_non_zero), drop = FALSE])
    Cinv <- solve(t(X) %*% model$matrix$Vinv %*% X)
    Xnames <- rownames(model$coefficients$fixed)
    Cinv_full <- matrix(
      0,
      nrow = ncol(model$matrix$X),
      ncol = ncol(model$matrix$X)
    )
    dimnames(Cinv_full) <- list(Xnames, Xnames)
    Cinv_full[select_non_zero, select_non_zero] <- as.matrix(Cinv[
      names(select_non_zero),
      names(select_non_zero)
    ])
    Cinv_full
  } else {
    model$Cfixed
  }
}

#' @rdname PEV
#' @export
PEV_XZ <- function(model) {
  model <- asreml_model_matrix(model, which = c("G", "X", "Z", "Vinv"))
  XVXinv <- PEV_XX(model)
  -XVXinv %*%
    t(as.matrix(model$matrix$X)) %*%
    model$matrix$Vinv %*%
    model$matrix$Z %*%
    model$matrix$G
}

#' @rdname PEV
#' @export
PEV_ZZ <- function(model) {
  model <- asreml_model_matrix(model, which = c("V", "G", "X", "Z", "P"))
  G <- model$matrix$G
  Z <- model$matrix$Z
  P <- model$matrix$P
  G - G %*% t(as.matrix(Z)) %*% P %*% Z %*% G
}
