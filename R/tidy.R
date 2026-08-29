#' Tidy an asreml object
#'
#' Get the model components.
#'
#' @param x An asreml object.
#' @param type The type of summary to get.
#' @param ... Extra arguments parsed into `asreml::wald` function.
#'
#' @return Returns a tibble with the following columns
#'
#' - type: whether it is the fixed effect, random effect, or variance component
#' - term: the name of the term
#' - group: the group name
#' - level: the level if it is a factor
#' - estimate: E-BLUE for fixed effects, E-BLUP for the random effecs and REML estimates for the variance components
#' - std.error: the standard error of the estimate
#' - statistic: the estimate divide it by standard error
#' - constraint: the constraint used for the variance component
#'
#' @examples
#' tidy(fit_besag_met, "fixed")
#' tidy(fit_besag_met, "random")
#' tidy(fit_besag_met, "vcomp")
#'
#'
#' @importFrom generics tidy
#' @export
tidy.asreml <- function(
  x,
  type = c("all", "fixed", "random", "vcomp", "varcomp", "wald"),
  ...
) {
  type <- match.arg(type)
  switch(
    type,
    "all" = {
      f <- tidy(x, type = "fixed")
      r <- tidy(x, type = "random")
      v <- tidy(x, type = "vcomp")
      dplyr::bind_rows(list(fixed = f, random = r, vcomp = v), .id = "type")
    },
    "fixed" = {
      fr <- x$coefficients$fixed
      sr <- x$coefficients$sparse
      fcc <- clean_asreml_coef(fr)
      scc <- clean_asreml_coef(sr)
      res <- tibble::tibble(
        term = c(fcc$term, scc$term),
        group = c(fcc$group, scc$group),
        level = c(fcc$level, scc$level),
        estimate = c(fcc$coef, scc$coef),
        std.error = c(
          sqrt(x$vcoeff$fixed * x$sigma2),
          sqrt(x$vcoeff$sparse * x$sigma2)
        )
      )
      res$statistic <- res$estimate / res$std.error
      res
    },
    "random" = {
      rr <- x$coefficients$random
      cc <- clean_asreml_coef(rr)
      res <- tibble::tibble(
        term = cc$term,
        group = cc$group,
        level = cc$level,
        estimate = cc$coef,
        std.error = sqrt(x$vcoeff$random * x$sigma2)
      )
      res$statistic <- res$estimate / res$std.error
      res
    },
    "vcomp" = {
      vr <- summary(x)$varcomp
      rw <- rownames(vr)
      rownames(vr) <- NULL
      tibble::tibble(
        term = rw,
        estimate = vr[, "component", drop = TRUE],
        std.error = vr[, "std.error", drop = TRUE],
        statistic = vr[, "z.ratio", drop = TRUE],
        constraint = vr[, "bound", drop = TRUE]
      )
    },
    "varcomp" = tidy(x, "vcomp"),
    "wald" = {
      res <- asreml::wald.asreml(x, ...)
      rw <- rownames(res)
      rownames(res) <- NULL
      tibble::tibble(
        term = rw,
        df = res[, "Df", drop = TRUE],
        sumsq = res[, "Sum of Sq", drop = TRUE],
        statistic = res[, "Wald statistic", drop = TRUE],
        p.value = res[, "Pr(Chisq)", drop = TRUE]
      )
    }
  )
}


clean_asreml_coef <- function(coef) {
  if (is.null(coef)) {
    return(NULL)
  }
  tms <- attr(coef, "terms") # assume that terms appears in this order
  rw <- rownames(coef)
  group <- rw
  i <- 1
  level <- rep(NA, length(rw))
  for (igrp in 1:nrow(tms)) {
    nm <- tms$tname[igrp]
    if (has_interaction(nm)) {
      nms <- strsplit(nm, ":")[[1]]
    } else {
      nms <- nm
    }
    ntrms <- length(nms)
    ngrp <- tms$n[igrp]
    index <- seq(i, i + ngrp - 1)
    # is it a group?
    pattern <- paste0("^", gsub("\\)", "\\\\)", gsub("\\(", "\\\\(", nms)), "_")
    levels <- strsplit(rw[index], ":")
    group[index] <- nm
    clevels <- lapply(1:ntrms, function(j) {
      jlevels <- sapply(levels, function(x) x[j])
      if (all(grepl(pattern[j], jlevels))) {
        gsub(pattern[j], "", jlevels)
      } else {
        rep("", length(index))
      }
    })
    level[index] <- sapply(1:length(clevels[[1]]), function(k) {
      paste0(sapply(clevels, function(a) a[k]), collapse = ":")
    })
    i <- i + ngrp
  }
  rownames(coef) <- NULL
  level[level == ""] <- NA
  list(
    coef = coef[, "effect", drop = TRUE],
    term = rw,
    group = group,
    level = level
  )
}
