


#' @importFrom generics tidy
#' @export
generics::tidy


#' @export
tidy.asreml <- function(x, type = c("all", "fixed", "random", "vcomp", "wald"), ...) {
  type <- match.arg(type)
  switch(type,
         "all" = {
           f <- tidy(x, type = "fixed")
           r <- tidy(x, type = "random")
           v <- tidy(x, type = "vcomp")
           dplyr::bind_rows(list(fixed = f, random = r, vcomp = v), .id = "type")
         },
         "fixed" = {
          fr <- coef(x)$fixed
          rw <- rownames(fr)
          rownames(fr) <- NULL
          tibble::tibble(term = rw, estimate = fr[, "effect", drop = TRUE])
         },
         "random" = {
           rr <- coef(x)$random
           rw <- rownames(rr)
           rownames(rr) <- NULL
           tibble::tibble(term = rw, estimate = rr[, "effect", drop = TRUE])
         },
         "vcomp" = {
           vr <- summary(x)$varcomp
           rw <- rownames(vr)
           rownames(vr) <- NULL
           tibble::tibble(term = rw,
                          estimate = vr[, "component", drop = TRUE],
                          std.error = vr[, "std.error", drop = TRUE],
                          statistic = vr[, "z.ratio", drop = TRUE],
                          constraint = vr[, "bound", drop = TRUE])

         },
         "wald" = {
           res <- wald(x, ...)
           rw <- rownames(res)
           rownames(res) <- NULL
           tibble::tibble(term = rw,
                          df = res[, "Df", drop = TRUE],
                          sumsq = res[, "Sum of Sq", drop = TRUE],
                          statistic = res[, "Wald statistic", drop = TRUE],
                          p.value = res[, "Pr(Chisq)", drop = TRUE])

         })
}


