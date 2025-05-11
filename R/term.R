
map_fixed_terms <- function(x, term) {
  res <- tidy(x, type = "fixed")
  res <- res[grepl(paste0("^", term), res$term), ]
  nm <- gsub(paste0("^", term, "_?"), "", res$term)
  setNames(res$estimate, nm)
}

has_interaction <- function(x) {
  grepl(":", x)
}

map_id_terms <- function(x, term, type = c("random", "vcomp")) {
  if(length(term) != 1) cli::cli_abort("The `term` should only be of size 1.")
  type <- match.arg(type)
  res <- tidy(x, type = type)
  if(has_interaction(term)) {
    fcts <- strsplit(term, ":")[[1]]
    res <- res[grepl(paste0(fcts, collapse = "[^:]*:"), res$term), ]
    nm <- res$term
    for(afct in fcts) {
      nm <- gsub(paste0(afct, "_?"), "", nm)
    }
  } else {
    res <- res[grepl(paste0("^", term), res$term) & !has_interaction(term), ]
    nm <- gsub(paste0("^", term, "_?"), "", res$term)
  }
  setNames(res$estimate, nm)
}

map_fa_terms <- function(x, term, type = c("random", "vcomp")) {
  type <- match.arg(type)
  res <- tidy(x, type = type)
  x$coefficients$random

  tidy(x, "random")
  tidy(x, "vcomp")

}

#tidy(fit_besag_met, "random")
#map_terms(fit_besag_met, "env", "random")
