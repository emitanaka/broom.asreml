stop_if_not_asreml <- function(x) {
  if (!inherits(x, "asreml")) {
    stop("`model` must be an `asreml` object.")
  }
}
