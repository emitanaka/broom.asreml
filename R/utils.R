stop_if_not_asreml <- function(x) {
  if (!inherits(x, "asreml")) {
    stop("`model` must be an `asreml` object.")
  }
}


# this is equivalent to diag(A %*% B %*% C) but faster
diag_ABC <- function(A, B, C) {
  rowSums(as.matrix((A %*% B) * t(as.matrix(C))))
  #rowSums(A * t(B %*% C))
}
