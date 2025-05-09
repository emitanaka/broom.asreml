% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy.R
\name{tidy.asreml}
\alias{tidy.asreml}
\title{Tidy an asreml object}
\usage{
\method{tidy}{asreml}(x, type = c("all", "fixed", "random", "vcomp", "varcomp", "wald"), ...)
}
\arguments{
\item{x}{An asreml object.}

\item{type}{The type of summary to get.}

\item{...}{Extra arguments parsed into \code{asreml::wald} function.}
}
\description{
Get the model components.
}
