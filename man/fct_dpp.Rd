% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_dpp.R
\name{fct_dpp}
\alias{fct_dpp}
\title{Internal Double Penalized Projection Function}
\usage{
fct_dpp(
  y,
  x,
  rank,
  lambda = NULL,
  alpha = 2 * sqrt(3),
  beta = 1,
  sigma,
  ptype = "grLasso",
  y_sparse = TRUE
)
}
\arguments{
\item{y}{A matrix of responses.}

\item{x}{A matrix of predictors.}

\item{rank}{The rank, if known.}

\item{lambda}{A vector of penalization parameters.}

\item{alpha}{A positive constant DPP parameter.}

\item{beta}{A positive constant DPP parameter.}

\item{sigma}{An estimated standard deviation}

\item{ptype}{A group penalized regression penalty type. See \link[grpreg]{grpreg}.}

\item{y_sparse}{Should Y coefficients be treated as sparse?}
}
\value{
A list containing estimated coefficients, covariance, and penalty parameters.
}
\description{
Internal Double Penalized Projection Function
}
\keyword{internal}
