% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modeling.R
\name{mer_model_matrix}
\alias{mer_model_matrix}
\title{The missing method}
\usage{
mer_model_matrix(fit, dat, ...)
}
\arguments{
\item{fit}{Fitted merModel object}

\item{dat}{New data to generate a model.matrix from. See model.matrix.}
}
\description{
Actually create a model matrix from a merMod object and new data (only
fixed effects, and leaves out the y value).
}

