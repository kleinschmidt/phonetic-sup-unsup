% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing.R
\name{exclusions}
\alias{exclusions}
\title{Apply exclusion criteria}
\usage{
exclusions(data_)
}
\arguments{
\item{data}{- parsed data frame.}
}
\value{
A data.frame with one row per excluded assignment.  Column `rank` has
n>1 for repeat takers, and NA otherwise.  Column loMinCorrect has the minimum
log-odds correct for either endpoint of the continuum (0ms or 70ms VOT).
}
\description{
List assignments to exclude from analysis for poor performance or repeats
}

