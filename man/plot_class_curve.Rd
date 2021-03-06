% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualization.R
\name{plot_class_curve}
\alias{plot_class_curve}
\title{Plot classification curve from data}
\usage{
plot_class_curve(.data, ...)
}
\arguments{
\item{.data}{Data frame with at least vot, respP, bvotCond, and supCond
(like what is returned by load_and_parse()}
}
\description{
Helper function to view classification curves, calculated by averaging
proportion /p/ responses for each VOT and condition.
}

