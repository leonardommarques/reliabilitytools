% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_curve_flexsurvreg.R
\name{add_curve_flexsurvreg}
\alias{add_curve_flexsurvreg}
\title{Distribution life curve}
\usage{
add_curve_flexsurvreg(model_flexsurvreg, lower = FALSE,
  log_transform = FALSE)
}
\arguments{
\item{log_transform:}{If true, add the \code{log = TRUE} to the distribution function.}

\item{lower:}{the \code{lower} argument of the dist functions.}

\item{model_flexsurvreg:}{an object of class \code{flexsurvreg}.}
}
\value{
A Layer for ggplot2 grafics, from a "stat_function()"
}
\description{
Make life curve from distribution
}
\examples{

}
