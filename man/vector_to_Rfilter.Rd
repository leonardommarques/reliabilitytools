% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector_to_Rfilter.R
\name{vector_to_Rfilter}
\alias{vector_to_Rfilter}
\title{& filter statement}
\usage{
vector_to_Rfilter(field_name = "", values = c(""))
}
\arguments{
\item{field_name:}{the name of the field in the data base}

\item{values:}{the values to be filtered}
}
\value{
a \code{character} containing the statement.
}
\description{
makes an '&' filter statement
}
\examples{
vector_to_Rfilter(field_name = 'country',
values = c('BRA', 'GER', 'JPN'))
}
