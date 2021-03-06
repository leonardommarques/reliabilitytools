% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interval_to_Rfilter.R
\name{interval_to_Rfilter}
\alias{interval_to_Rfilter}
\title{Interval R filter}
\usage{
interval_to_Rfilter(field_name = "", values = c(""))
}
\arguments{
\item{field_name:}{the name of the field in the data base}

\item{values:}{the interval velues to be filtered}
}
\value{
: A \code{character} containing the interval filter statement.
}
\description{
makes an interval filter statement
}
\details{
Makes a filter statement for interval limit.
}
\examples{
interval_to_Rfilter(field_name = 'age', c('>=18','<=20'))

}
