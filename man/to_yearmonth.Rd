% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_yearmonth.R
\name{to_yearmonth}
\alias{to_yearmonth}
\title{convert to year month}
\usage{
to_yearmonth(date, format = "\%Y\%m")
}
\arguments{
\item{format:}{A character string. If not specified, it will try "%Y-%m-%d" then "%Y/%m/%d" on the first non-NA element, and give an error if neither works. Otherwise, the processing is via strptime}

\item{date:}{An \code{integer} or \code{character}}
}
\value{
A \code{Date}
}
\description{
convert an \code{integer} or \code{character} to year month .
}
\details{
Transforms year month to year month day \code{Date} object.
}
\examples{
to_yearmonth(201801)
to_yearmonth('2018-11', format = '\%Y-\%m')
}
