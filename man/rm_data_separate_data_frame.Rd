% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rm_data_separate_data_frame.R
\name{rm_data_separate_data_frame}
\alias{rm_data_separate_data_frame}
\title{RMx data.frame}
\usage{
rm_data_separate_data_frame(data_base = NA, RM_size = 6, current_month,
  population_filter = NA, assemb_col = "DWASSD_yearmon",
  repair_col = "FPAYDT_yearmon")
}
\arguments{
\item{assemb_col, }{repair_col: assembly and repair coluns}

\item{current_month:}{A \code{character} object of Month of the given RM (YYYY-MM-DD).}

\item{data_base:}{A source \code{data.frame} to take data from.}

\item{population_filter:}{A filter \code{character} statement.}

\item{RM_size::}{the size, in momths, of the RM. Typically 6, 9, 12, 15, 18, 21 or 24}
}
\value{
A \code{tibble}.
}
\description{
Separates the data to make the RMx of the given month.
}
\details{
Makes the RMx data set of the given month
}
\examples{

}
