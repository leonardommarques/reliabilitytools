% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_break_fun.R
\name{my_break_fun}
\alias{my_break_fun}
\title{Step axis grid breaks}
\usage{
my_break_fun(x, step = 50000)
}
\arguments{
\item{step:}{the step.}

\item{x:}{limits}
}
\value{
Return break vector with the given 'step'
}
\description{
set grid breaks by step
}
\details{
In scale_(x|y)_continuous, one can only explicitly set the grid breaks. With this function one can specify the breaks with a step.
}
\examples{
ggplot(cars, aes(dist, speed)) + 
  geom_point()+
  scale_x_continuous(breaks 

}
