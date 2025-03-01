% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_diff.R
\name{time_diff}
\alias{time_diff}
\title{Time differences by any time unit}
\usage{
time_diff(x, y, timespan = 1L)
}
\arguments{
\item{x}{Start date or datetime.}

\item{y}{End date or datetime.}

\item{timespan}{A \link{timespan} used to divide the difference.}
}
\value{
A numeric vector recycled to the length of \code{max(length(x), length(y))}.
}
\description{
The time difference between 2 date or date-time vectors.
}
\examples{
library(timeplyr)
library(lubridate)
time_diff(today(), today() + days(10), "days")
time_diff(today(), today() + days((0:3) * 7), weeks(1))
time_diff(today(), today() + days(100), timespan("days", 1:100))
time_diff(1, 1 + 0:100, 3)

}
