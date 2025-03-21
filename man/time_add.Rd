% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_arithmetic.R
\name{time_add}
\alias{time_add}
\alias{time_subtract}
\alias{time_floor}
\alias{time_ceiling}
\title{Add/subtract timespans to dates and date-times}
\usage{
time_add(
  x,
  timespan,
  roll_month = getOption("timeplyr.roll_month", "xlast"),
  roll_dst = getOption("timeplyr.roll_dst", c("NA", "xfirst"))
)

time_subtract(
  x,
  timespan,
  roll_month = getOption("timeplyr.roll_month", "xlast"),
  roll_dst = getOption("timeplyr.roll_dst", c("NA", "xfirst"))
)

time_floor(x, timespan, week_start = getOption("lubridate.week.start", 1))

time_ceiling(
  x,
  timespan,
  week_start = getOption("lubridate.week.start", 1),
  change_on_boundary = is_date(x)
)
}
\arguments{
\item{x}{Time vector. \cr
E.g. a \code{Date}, \code{POSIXt}, \code{numeric} or any time-based vector.}

\item{timespan}{\link{timespan}.}

\item{roll_month}{See \code{?timechange::time_add}. Additional choices
include \code{xlast} (default) and \code{xfirst}. These work conceptually similar to
skipped DST intervals.}

\item{roll_dst}{See \code{?timechange::time_add}.}

\item{week_start}{day on which week starts following ISO conventions - 1
means Monday, 7 means Sunday (default). When \code{label = TRUE},
this will be the first level of the returned factor.
You can set \code{lubridate.week.start} option to control this parameter globally.}

\item{change_on_boundary}{\code{?timechange::time_floor}}
}
\value{
A date, date-time, or other time-based vector.
}
\description{
A very fast method of adding time units to dates and date-times.
}
\details{
The methods are continuously being improved over time.
Date arithmetic should be very fast regardless of the timespan supplied.
Date-time arithmetic, specifically
when supplied days, weeks, months and years, is being improved.
}
