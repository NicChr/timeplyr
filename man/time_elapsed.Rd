% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_elapsed.R
\name{time_elapsed}
\alias{time_elapsed}
\title{Fast grouped time elapsed}
\usage{
time_elapsed(
  x,
  time_by = NULL,
  g = NULL,
  time_type = getOption("timeplyr.time_type", "auto"),
  rolling = TRUE,
  fill = NA,
  na_skip = TRUE
)
}
\arguments{
\item{x}{Time variable. \cr
Can be a \code{Date}, \code{POSIXt}, \code{numeric}, \code{integer}, \code{yearmon}, or \code{yearqtr}.}

\item{time_by}{Must be one of the three:
\itemize{
\item string, specifying either the unit or the number and unit, e.g
\code{time_by = "days"} or \code{time_by = "2 weeks"}
\item named list of length one, the unit being the name, and
the number the value of the list, e.g. \code{list("days" = 7)}.
For the vectorized time functions, you can supply multiple values,
e.g. \code{list("days" = 1:10)}.
\item Numeric vector. If time_by is a numeric vector and x is not a date/datetime,
then arithmetic is used, e.g \code{time_by = 1}.
}}

\item{g}{Object to be used for grouping \code{x}, passed onto \code{collapse::GRP()}.}

\item{time_type}{Time type, either "auto", "duration" or "period".
With larger data, it is recommended to use \code{time_type = "duration"} for
speed and efficiency.}

\item{rolling}{If \code{TRUE} (the default) then lagged
time differences are calculated on a rolling basis,
essentially like \code{diff()}. \cr
If \code{FALSE} then time differences compared to the index (first) time
are calculated.}

\item{fill}{When \code{rolling = TRUE}, this is the value that fills
the first elapsed time. The default is \code{NA}.}

\item{na_skip}{Should \code{NA} values be skipped? Default is \code{TRUE}.}
}
\value{
A numeric vector the same length as \code{x}.
}
\description{
Calculate how much time has passed
on a rolling or cumulative basis. \cr
}
\details{
\code{time_elapsed()} is quite efficient when there are many groups,
especially if your data is sorted in order of those groups.
In the case that \code{g} is supplied, it is most efficient when your data is
sorted by \code{g} .
When \code{na_skip} is \code{TRUE} and \code{rolling} is also \code{TRUE}, \code{NA} values are simply
skipped and hence the time differences between the current value and the
previous non-NA value are calculated. For example,
\code{c(3, 4, 6, NA, NA, 9)} becomes \code{c(NA, 1, 2, NA, NA, 3)}. \cr
When \code{na_skip} is \code{TRUE} and \code{rolling} is \code{FALSE}, time differences between
the current value and the first non-NA value of the series are calculated.
For example,
\code{c(NA, NA, 3, 4, 6, NA, 8)} becomes \code{c(NA, NA, 0, 1, 3, NA, 5)}.
}
\examples{
library(timeplyr)
library(dplyr)
library(lubridate)
\dontshow{
.n_dt_threads <- data.table::getDTthreads()
.n_collapse_threads <- collapse::get_collapse()$nthreads
data.table::setDTthreads(threads = 2L)
collapse::set_collapse(nthreads = 1L)
}
x <- time_seq(today(), length.out = 25, time_by = "3 days")
time_elapsed(x)
time_elapsed(x, rolling = FALSE, time_by = "day")

# Grouped example
set.seed(99)
# ~ 100k groups, 1m rows
x <- sample(time_seq_v2(20, today(), "day"), 10^6, TRUE)
g <- sample.int(10^5, 10^6, TRUE)

time_elapsed(x, time_by = "day", g = g)
\dontshow{
data.table::setDTthreads(threads = .n_dt_threads)
collapse::set_collapse(nthreads = .n_collapse_threads)
}
}
