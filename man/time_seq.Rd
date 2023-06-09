% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_seq.R
\name{time_seq}
\alias{time_seq}
\alias{time_seq_sizes}
\alias{time_seq_v}
\alias{time_seq_v2}
\title{Time based version of \code{base::seq()}}
\usage{
time_seq(
  from,
  to,
  time_by,
  length.out = NULL,
  time_type = c("auto", "duration", "period"),
  week_start = getOption("lubridate.week.start", 1),
  time_floor = FALSE,
  roll_month = "preday",
  roll_dst = "pre",
  tz = if (!missing(from)) lubridate::tz(from) else lubridate::tz(to),
  by = NULL,
  seq_type = NULL,
  floor_date = NULL
)

time_seq_sizes(
  from,
  to,
  time_by,
  time_type = c("auto", "duration", "period"),
  as_period = FALSE
)

time_seq_v(
  from,
  to,
  time_by,
  time_type = c("auto", "duration", "period"),
  roll_month = "preday",
  roll_dst = "pre",
  time_floor = FALSE,
  week_start = getOption("lubridate.week.start", 1),
  by = NULL,
  seq_type = NULL
)

time_seq_v2(
  sizes,
  from,
  time_by,
  time_type = c("auto", "duration", "period"),
  time_floor = FALSE,
  week_start = getOption("lubridate.week.start", 1),
  roll_month = "preday",
  roll_dst = "pre"
)
}
\arguments{
\item{from}{Start date/datetime of sequence.}

\item{to}{End date/datetime of sequence.}

\item{time_by}{Time unit increment. \cr
Must be one of the three:
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

\item{length.out}{Length of the sequence.}

\item{time_type}{If "auto", \code{periods} are used for
the time expansion when days, weeks,
months or years are specified, and \code{durations}
are used otherwise. If \code{durations}
are used the output is always of class \code{POSIXt}.}

\item{week_start}{day on which week starts following ISO conventions - 1
means Monday (default), 7 means Sunday.
This is only used when \code{time_floor = TRUE}.}

\item{time_floor}{Should \code{from} be floored to
the nearest unit specified through the \code{time_by}
argument? This is particularly useful for
starting sequences at the beginning of a week
or month for example.}

\item{roll_month}{Control how impossible dates are handled when
month or year arithmetic is involved.
Options are "preday", "boundary", "postday", "full" and "NA".
See \code{?timechange::time_add} for more details.}

\item{roll_dst}{See \code{?timechange::time_add} for the full list of details.}

\item{tz}{Timezone of returned time sequence.}

\item{by}{\bold{Deprecated}. Use \code{time_by} instead}

\item{seq_type}{\bold{Deprecated}. Use \code{time_type} instead.}

\item{floor_date}{\bold{Deprecated}. Use \code{time_floor} instead.}

\item{as_period}{Logical. Should time interval be coerced to a period
before time difference is calculated? This is useful for calculating
for example age in exact years or months.}

\item{sizes}{Time sequence sizes.}
}
\description{
Time based version of \code{base::seq()}
}
\details{
This works like \code{seq()},
but using \code{timechange} for the period calculations and
\code{base::seq.POSIXT()} for the duration calculations.
In many ways it is improved over \code{seq} as
dates and/or datetimes can be supplied with no errors to
the start and end points.
Examples like,\cr
\code{time_seq(now(), length.out = 10, by = "0.5 days", seq_type = "dur")}
and \cr
\code{time_seq(today(), length.out = 10, by = "0.5 days", seq_type = "dur")}\cr
produce more expected results compared to \cr
\code{seq(now(), length.out = 10, by = "0.5 days")} or \cr
\code{seq(today(), length.out = 10, by = "0.5 days")}.\cr

For a vectorized implementation with multiple start/end times,
use \code{time_seq_v()}/\code{time_seq_v2()}

\code{time_seq_sizes()} is a convenience
function to calculate time sequence lengths, given start/end times.
}
\examples{
library(timeplyr)
library(lubridate)

# Dates
today <- today()
now <- now()

time_seq(today, today + years(1), time_by = "day")
time_seq(today, length.out = 10, time_by = "day")
time_seq(today, length.out = 10, time_by = "hour")

time_seq(today, today + years(1), time_by = list("days" = 1)) # Alternative
time_seq(today, today + years(1), time_by = "week")
time_seq(today, today + years(1), time_by = "fortnight")
time_seq(today, today + years(1), time_by = "year")
time_seq(today, today + years(10), time_by = "year")
time_seq(today, today + years(100), time_by = "decade")

# Datetimes
time_seq(now, now + years(1), time_by = "12 hours")
time_seq(now, now + years(1), time_by = "day")
time_seq(now, now + years(1), time_by = "week")
time_seq(now, now + years(1), time_by = "fortnight")
time_seq(now, now + years(1), time_by = "year")
time_seq(now, now + years(10), time_by = "year")
time_seq(now, today + years(100), time_by = "decade")

# You can seamlessly mix dates and datetimes with no errors.
time_seq(now, today + days(3), time_by = "day")
time_seq(now, today + days(3), time_by = "hour")
time_seq(today, now + days(3), time_by = "day")
time_seq(today, now + days(3), time_by = "hour")

# Choose between durations or periods

start <- dmy(31012020)
# If time_type is left as is,
# periods are used for days, weeks, months and years.
time_seq(start, time_by = "month", length.out = 12,
         time_type = "period")
time_seq(start, time_by = "month", length.out = 12,
         time_type = "duration")
# Notice how strange base R version is.
seq(start, by = "month", length.out = 12)

# Roll forward or backward impossible dates

leap <- dmy(29022020) # Leap day
end <- dmy(01032021)
# 3 different options
time_seq(leap, to = end, time_by = "year", time_type = "per",
         roll_month = "NA")
time_seq(leap, to = end, time_by = "year", time_type = "per",
         roll_month = "postday")
time_seq(leap, to = end, time_by = "year", time_type = "per",
         roll_month = "preday")
}
\seealso{
\link[timeplyr]{seq_id} \link[timeplyr]{time_seq_id}
}
