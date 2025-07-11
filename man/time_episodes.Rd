% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_episodes.R
\name{time_episodes}
\alias{time_episodes}
\title{Episodic calculation of time-since-event data}
\usage{
time_episodes(
  data,
  time,
  time_by = NULL,
  window = 1,
  roll_episode = TRUE,
  switch_on_boundary = TRUE,
  fill = 0,
  .add = FALSE,
  event = NULL,
  .by = NULL
)
}
\arguments{
\item{data}{A data frame.}

\item{time}{Date or datetime variable to use for the episode calculation.
Supply the variable using \code{tidyselect} notation.}

\item{time_by}{Time units used to calculate episode flags.
If \code{time_by} is \code{NULL} then a heuristic will try and estimate the highest
order time unit associated with the time variable.
If specified, then by must be one of the three:
\itemize{
\item string, specifying either the unit or the number and unit, e.g
\code{time_by = "days"} or \code{time_by = "2 weeks"}
\item named list of length one, the unit being the name, and
the number the value of the list, e.g. \code{list("days" = 7)}.
For the vectorized time functions, you can supply multiple values,
e.g. \code{list("days" = 1:10)}.
\item Numeric vector. If by is a numeric vector and x is not a date/datetime,
then arithmetic is used, e.g \code{time_by = 1}.
}}

\item{window}{Single number defining the episode threshold.
When \code{rolling = TRUE} events with a
\code{t_elapsed >= window} since the last event
are defined as a new episode. \cr
When \code{rolling = FALSE} events with a
\code{t_elapsed >= window} since the first event of the corresponding episode
are defined as a new episode. \cr
By default, \code{window = 1} which assigns every event to a new episode.}

\item{roll_episode}{Logical.
Should episodes be calculated using a rolling or fixed window?
If \code{TRUE} (the default), an amount of time must have passed (\verb{>= window})
since the last event, with each new event
effectively resetting the time at which
you start counting. \cr
If \code{FALSE}, the elapsed time is fixed and
new episodes are defined based on how much cumulative time has
passed since the first event of each episode.}

\item{switch_on_boundary}{When an exact amount of time
(specified in \code{time_by}) has passed, should there be an increment in ID? \cr
The default is \code{TRUE}. \cr
For example, if \code{time_by = "days"} and
\code{switch_on_boundary = FALSE}, \verb{> 1} day must have passed, otherwise
\verb{>= 1} day must have passed.}

\item{fill}{Value to fill first time elapsed value. Only applicable when
\code{roll_episode = TRUE}. \cr
Default is \code{0}.}

\item{.add}{Should episodic variables be added to the data? \cr
If \code{FALSE} (the default), then only the relevant variables are returned. \cr
If \code{TRUE}, the episodic variables are added to the original data.
In both cases, the order of the data is unchanged.}

\item{event}{(\bold{Optional}) List that encodes which rows are events,
and which aren't.
By default \code{time_episodes()}
assumes every observation (row) is an event
but this need not be the case. \cr
\code{event} must be a named list of length 1 where the values of the
list element represent the event. For example, if your events were coded as
\code{0} and \code{1} in a variable named "evt" where \code{1} represents the event,
you would supply \code{event = list(evt = 1)}.}

\item{.by}{(Optional). A selection of columns to group by for this operation.
Columns are specified using \code{tidyselect}.}
}
\value{
A \code{data.frame} in the same order as it was given.
}
\description{
This function assigns episodes to events
based on a pre-defined threshold of a chosen time unit.
}
\details{
\code{time_episodes()} calculates the time elapsed (rolling or fixed) between
successive events, and flags these events as episodes or not based on how much
time has passed.

An example of episodic analysis can include disease infections over time.

In this example, a positive test result represents an \bold{event} and \cr
a new infection represents a new \bold{episode}.

It is assumed that after a pre-determined amount of time, a positive result
represents a new episode of infection.

To perform simple time-since-event analysis, which means one
is not interested in episodes, simply use \code{time_elapsed()} instead.

To find implicit missing gaps in time, set \code{window} to \code{1} and
\code{switch_on_boundary} to \code{FALSE}. Any event classified as an
episode in this scenario is an event following a gap in time.

The data are always sorted before calculation and then
sorted back to the input order.

4 Key variables will be calculated:
\itemize{
\item \bold{ep_id} - An integer variable signifying
which episode each event belongs to. \cr
Non-events are assigned \code{NA}. \cr
\code{ep_id} is an increasing integer starting at 1.
In the infections scenario, 1 are positives within the
first episode of infection,
2 are positives within the second episode of infection and so on.
\item \bold{ep_id_new} - An integer variable signifying the first
instance of each new episode.
This is an increasing integer where
0 signifies within-episode observations and >= 1
signifies the first instance of the respective episode.
\item \bold{t_elapsed} - The time elapsed since the last event. \cr
When \code{roll_episode = FALSE}, this becomes the time elapsed since the
first event of the current episode.
Time units are specified in the by argument.
\item \bold{ep_start} - Start date/datetime of the episode.
}

\code{data.table} and \code{collapse} are used for speed and efficiency.
}
\examples{
library(timeplyr)
library(dplyr)
library(nycflights13)
library(lubridate)
library(ggplot2)
\dontshow{
.n_dt_threads <- data.table::getDTthreads()
.n_collapse_threads <- collapse::get_collapse()$nthreads
data.table::setDTthreads(threads = 1L)
collapse::set_collapse(nthreads = 1L)
}
# Say we want to flag origin-destination pairs
# that haven't seen departures or arrivals for a week

events <- flights |>
  mutate(date = as_date(time_hour)) |>
  group_by(origin, dest) |>
  time_episodes(date, "week", window = 1)

events

episodes <- events |>
  filter(ep_id_new > 1)
nrow(fastplyr::f_distinct(episodes, origin, dest)) # 55 origin-destinations

# As expected summer months saw the least number of
# dry-periods
episodes |>
  ungroup() |>
  time_by(ep_start, "week", .name = "ep_start") |>
  count(ep_start = interval_start(ep_start)) |>
  ggplot(aes(x = ep_start, y = n)) +
  geom_bar(stat = "identity")
\dontshow{
data.table::setDTthreads(threads = .n_dt_threads)
collapse::set_collapse(nthreads = .n_collapse_threads)
}
}
\seealso{
\link{time_elapsed} \link{time_seq_id}
}
