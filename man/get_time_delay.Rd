% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_time_delay.R
\name{get_time_delay}
\alias{get_time_delay}
\title{Get summary statistics of time delay}
\usage{
get_time_delay(
  data,
  origin,
  end,
  timespan = 1L,
  min_delay = -Inf,
  max_delay = Inf,
  probs = c(0.25, 0.5, 0.75, 0.95),
  .by = NULL,
  include_plot = TRUE,
  x_scales = "fixed",
  bw = "sj",
  ...
)
}
\arguments{
\item{data}{A data frame.}

\item{origin}{Origin date variable.}

\item{end}{End date variable.}

\item{timespan}{\link{timespan}.}

\item{min_delay}{The minimum acceptable delay,
all delays less than this are removed before calculation.
Default is \code{min_delay = -Inf}.}

\item{max_delay}{The maximum acceptable delay,
all delays greater than this are removed before calculation.
Default is \code{max_delay = Inf}.}

\item{probs}{Probabilities used in the quantile summary.
Default is \code{probs = c(0.25, 0.5, 0.75, 0.95)}.}

\item{.by}{(Optional). A selection of columns to group by for this operation.
Columns are specified using tidy-select.}

\item{include_plot}{Should a \code{ggplot} graph of delay distributions be included in the output?}

\item{x_scales}{Option to control how the x-axis is displayed for multiple facets.
Choices are "fixed" or "free_x".}

\item{bw}{The smoothing bandwidth selector for the Kernel Density estimator.
If numeric, the standard deviation of the smoothing kernel.
If character, a rule to choose the bandwidth. See \code{?stats::bw.nrd} for more details.
The default has been set to "SJ" which implements the Sheather & Jones (1991) method,
as recommended by the R team \code{?stats::density}.
This differs from the default implemented by \code{stats::density()}
which uses Silverman's rule-of-thumb.}

\item{...}{Further arguments to be passed on to \code{ggplot2::geom_density()}.}
}
\value{
A list containing summary data, summary statistics and an optional \code{ggplot}.
}
\description{
The output is a \code{list} containing summary statistics of time delay between two date/datetime vectors.
This can be especially useful in estimating reporting delay for example.
\itemize{
\item \bold{data} - A data frame containing the origin, end and calculated time delay.
\item \bold{unit} - The chosen time unit.
\item \bold{num} - The number of time units.
\item \bold{summary} - \code{tibble} with summary statistics.
\item \bold{delay} - \code{tibble} containing the empirical cumulative distribution function
values by time delay.
\item \bold{plot} - A \code{ggplot} of the time delay distribution.
}
}
\examples{
library(timeplyr)
library(outbreaks)
library(dplyr)
\dontshow{
.n_dt_threads <- data.table::getDTthreads()
.n_collapse_threads <- collapse::get_collapse()$nthreads
data.table::setDTthreads(threads = 1L)
collapse::set_collapse(nthreads = 1L)
}
ebola_linelist <- ebola_sim_clean$linelist

# Incubation period distribution

# 95\% of individuals experienced an incubation period of <= 26 days
inc_distr_days <- ebola_linelist |>
  get_time_delay(date_of_infection,
                 date_of_onset,
                 time = "days")
head(inc_distr_days$data)
inc_distr_days$unit
inc_distr_days$num
inc_distr_days$summary
head(inc_distr_days$delay) # ECDF and freq by delay
inc_distr_days$plot

# Can change bandwidth selector
inc_distr_days <- ebola_linelist |>
  get_time_delay(date_of_infection,
                 date_of_onset,
                 time = "day",
                 bw = "nrd")
inc_distr_days$plot

# Can choose any time units
inc_distr_weeks <- ebola_linelist |>
  get_time_delay(date_of_infection,
                 date_of_onset,
                 time = "weeks",
                 bw = "nrd")
inc_distr_weeks$plot
\dontshow{
data.table::setDTthreads(threads = .n_dt_threads)
collapse::set_collapse(nthreads = .n_collapse_threads)
}
}
