% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fast_stats.R
\name{fn}
\alias{fn}
\alias{fnmiss}
\alias{fprop_complete}
\title{Supplementary fast statistical functions, \code{collapse} style}
\usage{
fn(x, g = NULL, sort = TRUE, expand = FALSE, use.g.names = !expand)

fnmiss(x, ...)

fprop_complete(x, ...)
}
\arguments{
\item{x}{A vector or data frame.
In the case of \code{fn()} this can be left unused as long as g is not \code{NULL},
otherwise it is used as a template with which to calculate group sizes.
For example, is \code{x} is a vector, lengths are calculated per-group,
and if \code{x} is a data frame, numbers of rows are calculated per-group.}

\item{g}{Object to be used for grouping,
passed directly to \code{collapse::GRP()}.}

\item{sort}{Should the grouped counts be ordered by the sorted groups?
If \code{FALSE} the result is ordered by groups of first appearance.}

\item{expand}{Should the grouped counts be expanded to match the length
and order of the data? Default is \code{FALSE}.}

\item{use.g.names}{If \code{TRUE} group names are added to the result as names.
This only applies to \code{fn()}. Default is \code{TRUE}.}

\item{...}{Additional parameters passed to \code{collapse::fsum()}.}
}
\description{
Supplementary fast statistical functions, \code{collapse} style
}
\details{
\code{fn()} Is different to the other \code{collapse}
fast statistical functions because given a data frame, it
operates on the entire data frame, instead of column-wise. It is similar
to the the other statistical functions in that order of the returned groups
matches that of \code{collapse::fnobs()}.
For example, \code{collapse::GRPN(c(2, 2, 1), expand = FALSE)}
returns \code{c(2, 1)} whereas \code{fn(g = c(2, 2, 1))} returns \code{c(1, 2)} which
is similar to \code{collapse::fnobs(rep(1, 3), g = c(2, 2, 1))}. \cr
While \code{fn()} is not entirely useful as a function, it is useful for
internal code that utilises \code{GRP} objects.
}
\examples{
\dontrun{
library(timeplyr)
library(dplyr)
library(nycflights13)
flights <- flights
fn(flights)
fn(flights, g = flights$origin)
fn(flights, g = flights$origin, expand = TRUE)

fnmiss(flights)
fprop_complete(flights)

bench::mark(e1 = fnmiss(flights),
            e2 = colSums(is.na(flights)))

# Compared to dplyr
bench::mark(e1 = fnmiss(flights, g = collapse::GRP(flights$tailnum, sort = FALSE)),
            e2 = flights \%>\%
              summarise(across(everything(), ~ sum(is.na(.x))),
                        .by = tailnum),
            e3 = flights \%>\%
              stat_summarise(.cols = names(flights),
                             stat = "nmiss",
                             .by = tailnum, sort = FALSE),
            check = FALSE)

library(data.table)
dt <- data.table(g = sample.int(10, size = 10^5, replace = TRUE))
dt[, .N, by = g] # Usual DT method
dt[, list(N = fn(g = g, sort = F))] # Using fn
fcount(dt, g) # fcount (preferred)
stat_summarise(dt, .by = g) # stat_summarise
}
}
