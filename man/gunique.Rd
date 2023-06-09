% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gunique.R
\name{gunique}
\alias{gunique}
\alias{gduplicated}
\alias{gwhich_duplicated}
\alias{gsort}
\alias{gorder}
\title{Grouped \code{unique()}, \code{sort()} and \code{duplicated()}}
\usage{
gunique(x, g = NULL, sort = FALSE, order = TRUE, use.g.names = TRUE)

gduplicated(x, g = NULL, order = TRUE, all = FALSE)

gwhich_duplicated(x, g = NULL, order = TRUE, all = FALSE)

gsort(x, g = NULL, order = TRUE, use.g.names = TRUE)

gorder(x, g = NULL, order = TRUE)
}
\arguments{
\item{x}{A vector or data frame.}

\item{g}{Object used for grouping, passed directly to \code{collapse::GRP()}.\cr
This can for example be a vector or data frame.}

\item{sort}{Should the result be sorted? \cr
This only applies to \code{gunique()}.}

\item{order}{Should the groups be treated as ordered groups?
Default is \code{TRUE}.}

\item{use.g.names}{Should group names be used? Default is \code{TRUE}.}

\item{all}{If \code{TRUE}, \code{gduplicated()} returns all duplicated values,
including the first occurrence.}
}
\description{
These functions use \code{collapse} and are like the
\code{collapse} counterpart but differ in that they accept a group \code{g} argument
which allows for more flexible by-group sorting.
}
\examples{
library(timeplyr)
library(dplyr)
library(tibble)
set.seed(81234)
iris <- slice_sample(iris, n = nrow(iris)) \%>\%
  as_tibble()

# gunique vs dplyr
# Unsorted unique values by group
gunique(iris$Sepal.Width, g = iris$Species, sort = FALSE)
iris \%>\%
  distinct(Species, Sepal.Width) \%>\%
  pull(Sepal.Width)
# Sorted unique values by ordered groups
gunique(iris$Sepal.Width, g = iris$Species, sort = TRUE)
iris \%>\%
  distinct(Species, Sepal.Width) \%>\%
  arrange(Species, Sepal.Width) \%>\%
  pull(Sepal.Width)
# Sorted unique values by unordered groups
gunique(iris$Sepal.Width, g = iris$Species,
        sort = TRUE, order = FALSE)
iris \%>\%
  reframe(Sepal.Width = sort(unique(Sepal.Width)), .by = Species) \%>\%
  pull(Sepal.Width)

# Very fast way of finding duplicate rows
df <- data.frame(group = sample.int(10^6, 10^4, TRUE))

which(gduplicated(df))
# More efficient
gwhich_duplicated(df)
}
