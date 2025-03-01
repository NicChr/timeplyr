% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roll_na_fill.R
\name{roll_na_fill}
\alias{roll_na_fill}
\title{Fast grouped "locf" \code{NA} fill}
\usage{
roll_na_fill(x, g = NULL, fill_limit = Inf)
}
\arguments{
\item{x}{A vector.}

\item{g}{An object use for grouping x
This may be a vector or data frame for example.}

\item{fill_limit}{(Optional) maximum number of consecutive NAs to fill
per \code{NA} cluster. Default is \code{Inf}.}
}
\value{
A filled vector of \code{x} the same length as \code{x}.
}
\description{
A fast and efficient by-group method for
"last-observation-carried-forward" \code{NA} filling.
}
\details{
\subsection{Method}{

When supplying groups using \code{g}, this method uses \code{radixorder(g)} to
specify how to loop through \code{x}, making this extremely efficient.

When \code{x} contains zero or all \code{NA} values, then \code{x} is returned with no copy
made.
}
}
