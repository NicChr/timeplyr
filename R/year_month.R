#' Fast methods for creating year-months and year-quarters
#'
#' @description
#' These are experimental methods for working with year-months and
#' year-quarters inspired by 'zoo' and 'tsibble'.
#'
#' @details
#' The biggest difference is that the underlying data is simply
#' the number of months/quarters since epoch. This makes integer
#' arithmetic very simple, and allows for fast sequence creation as well as
#' fast coercion to `year_month` and `year_quarter`
#' from numeric vectors.
#'
#' Printing method is also fast.
#'
#' @param length Length of `year_month` or `year_quarter`.
#' @param x A `year_month`, `year_quarter`, or any other time-based object.
#'
#' @examples
#' library(timeplyr)
#' library(lubridate)
#'
#' x <- year_month(today())
#'
#' # Adding 1 adds 1 month
#' x + 1
#' # Adding 12 adds 1 year
#' x + 12
#' # Sequence of yearmonths
#' x + 0:12
#'
#' # If you unclass, do the same arithmetic, and coerce back to year_month
#' # The result is always the same
#' year_month(unclass(x) + 1)
#' year_month(unclass(x) + 12)
#'
#' # Initialise a year_month or year_quarter to the specified length
#' YM(0)
#' YQ(0)
#' YM(3)
#' YQ(3)
#'
#' @rdname year_month
#' @export
year_month <- function(x){
  if (is.numeric(x)){
    new_year_month(strip_attrs(unclass(x)))
  } else {
    x_posix <- as.POSIXlt(x)
    y <- x_posix$year + 1900L
    m <- x_posix$mon
    new_year_month( ((y - 1970L) * 12L ) + m )
  }
}
#' @rdname year_month
#' @export
year_quarter <- function(x){
  if (is.numeric(x)){
    new_year_quarter(strip_attrs(unclass(x)))
  } else {
    x_posix <- as.POSIXlt(x)
    y <- x_posix$year + 1900L
    m <- x_posix$mon
    new_year_quarter( ((y - 1970L) * 4L ) + (m %/% 3L) )
  }
}
#' @rdname year_month
#' @export
YM <- function(length = 0L){
  new_year_month(integer(length))
}
new_year_month <- function(x){
  check_is_num(x)
  class(x) <- "year_month"
  x
}
`[.year_month` <- function(x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("[")
  class(val) <- cl
  val
}
`[[.year_month` <- function(x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("[[")
  class(val) <- cl
  val
}
`+.year_month` <- function(e1, e2){
  out <- unclass(e1) + unclass(e2)
  both_year_month <- inherits(e1, "year_month") && inherits(e2, "year_month")
  if (!both_year_month){
    class(out) <- "year_month"
  }
  out
}
`-.year_month` <- function(e1, e2){
  out <- unclass(e1) - unclass(e2)
  both_year_month <- inherits(e1, "year_month") && inherits(e2, "year_month")
  if (!both_year_month){
    class(out) <- "year_month"
  }
  out
}
`c.year_month` <- function(...){
  new_year_month(do.call(c, lapply(list(...), unclass)))
}
print.year_month <- function(x, ...){
  z <- unclass(x)
  if (length(z) == 0){
    print("YM(0)", ...)
  } else {
    y <- 1970L + (z %/% 12L)
    m <- (z %% 12L) + 1L
    mf <- .months[m]
    print(stringr::str_c(y, mf, sep = " "), ...)
    # print(sprintf("%.4d %02d", y, m))
  }
  invisible(x)
}
as.character.year_month <- function(x, ...){
  x <- unclass(x)
  if (length(x) == 0){
    "YM(0)"
  } else {
    y <- 1970L + (x %/% 12L)
    m <- (x %% 12L) + 1L
    mf <- .months[m]
    stringr::str_c(y, mf, sep = " ")
  }
}
format.year_month <- function(x, ...){
  x <- unclass(x)
  if (length(x) == 0){
    "YM(0)"
  } else {
    y <- 1970L + (x %/% 12L)
    m <- (x %% 12L) + 1L
    mf <- .months[m]
    format(stringr::str_c(y, mf, sep = " "), ...)
  }
}
unique.year_month <- function(x, incomparables = FALSE, ...){
  new_year_month(unique.default(x, incomparables = incomparables, ...))
}
as.Date.year_month <- function(x, ...){
  x <- unclass(x)
  y <- x %/% 12L
  m <- (x %% 12L) + 1L
  lubridate::make_date(year = 1970L + y, month = m, day = 1L)
}
as.POSIXct.year_month <- function(x, tz = "", ...){
  x <- unclass(x)
  y <- x %/% 12L
  m <- (x %% 12L) + 1L
  lubridate::make_datetime(year = 1970L + y, month = m, day = 1L)
}
as.POSIXlt.year_month <- function(x, tz = "", ...){
  as.POSIXlt(as.POSIXct(x, tz = tz))
}
new_year_quarter <- function(x){
  check_is_num(x)
  class(x) <- "year_quarter"
  x
}
#' @rdname year_month
#' @export
YQ <- function(length = 0L){
  new_year_quarter(integer(length))
}
`[.year_quarter` <- function(x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("[")
  class(val) <- cl
  val
}
`[[.year_quarter` <- function(x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("[[")
  class(val) <- cl
  val
}
`+.year_quarter` <- function(e1, e2){
  out <- unclass(e1) + unclass(e2)
  both_year_quarter <- inherits(e1, "year_quarter") && inherits(e2, "year_quarter")
  if (!both_year_quarter){
    class(out) <- "year_quarter"
  }
  out
}
`-.year_quarter` <- function(e1, e2){
  out <- unclass(e1) - unclass(e2)
  both_year_quarter <- inherits(e1, "year_quarter") && inherits(e2, "year_quarter")
  if (!both_year_quarter){
    class(out) <- "year_quarter"
  }
  out
}
`c.year_quarter` <- function(...){
  new_year_quarter(do.call(c, lapply(list(...), unclass)))
}
print.year_quarter <- function(x, ...){
  z <- unclass(x)
  if (length(z) == 0){
    print("YQ(0)")
  } else {
    y <- 1970L + (z %/% 4L)
    q <- (z %% 4L) + 1L
    qf <- c("Q1", "Q2", "Q3", "Q4")[q]
    print(stringr::str_c(y, qf, sep = " "), ...)
  }
  invisible(x)
}
as.character.year_quarter <- function(x, ...){
  x <- unclass(x)
  if (length(x) == 0){
    "YQ(0)"
  } else {
    y <- 1970L + (x %/% 4L)
    q <- (x %% 4L) + 1L
    qf <- c("Q1", "Q2", "Q3", "Q4")[q]
    stringr::str_c(y, qf, sep = " ")
  }
}
format.year_quarter <- function(x, ...){
  x <- unclass(x)
  if (length(x) == 0){
    "YQ(0)"
  } else {
    y <- 1970L + (x %/% 4L)
    q <- (x %% 4L) + 1L
    qf <- c("Q1", "Q2", "Q3", "Q4")[q]
    format(stringr::str_c(y, qf, sep = " "), ...)
  }
}
unique.year_quarter <- function(x, incomparables = FALSE, ...){
  new_year_quarter(unique.default(x, incomparables = incomparables, ...))
}

as.Date.year_quarter <- function(x, ...){
  x <- unclass(x)
  y <- 1970L + (x %/% 4L)
  m <- 3L * (x %% 4L) + 1L
  lubridate::make_date(year = y, month = m, day = 1L)
}
as.POSIXct.year_quarter <- function(x, tz = "", ...){
  x <- unclass(x)
  y <- 1970L + (x %/% 4L)
  m <- 3L * (x %% 4L) + 1L
  lubridate::make_datetime(year = y, month = m, day = 1L)
}
as.POSIXlt.year_quarter <- function(x, tz = "", ...){
  as.POSIXlt(as.POSIXct(x, tz = tz))
}
rep_len.year_month <- function(x, length.out){
  x[rep_len(seq_along(x), length.out = length.out)]
}
rep.int.year_month <- function(x, ...){
  x[rep.int(seq_along(x), ...)]
}
rep.year_month <- function(x, ...){
  x[rep(seq_along(x), ...)]
}
rep_len.year_quarter <- function(x, length.out){
  x[rep_len(seq_along(x), length.out = length.out)]
}
rep.int.year_quarter <- function(x, ...){
  x[rep.int(seq_along(x), ...)]
}
rep.year_quarter <- function(x, ...){
  x[rep(seq_along(x), ...)]
}
.months <- c("Jan", "Feb", "Mar",
             "Apr", "May", "Jun",
             "Jul", "Aug", "Sep",
             "Oct", "Nov", "Dec")
