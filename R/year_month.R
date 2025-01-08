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
#' @rdname year_month
#' @export
year_month_decimal <- function(x){
  m <- round(( x * 12 ) - (1970 * 12))
  new_year_month(m)
}
#' @rdname year_month
#' @export
decimal_year_month <- function(x){
  ( unclass(x) + (1970L * 12L) ) / 12L
}
new_year_month <- function(x){
  check_is_num(x)
  x <- as.integer(x)
  class(x) <- "year_month"
  x
}
#' @export
`[.year_month` <- function(x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("[")
  class(val) <- cl
  val
}
#' @export
`[[.year_month` <- function(x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("[[")
  class(val) <- cl
  val
}
#' @export
`+.year_month` <- function(e1, e2){
  out <- unclass(e1) + unclass(e2)
  both_year_month <- inherits(e1, "year_month") && inherits(e2, "year_month")
  if (!both_year_month){
    class(out) <- "year_month"
  }
  out
}
#' @export
`-.year_month` <- function(e1, e2){
  out <- unclass(e1) - unclass(e2)
  both_year_month <- inherits(e1, "year_month") && inherits(e2, "year_month")
  if (!both_year_month){
    class(out) <- "year_month"
  }
  out
}
#' @exportS3Method base::c
`c.year_month` <- function(...){
  new_year_month(NextMethod("c"))
}
#' @exportS3Method base::print
print.year_month <- function(x, max = NULL, ...){
  z <- unclass(x)
  if (length(z) == 0){
    print("YM(0)", ...)
  } else {
    if (is.null(max)){
      max <- getOption("max.print", 9999L)
    }
    max <- min(max, length(z))
    if (max < length(z)){
      z <- z[seq_len(max)]
      additional_msg <- paste(" [ reached 'max' / getOption(\"max.print\") -- omitted",
                               length(x) - max, "entries ]\n")
    } else {
      additional_msg <- character()
    }
    y <- 1970L + (z %/% 12L)
    m <- (z %% 12L) + 1L
    mf <- .months[m]
    print(stringr::str_c(y, mf, sep = " "), max = max + 1, ...)
    cat(additional_msg)
  }
  invisible(x)
}
#' @exportS3Method base::as.character
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
#' @exportS3Method base::format
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
#' @exportS3Method base::unique
unique.year_month <- function(x, incomparables = FALSE, ...){
  new_year_month(unique.default(x, incomparables = incomparables, ...))
}
#' @exportS3Method base::as.Date
as.Date.year_month <- function(x, ...){
  x <- unclass(x)
  y <- x %/% 12L
  m <- (x %% 12L) + 1L
  lubridate::make_date(year = 1970L + y, month = m, day = 1L)
}
#' @exportS3Method base::as.POSIXct
as.POSIXct.year_month <- function(x, tz = "UTC", ...){
  x <- unclass(x)
  y <- x %/% 12L
  m <- (x %% 12L) + 1L
  lubridate::make_datetime(year = 1970L + y, month = m, day = 1L, tz = tz)
}
#' @exportS3Method base::as.POSIXlt
as.POSIXlt.year_month <- function(x, tz = "UTC", ...){
  as.POSIXlt(as.POSIXct(x, tz = tz))
}
new_year_quarter <- function(x){
  check_is_num(x)
  x <- as.integer(x)
  class(x) <- "year_quarter"
  x
}
#' @rdname year_month
#' @export
YQ <- function(length = 0L){
  new_year_quarter(integer(length))
}
#' @rdname year_month
#' @export
year_quarter_decimal <- function(x){
  q <- round(( x * 4 ) - (1970 * 4))
  new_year_quarter(q)
}
#' @rdname year_month
#' @export
decimal_year_quarter <- function(x){
  ( unclass(x) + (1970L * 4L) ) / 4L
}
#' @export
`[.year_quarter` <- function(x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("[")
  class(val) <- cl
  val
}
#' @export
`[[.year_quarter` <- function(x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("[[")
  class(val) <- cl
  val
}
#' @export
`+.year_quarter` <- function(e1, e2){
  out <- unclass(e1) + unclass(e2)
  both_year_quarter <- inherits(e1, "year_quarter") && inherits(e2, "year_quarter")
  if (!both_year_quarter){
    class(out) <- "year_quarter"
  }
  out
}
#' @export
`-.year_quarter` <- function(e1, e2){
  out <- unclass(e1) - unclass(e2)
  both_year_quarter <- inherits(e1, "year_quarter") && inherits(e2, "year_quarter")
  if (!both_year_quarter){
    class(out) <- "year_quarter"
  }
  out
}
#' @exportS3Method base::c
`c.year_quarter` <- function(...){
  new_year_quarter(NextMethod("c"))
  # new_year_quarter(do.call(c, lapply(list(...), unclass)))
}
#' @exportS3Method base::print
print.year_quarter <- function(x, max = NULL, ...){
  z <- unclass(x)
  if (length(z) == 0){
    print("YQ(0)", ...)
  } else {
    if (is.null(max)){
      max <- getOption("max.print", 9999L)
    }
    max <- min(max, length(z))
    if (max < length(z)){
      z <- z[seq_len(max)]
      additional_msg <- paste(" [ reached 'max' / getOption(\"max.print\") -- omitted",
                              length(x) - max, "entries ]\n")
    } else {
      additional_msg <- character()
    }
    y <- 1970L + (z %/% 4L)
    q <- (z %% 4L) + 1L
    qf <- c("Q1", "Q2", "Q3", "Q4")[q]
    print(stringr::str_c(y, qf, sep = " "), max = max + 1, ...)
    cat(additional_msg)
  }
  invisible(x)
}
#' @exportS3Method base::as.character
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
#' @exportS3Method base::format
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
#' @exportS3Method base::unique
unique.year_quarter <- function(x, incomparables = FALSE, ...){
  new_year_quarter(unique.default(x, incomparables = incomparables, ...))
}
#' @exportS3Method base::as.Date
as.Date.year_quarter <- function(x, ...){
  x <- unclass(x)
  y <- 1970L + (x %/% 4L)
  m <- 3L * (x %% 4L) + 1L
  lubridate::make_date(year = y, month = m, day = 1L)
}
#' @exportS3Method base::as.POSIXct
as.POSIXct.year_quarter <- function(x, tz = "UTC", ...){
  x <- unclass(x)
  y <- 1970L + (x %/% 4L)
  m <- 3L * (x %% 4L) + 1L
  lubridate::make_datetime(year = y, month = m, day = 1L, tz = tz)
}
#' @exportS3Method base::as.POSIXlt
as.POSIXlt.year_quarter <- function(x, tz = "UTC", ...){
  as.POSIXlt(as.POSIXct(x, tz = tz))
}
#' @exportS3Method base::rep
rep.year_month <- function(x, ...){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("rep")
  class(val) <- cl
  val
}
#' @exportS3Method base::rep
rep.year_quarter <- function(x, ...){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("rep")
  class(val) <- cl
  val
}

is_year_month <- function(x){
  inherits(x, "year_month")
}

is_year_quarter <- function(x){
  inherits(x, "year_quarter")
}

.months <- c("Jan", "Feb", "Mar",
             "Apr", "May", "Jun",
             "Jul", "Aug", "Sep",
             "Oct", "Nov", "Dec")
