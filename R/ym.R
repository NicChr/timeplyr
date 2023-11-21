#' Fast methods for creating year-months and year-quarters
#'
#' @description
#' These are experimental methods for working with year-months and
#' year-quarters inspired by 'zoo' and 'tsibble'.
#'
#' @details
#' The biggest difference is that the underlying data is simply
#' the number of months/quarters since epoch. This makes integer
#' arithmetic very simple, and allows for fast coercion to `ym` and `yq`
#' from numeric vectors.
#'
#' Printing method is also fast.
#'
#' @param length Length of `ym` or `yq`.
#' @param x A `ym`, `yq`, or any other time-based object.
#'
#' @examples
#' library(timeplyr)
#' library(lubridate)
#'
#' x <- ym(today())
#'
#' # Adding 1 adds 1 month
#' x + 1
#' # Adding 12 adds 1 year
#' x + 12
#' # Sequence of yearmonths
#' x + 0:12
#'
#' # If you unclass, do the same arithmetic, and coerce back to ym
#' # The result is always the same
#' ym(unclass(x) + 1)
#' ym(unclass(x) + 12)
#'
#' # Initialise a ym or yq to the specified length
#' YM(0)
#' YQ(0)
#' YM(3)
#' YQ(3)
#'
#' @rdname ym
#' @export
ym <- function(x){
  if (is.numeric(x)){
    new_ym(strip_attrs(unclass(x)))
  } else {
    x_posix <- as.POSIXlt(x)
    y <- x_posix$year + 1900L
    m <- x_posix$mon
    new_ym( ((y - 1970L) * 12L ) + m )
  }
}
#' @rdname ym
#' @export
yq <- function(x){
  if (is.numeric(x)){
    new_yq(strip_attrs(unclass(x)))
  } else {
    x_posix <- as.POSIXlt(x)
    y <- x_posix$year + 1900L
    m <- x_posix$mon
    new_yq( ((y - 1970L) * 4L ) + (m %/% 3L) )
  }
}
#' @rdname ym
#' @export
YM <- function(length = 0L){
  new_ym(integer(length))
}
new_ym <- function(x){
  check_is_num(x)
  class(x) <- "ym"
  x
}
`[.ym` <- function(x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("[")
  class(val) <- cl
  val
}
`[[.ym` <- function(x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("[[")
  class(val) <- cl
  val
}
`+.ym` <- function(e1, e2){
  out <- unclass(e1) + unclass(e2)
  both_ym <- inherits(e1, "ym") && inherits(e2, "ym")
  if (!both_ym){
    class(out) <- "ym"
  }
  out
}
`-.ym` <- function(e1, e2){
  out <- unclass(e1) - unclass(e2)
  both_ym <- inherits(e1, "ym") && inherits(e2, "ym")
  if (!both_ym){
    class(out) <- "ym"
  }
  out
}
`c.ym` <- function(...){
  new_ym(do.call(c, lapply(list(...), unclass)))
}
print.ym <- function(x, ...){
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
as.character.ym <- function(x, ...){
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
format.ym <- function(x, ...){
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
unique.ym <- function(x, incomparables = FALSE, ...){
  new_ym(unique.default(x, incomparables = incomparables, ...))
}
as.Date.ym <- function(x, ...){
  x <- unclass(x)
  y <- x %/% 12L
  m <- (x %% 12L) + 1L
  lubridate::make_date(year = 1970L + y, month = m, day = 1L)
}
as.POSIXct.ym <- function(x, tz = "", ...){
  x <- unclass(x)
  y <- x %/% 12L
  m <- (x %% 12L) + 1L
  lubridate::make_datetime(year = 1970L + y, month = m, day = 1L)
}
as.POSIXlt.ym <- function(x, tz = "", ...){
  as.POSIXlt(as.POSIXct(x, tz = tz))
}
new_yq <- function(x){
  check_is_num(x)
  class(x) <- "yq"
  x
}
#' @rdname ym
#' @export
YQ <- function(length = 0L){
  new_yq(integer(length))
}
`[.yq` <- function(x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("[")
  class(val) <- cl
  val
}
`[[.yq` <- function(x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  val <- NextMethod("[[")
  class(val) <- cl
  val
}
`+.yq` <- function(e1, e2){
  out <- unclass(e1) + unclass(e2)
  both_yq <- inherits(e1, "yq") && inherits(e2, "yq")
  if (!both_yq){
    class(out) <- "yq"
  }
  out
}
`-.yq` <- function(e1, e2){
  out <- unclass(e1) - unclass(e2)
  both_yq <- inherits(e1, "yq") && inherits(e2, "yq")
  if (!both_yq){
    class(out) <- "yq"
  }
  out
}
`c.yq` <- function(...){
  new_yq(do.call(c, lapply(list(...), unclass)))
}
print.yq <- function(x, ...){
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
as.character.yq <- function(x, ...){
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
format.yq <- function(x, ...){
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
unique.yq <- function(x, incomparables = FALSE, ...){
  new_yq(unique.default(x, incomparables = incomparables, ...))
}

as.Date.yq <- function(x, ...){
  x <- unclass(x)
  y <- 1970L + (x %/% 4L)
  m <- 3L * (x %% 4L) + 1L
  lubridate::make_date(year = y, month = m, day = 1L)
}
as.POSIXct.yq <- function(x, tz = "", ...){
  x <- unclass(x)
  y <- 1970L + (x %/% 4L)
  m <- 3L * (x %% 4L) + 1L
  lubridate::make_datetime(year = y, month = m, day = 1L)
}
as.POSIXlt.yq <- function(x, tz = "", ...){
  as.POSIXlt(as.POSIXct(x, tz = tz))
}
rep_len.ym <- function(x, length.out){
  x[rep_len(seq_along(x), length.out = length.out)]
}
rep.int.ym <- function(x, ...){
  x[rep.int(seq_along(x), ...)]
}
rep.ym <- function(x, ...){
  x[rep(seq_along(x), ...)]
}
rep_len.yq <- function(x, length.out){
  x[rep_len(seq_along(x), length.out = length.out)]
}
rep.int.yq <- function(x, ...){
  x[rep.int(seq_along(x), ...)]
}
rep.yq <- function(x, ...){
  x[rep(seq_along(x), ...)]
}
.months <- c("Jan", "Feb", "Mar",
             "Apr", "May", "Jun",
             "Jul", "Aug", "Sep",
             "Oct", "Nov", "Dec")
