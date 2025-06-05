#' @noRd

get_from_package <- function(x, package){
  get(x, asNamespace(package), inherits = FALSE)
}
# N expressions in ...
dots_length <- function(...){
  nargs()
}

dot_nms <- function(...){
  vapply(as.list(substitute(alist(...)))[-1L], deparse2, "", USE.NAMES = FALSE)
}
deparse2 <- function(expr, collapse = " ", width.cutoff = 500L, nlines = 10L, ...){
  paste(deparse(expr, width.cutoff, nlines = nlines, ...), collapse = collapse)
}
cpp_loc_set_replace <- get_from_package("cpp_loc_set_replace", "cheapr")

set_recycle_args <- function(..., length = NULL, use.names = TRUE){
  if (identical(base::parent.frame(n = 1), base::globalenv())){
    stop("Users cannot use set_recycle_args from the global environment")
  }
  recycled_list <- cheapr::recycle(..., length = length)
  if (use.names){
    names(recycled_list) <- dot_nms(...)
  }
  out_nms <- names(recycled_list)
  for (i in seq_along(recycled_list)){
    assign(out_nms[i], recycled_list[[i]], envir = parent.frame(n = 1))
  }
}
# Drop leading zeroes
drop_leading_zeros <- function(x, sep = "."){
  pattern <- paste0("^([^[:digit:]]{0,})0{1,}\\", sep, "{1}")
  sub(pattern, paste0("\\1", sep), x, perl = TRUE)
}

are_whole_numbers <- function(x){
  if (is.integer(x)){
    return(rep_len(TRUE, length(x)))
  }
  abs(x - round(x)) < sqrt(.Machine$double.eps)
}
# Unique number from positive numbers
# This was originally conceptualised as a way of turning the duration part of
# lubridate intervals
# into unique data points
# pair_unique <- function(x, y){
#   ( ( (x + y + 1) * (x + y) ) / 2 ) + x
# }

vec_head <- function(x, n = 1L){
  check_length(n, 1L)
  N <- cheapr::vector_length(x)
  if (n >= 0){
    size <- min(n, N)
  } else {
    size <- max(0L, N + n)
  }
  sset(x, seq_len(size))
}
vec_tail <- function(x, n = 1L){
  check_length(n, 1L)
  N <- cheapr::vector_length(x)
  if (n >= 0){
    size <- min(n, N)
  } else {
    size <- max(0L, N + n)
  }
  sset(x, seq.int(from = N - size + 1L, by = 1L, length.out = size))
}

# Base R version of purrr::pluck, alternative to [[
fpluck <- function(x, .cols = NULL, .default = NULL){
  if (is.null(.cols)){
    return(x)
  }
  if (length(.cols) > 1L){
    stop(".cols must have length 1")
  }
  if (is.numeric(.cols)){
    icol <- match(.cols, seq_along(x))
  } else {
    icol <- match(.cols, names(x))
  }
  # If no match just return .default
  if (length(icol) == 0L || is.na(icol)){
    return(.default)
  }
  .subset2(x, icol)
}

# Round up to nearest n
ceiling_nearest_n <- function(x, n){
  ceiling(x / n) * n
}
# How many 10s is a number divisible by?
log10_divisibility <- function(x){
  x[x == 0] <- 1
  floor(log10(abs(x)))
}
pretty_ceiling <- function(x){
  ceiling_nearest_n(x, n = 10^(log10_divisibility(x)))
}

bin_grouped <- function(x, breaks, gx = NULL, gbreaks = NULL, codes = TRUE,
                        right = TRUE,
                        include_lowest = FALSE,
                        include_oob = FALSE){
  x_list <- gsplit2(x, g = gx)
  breaks_list <- gsplit2(breaks, g = gbreaks)

  stopifnot(length(x_list) == length(breaks_list))
  out <- cheapr::new_list(length(x_list))


  for (i in seq_along(x_list)){
   out[[i]] <- cheapr::bin(x_list[[i]], breaks_list[[i]],
                          left_closed = !right,
                          include_endpoint = include_lowest,
                          include_oob = include_oob, codes = codes)

  }
  ptype <- if (codes) integer() else x[0L]
  vctrs::list_unchop(out, ptype = ptype)
}

check_is_num <- function(x){
  if (!is.numeric(x)){
    cli::cli_abort("{.arg x} must be numeric")
  }
}
# TRUE when x is sorted and contains no NA
is_sorted <- function(x){
  isTRUE(!is.unsorted(x))
}
check_sorted <- function(x){
  if (!is_sorted(x)){
    stop(paste(deparse2(substitute(x)), "must be in ascending order"))
  }
}
# Retains integer class of a if b is 1 and a is integer
divide <- function(a, b){
  if (is.integer(a) && allv2(b, 1)){
    a
  } else {
    a / b
  }
}
# Initialise a single NA value of correct type
na_init <- function(x, size = 1L){
  rep(x[NA_integer_], size)
}
strip_attrs <- function(x, set = FALSE){
  cheapr::attrs_rm(x, .set = set)
}
strip_attr <- function(x, which, set = FALSE){
  cheapr::attrs_add(x, .args = `names<-`(list(NULL), which), .set = set)
}
is_integerable <- function(x){
  abs(x) <= .Machine$integer.max
}
all_integerable <- function(x, shift = 0){
  all(
    (abs(collapse::frange(x, na.rm = TRUE)) + shift ) <= .Machine$integer.max,
    na.rm = TRUE
  )
}
add_attr <- function(x, which, value, set = FALSE){
  cheapr::attrs_add(x, `names<-`(list(value), which), .set = set)
}

add_names <- function(x, value){
  names(x) <- value
  x
}
check_is_list <- function(x){
  if (!is.list(x)){
    stop(paste(deparse2(substitute(x)), "must be a list"))
  }
}
check_length <- function(x, size){
  if (length(x) != size){
    cli::cli_abort("{.arg x} must be of length {size}")
  }
}
check_length_lte <- function(x, size){
  if (!(length(x) <= size)){
    cli::cli_abort("{.arg x} must have length <= {size}")
  }
}
# collapse allv and allna with extra length check
allv2 <- function(x, value){
  if (!length(x)){
   return(FALSE)
  }
  collapse::allv(x, value)
}

# anyDuplicated but returns a logical(1)
anyduplicated <- function(x){
  anyDuplicated.default(x) > 0L
}
# Taken from stats
hasTsp <- function(x){
  if (is.null(attr(x, "tsp"))){
    attr(x, "tsp") <- c(1, NROW(x), 1)
  }
  x
}
tsp <- function(x){
  attr(x, "tsp")
}

trunc2 <- function(x){
  if (is.integer(x)) x else trunc(x)
}
round2 <- function(x, digits = 0){
  if (is.integer(x) && all(digits >= 0)) x else round(x, digits)
}
floor2 <- function(x){
  if (is.integer(x)) x else floor(x)
}
ceiling2 <- function(x){
  if (is.integer(x)) x else ceiling(x)
}

# Cheapr functions --------------------------------------------------------

gcd_diff <- function(x){
  cheapr::gcd(diff_(x), na_rm = TRUE)
}
which <- cheapr::which_

which_in <- function(x, table){
  cheapr::na_find(
    collapse::fmatch(x, table, overid = 2L, nomatch = NA_integer_),
    invert = TRUE
  )
}
which_not_in <- function(x, table){
  cheapr::na_find(
    collapse::fmatch(x, table, overid = 2L, nomatch = NA_integer_)
  )
}
`%in_%` <- cheapr::`%in_%`
`%!in_%` <- cheapr::`%!in_%`

sequences <- function(size, from = 1L, by = 1L, add_id = FALSE){
  time_cast(cheapr::sequence_(size, from, by, add_id), from)
}
spark_bar <- function(x){
  bars <- intToUtf8(c(9601L, 9602L, 9603L, 9605L, 9606L, 9607L),
                    multiple = TRUE)
  bar_codes <- cheapr::bin(
    x, seq.int(0, to = 1, length.out = length(bars) + 1L),
    left_closed = TRUE, include_oob = TRUE, include_endpoint = TRUE
  )
  bar_codes[bar_codes == 0L] <- NA_integer_
  out <- bars[bar_codes]
  paste0(out, collapse = "")
}
inline_hist <- function(x, n_bins = 5L){
  if (length(x) < 1L) {
    return("")
  }
  if (is.infinite(max(abs(collapse::frange(x, na.rm = TRUE))))) {
    x[cheapr::val_find(is.infinite(x), TRUE)] <- NA
  }
  n_nas <- cheapr::na_count(x)
  all_na <- n_nas == length(x)
  if (all_na) {
    return("")
  }
  if (cheapr::val_count(x, 0) == (length(x) - n_nas)) {
    x <- x + 1
  }
  hist_dt <- tabulate(
    cut(x, n_bins, right = TRUE, labels = FALSE),
    nbins = n_bins
  )
  hist_dt <- hist_dt / max(hist_dt)
  spark_bar(hist_dt)
}
window_sequence <- cheapr::window_sequence
arithmetic_mean <- function(x, weights = NULL, na.rm = TRUE, ...){
  collapse::fmean(x, w = weights, na.rm = na.rm, ...)
}
geometric_mean <- function(x, weights = NULL, na.rm = TRUE, ...){
  exp(arithmetic_mean(log(x), weights = weights, na.rm = na.rm, ...))
}
harmonic_mean <- function(x, weights = NULL, na.rm = TRUE, ...){
  1 / arithmetic_mean(1/x, weights = weights, na.rm = na.rm, ...)
}

unique_count_col <- function(data, col = "n"){
  data_nms <- names(data)
  if (is.null(data_nms)) data_nms <- data
  if (col %in% data_nms){
    unique_count_col(data, col = paste0(col, "n"))
  } else {
    col
  }
}
# Checks if dataset has a variable name and returns unique name
unique_col_name <- function(data, col){
  data_nms <- names(data)
  if (is.null(data_nms)) data_nms <- data
  i <- 1L
  grp_nm <- col
  while (col %in% data_nms){
    i <- i + 1L
    col <- paste0(grp_nm, i)
  }
  col
}

tidy_select_names <- get_from_package("tidy_select_names", "fastplyr")

across_col_names <- function(.cols = NULL, .fns = NULL, .names = NULL){
  fns_null <- is.null(.fns)
  nms_null <- is.null(.names)
  if (fns_null && !nms_null) {
    .fns <- ""
    fns_null <- FALSE
  }
  n_fns <- length(.fns)
  n_cols <- length(.cols)
  if (fns_null && nms_null) {
    out <- as.character(.cols)
  }
  else if (nms_null && n_fns == 1L) {
    out <- .cols
  }
  else if (nms_null && n_cols == 1L) {
    out <- .fns
    out <- cheapr::name_repair(out, empty_sep = paste0(.cols,
                                                       "_"), dup_sep = "_")
  }
  else {
    .fns <- cheapr::name_repair(.fns %||% "", empty_sep = "",
                                dup_sep = "")
    out <- character(n_cols * n_fns)
    init <- 0L
    if (nms_null) {
      for (.col in .cols) {
        out[seq_len(n_fns) + init] <- paste0(.col, "_",
                                             .fns)
        init <- init + n_fns
      }
    }
    else {
      .fn <- .fns
      for (.col in .cols) {
        out[seq_len(n_fns) + init] <- stringr::str_glue(.names)
        init <- init + n_fns
      }
    }
  }
  out
}

old_group_id <- function(data, ...,
                         order = TRUE,
                         ascending = TRUE,
                         .by = NULL, .cols = NULL,
                         .name = NULL,
                         as_qg = FALSE){
  fastplyr::add_group_id(
    data, ...,
    .order = order,
    .ascending = ascending,
    .by = {{ .by }},
    .cols = .cols,
    .name = ".internal.temp.group.id",
    as_qg = as_qg
  )[[".internal.temp.group.id"]]
}

# if else as a function for ease-of-use
scalar_if_else <- function(condition, true, false) if (condition) true else false

# Memory efficient (but slower) scalar versions of all and any
all_val <- function(x, value){
  cheapr::val_count(x, value, recursive = TRUE) == cheapr::unlisted_length(x)
}
