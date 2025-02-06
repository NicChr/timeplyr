#' @noRd

get_from_package <- function(x, package){
  get(x, asNamespace(package), inherits = FALSE)
}
# N expressions in ...
dots_length <- function(...){
  nargs()
}

dot_nms <- get_from_package("expr_names", "cheapr")
deparse2 <- get_from_package("deparse2", "cheapr")
r_copy <- get_from_package("r_copy", "cheapr")
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
  # x[rep_len(NA_integer_, size)]
  # rep_len(x[NA_integer_], size)
}
strip_attrs <- function(x, set = FALSE){
  if (set){
    set_rm_attributes(x)
  } else {
    attributes(x) <- NULL
    x
  }
}
strip_attr <- function(x, which, set = FALSE){
  if (set){
    set_rm_attr(x, which)
  } else {
    attr(x, which) <- NULL
    x
  }
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
  if (set){
    set_add_attr(x, which, value)
  } else {
    attr(x, which) <- value
    x
  }
}
add_attrs <- function(x, value, set = FALSE){
  if (set){
    set_add_attributes(x, value, add = FALSE)
  } else {
    attributes(x) <- value
    x
  }
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
which_na <- get_from_package("which_na", "cheapr")
which_not_na <- get_from_package("which_not_na", "cheapr")
which_in <- get_from_package("which_in", "cheapr")
which_not_in <- get_from_package("which_not_in", "cheapr")
`%in_%` <- cheapr::`%in_%`
`%!in_%` <- cheapr::`%!in_%`

sequences <- function(size, from = 1L, by = 1L, add_id = FALSE){
  time_cast(cheapr::sequence_(size, from, by, add_id), from)
}
inline_hist <- get_from_package("inline_hist", "cheapr")
window_sequence <- cheapr::window_sequence
sset <- cheapr::sset
set_add_attr <- get_from_package("cpp_set_add_attr", "cheapr")
set_add_attributes <- get_from_package("cpp_set_add_attributes", "cheapr")
set_rm_attr <- get_from_package("cpp_set_rm_attr", "cheapr")
set_rm_attributes <- get_from_package("cpp_set_rm_attributes", "cheapr")

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

mutate_summary_grouped <- get_from_package("mutate_summary_grouped", "fastplyr")
mutate_summary_ungrouped <- get_from_package("mutate_summary_ungrouped", "fastplyr")
tidy_group_info <- get_from_package("tidy_group_info", "fastplyr")
col_select_names <- get_from_package("col_select_names", "fastplyr")
tidy_select_names <- get_from_package("tidy_select_names", "fastplyr")
tidy_select_pos <- get_from_package("tidy_select_pos", "fastplyr")
across_col_names <- get_from_package("across_col_names", "fastplyr")

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
