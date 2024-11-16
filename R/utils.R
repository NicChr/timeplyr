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
# Row products
rowProds <- function(x, na.rm = FALSE, dims = 1L){
  exp(rowSums(log(x), na.rm = na.rm, dims = dims))
}
# Wrapper around order() to use radix order
radix_order <- function(x, na.last = TRUE, ...){
  order(x, method = "radix", na.last = na.last, ...)
}
# Wrapper around order() to use radix sort
radix_sort <- function(x, na.last = TRUE, ...){
  x[radix_order(x, na.last = na.last, ...)]
}

# Drop leading zeroes
drop_leading_zeros <- function(x, sep = "."){
  pattern <- paste0("^([^[:digit:]]{0,})0{1,}\\", sep, "{1}")
  sub(pattern, paste0("\\1", sep), x, perl = TRUE)
}

# A wrapper around sample to account for length 1 vectors.
# This is a well known problem (and solution)
sample2 <- function(x, size = length(x), replace = FALSE, prob = NULL){
  x[sample.int(length(x), size = size, replace = replace, prob = prob)]
}

fcumsum <- get_from_package("fcumsum", "collapse")
# set <- get_from_package("set", "data.table")
fsum <- get_from_package("fsum", "collapse")
fmin <- get_from_package("fmin", "collapse")
fmax <- get_from_package("fmax", "collapse")
fmean <- get_from_package("fmean", "collapse")
fmode <- get_from_package("fmode", "collapse")
fsd <- get_from_package("fsd", "collapse")
fvar <- get_from_package("fvar", "collapse")
fmedian <- get_from_package("fmedian", "collapse")
ffirst <- get_from_package("ffirst", "collapse")
flast <- get_from_package("flast", "collapse")
fndistinct <- get_from_package("fndistinct", "collapse")

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

packageName <- function (env = parent.frame()){
  if (!is.environment(env))
    stop("'env' must be an environment")
  env <- topenv(env)
  if (!is.null(pn <- get0(".packageName", envir = env, inherits = FALSE)))
    pn
  else if (identical(env, .BaseNamespaceEnv))
    "base"
}
# Wrapper around expand.grid without factors and df coercion
# Sorting mimics CJ()
# Overhead is small for small joins
CJ2 <- function(X){
  nargs <- length(X)
  if (nargs <= 1L){
    return(X)
  }
  out <- vector("list", nargs)
  d <- cheapr::lengths_(X)
  orep <- prod(d)
  if (orep == 0L){
    for (i in seq_len(nargs)){
      out[[i]] <- .subset(.subset2(X, i), FALSE)
    }
    return(out)
  }
  rep.fac <- 1L
  for (i in seq.int(from = nargs, to = 1L, by = -1L)){
    x <- .subset2(X, i)
    nx <- .subset2(d, i)
    orep <- orep/nx
    x <- x[rep.int(rep(seq_len(nx), each = rep.fac), times = orep)]
    out[[i]] <- x
    rep.fac <- rep.fac * nx
  }
  out
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

# round down to nearest n
floor_nearest_n <- function(x, n){
  floor(x / n) * n
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
# Sensible rounding
pretty_floor <- function(x){
  floor_nearest_n(x, n = 10^(log10_divisibility(x)))
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
  # out <- cpp_bin_grouped(x_list, breaks_list, codes = codes,
  #                        include_lowest = include_lowest,
  #                        right = right,
  #                        include_oob = include_oob)
  ptype <- if (codes) integer() else x[0L]
  vctrs::list_unchop(out, ptype = ptype)
}

check_is_num <- function(x){
  if (!is.numeric(x)){
    stop(paste(deparse1(substitute(x)), "must be numeric"))
  }
}
check_is_double <- function(x){
  if (!is.double(x)){
    stop(paste(deparse1(substitute(x)), "must be a double"))
  }
}
# TRUE when x is sorted and contains no NA
is_sorted <- function(x){
  isTRUE(!is.unsorted(x))
}
check_sorted <- function(x){
  if (!is_sorted(x)){
    stop(paste(deparse1(substitute(x)), "must be in ascending order"))
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
    stop(paste(deparse1(substitute(x)), "must be a list"))
  }
}
check_length <- function(x, size){
  if (length(x) != size){
    stop(paste(deparse1(substitute(x)), "must be of length", size))
  }
}
check_length_lte <- function(x, size){
  if (!(length(x) <= size)){
    stop(paste(deparse1(substitute(x)), "must have length <=", size))
  }
}
# collapse allv and allna with extra length check
allv2 <- function(x, value){
  if (!length(x)){
   return(FALSE)
  }
  collapse::allv(x, value)
}

list_of_empty_vectors <- function(x){
  lapply(x, function(x) x[0L])
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

# Sort x with no copy
# If y is supplied, sort x using y
# set_order <- function(x, y = NULL){
#   df <- collapse::qDT(list3(x = x, y = y))
#   data.table::setorderv(df, cols = names(df)[df_ncol(df)])
#   invisible(x)
# }

# setdiff where x and y are unique vectors
setdiff2 <- function(x, y){
  x[match(x, y, 0L) == 0L]
}
intersect2 <- function(x, y){
  if (is.null(x) || is.null(y)){
    return(NULL)
  }
  c(x[match(x, y, 0L) > 0L], y[numeric()])
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
list_rm_null <- get_from_package("cpp_list_rm_null", "cheapr")
which_in <- get_from_package("which_in", "cheapr")
which_not_in <- get_from_package("which_not_in", "cheapr")
which_val <- get_from_package("which_val", "cheapr")
val_rm <- get_from_package("val_rm", "cheapr")
`%in_%` <- cheapr::`%in_%`
`%!in_%` <- cheapr::`%!in_%`

sequences <- function(size, from = 1L, by = 1L, add_id = FALSE){
  time_cast(cheapr::sequence_(size, from, by, add_id), from)
}
df_select <- get_from_package("df_select", "cheapr")
list_as_df <- get_from_package("list_as_df", "cheapr")
inline_hist <- get_from_package("inline_hist", "cheapr")
new_list <- cheapr::new_list
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
cpp_set_list_element <- get_from_package("cpp_set_list_element", "fastplyr")

# A `data.table::setorder()` that works for any data frame
df_set_order <- function(x, .cols = names(x), .order = 1L){

  ## Make sure this only works for data frames of simple vectors

  group_vars <- group_vars(x)

  y <- cheapr::new_list(length(names(x)))
  names(y) <- names(x)
  for (i in seq_along(y)){
    cpp_set_list_element(y, i, x[[i]])
  }

  # setDT() creates a sort of shallow copy
  # so we can't directly use it on x
  data.table::setDT(y)
  data.table::setorderv(y, cols = col_select_names(x, .cols), na.last = TRUE, order = .order)

  # Add cols back to x by reference
  # This ensures materialised ALTREP objects in y
  # are definitely copied back to x

  for (i in seq_along(y)){
    cpp_set_list_element(x, i, y[[i]])
  }
  if (length(group_vars) > 0){
    # Add re-calculated group data
    groups <- group_data(fastplyr::f_group_by(fastplyr::f_ungroup(x), .cols = group_vars))
    set_add_attr(x, "groups", groups)
  } else {
    x
  }
}
