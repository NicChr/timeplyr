#' Fast versions of `tidyr::expand()` and `tidyr::complete()`.
#'
#' @description
#' For un-grouped data `fexpand()` is similar in speed to `tidyr::expand()`.
#' When the data contain many groups, `fexpand()` is much much faster (see examples).
#'
#' The 2 main differences between `fexpand()` and `tidyr::expand()` are that:
#'
#' * tidyr style helpers like `nesting()` and `crossing()` are ignored.
#' The type of expansion used is controlled through `expand_type` and applies to
#' all supplied variables.
#' * Expressions are first calculated on the entire ungrouped dataset before being
#' expanded but within-group expansions will work on variables that already exist
#' in the dataset.
#' For example, `iris %>% group_by(Species) %>% fexpand(Sepal.Length, Sepal.Width)`
#' will perform a grouped expansion but
#' `iris %>% group_by(Species) %>% fexpand(range(Sepal.Length))`
#' will not.
#'
#' For efficiency, when supplying groups, expansion is done on a by-group basis only if
#' there are 2 or more variables that aren't part of the grouping.
#' The reason is that a by-group calculation does not need to be done with 1 expansion variable
#' as all combinations across groups already exist against that 1 variable.
#' When `expand_type = "nesting"` groups are ignored for speed purposes as the result is the same.
#'
#' An advantage of `fexpand()` is that it returns a data frame with the same class
#' as the input. It also uses `data.table` for memory efficiency and `collapse` for speed.
#'
#' A future development for `fcomplete()` would be to only fill values of variables that
#' correspond only to both additional completed rows and rows that match the expanded rows, are
#' filled in. For example,
#' `iris %>% mutate(test = NA_real_) %>% complete(Sepal.Length = 0:100, fill = list(test = 0))`
#' fills in all `NA` values of test, whereas
#' `iris %>% mutate(test = NA_real_) %>% fcomplete(Sepal.Length = 0:100, fill = list(test = 0))`
#' should only fill in values of test that correspond to Sepal.Length values of `0:100`.
#'
#' An additional note to add when `expand_type = "nesting"` is that if one of the
#' supplied variables in `...` does not exist in the data, but can be recycled
#' to the length of the data, then it is added and treated as a data variable.
#'
#' @param data A data frame
#' @param ... Variables to expand
#' @param expand_type Type of expansion to use where "nesting"
#' finds combinations already present in the data
#' (exactly the same as using `distinct()` but `fexpand()`
#' allows new variables to be created on the fly
#' and columns are sorted in the order given.
#' "crossing" finds all combinations of values in the group variables.
#' @param fill A named list containing value-name pairs
#' to fill the named implicit missing values.
#' @param sort Logical. If `TRUE` expanded/completed variables are sorted.
#' The default is `FALSE`.
#' @param .by (Optional). A selection of columns to group by for this operation.
#' Columns are specified using tidy-select.
#' @param keep_class Logical.
#' If `TRUE` then the class of the input data is retained.
#' If `FALSE`, which is sometimes faster, a `data.table` is returned.
#' @param log_limit The maximum log10 number of rows that can be expanded.
#' Anything exceeding this will throw an error.
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' library(nycflights13)
#'
#' flights %>%
#'   fexpand(origin, dest)
#' flights %>%
#'   fexpand(origin, dest, sort = FALSE)
#'
#' # Grouped expansions example
#' # 1 extra group (carrier) this is very quick
#' flights %>%
#'   group_by(origin, dest, tailnum) %>%
#'   fexpand(carrier)
#' # 2 extra groups, this is where the grouped calculation actually happens
#' # still very quick
#' flights %>%
#'   group_by(origin, dest, tailnum) %>%
#'   fexpand(carrier, flight)
#'
#' # Tidyverse select helpers and data masking can be used
#' flights %>%
#'   fexpand(date = as_date(time_hour),
#'           across(all_of(c("origin", "dest"))),
#'           pick("carrier"))
#' # Alternatively
#' # flights %>%
#' #   fexpand(carrier, flight,
#' #           .by = c(origin, dest, tailnum))
#' # Return data.table
#' # flights %>%
#' #   fexpand(carrier, flight,
#' #           .by = c(origin, dest, tailnum), keep_class = FALSE)
#'
#' \dontrun{
#' library(microbenchmark)
#' library(tidyr)
#' # ~ 20x faster
#' microbenchmark(m1 = flights %>%
#'                  group_by(origin, dest, tailnum) %>%
#'                  fexpand(carrier, flight),
#'                m2 = flights %>%
#'                  group_by(origin, dest, tailnum) %>%
#'                  expand(carrier, flight),
#'                times = 1)
#' }
#' @rdname fexpand
#' @export
fexpand <- function(data, ..., expand_type = c("crossing", "nesting"),
                    sort = FALSE,
                    .by = NULL,
                    keep_class = TRUE,
                    log_limit = 8){
  expand_type <- match.arg(expand_type)
  group_vars <- get_groups(data, {{ .by }})
  summarise_vars <- summarise_list(data, ...)
  grps_missed <- setdiff(group_vars, names(summarise_vars))
  # Add group vars to summary list
  if (length(grps_missed) > 0){
    summarise_vars <- c(setnames(
      lapply(
        grps_missed, function(x) fpluck(
          dplyr_summarise(
            safe_ungroup(data), across(all_of(x))
          ), 1)
      ), grps_missed
    ),
    summarise_vars)
  }
  # Re-order list so that groups are first
  summarise_vars <- summarise_vars[c(group_vars,
                                     setdiff(names(summarise_vars),
                                             group_vars))]
  summarise_var_nms <- names(summarise_vars)
  out_nms <- c(group_vars, setdiff(summarise_var_nms, group_vars))
  # All variables minus grouped ones
  leftover_grp_nms <- setdiff(summarise_var_nms, group_vars)
  if (expand_type == "nesting" ||
      # Special case when data is grouped but only 1 data variable is specified to expand
      # There is no need from a speed perspective to do grouped calculation in this case
      (length(group_vars) > 0L &&
       length(leftover_grp_nms) <= 1L &&
       expand_type == "crossing")){
    out <- nested_join(summarise_vars, N = nrow2(data),
                       sort = sort,
                       log_limit = log_limit)
  } else {
    # Method for grouped data which performs a separate cross-join of
    # non-grouped variables for each group
    if (length(group_vars) > 0L && length(leftover_grp_nms) >= 2L){
      out1 <- nested_join(summarise_vars, N = nrow2(data),
                          sort = FALSE,
                          log_limit = log_limit)
      # Add group ID
      grp_nm <- new_var_nm(out1, ".group.id")
      out1[, (grp_nm) := group_id.default(.SD, order = FALSE),
           .SDcols = group_vars]
      setorderv2(out1, cols = grp_nm)
      # Add group IDs for each non-group variable
      # This will allow us to calculate final expanded size
      for (i in seq_along(leftover_grp_nms)){
        assign(paste0("grp_nm_", i),
               new_var_nm(out1, ".group.id"))
        out1[, (get(paste0("grp_nm_", i))) := group_id.default(.SD, order = FALSE),
             .SDcols = leftover_grp_nms[[i]]]
      }
      group_id_nms <- unlist(mget(paste0("grp_nm_",
                                         seq_len(length(leftover_grp_nms)))),
                             recursive = FALSE, use.names = FALSE)
      # Figure out final size before expansion, to do this we can
      # Calculate the vector product of unique expanded elements across groups.
      out_temp <- collapse::fndistinct(out1[, group_id_nms, with = FALSE],
                                       g = out1[[grp_nm]],
                                       use.g.names = FALSE, na.rm = FALSE)
      sizes <- rowProds(out_temp)
      expanded_nrow <- sum(sizes)
      expand_check(expanded_nrow, log_limit)
      # out <- out1[, do.call(crossed_join,
      #                args = list(X = .SD,
      #                            unique = TRUE,
      #                            strings_as_factors = FALSE,
      #                            as_dt = FALSE)),
      #      keyby = grp_nm,
      #      .SDcols = group_id_nms]
      # out2 <- collapse::collapv(out1, FUN = function(x) list(collapse::funique(x)),
      #                           by = grp_nm,
      #                           cols = group_id_nms)
      data.table::setkeyv(out1, cols = grp_nm)
      out2 <- out1[, lapply(.SD, function(x) list(collapse::funique(x))),
                   keyby = grp_nm,
                   .SDcols = group_id_nms]
      # # This is fastest but can't get it to work in the package
      # out <- out2[, .Call(Ccj, unlist(.SD, recursive = FALSE, use.names = FALSE)),
      #             keyby = grp_nm,
      #             .SDcols = group_id_nms]
      # out <- out2[, do.call(CJ2,
      #                       args = unlist(.SD,
      #                                     recursive = FALSE,
      #                                     use.names = FALSE)),
      #             keyby = grp_nm,
      #             .SDcols = group_id_nms]
      out <- out2[, CJ2(unlist(.SD, recursive = FALSE, use.names = FALSE)),
                  keyby = grp_nm,
                  .SDcols = group_id_nms]
      data.table::setnames(out, new = c(grp_nm, leftover_grp_nms))
      data.table::setalloccol(out)
      for (i in seq_along(group_id_nms)){
        data.table::set(out, j = leftover_grp_nms[[i]],
                        value = out1[[leftover_grp_nms[[i]]]][match(out[[leftover_grp_nms[[i]]]],
                                                                    out1[[group_id_nms[[i]]]])])
      }
      for (i in seq_along(group_vars)){
        data.table::set(out, j = group_vars[[i]],
                        value = out1[[group_vars[[i]]]][match(out[[grp_nm]],
                                                              out1[[grp_nm]])])
      }
      set_rm_cols(out, grp_nm)
      if (sort){
        setorderv2(out, cols = c(group_vars, leftover_grp_nms))
      }
    }
    # If no groups then cross-join everything
    else {
      out <- crossed_join(summarise_vars, sort = sort,
                          unique = TRUE,
                          log_limit = log_limit)
    }
  }
  if (length(out_nms) == 0L) out_nms <- NULL
  data.table::setcolorder(out, out_nms)
  if (keep_class){
    out <- df_reconstruct(out, data)
  }
  out
}
# Nested join, recycling newly created variables with data variables
nested_join <- function(X, sort = FALSE, log_limit = 8, N){
  X_nms <- names(X)
  if (length(X_nms) == 0L){
    X_nms <- rep_len("Var", length(X))
    X_nms <- paste0(X_nms, seq_len(length(X)))
  }
  X_lens <- collapse::vlengths(X, use.names = FALSE)
  # If N is not supplied, then calculate N iff all list lengths are equal
  if (missing(N)){
    N <- unique(X_lens)
  }
  stopifnot(length(N) == 1L)
  # Data variables
  data_nms <- X_nms[(X_lens %% N) == 0]
  # Newly created variables
  other_nms <- X_nms[!X_nms %in% data_nms]
  df <- data.table::as.data.table(X[X_nms %in% data_nms])
  n_data <- nrow2(df)
  if (n_data > 0L){
    df <- collapse::funique(df)
    n_data <- nrow2(df)
  }
  X_other <- X[X_nms %in% other_nms]
  X_other <- lapply(X_other, function(x) collapse::funique(x, sort = FALSE))
  n_data <- max(n_data, 1L)
  n_other <- prod(collapse::vlengths(X_other, use.names = FALSE))
  n_other <- max(n_other, 1, na.rm = TRUE)
  expanded_n <- prod(c(n_data, n_other), na.rm = TRUE)
  expand_check(expanded_n, log_limit)
  # Nested cross-join
  grp_seq <- seq_len(n_data)
  if (nrow2(df) == 0L){
    out <- crossed_join(X_other, unique = FALSE)
  } else {
    out <- df_row_slice(df, rep(grp_seq, each = n_other))
    # out <- df[rep(grp_seq, each = n_other), , drop = FALSE]
    if (length(X_other) > 0L){
      rep_times <- nrow2(out) / collapse::vlengths(X_other, use.names = FALSE)
      for (i in seq_along(X_other)){
        data.table::set(out, j = other_nms[i],
                        value = rep(X_other[[i]], rep_times[i]))
      }
    }
  }
  if (sort){
    setorderv2(out, cols = names(out))
  }
  out
}
#' @rdname fexpand
#' @export
fcomplete <- function(data, ..., expand_type = c("crossing", "nesting"),
                      sort = FALSE, .by = NULL,
                      keep_class = TRUE, fill = NA,
                      log_limit = 8){
  expand_type <- match.arg(expand_type)
  group_vars <- get_groups(data, {{ .by }})
  expanded_df <- fexpand(data,
                         ...,
                         sort = FALSE, .by = {{ .by }},
                         expand_type = expand_type,
                         keep_class = FALSE,
                         log_limit = log_limit)
  out <- as_DT(data)
  fill_na <- any(!is.na(fill))
  # Full-join
  if (nrow2(expanded_df) > 0 && ncol(expanded_df) > 0){
    out <- merge(out, expanded_df,
                 all = TRUE, by = names(expanded_df),
                 allow.cartesian = TRUE, sort = FALSE)
    if (sort){
      setorderv2(out, cols = names(expanded_df))
    }
  }
  # Replace NA with fill
  if (fill_na){
    fill <- fill[!is.na(fill)]
    fill_nms <- names(fill)
    for (i in seq_along(fill)){
      out[, (fill_nms[[i]]) := data.table::fifelse(is.na(get(fill_nms[[i]])),
                                                   fill[[i]],
                                                   get(fill_nms[[i]]))]
    }
  }
  out_order <- c(names(data),
                 setdiff(names(out),
                         names(data)))
  data.table::setcolorder(out, neworder = out_order)
  if (keep_class){
    out <- df_reconstruct(out, data)
  }
  out
}
expand_check <- function(N, log_limit){
  if (log10(N) >= log_limit || N > .Machine$integer.max){
    stop("Requested expansion results in >= ",
         N,
         " rows, aborting.")
  }
}
