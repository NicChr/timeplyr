#' Fast versions of `tidyr::expand()` and `tidyr::complete()`.
#'
#' @details
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
#' @returns
#' A `data.frame` of expanded groups.
#'
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(lubridate)
#' library(nycflights13)
#' \dontshow{
#' .n_dt_threads <- data.table::getDTthreads()
#' .n_collapse_threads <- collapse::get_collapse()$nthreads
#' data.table::setDTthreads(threads = 2L)
#' collapse::set_collapse(nthreads = 1L)
#' }
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
#' \dontshow{
#' data.table::setDTthreads(threads = .n_dt_threads)
#' collapse::set_collapse(nthreads = .n_collapse_threads)
#'}
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
    summarise_vars <- c(add_names(
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
    out <- nested_join(summarise_vars, N = df_nrow(data),
                       sort = sort,
                       log_limit = log_limit)
  } else {
    # Method for grouped data which performs a separate cross-join of
    # non-grouped variables for each group
    if (length(group_vars) > 0L && length(leftover_grp_nms) >= 2L){
      out1 <- nested_join(summarise_vars, N = df_nrow(data),
                          sort = FALSE,
                          log_limit = log_limit)
      # Add group ID
      grp_nm <- new_var_nm(out1, ".group.id")
      out1[, (grp_nm) := group_id(.SD, order = FALSE, .cols = names(.SD)),
           .SDcols = group_vars]
      setorderv2(out1, cols = grp_nm)
      # Add group IDs for each non-group variable
      # This will allow us to calculate final expanded size
      for (i in seq_along(leftover_grp_nms)){
        assign(paste0("grp_nm_", i),
               new_var_nm(out1, ".group.id"))
        out1[, (get(paste0("grp_nm_", i))) := group_id(.SD, order = FALSE, .cols = names(.SD)),
             .SDcols = leftover_grp_nms[[i]]]
      }
      group_id_nms <- unlist(mget(paste0("grp_nm_",
                                         seq_len(length(leftover_grp_nms)))),
                             recursive = FALSE, use.names = FALSE)
      # Figure out final size before expansion, to do this we can
      # Calculate the vector product of unique expanded elements across groups.
      out_temp <- collapse::fndistinct(fselect(out1, .cols = group_id_nms),
                                       g = out1[[grp_nm]],
                                       use.g.names = FALSE, na.rm = FALSE)
      sizes <- rowProds(out_temp)
      expanded_nrow <- sum(sizes)
      expand_check(expanded_nrow, log_limit)
      data.table::setkeyv(out1, cols = grp_nm)
      out2 <- out1[, lapply(.SD, function(x) list(collapse::funique(x))),
                   keyby = grp_nm,
                   .SDcols = group_id_nms]
      # # This is fastest but can't get it to work in the package
      # out <- out2[, .Call(Ccj, unlist(.SD, recursive = FALSE, use.names = FALSE)),
      #             keyby = grp_nm,
      #             .SDcols = group_id_nms]
      out <- out2[, CJ2(unlist(.SD, recursive = FALSE, use.names = FALSE)),
                  keyby = grp_nm,
                  .SDcols = group_id_nms]
      out <- frename(out, .cols = add_names(names(out), c(grp_nm, leftover_grp_nms)))
      for (i in seq_along(group_id_nms)){
        grp_to_modify <- leftover_grp_nms[[i]]
        grp_to_match_on <- group_id_nms[[i]]
        data.table::set(out, j = grp_to_modify,
                        value = out1[[grp_to_modify]][
                          collapse::fmatch(out[[grp_to_modify]],
                                           out1[[grp_to_match_on]],
                                           overid = 2L)
                        ])
      }
      for (i in seq_along(group_vars)){
        data.table::set(out, j = group_vars[[i]],
                        value = out1[[group_vars[[i]]]][
                          collapse::fmatch(out[[grp_nm]],
                                           out1[[grp_nm]],
                                           overid = 2L)
                        ])
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
  out <- fselect(out, .cols = out_nms)
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
  check_length(N, 1L)
  # Data variables
  data_nms <- X_nms[(X_lens %% N) == 0]
  # Newly created variables
  other_nms <- X_nms[!X_nms %in% data_nms]
  df <- data.table::as.data.table(X[X_nms %in% data_nms])
  n_data <- df_nrow(df)
  if (n_data > 0L){
    df <- collapse::funique(df)
    n_data <- df_nrow(df)
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
  if (df_nrow(df) == 0L){
    out <- crossed_join(X_other, unique = FALSE)
  } else {
    out <- df_row_slice(df, rep(grp_seq, each = n_other))
    if (length(X_other) > 0L){
      rep_times <- df_nrow(out) / collapse::vlengths(X_other, use.names = FALSE)
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
  if (df_nrow(expanded_df) > 0 && df_ncol(expanded_df) > 0){
    out <- dplyr::full_join(out, expanded_df, by = names(expanded_df))
    # out <- collapse_join(out, expanded_df,
    #                      on = names(expanded_df),
    #                      how = "full",
    #                      sort = sort)
    if (sort){
      out <- farrange(out, .cols = names(expanded_df))
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
  out <- fselect(out, .cols = out_order)
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
