#' Apply special grouped rolling functions to multiple columns - \bold{EXPERIMENTAL}
#'
#' @description
#' Some rolling functions in this package require data
#' to be sorted in a specific way prior to grouped calculations.
#' This can offer tremendous speed and efficiency, but due to the fact the data
#' is reordered after the calculation, this method makes less sense when
#' applied to many unsorted variables.
#' `roll_across` attempts to solve this bottleneck by sorting all the data
#' based on the supplied groups, apply the rolling functions, then reorder the
#' data back in the original order.
#'
#' @param .data A data frame.
#' @param .cols Variables to apply the function to, specified with `tidy-select`.
#' @param fun A function. Currently, the list of optimised rolling functions
#' include: \cr
#' "time_elapsed", "time_roll_mean", "time_id",
#' "time_roll_apply", "time_roll_growth_rate",
#' "roll_mean", "roll_sum", "roll_apply",
#' "roll_na_fill",
#' "roll_geometric_mean", "roll_harmonic_mean",
#' "roll_growth_rate", "flag2", "fdiff2"
#'
#' Any other function supplied is "rollified" by being passed to
#' `roll_apply()`.
#'
#' @param ... Additional arguments to pass to the function.
#' @param .names Column name specification. See `?dplyr::across` for more info.
#' @param .by Alternative way of specifying groups using `tidy-select`.
#'
roll_across <- function(.data, .cols, fun, ..., .names = "{.col}",
                        .by = NULL){
  rlang::check_required(fun)
  .cols2 <- tidy_select_pos(.data, !!rlang::enquo(.cols))
  roll_funs <- c(
    "time_elapsed", "time_roll_mean", "time_id",
    "time_roll_apply", "time_roll_growth_rate",
    "roll_mean", "roll_sum", "roll_apply",
    "roll_na_fill",
    "roll_geometric_mean", "roll_harmonic_mean",
    "roll_growth_rate", "flag2", "fdiff2"
  )
  is_function <- is.function(fun)
  if (is_function){
    if (!is.primitive(fun) && is.null(packageName(environment(fun)))){
      stop("fun must not be anonymous, to supply additional arguments, use ...")
    }
    fun_string <- gsub("function\\(x.+\\) |\\(.+\\)$", "",
                       deparse1(substitute(fun)), perl = TRUE)

  } else {
    fun_string <- as.character(substitute(fun))
    fun <- match.fun(fun_string)
  }
  special_roll_fun <- fun_string %in% roll_funs
  if (special_roll_fun){
    rlang::inform(paste0("Optimised grouped rolling function ", fun_string, " will be used."))
  }
  is_grouped_df <- inherits(.data, "grouped_df")
  if (is_grouped_df){
    group_df <- group_data(.data)
  }
  group_info <- group_info(.data, .by = {{ .by }},
                           .cols = .cols2,
                           ungroup = TRUE,
                           rename = TRUE,
                           unique_groups = FALSE,
                           dots_type = "tidy-select")
  out <- group_info[["data"]]
  group_vars <- group_info[["dplyr_groups"]]
  dot_vars <- group_info[["extra_groups"]]
  non_group_dot_vars <- setdiff(dot_vars, group_vars)
  has_groups <- length(group_vars) > 0

  if ("g" %in% names(args)){
    rlang::inform("g has been supplied, this will overwrite any existing groups")
    groups <- GRP2(args[["g"]])
  } else {
    groups <- df_to_GRP(.data, .cols = group_vars, order = TRUE)
  }
  if (is.null(groups)){
    N <- df_nrow(.data)
    groups <- sorted_group_id_to_GRP(seq_ones(N),
                                     n_groups = min(1L, N),
                                     group_sizes = N)
  }
  # Output names
  out_nms <- across_col_names(.cols = non_group_dot_vars,
                              .fns = fun_string,
                              .names = .names)
  # Initialise new variables if there are any

  if (length(setdiff(out_nms, names(out))) > 0){
    out <- fselect(out, .cols = c(setdiff(names(out), out_nms),
                                  add_names(non_group_dot_vars, out_nms)))

  }
  temp_out <- data.table::copy(
    collapse::qDT(
      fselect(out, .cols = out_nms)
    )
  )

  row_id_nm <- new_var_nm(temp_out, ".temp.row.id")
  group_id_nm <- new_var_nm(temp_out, ".temp.group.id")
  temp_out[, (row_id_nm) := seq_len(df_nrow(out))]
  temp_out[, (group_id_nm) := group_id(groups)]

  is_sorted <- GRP_is_sorted(groups)
  if (!is_sorted){
    data.table::setorderv(temp_out, cols = group_id_nm)
  }
  sorted_groups <- sorted_group_id_to_GRP(temp_out[[group_id_nm]],
                                          n_groups = GRP_n_groups(groups),
                                          group_sizes = GRP_group_sizes(groups))

  # Use special rolling functions or just use user supplied function?
  if (special_roll_fun){
    for (i in seq_along(out_nms)){
      data.table::set(temp_out,
                      j = out_nms[i],
                      value =  do.call(fun, c(list(temp_out[[out_nms[i]]],
                                                   g = sorted_groups), ...)))
    }
  } else {
    for (i in seq_along(out_nms)){
      data.table::set(temp_out,
                      j = out_nms[i],
                      value =
                        do.call(roll_apply, list(temp_out[[out_nms[i]]],
                                                 fun = \(x) fun(x, ...))))

    }
  }
  if (!is_sorted){
    data.table::setorderv(temp_out, cols = row_id_nm)
  }
  for (col in out_nms){
    out[[col]] <- temp_out[[col]]
  }
  if (is_grouped_df){
    attr(out, "groups") <- group_df
    attr(out, "class") <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  }
  out
}
# roll_across <- function(.data, .cols, fun, args = list(), .names = "{.col}",
#                         .by = NULL){
#   rlang::check_required(fun)
#   cols_quo <- rlang::enquo(.cols)
#   if (is.numeric(.cols) || is.character(.cols)){
#     .cols2 <- .cols
#   } else {
#     .cols2 <- tidy_select_pos(.data, !!cols_quo)
#   }
#   roll_funs <- c(
#     "time_elapsed", "time_roll_mean", "time_id",
#     "time_roll_apply", "time_roll_growth_rate",
#     "roll_mean", "roll_sum", "roll_apply",
#     "roll_na_fill",
#     "roll_geometric_mean", "roll_harmonic_mean",
#     "roll_growth_rate"
#   )
#   is_function <- is.function(fun)
#   if (is_function){
#     # fun_string <- gsub("function\\(x\\) |\\(.+\\)$", "",
#     #                    deparse1(as.list(match.call())[["fun"]]), perl = TRUE)
#     if (is.null(packageName(environment(fun)))){
#       stop("fun must not be anonymous, use args to supply additional arguments")
#     }
#     fun_string <- gsub("function\\(x.+\\) |\\(.+\\)$", "",
#                        deparse1(substitute(fun)), perl = TRUE)
#
#   } else {
#     fun_string <- as.character(substitute(fun))
#     fun <- get(fun_string)
#   }
#   # stopifnot(is.character(fun))
#   # fun_string <- fun
#   special_roll_fun <- fun_string %in% roll_funs
#   if (special_roll_fun){
#     rlang::inform(paste0("Optimised grouped rolling function ", fun_string, " will be used."))
#   }
#   # if (!valid_roll_fun){
#   #   rlang::abort(c("fun must be one of:",
#   #                  roll_funs))
#   # }
#   is_grouped_df <- inherits(.data, "grouped_df")
#   if (is_grouped_df){
#     group_df <- group_data(.data)
#   }
#   group_info <- group_info(.data, .by = {{ .by }},
#                            .cols = .cols2,
#                            ungroup = TRUE,
#                            rename = TRUE,
#                            unique_groups = FALSE,
#                            dots_type = "tidy-select")
#   out <- group_info[["data"]]
#   group_vars <- group_info[["dplyr_groups"]]
#   dot_vars <- group_info[["extra_groups"]]
#   non_group_dot_vars <- setdiff(dot_vars, group_vars)
#   has_groups <- length(group_vars) > 0
#
#   if ("g" %in% names(args)){
#     rlang::inform("g has been supplied, this will overwrite any existing groups")
#     groups <- GRP2(args[["g"]])
#   } else {
#     groups <- df_to_GRP(.data, .cols = group_vars, order = TRUE)
#   }
#   if (is.null(groups)){
#     N <- df_nrow(.data)
#     groups <- sorted_group_id_to_GRP(seq_ones(N),
#                                      n_groups = min(1L, N),
#                                      group_sizes = N)
#   }
#   # Output names
#   out_nms <- across_col_names(.cols = non_group_dot_vars,
#                               .fns = fun_string,
#                               .names = .names)
#   # Initialise new variables if there are any
#
#   if (length(setdiff(out_nms, names(out))) > 0){
#     out <- fselect(out, .cols = c(setdiff(names(out), out_nms),
#                                   add_names(non_group_dot_vars, out_nms)))
#
#   }
#   temp_out <- data.table::copy(
#     collapse::qDT(
#       fselect(out, .cols = out_nms)
#     )
#   )
#
#   row_id_nm <- new_var_nm(temp_out, ".temp.row.id")
#   group_id_nm <- new_var_nm(temp_out, ".temp.group.id")
#   temp_out[, (row_id_nm) := seq_len(df_nrow(out))]
#   temp_out[, (group_id_nm) := group_id(groups)]
#
#   is_sorted <- GRP_is_sorted(groups)
#   if (!is_sorted){
#     data.table::setorderv(temp_out, cols = group_id_nm)
#   }
#   sorted_groups <- sorted_group_id_to_GRP(temp_out[[group_id_nm]],
#                                           n_groups = GRP_n_groups(groups),
#                                           group_sizes = GRP_group_sizes(groups))
#
#   # Use special rolling functions or just use user supplied function?
#   if (!"g" %in% names(args)){
#     args <- c(args, list(g = sorted_groups))
#   }
#   if (special_roll_fun){
#     for (i in seq_along(out_nms)){
#       data.table::set(temp_out,
#                       j = out_nms[i],
#                       value =  do.call(fun, c(list(temp_out[[out_nms[i]]]),
#                                               args)))
#     }
#   } else {
#     for (i in seq_along(out_nms)){
#       data.table::set(temp_out,
#                       j = out_nms[i],
#                       value =
#                         do.call(roll_apply, c(list(temp_out[[out_nms[i]]]),
#                                               args, list(fun = fun))))
#
#     }
#   }
#   if (!is_sorted){
#     data.table::setorderv(temp_out, cols = row_id_nm)
#   }
#   for (col in out_nms){
#     out[[col]] <- temp_out[[col]]
#   }
#   if (is_grouped_df){
#     attr(out, "groups") <- group_df
#     attr(out, "class") <- c("grouped_df", "tbl_df", "tbl", "data.frame")
#   }
#   out
# }
# roll_across <- function(.data, ..., fun, args = list(), .names = "{.col}",
#                         .by = NULL, .cols = NULL){
#   rlang::check_required(fun)
#   # fun <- rlang::as_name(rlang::enquo(fun))
#   roll_funs <- c(
#     "time_elapsed", "time_roll_mean", "time_id",
#     "time_roll_apply",
#     "roll_mean", "roll_sum", "roll_apply",
#     "roll_na_fill",
#     "roll_geometric_mean", "roll_harmonic_mean"
#   )
#   # valid_roll_fun <- !collapse::allNA(stringr::str_match(deparse(substitute(fun)), roll_funs))
#   # if (!valid_roll_fun){
#   #   rlang::abort(c("fun must be one of:",
#   #                  roll_funs))
#   # }
#   # if (is.character(fun)){
#   #   fun_string <- fun
#   #   fun <- match.fun(fun)
#   #   # args <- formals(fun)
#   # } else {
#   #   # Extract function as string
#   #   if (is.function(fun)){
#   #     fun_args <- formals(fun)
#   #   }
#   #   fun_string <- gsub("function\\(x\\) |\\(.+\\)$", "",
#   #                      deparse1(as.list(match.call())[["fun"]]), perl = TRUE)
#   #   # fun <- match.fun(fun_string)
#   # }
#   stopifnot(is.character(fun))
#   fun_string <- fun
#   valid_roll_fun <- fun_string %in% roll_funs
#   if (!valid_roll_fun){
#     rlang::abort(c("fun must be one of:",
#                    roll_funs))
#   }
#   is_grouped_df <- inherits(.data, "grouped_df")
#   if (is_grouped_df){
#     group_df <- group_data(.data)
#   }
#
#   group_info <- group_info(.data, ..., .by = {{ .by }},
#                            .cols = .cols,
#                            ungroup = TRUE,
#                            rename = TRUE,
#                            unique_groups = FALSE,
#                            dots_type = "tidy-select")
#   out <- group_info[["data"]]
#   group_vars <- group_info[["dplyr_groups"]]
#   dot_vars <- group_info[["extra_groups"]]
#   non_group_dot_vars <- setdiff(dot_vars, group_vars)
#   has_groups <- length(group_vars) > 0
#
#   if ("g" %in% names(args)){
#     rlang::inform("g has been supplied, this will overwrite any existing groups")
#     groups <- args[["g"]]
#   } else {
#     groups <- df_to_GRP(.data, .cols = group_vars, order = TRUE)
#   }
#   # Output names
#   out_nms <- across_col_names(.cols = non_group_dot_vars,
#                               .fns = fun_string,
#                               .names = .names)
#   # Initialise new variables if there are any
#
#   if (length(setdiff(out_nms, names(out))) > 0){
#     out <- fselect(out, .cols = c(setdiff(names(out), out_nms),
#                                   add_names(non_group_dot_vars, out_nms)))
#
#   }
#
#   if (length(non_group_dot_vars) > 0){
#     # Use this function to do 3 things:
#     # Neatly sort the first variable
#     # Retrieve the sorted GRP
#     # Retrieve the order vector to sort the rest of the variables
#     sorted_group_info <- sort_data_by_GRP(out[[non_group_dot_vars[1L]]],
#                                           g = groups, sorted_group_starts = FALSE)
#     sorted_groups <- sorted_group_info[["sorted_GRP"]]
#     is_sorted <- sorted_group_info[["sorted"]]
#     out[[out_nms[1L]]] <- sorted_group_info[["x"]]
#     ## Now we sort the rest of the variables
#     group_order <- sorted_group_info[["group_order"]]
#     if (!is_sorted){
#       for (i in seq_len(length(non_group_dot_vars) - 1)){
#         out[[out_nms[i + 1]]] <- out[[out_nms[i + 1]]][group_order]
#       }
#     }
#
#     # out <- sorted_group_info[["x"]]
#     for (i in seq_along(non_group_dot_vars)){
#       out[[out_nms[i]]] <- do.call(fun, c(list(out[[out_nms[i]]]),
#                                           args, list(g = sorted_groups)))
#       # out[[out_nms[i]]] <- do.call(fun, c(list(out[[non_group_dot_vars[i]]]),
#       #                                     args, list(g = sorted_groups)))
#     }
#     if (!is_sorted){
#       for (col in out_nms){
#         out[[col]] <- greorder2(out[[col]], g = groups)
#       }
#       # out <- df_reorder(out, g = groups)
#     }
# }
#   if (is_grouped_df){
#     attr(out, "groups") <- group_df
#     attr(out, "class") <- c("grouped_df", "tbl_df", "tbl", "data.frame")
#   }
#   out
# }
# roll_across2 <- function(.data, .cols, fun, args = list(), .names = "{.col}",
#                         .by = NULL){
#   rlang::check_required(fun)
#   if (is.numeric(.cols) || is.character(.cols)){
#     .cols2 <- .cols
#   } else {
#     .cols2 <- tidy_select_pos(.data, !!!rlang::enquos(.cols))
#   }
#   roll_funs <- c(
#     "time_elapsed", "time_roll_mean", "time_id",
#     "time_roll_apply",
#     "roll_mean", "roll_sum", "roll_apply",
#     "roll_na_fill",
#     "roll_geometric_mean", "roll_harmonic_mean"
#   )
#   stopifnot(is.character(fun))
#   fun_string <- fun
#   valid_roll_fun <- fun_string %in% roll_funs
#   if (!valid_roll_fun){
#     rlang::abort(c("fun must be one of:",
#                    roll_funs))
#   }
#   is_grouped_df <- inherits(.data, "grouped_df")
#   if (is_grouped_df){
#     group_df <- group_data(.data)
#   }
#
#   group_info <- group_info(.data, .by = {{ .by }},
#                            .cols = .cols2,
#                            ungroup = TRUE,
#                            rename = TRUE,
#                            unique_groups = FALSE,
#                            dots_type = "tidy-select")
#   out <- group_info[["data"]]
#   group_vars <- group_info[["dplyr_groups"]]
#   dot_vars <- group_info[["extra_groups"]]
#   non_group_dot_vars <- setdiff(dot_vars, group_vars)
#   has_groups <- length(group_vars) > 0
#
#   if ("g" %in% names(args)){
#     rlang::inform("g has been supplied, this will overwrite any existing groups")
#     groups <- GRP2(args[["g"]])
#   } else {
#     groups <- df_to_GRP(.data, .cols = group_vars, order = TRUE)
#   }
#   # Output names
#   out_nms <- across_col_names(.cols = non_group_dot_vars,
#                               .fns = fun_string,
#                               .names = .names)
#   # Initialise new variables if there are any
#
#   if (length(setdiff(out_nms, names(out))) > 0){
#     out <- fselect(out, .cols = c(setdiff(names(out), out_nms),
#                                   add_names(non_group_dot_vars, out_nms)))
#
#   }
#   temp_out <- as_DT(out)
#   row_id_nm <- new_var_nm(temp_out, ".temp.row.id")
#   group_id_nm <- new_var_nm(temp_out, ".temp.group.id")
#   temp_out[, (row_id_nm) := seq_len(df_nrow(temp_out))]
#   temp_out[, (group_id_nm) := group_id(groups)]
#
#   is_sorted <- GRP_is_sorted(groups)
#   if (!is_sorted){
#     data.table::setorderv(temp_out, cols = group_id_nm)
#   }
#   sorted_groups <- sorted_group_id_to_GRP(temp_out[[group_id_nm]],
#                                           n_groups = GRP_n_groups(groups),
#                                           group_sizes = GRP_n_groups(groups))
#   for (i in seq_along(out_nms)){
#     data.table::set(temp_out,
#                     j = out_nms[i],
#                     value =  do.call(fun, c(list(temp_out[[out_nms[i]]]),
#                                             args, list(g = sorted_groups))))
#   }
#   if (!is_sorted){
#     data.table::setorderv(temp_out, cols = row_id_nm)
#   }
#   for (col in out_nms){
#     out[[col]] <- temp_out[[col]]
#   }
#   set_rm_cols(temp_out, cols = c(row_id_nm, group_id_nm))
#   # out <- df_reconstruct(out, safe_ungroup(.data))
#   if (is_grouped_df){
#     attr(out, "groups") <- group_df
#     attr(out, "class") <- c("grouped_df", "tbl_df", "tbl", "data.frame")
#   }
#   out
# }
