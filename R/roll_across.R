# roll_across <- function(.data, .cols, fun, ..., .names = "{.col}",
#                         .by = NULL){
#   rlang::check_required(fun)
#   .cols2 <- tidy_select_pos(.data, !!rlang::enquo(.cols))
#   roll_funs <- c(
#     "time_elapsed", "time_roll_mean", "time_id",
#     "time_roll_apply", "time_roll_growth_rate",
#     "roll_mean", "roll_sum", "roll_apply",
#     "roll_na_fill",
#     "roll_geometric_mean", "roll_harmonic_mean",
#     "roll_growth_rate", "flag2", "fdiff2"
#   )
#   is_function <- is.function(fun)
#   if (is_function){
#     if (!is.primitive(fun) && is.null(packageName(environment(fun)))){
#       stop("fun must not be anonymous, to supply additional arguments, use ...")
#     }
#     fun_string <- gsub("function\\(x.+\\) |\\(.+\\)$", "",
#                        deparse1(substitute(fun)), perl = TRUE)
#
#   } else {
#     fun_string <- as.character(substitute(fun))
#     fun <- match.fun(fun_string)
#   }
#   special_roll_fun <- fun_string %in% roll_funs
#   if (special_roll_fun){
#     rlang::inform(paste0("Optimised grouped rolling function ", fun_string, " will be used."))
#   }
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
#   if (special_roll_fun){
#     for (i in seq_along(out_nms)){
#       data.table::set(temp_out,
#                       j = out_nms[i],
#                       value =  do.call(fun, c(list(temp_out[[out_nms[i]]],
#                                                    g = sorted_groups), ...)))
#     }
#   } else {
#     for (i in seq_along(out_nms)){
#       data.table::set(temp_out,
#                       j = out_nms[i],
#                       value =
#                         do.call(roll_apply, list(temp_out[[out_nms[i]]],
#                                                  fun = function(x) fun(x, ...))))
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
