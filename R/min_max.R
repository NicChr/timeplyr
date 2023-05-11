#' min-max summary
#'
#' @description This is equivalent to
#' `data %>% group_by(...) %>% summarise(across(..., list(min, max)))`
#' but much faster and more efficient.
#' @param data A data frame.
#' @param ... Variables to calculate the min/max with.
#' @param .add Should the min/max variables be added to the dataset?
#' If `TRUE` the data is kept as is, and the min/max variables are simply
#' added as additional columns.
#' If `FALSE` (the default) a summary data frame of unique groups is returned,
#' containing the min/max values for each group.
#' @param sort Should the data frame be sorted by the groups?
#' The default is `FALSE`.
#' @param .by Alternative way of supplying groups using `tidyselect` notation.
#' This is kept to be consistent with other functions.
#' @return A summary data frame containing the min/max values for each group.
#' @examples
#' \dontrun{
#' library(timeplyr)
#' library(dplyr)
#' library(nycflights13)
#' flights <- flights %>%
#'   group_by(origin, dest, tailnum)
#' flights %>%
#'   min_max(dep_time, arr_time)
#'   bench::mark(e1 = flights %>%
#'                 min_max(dep_time, arr_time, sort = TRUE),
#'               e2 = flights %>%
#'                 summarise(across(c(dep_time, arr_time),
#'                                  list(~ min(.x, na.rm = TRUE),
#'                                       ~ max(.x, na.rm = TRUE)))),
#'               check = FALSE)
#' }
# min_max <- function(data, ..., .add = FALSE,
#                     sort = FALSE, .by = NULL){
#   # sort <- rlang::quo_is_null(enquo(.by))
#   n_dots <- dots_length(...)
#   out <- safe_ungroup(data)
#   group_vars <- get_groups(data, .by = {{ .by }})
#   if (.add){
#     .keep <- "all"
#   } else {
#    .keep <- "none"
#   }
#   out <- dplyr::mutate(out, across(all_of(group_vars)), ...,
#                        .keep = .keep)
#   dot_vars <- tidy_transform_names(data, ...)
#   if (!.add){
#     out <- collapse::fselect(out, c(group_vars, dot_vars))
#   }
#   if (length(group_vars) == 0L){
#     g <- NULL
#     gid <- rep_len(1L, nrow2(out))
#   } else {
#     g <- GRP2(out, sort = TRUE, by = group_vars)
#     gid <- g[["group.id"]]
#   }
#   out <- dplyr::mutate(out, across(dot_vars, ~ gmin(.x, g = g),
#                         .names = "{.col}_min"))
#   out <- dplyr::mutate(out, across(dot_vars, ~ gmax(.x, g = g),
#                                    .names = "{.col}_max"))
#   out <- collapse::fselect(out, setdiff(names(out), dot_vars))
#   # i <- which(growid(gid, g = g) == 1L)
#   # df_row_slice(out, i)
#   if (!.add){
#     grp_nm <- new_var_nm(names(out), ".group")
#     out[[grp_nm]] <- gid
#     out <- collapse::funique(out, cols = grp_nm, sort = sort)
#     out[[grp_nm]] <- NULL
#   }
#   if (.add && sort){
#     out <- df_row_slice(out, g[["order"]],
#                         reconstruct = FALSE)
#   }
#   out
# }
