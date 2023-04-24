# fgroup_by <- function(data, ..., .add = FALSE,
#                       order = TRUE,
#                       .by = NULL,
#                       .drop = dplyr::group_by_drop_default(data)){
#   if (.add){
#     group_data <- group_collapse(data, !!!enquos(...),
#                                  order = order,
#                                  loc = TRUE, sort = TRUE,
#                                  size = FALSE,
#                                  start = FALSE, end = FALSE,
#                                  .by = NULL)
#   } else {
#     group_data <- group_collapse(safe_ungroup(data), !!!enquos(...),
#                                  order = order,
#                                  loc = TRUE, sort = TRUE,
#                                  size = FALSE,
#                                  start = FALSE, end = FALSE,
#                                  .by = NULL)
#   }
#   group_data[[".group"]] <- NULL
#   names(group_data)[names(group_data) == ".loc"] <- ".rows"
#   attr(data, "groups") <- group_data
#   attr(attr(data, "groups"), ".drop") <- .drop
#   attr(data, "class") <- c("grouped_df", "tbl_df", "tbl", "data.frame")
#   data
# }
fgroup_by <- function(data, ..., .add = FALSE,
                      order = TRUE,
                      .by = NULL,
                      .drop = dplyr::group_by_drop_default(data)){
  n_dots <- dots_length(...)
  if (!.add){
    data <- safe_ungroup(data)
  }
  if (n_dots > 0){
    data <- dplyr::mutate(data, !!!enquos(...),
                          .by = {{ .by }})
  }
  group_info <- get_group_info(data, !!!enquos(...), .by = {{ .by }},
                               type = "data-mask")
  group_vars <- group_info[["dplyr_groups"]]
  data_vars <- group_info[["extra_groups"]]
  all_vars <- group_info[["all_groups"]]
  group_data <- group_collapse(data, all_of(data_vars),
                               order = order,
                               loc = TRUE, sort = TRUE,
                               size = FALSE,
                               start = FALSE, end = FALSE,
                               .by = {{ .by }})
  group_data[[".group"]] <- NULL
  names(group_data)[names(group_data) == ".loc"] <- ".rows"
  attr(data, "groups") <- group_data
  attr(attr(data, "groups"), ".drop") <- .drop
  attr(data, "class") <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  data
}
