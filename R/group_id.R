#' Fast group IDs
#'
#' @description
#' This is an extension to `dplyr::group_indices()` to allow for additional
#' groups. \cr
#' `group_id()` returns an integer vector of group IDs and
#' `add_group_id()` adds an integer column of group IDs.
#' dplyr group indices are returned by default unless additional
#' groups are specified through `...` and/or `.by`.
#' It's very fast and takes significantly less memory
#' than using `dplyr::group_by()` and `group_indices()`.
#' @param data A data frame or vector.
#' @param ... Additional groups using tidy select notation.
#' @param sort Should the order of the groups be retained?
#' The input of the data is never changed but if sort is
#' `TRUE` (the default) then the order of the group IDs will be sorted.
#' If `FALSE` the order of the group IDs will be based on first appearance.
#' @param .by Alternative way of supplying groups using tidy
#' select notation. This is kept to be consistent with other functions.
#' @param .name Name of the added group ID column which should be a
#' character vector of length 1.
#' If `NULL` then a column named "group_id" will be added,
#' and if one already exists, a unique name will be used.
#' @param as_qg Should the group IDs be returned as a
#' collapse "qG" class? The default (`FALSE`) always returns
#' an integer vector.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(ggplot2)
#' group_id(iris) # No groups
#' group_id(iris, Species) # Species groups
#' iris %>%
#'   group_by(Species) %>%
#'   group_id() # Same thing
#' group_id(iris, where(is.numeric)) # Groups across numeric values
#'
#' iris %>%
#'   add_group_id(Species) %>%
#'   distinct(Species, group_id)
#'
#' mm_mpg <- mpg %>%
#'   select(manufacturer, model) %>%
#'   arrange(desc(pick(everything())))
#'
#' # Sorted/non-sorted groups
#' mm_mpg %>%
#'   add_group_id(everything(),
#'                .name = "sorted_id", sort = TRUE) %>%
#'   add_group_id(manufacturer, model,
#'                .name = "not_sorted_id", sort = FALSE) %>%
#'   distinct()
#' @rdname group_id
#' @export
group_id <- function(data, ...,
                     sort = TRUE,
                     .by = NULL,
                     .name = NULL,
                     as_qg = FALSE){
  UseMethod("group_id")
}
#' @export
group_id.default <- function(data, ..., sort = TRUE, as_qg = FALSE){
  out <- GRP2(safe_ungroup(data),
              sort = sort,
              decreasing = FALSE,
              na.last = TRUE,
              return.groups = FALSE,
              return.order = FALSE,
              method = "auto",
              call = FALSE)[["group.id"]]
  if (as_qg){
    out <- collapse::qG(out, sort = sort, ordered = FALSE, na.exclude = FALSE)
  }
  out
}
#' @export
group_id.Interval <- function(data, ..., sort = TRUE, as_qg = FALSE){
  data <- GRP.Interval(data, sort = sort,
                       call = FALSE, return.groups = FALSE)[["group.id"]]
  group_id.default(data, ..., sort = sort, as_qg = as_qg)
}
#' @export
group_id.data.frame <- function(data, ...,
                                sort = TRUE,
                                .by = NULL,
                                as_qg = FALSE){
  N <- nrow2(data)
  group_vars <- group_vars(data)
  by_vars <- tidy_select_names(data, {{ .by }})
  dot_vars <- tidy_select_names(data, !!!enquos(...))
  if (length(by_vars) > 0L){
    if (length(group_vars) > 0L){
      stop(".by cannot be used on a grouped_df")
    }
  }
  select_info <- tidy_select_info(data, all_of(c(group_vars, by_vars)),
                                  !!!enquos(...))
  pos <- select_info[["pos"]]
  in_nms <- select_info[["in_nms"]]
  out_nms <- select_info[["out_nms"]]
  data <- collapse::fselect(data, pos)
  names(data) <- out_nms
  # Group var might have been renamed, so use select info
  group_vars2 <- out_nms[match(c(group_vars, by_vars), in_nms)]
  # by and group vars cannot both be supplied (unless .overwrite = TRUE)
  needs_regrouping <- !isTRUE(sort &&
                                length(group_vars) > 0L &&
                                length(by_vars) == 0L &&
                                length(dot_vars) == 0L)
    # Usual Method for when data does not contain interval
    if (length(group_vars2) == 0L &&
        length(dot_vars) == 0L){
      out <- rep_len(1L, N)
      # Method for grouped_df
    } else if (!needs_regrouping){
      out <- dplyr::group_indices(data)
    } else {
      out <- GRP2(safe_ungroup(data),
                  sort = sort,
                  decreasing = FALSE,
                  na.last = TRUE,
                  return.groups = FALSE,
                  return.order = FALSE,
                  method = "auto",
                  call = FALSE)[["group.id"]]
    }
  if (as_qg){
    out <- collapse::qG(out, sort = sort, ordered = FALSE, na.exclude = FALSE)
  } else {
    out <- as.integer(out)
  }
  out
}
#' @rdname group_id
#' @export
add_group_id <- function(data, ...,
                         sort = TRUE,
                         .by = NULL,
                         .name = NULL,
                         as_qg = FALSE){
  if (is.null(.name)) .name <- new_var_nm(names(data), "group_id")
  data[[.name]] <- group_id(data, !!!enquos(...),
                            sort = sort, .by = {{ .by }},
                            as_qg = as_qg)
  data
}
GRP.Interval <- function(X, ...){
  X <- dplyr::tibble(!!"start" := lubridate::int_start(X),
                     !!"data" := lubridate::int_length(X))
  collapse::GRP(X, ...)
}
# data frame Wrapper around GRP to convert lubridate intervals to group IDs
GRP2 <- function(X, ...){
  if (is_df(X) && has_interval(X, quiet = TRUE)){
    which_int <- which(purrr::map_lgl(X, lubridate::is.interval))
    for (i in seq_along(which_int)){
      X[[which_int[[i]]]] <- group_id.Interval(X[[which_int[[i]]]],
                                               sort = TRUE, as_qg = FALSE)
    }
    collapse::GRP(X, ...)
  } else {
    do.call(get("GRP", asNamespace("collapse")),
            as.list(match.call())[-1L],
            envir = parent.frame())
  }
}
