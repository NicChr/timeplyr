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
#' @param data A data frame.
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
#' @param .overwrite If `TRUE` then groups supplied through `.by`
#' (as well as through `...`) overwrite existing dplyr groups.
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
                     .overwrite = FALSE,
                     as_qg = FALSE){
  N <- nrow2(data)
  group_vars <- group_vars(data)
  by_vars <- tidy_select_names(data, {{ .by }})
  dot_vars <- tidy_select_names(data, ...)
  # by and group vars cannot both be supplied (unless .overwrite = TRUE)
  if (length(by_vars) > 0L){
    if (!.overwrite && length(group_vars) > 0L){
      stop(".by cannot be used on a grouped_df")
    } else {
      group_vars2 <- by_vars
    }
  } else {
    group_vars2 <- group_vars
  }
  # If there tidy-selected variables have been renamed then rename them..
  if (length(dot_vars) > 0L && length(setdiff(dot_vars, names(data))) > 0L){
    data <- data %>%
      dplyr::select(dplyr::everything(), !!!enquos(...))
      # dplyr::select(all_of(group_vars2, !!!enquos(...)))

  } else {
    data <- collapse::fselect(data, c(group_vars2, dot_vars))
    # Make sure data is subset on columns we need (in correct order)
    # data <- data[, c(group_vars2, dot_vars), drop = FALSE]
  }
  needs_regrouping <- !isTRUE(sort &&
    length(group_vars) > 0L &&
    length(by_vars) == 0L &&
    length(dot_vars) == 0L)
  # If data contains lubridate interval, use dplyr grouping, otherwise collapse
  if (has_interval(data, quiet = TRUE)){
    if (!needs_regrouping){
      out <- dplyr::group_indices(data)
    } else {
      out <- data %>%
        dplyr::group_by(across(all_of(c(group_vars2, dot_vars)))) %>%
        dplyr::group_indices()
    }
    if (!sort) out <- collapse::group(out,
                                      group.sizes = FALSE,
                                      starts = FALSE)
  } else {
    if (length(group_vars2) == 0L &&
        length(dot_vars) == 0L){
      out <- rep_len(1L, nrow2(data))
      # Method for grouped_df
    } else if (!needs_regrouping){
      out <- dplyr::group_indices(data)
    } else {
      if (sort){
        out <- collapse::GRP(safe_ungroup(data),
                             by = c(group_vars2, dot_vars),
                             sort = TRUE,
                             decreasing = FALSE,
                             na.last = TRUE,
                             return.groups = FALSE,
                             return.order = FALSE,
                             method = "auto",
                             call = FALSE)[["group.id"]]
      } else {
        out <- collapse::group(safe_ungroup(data),
                               group.sizes = FALSE,
                               starts = FALSE)
      }

    }
  }
  if (as_qg){
    out <- collapse::qG(out, sort = sort, ordered = FALSE)
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
                         .overwrite = FALSE,
                         .name = NULL,
                         as_qg = FALSE){
  if (is.null(.name)) .name <- new_var_nm(names(data), "group_id")
  data[[.name]] <- group_id(data, !!!enquos(...),
                            sort = sort, .by = {{ .by }},
                            .overwrite = .overwrite,
                            as_qg = as_qg)
  data
}
