##### data.table specific helpers #####

empty_dt <- function(){
  out <- list()
  class(out) <- c("data.table", "data.frame")
  data.table::setalloccol(out)
  out
  # collapse::qDT(empty_df()) # Faster
}

# This makes a copy
# Also data.tables currently can't have (n > 0) x 0 structure
new_dt <- function(..., .copy = TRUE, .recycle = FALSE){
  df_as_dt(
    new_df(..., .recycle = .recycle),
    .copy = .copy
  )
}

df_as_dt <- function(x, .copy = TRUE){
  out <- x
  if (.copy){
    out <- cpp_copy(out)
  }
  # Prefer collapse::qDT() to data.table::setalloccol()
  # Because the latter destroys time intervals
  out <- collapse::qDT(out, keep.attr = TRUE)
  data.table::setattr(out, "row.names", attr(x, "row.names"))
  out
}
# key and sort with na.last argument
# When cols = character(0) nothing is changed
# When cols = NULL the key is removed
setorderv2 <- function(x, cols, order = 1L, na.last = TRUE){
  if (length(cols) > 0L){
    data.table::setorderv(x, cols = cols, order = order, na.last = na.last)
  }
}
setkeyv2 <- function(x, cols, verbose = getOption("datatable.verbose"),
                     physical = TRUE){
  if (is.null(cols)){
    data.table::setkeyv(x, cols = NULL, verbose = verbose, physical = physical)
  } else {
    stopifnot(is.character(cols))
    if (length(cols) > 0L){
      data.table::setkeyv(x,
                          cols = cols,
                          verbose = verbose,
                          physical = physical)
    }
  }
}
# Slightly safer way of removing DT cols
set_rm_cols <- function(DT, cols = NULL){
  if (is.character(cols)){
    length_check <- length(intersect2(cols, names(DT))) > 0L
  } else {
    cols <- as.integer(cols)
    length_check <- length(intersect2(cols, seq_along(DT))) > 0L
  }
  if (length_check){
    data.table::set(DT, j = cols, value = NULL)
  }
}
set_add_cols <- function(DT, cols = NULL){
  data.table::set(DT, j = names(cols), value = cols)
}
dplyr_col_modify.data.table <- function(data, cols){
  cols <- vctrs::vec_recycle_common(!!!cols, .size = df_nrow(data))
  out <- as.list(vctrs::vec_data(data))
  nms <- rlang::as_utf8_character(rlang::names2(cols))
  names(out) <- rlang::as_utf8_character(rlang::names2(out))
  for (i in seq_along(cols)) {
    nm <- nms[[i]]
    out[[nm]] <- cols[[i]]
  }
  row_names <- .row_names_info(data, type = 0L)
  out <- vctrs::new_data_frame(out, n = df_nrow(data), row.names = row_names)
  dplyr::dplyr_reconstruct(out, data)
}
