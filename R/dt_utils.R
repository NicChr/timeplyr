##### data.table specific helpers #####

df_as_dt <- function(x, .copy = TRUE){
  out <- x
  if (.copy){
    out <- cheapr::deep_copy(out)
  }
  # Prefer collapse::qDT() to data.table::setalloccol()
  # Because the latter destroys time intervals
  out <- collapse::qDT(out, keep.attr = TRUE)
  data.table::setattr(out, "row.names", attr(x, "row.names"))
  out
}

# This makes a copy
# Also data.tables currently can't have (n > 0) x 0 structure
new_dt <- function(..., .copy = TRUE, .recycle = FALSE){
  df_as_dt(
    cheapr::new_df(..., .recycle = .recycle),
    .copy = .copy
  )
}
#' @exportS3Method dplyr::dplyr_col_modify
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
