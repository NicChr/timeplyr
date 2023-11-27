##### data.table specific helpers #####

# set_dt_threads <- function(threads = 1L){
#   setDTthreads <- try(get("setDTthreads", asNamespace("data.table")))
#   if (exists("setDTthreads", inherits = FALSE)){
#     setDTthreads(threads = threads)
#   }
# }

# Convert to data table
as_DT <- function(x){
  if (inherits(x, "data.table")){
    x[TRUE]
  } else if (inherits(x, "data.frame") &&
             collapse::fncol(x) > 0L){
    collapse::qDT(x[TRUE])
  } else {
    data.table::as.data.table(x)
  }
}
# df_complete_cases <- function(data, .cols = names(data)){
#   df_row_slice(data, vctrs::vec_detect_complete(
#     fselect(data, .cols = .cols)
#   ))
#   # df_row_slice(data, collapse::whichv(rowSums(is.na(
#   #   fselect(data, .cols = .cols)
#   # )), 0))
# }

# key and sort with na.last argument
#
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

# Data.table version of bind_cols, needs more work
# set_bind_cols <- function(x, y,
#                           suffix = ".y"){
#   if (missing(y)) return(x)
#   x_nms <- names(x)
#   y_nms <- names(y)
#   common_cols <- intersect(x_nms, y_nms)
#   suffix <- rep_len(suffix, length(common_cols))
#   new_col_nms <- paste0(common_cols, suffix)
#   y_nms[y_nms %in% common_cols] <- new_col_nms
#   names(y) <- y_nms
#   data.table::set(x, j = y_nms, value = y)[]
# }
