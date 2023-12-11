# Time intervals (A near-future work in progress)


time_interval <- function(start, end){
  out <- recycle_args(start = start, end = end)
  attributes(out) <- list(class = c("time_interval", "tbl_df", "tbl", "data.frame"),
                          row.names = c(NA_integer_, -length(out[[1L]])),
                          names = c("start", "end"))
  out
}


`+.time_interval` <- function(e1, e2){
  e1[["start"]] <- time_add2(e1[["start"]], e2)
  e1[["end"]] <- time_add2(e1[["end"]], e2)
  e1
}


`-.time_interval` <- function(e1, e2){
  time_by <- time_by_list(e2)
  time_by <- add_names(
    list(-time_by_num(time_by)
    ),
    time_by_unit(time_by)
  )
  e1[["start"]] <- time_add2(e1[["start"]], time_by)
  e1[["end"]] <- time_add2(e1[["end"]], e2)
  e1
}


as.character.time_interval <- function(x, ...){
  start <- x[["start"]]
  end <- x[["end"]]
  n <- length(start)
  out <- paste0("[", start, "--", end, ")")
  out[n] <- paste0("[", start[n], "--", end[n], "]")
  out
}

format.time_interval <- function(x, ...){
  format(as.character(x), ...)
}


print.time_interval <- function(x, max = NULL, ...){
  N <- df_nrow(x)
  out <- list_to_data_frame(unclass(x))
  if (is.null(max)){
    max <- getOption("max.print", 9999L)
  }
  max <- min(max, N)
  if (max < N){
    # out <- out[seq_len(max)]
    out <- fslice(out, seq_len(max))
    additional_msg <- paste(" [ reached 'max' / getOption(\"max.print\") -- omitted",
                            N - max, "entries ]\n")
  } else {
    additional_msg <- character()
  }
  out <- time_interval(out[[1L]], out[[2L]])
  print(as.character(out), max = max + 1, ...)
  cat(additional_msg)
  invisible(x)
}


`[.time_interval` <- function(x, ..., drop = TRUE){
  df_row_slice(x, ...)
}


# This is the vector based alternative, probably easier to use this instead
# new_time_interval <- function(start, end){
#   interval <- recycle_args(start = start, end = end)
#   out <- interval[["start"]]
#   end <- interval[["end"]]
#   attributes(out) <- list(class = "time_interval", end = end)
#   out
# }
#
# `+.time_interval` <- function(e1, e2){
#   end <- attr(e1, "end")
#   out <- time_add2(e1, e2)
#   attr(out, "end") <- time_add2(end, e2)
#   out
# }
# `-.time_interval` <- function(e1, e2){
#   time_by <- time_by_list(e2)
#   time_by <- add_names(
#     list(-time_by_num(time_by)
#     ),
#     time_by_unit(time_by)
#   )
#   end <- attr(e1, "end")
#   out <- time_add2(e1, e2)
#   attr(out, "end") <- time_add2(end, time_by)
#   out
# }
# as.character.time_interval <- function(x, ...){
#   end <- attr(x, "end")
#   start <- x
#   n <- length(start)
#   out <- paste0("[", start, "--", end, ")")
#   out[n] <- paste0("[", start[n], ", ", end[n], "]")
#   out
# }
# format.time_interval <- function(x, ...){
#   format(as.character(x), ...)
# }
# print.time_interval <- function(x, max = NULL, ...){
#   N <- length(x)
#   out <- x
#   if (is.null(max)){
#     max <- getOption("max.print", 9999L)
#   }
#   max <- min(max, N)
#   if (max < N){
#     i <- seq_len(max)
#     out <- out[i]
#     end <- end[i]
#     additional_msg <- paste(" [ reached 'max' / getOption(\"max.print\") -- omitted",
#                             N - max, "entries ]\n")
#   } else {
#     additional_msg <- character()
#   }
#   attr(out, "end") <- end
#   print(as.character(out), max = max + 1, ...)
#   cat(additional_msg)
#   invisible(x)
# }
# `[.time_interval` <- function(x, ..., drop = TRUE){
#   cl <- oldClass(x)
#   class(x) <- NULL
#   val <- NextMethod("[")
#   class(val) <- cl
#   val
# }
# `[[.time_interval` <- function(x, ..., drop = TRUE){
#   cl <- oldClass(x)
#   class(x) <- NULL
#   val <- NextMethod("[")
#   class(val) <- cl
#   val
# }
