
# time_interval3 <- function(start, end){
#   out <- time_interval3_list(start, end)
#   # attributes(out) <- list(class = c("time_interval3", "data.frame"),
#   #                         row.names = c(NA_integer_, -length(out[[1L]])),
#   #                         names = c("start", "end"))
#   # attributes(out) <- list(class = "data.frame",
#   #                         row.names = c(NA_integer_, -length(out[[1L]])),
#   #                         names = c("start", "end"))
#   vctrs::new_rcrd(out, class = "time_interval3")
#   # out
# }
# `+.time_interval3` <- function(e1, e2){
#   e1[["start"]] <- time_add2(e1[["start"]], e2)
#   e1[["end"]] <- time_add2(e1[["end"]], e2)
#   e1
# }
# `-.time_interval3` <- function(e1, e2){
#   time_by <- time_by_list(e2)
#   time_by <- add_names(
#     list(-time_by_num(time_by)
#     ),
#     time_by_unit(time_by)
#   )
#   e1[["start"]] <- time_add2(e1[["start"]], time_by)
#   e1[["end"]] <- time_add2(e1[["end"]], e2)
#   e1
# }
# `/.time_interval3` <- function(e1, e2){
#   time_diff(e1[["start"]], e1[["end"]], time_by = e2)
# }
# as.character.time_interval3 <- function(x, ...){
#   start <- vctrs::field(x, "start")
#   end <- vctrs::field(x, "end")
#   paste0("[", start, "--", end, ")")
# }
# format.time_interval3 <- function(x, ...){
#   format(as.character(x), ...)
# }
# print.time_interval3 <- function(x, max = NULL, ...){
#   N <- df_nrow(x)
#   out <- list_to_data_frame(unclass(x))
#   if (is.null(max)){
#     max <- getOption("max.print", 9999L)
#   }
#   max <- min(max, N)
#   if (max < N){
#     out <- fslice(out, seq_len(max))
#     additional_msg <- paste(" [ reached 'max' / getOption(\"max.print\") -- omitted",
#                             N - max, "entries ]\n")
#   } else {
#     additional_msg <- character()
#   }
#   out <- time_interval3(out[[1L]], out[[2L]])
#   print(as.character(out), max = max + 1, ...)
#   cat(additional_msg)
#   invisible(x)
# }
# `[.time_interval3` <- function(x, ..., drop = TRUE){
#   df_row_slice(x, ...)
# }
# `[<-.time_interval3` <- function(x, i, value){
#   x[["start"]][i] <- value[["start"]]
#   x[["end"]][i] <- value[["end"]]
#   x
# }
# c.time_interval3 <- function(...){
#   vctrs::vec_rbind(...)
# }
# length.time_interval3 <- function(x, i, value){
#   df_nrow(x)
# }
# type_sum.time_interval3 <- function(x) {
#   "time_intrv"
# }
# pillar_shaft.time_interval3 <- function(x, ...) {
#   out <- format(x)
#   pillar::new_pillar_shaft_simple(out, align = "right")
# }

# time_interval3_list <- function(start, end){
#   set_time_cast(start, end)
#   if (typeof(start) == "double" && (
#     collapse::anyv(unclass(start), Inf) ||
#     collapse::anyv(unclass(start), -Inf)
#   )){
#     stop("start must be finite")
#   }
#   recycle_args(start = start, end = end)
# }
# vec_proxy.time_interval3 <- function(x) {
#   as.data.frame(unclass(x))
# }
