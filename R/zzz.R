# .onLoad <- function(libname, pkgname){
#   register_all_s3_methods()
# }
# register_s3_method <- function(pkg, generic, class, fun = NULL){
#   stopifnot(is.character(pkg), length(pkg) == 1)
#   stopifnot(is.character(generic), length(generic) == 1)
#   stopifnot(is.character(class), length(class) == 1)
#
#   if (is.null(fun)) {
#     fun <- get(paste0(generic, ".", class), envir = parent.frame())
#   } else {
#     stopifnot(is.function(fun))
#   }
#
#   if (pkg %in% loadedNamespaces()){
#     registerS3method(generic, class, fun, envir = asNamespace(pkg))
#   }
#
#   # Always register hook in case package is later unloaded & reloaded
#   setHook(
#     packageEvent(pkg, "onLoad"),
#     function(...) {
#       registerS3method(generic, class, fun, envir = asNamespace(pkg))
#     }
#   )
# }
#
# register_all_s3_methods <- function(){
#   register_s3_method("collapse", "GRP", "Interval")
#   register_s3_method("collapse", "GRP", "NULL")
#   # register_s3_method("collapse", "funique", "time_interval")
#   # register_s3_method("collapse", "funique", "vctrs_rcrd")
#   register_s3_method("timeplyr", "roll_lag", "vctrs_rcrd")
#   register_s3_method("timeplyr", "roll_diff", "vctrs_rcrd")
#   register_s3_method("zoo", "rep", "yearmon")
#   register_s3_method("zoo", "rep.int", "yearmon")
#   register_s3_method("zoo", "rep_len", "yearmon")
#   register_s3_method("zoo", "rep", "yearqtr")
#   register_s3_method("zoo", "rep.int", "yearqtr")
#   register_s3_method("zoo", "rep_len", "yearqtr")
#   register_s3_method("pillar", "tbl_sum", "time_tbl_df")
#   register_s3_method("pillar", "tbl_sum", "episodes_tbl_df")
#   register_s3_method("base", "rep", "year_month")
#   register_s3_method("base", "rep.int", "year_month")
#   register_s3_method("base", "rep_len", "year_month")
#   register_s3_method("base", "print", "year_month")
#   register_s3_method("base", "+", "year_month")
#   register_s3_method("base", "-", "year_month")
#   register_s3_method("base", "c", "year_month")
#   register_s3_method("base", "format", "year_month")
#   register_s3_method("base", "as.character", "year_month")
#   register_s3_method("base", "unique", "year_month")
#   register_s3_method("base", "as.Date", "year_month")
#   register_s3_method("base", "as.POSIXct", "year_month")
#   register_s3_method("base", "as.POSIXlt", "year_month")
#   register_s3_method("base", "[", "year_month")
#   register_s3_method("base", "[[", "year_month")
#   register_s3_method("base", "rep", "year_quarter")
#   register_s3_method("base", "rep.int", "year_quarter")
#   register_s3_method("base", "rep_len", "year_quarter")
#   register_s3_method("base", "print", "year_quarter")
#   register_s3_method("base", "+", "year_quarter")
#   register_s3_method("base", "-", "year_quarter")
#   register_s3_method("base", "c", "year_quarter")
#   register_s3_method("base", "format", "year_quarter")
#   register_s3_method("base", "as.character", "year_quarter")
#   register_s3_method("base", "unique", "year_quarter")
#   register_s3_method("base", "as.Date", "year_quarter")
#   register_s3_method("base", "as.POSIXct", "year_quarter")
#   register_s3_method("base", "as.POSIXlt", "year_quarter")
#   register_s3_method("base", "[", "year_quarter")
#   register_s3_method("base", "[[", "year_quarter")
#
#   register_s3_method("base", "format", "time_interval")
#   register_s3_method("base", "as.character", "time_interval")
#   register_s3_method("base", "as.data.frame", "time_interval")
#   register_s3_method("base", "as.list", "time_interval")
#   register_s3_method("base", "as.Date", "time_interval")
#   register_s3_method("base", "as.POSIXct", "time_interval")
#   register_s3_method("base", "as.POSIXlt", "time_interval")
#   register_s3_method("base", "+", "time_interval")
#   register_s3_method("base", "-", "time_interval")
#   register_s3_method("base", "/", "time_interval")
#   # register_s3_method("base", "c", "time_interval")
#   # register_s3_method("base", "unique", "time_interval")
#   register_s3_method("base", "duplicated", "time_interval")
#   register_s3_method("base", "xtfrm", "time_interval")
#   register_s3_method("base", "sort", "time_interval")
#   register_s3_method("base", "print", "time_interval")
#   # register_s3_method("base", "rep", "time_interval")
#   # register_s3_method("base", "rep_len", "time_interval")
#   # register_s3_method("base", "rep.int", "time_interval")
#   # register_s3_method("base", "[", "time_interval")
#   # register_s3_method("base", "[[", "time_interval")
#   # register_s3_method("base", "is.na", "time_interval")
#   # register_s3_method("base", "[<-", "time_interval")
#   # register_s3_method("pillar", "type_sum", "time_interval")
#   # register_s3_method("pillar", "pillar_shaft", "time_interval")
#   register_s3_method("collapse", "GRP", "time_interval")
#   register_s3_method("vctrs", "vec_ptype_full", "time_interval")
#   register_s3_method("vctrs", "vec_ptype_abbr", "time_interval")
#   # register_s3_method("vctrs", "vec_ptype2", "time_interval.date")
#   register_s3_method("dplyr", "dplyr_reconstruct", "time_tbl_df")
#   register_s3_method("dplyr", "dplyr_reconstruct", "episodes_tbl_df")
#   register_s3_method("dplyr", "dplyr_row_slice", "time_tbl_df")
#   register_s3_method("dplyr", "dplyr_row_slice", "episodes_tbl_df")
#   register_s3_method("dplyr", "dplyr_col_modify", "data.table")
#   register_s3_method("cheapr", "is_na", "Interval")
# }
#
# on_package_load <- function(pkg, expr){
#   if (isNamespaceLoaded(pkg)){
#     expr
#   } else {
#     thunk <- function(...) expr
#     setHook(packageEvent(pkg, "onLoad"), thunk)
#   }
# }
.onAttach <- function(...){
  # .initial_options <- options()
  options("timeplyr.time_type" = getOption("timeplyr.time_type", "auto"),
          "timeplyr.roll_month" = getOption("timeplyr.roll_month", "preday"),
          "timeplyr.roll_dst" = getOption("timeplyr.roll_dst", "NA"),
          "timeplyr.interval_style" = getOption("timeplyr.interval_style", "full"),
          "timeplyr.interval_sub_formatter" = getOption("timeplyr.interval_sub_formatter", identity),
          "timeplyr.use_intervals" = getOption("timeplyr.use_intervals", FALSE))
}
.onUnload <- function(libname, pkgname){
  options(timeplyr.time_type = NULL,
          timeplyr.roll_month = NULL,
          timeplyr.roll_dst = NULL,
          timeplyr.interval_style = NULL,
          timeplyr.interval_sub_formatter = NULL,
          timeplyr.use_intervals = NULL)
}
