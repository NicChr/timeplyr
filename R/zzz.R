.onLoad <- function(libname, pkgname){
  register_all_s3_methods()
}
register_s3_method <- function(pkg, generic, class, fun = NULL){
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()){
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

register_all_s3_methods <- function(){
  register_s3_method("collapse", "GRP", "Interval")
  register_s3_method("collapse", "GRP", "NULL")
  register_s3_method("zoo", "rep", "yearmon")
  register_s3_method("zoo", "rep.int", "yearmon")
  register_s3_method("zoo", "rep_len", "yearmon")
  register_s3_method("zoo", "rep", "yearqtr")
  register_s3_method("zoo", "rep.int", "yearqtr")
  register_s3_method("zoo", "rep_len", "yearqtr")
  register_s3_method("pillar", "tbl_sum", "time_tbl_df")
  register_s3_method("pillar", "tbl_sum", "episodes_tbl_df")
  register_s3_method("timeplyr", "rep", "year_month")
  register_s3_method("timeplyr", "rep.int", "year_month")
  register_s3_method("timeplyr", "rep_len", "year_month")
  register_s3_method("timeplyr", "print", "year_month")
  register_s3_method("timeplyr", "+", "year_month")
  register_s3_method("timeplyr", "-", "year_month")
  register_s3_method("timeplyr", "c", "year_month")
  register_s3_method("timeplyr", "format", "year_month")
  register_s3_method("timeplyr", "as.character", "year_month")
  register_s3_method("timeplyr", "unique", "year_month")
  register_s3_method("timeplyr", "as.Date", "year_month")
  register_s3_method("timeplyr", "as.POSIXct", "year_month")
  register_s3_method("timeplyr", "as.POSIXlt", "year_month")
  register_s3_method("timeplyr", "[", "year_month")
  register_s3_method("timeplyr", "[[", "year_month")
  register_s3_method("timeplyr", "rep", "year_quarter")
  register_s3_method("timeplyr", "rep.int", "year_quarter")
  register_s3_method("timeplyr", "rep_len", "year_quarter")
  register_s3_method("timeplyr", "print", "year_quarter")
  register_s3_method("timeplyr", "+", "year_quarter")
  register_s3_method("timeplyr", "-", "year_quarter")
  register_s3_method("timeplyr", "c", "year_quarter")
  register_s3_method("timeplyr", "format", "year_quarter")
  register_s3_method("timeplyr", "as.character", "year_quarter")
  register_s3_method("timeplyr", "unique", "year_quarter")
  register_s3_method("timeplyr", "as.Date", "year_quarter")
  register_s3_method("timeplyr", "as.POSIXct", "year_quarter")
  register_s3_method("timeplyr", "as.POSIXlt", "year_quarter")
  register_s3_method("timeplyr", "[", "year_quarter")
  register_s3_method("timeplyr", "[[", "year_quarter")

  register_s3_method("timeplyr", "format", "time_interval")
  register_s3_method("timeplyr", "as.character", "time_interval")
  register_s3_method("timeplyr", "as.data.frame", "time_interval")
  register_s3_method("timeplyr", "as.list", "time_interval")
  register_s3_method("timeplyr", "as.Date", "time_interval")
  register_s3_method("timeplyr", "as.POSIXct", "time_interval")
  register_s3_method("timeplyr", "as.POSIXlt", "time_interval")
  register_s3_method("timeplyr", "+", "time_interval")
  register_s3_method("timeplyr", "-", "time_interval")
  register_s3_method("timeplyr", "/", "time_interval")
  # register_s3_method("timeplyr", "c", "time_interval")
  register_s3_method("timeplyr", "unique", "time_interval")
  register_s3_method("timeplyr", "duplicated", "time_interval")
  # register_s3_method("timeplyr", "sort", "time_interval")
  register_s3_method("timeplyr", "print", "time_interval")
  # register_s3_method("timeplyr", "rep", "time_interval")
  # register_s3_method("timeplyr", "rep_len", "time_interval")
  # register_s3_method("timeplyr", "rep.int", "time_interval")
  # register_s3_method("timeplyr", "[", "time_interval")
  # register_s3_method("timeplyr", "[[", "time_interval")
  # register_s3_method("timeplyr", "is.na", "time_interval")
  # register_s3_method("timeplyr", "[<-", "time_interval")
  # register_s3_method("pillar", "type_sum", "time_interval")
  # register_s3_method("pillar", "pillar_shaft", "time_interval")
  register_s3_method("collapse", "GRP", "time_interval")
  register_s3_method("vctrs", "vec_ptype_full", "time_interval")
  register_s3_method("vctrs", "vec_ptype_abbr", "time_interval")
  # register_s3_method("vctrs", "vec_ptype2", "time_interval.date")
}

on_package_load <- function(pkg, expr){
  if (isNamespaceLoaded(pkg)){
    expr
  } else {
    thunk <- function(...) expr
    setHook(packageEvent(pkg, "onLoad"), thunk)
  }
}
.onAttach <- function(...){
  options("timeplyr.time_type" = getOption("timeplyr.time_type", "auto"),
          "timeplyr.roll_month" = getOption("timeplyr.roll_month", "preday"),
          "timeplyr.roll_dst" = getOption("timeplyr.roll_dst", "boundary"),
          "timeplyr.interval_style" = getOption("timeplyr.interval_style", "full"),
          "timeplyr.interval_sub_formatter" = getOption("timeplyr.interval_sub_formatter", identity))

}
.onUnload <- function(libname, pkgname){
  options(timeplyr.time_type = NULL,
          timeplyr.roll_month = NULL,
          timeplyr.roll_dst = NULL,
          timeplyr.interval_style = NULL,
          timeplyr.interval_sub_formatter = NULL)
}
