#' @exportS3Method cheapr::is_na
is_na.Interval <- function(x){
  cheapr::row_any_na(
    new_df(
      start = interval_start(x),
      duration = unclass(x)
    )
  )
}
