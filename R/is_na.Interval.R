#' @exportS3Method cheapr::is_na
is_na.Interval <- function(x){
  cheapr::row_any_na(
    cheapr::new_df(
      start = interval_start(x),
      duration = unclass(x)
    )
  )
}
