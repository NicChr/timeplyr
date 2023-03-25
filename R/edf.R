#' Grouped empirical cumulative distribution function applied to data
#'
#' @description Like `dplyr::cume_dist(x)` and `ecdf(x)(x)`
#' but with added grouping functionality.
#' @param x Numeric vector.
#' @param g Numeric vector of group IDs.
#' @param wt Frequency weights.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' x <- sample(seq(-10, 10, 0.5), size = 10^2, replace = TRUE)
#' plot(sort(edf(x)))
#' all.equal(edf(x), ecdf(x)(x))
#' all.equal(edf(x), cume_dist(x))
#' \dontrun{
#' library(microbenchmark)
#' microbenchmark(edf(x), cume_dist(x))
#' }
#' g <- sample(letters[1:3], size = 10^2, replace = TRUE)
#'
#' edf1 <- tibble(x, g) %>%
#'   mutate(edf = cume_dist(x),
#'          .by = g) %>%
#'   pull(edf)
#' edf2 <- edf(x, g = g)
#' all.equal(edf1, edf2)
#' @export
edf <- function(x, g = NULL, wt = NULL){
  n_na <- sum(is.na(x))
  if (is.null(g)){
    x_order <- radix_order(x)
    x <- x[x_order]
    if (n_na > 0) x <- x[seq_len(length(x) - n_na)]
    times <- collapse::GRPN(x, expand = FALSE)
    ### No weights
    if (is.null(wt)){
      grpn <- times
      N <- length(x)
    } else {
      ### With weights
      if (!length(wt) %in% c(1, length(x))){
        stop("wt must be of length 1 or length(x)")
      }
      if (length(wt) == 1L){
        wt <- rep_len(wt, length(x))
      }
      grpn <- times * wt
      N <- sum(wt)
    }
    sum_run <- rep(collapse::fcumsum(grpn, na.rm = FALSE),
                   times = times)
    out <- sum_run / N
    if (n_na > 0) out <- c(out, rep_len(NA_real_, n_na))
    out <- out[radix_order(x_order)]
  } else {
    # Create group IDs
    df <- data.table::data.table(x, g, wt)
    df[, ("g1") := group_id(df, all_of("x"), sort = TRUE,
                            as_qg = FALSE)]
    df[, ("g3") := group_id(df, all_of(c("g", "g1")),
                            sort = TRUE, as_qg = FALSE)]
    # Original order
    df[, ("id") := seq_len(.N)]
    # Order if NAs are shifted to the end
    which_na <- which(is.na(x))
    df[, ("id") := data.table::fifelse(is.na(get("x")), NA_integer_,
                                       get("id"))]
    # Sort data in ascending order
    data.table::setorderv(df, cols = "g3")
    if (n_na > 0) df <- df[!is.na(get("x"))]
    # Group sizes
    grp_n2 <- collapse::GRPN(df[["g"]], expand = TRUE)
    times <- collapse::GRPN(df[["g3"]], expand = FALSE)
    if (is.null(wt)){
      grp_n3 <- times
      N <- grp_n2
    } else {
      grp_n3 <- times * df[["wt"]]
      N <- gsum(df[["wt"]], g = df[["g"]])
    }

    sum_run <- rep(collapse::fcumsum(grp_n3, na.rm = FALSE,
                                      g = collapse::funique(df,
                                                            cols = "g3")[["g"]]),
                    times)
    out <- sum_run / N
    # Return using input order
    if (n_na > 0){
      out <- c(out, rep_len(NA_real_, n_na))
      id <- c(df[["id"]], which_na)
    } else {
      id <- df[["id"]]
    }
    out <- out[radix_order(id)]
  }
  out
}
