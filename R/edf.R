#' Grouped empirical cumulative distribution function applied to data
#'
#' @description Like `dplyr::cume_dist(x)` and `ecdf(x)(x)`
#' but with added grouping functionality.
#' @param x Numeric vector.
#' @param g Nuemric vector of group IDs.
#' @param na.rm Should `NA` values be removed? Default is `TRUE`.
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
edf <- function(x, g = NULL, na.rm = TRUE){
  if (is.null(g)){
    x_order <- radix_order(x)
    x <- x[x_order]
    grpn <- collapse::GRPN(x, expand = TRUE)
    grpn[collapse::fdiff(x, n = 1L) == 0] <- 0L
    run_sum <- collapse::fcumsum(grpn,
                                 na.rm = na.rm,
                                 check.o = FALSE)
    out <- run_sum / length(x)
    out <- out[radix_order(x_order)]
  } else {
    # Create group IDs
    df <- data.table::data.table(x, g)
    df[, ("g1") := group_id(df, all_of("x"), sort = TRUE,
                            as_qg = FALSE)]
    df[, ("g2") := group_id(df, all_of("g"), sort = FALSE,
                            as_qg = FALSE)]
    df[, ("g3") := group_id(df, all_of(c("g2", "g1")),
                            sort = TRUE, as_qg = FALSE)]
    df[, ("id") := seq_len(.N)]
    # Sort data in ascending order
    data.table::setorderv(df, cols = "g3")
    # Group sizes
    grp_n2 <- collapse::GRPN(df[["g2"]], expand = TRUE)
    # Size of each run-segment within each group
    grp_n3 <- collapse::GRPN(df[["g3"]], expand = TRUE)
    # Want to retain the numbers at the start of each run-segment
    # To then create a running total
    if (nrow2(df) > 0L){
      grp3_diff <- collapse::fdiff(df[["g3"]], n = 1L)
      collapse::setv(grp_n3, which(grp3_diff == 0L), 0L, vind1 = TRUE)
    }
    # grp_n3[grp3_diff == 0L] <- 0L
    run_sum <- collapse::fcumsum(grp_n3,
                                 na.rm = na.rm,
                                 check.o = FALSE,
                                 g = df[["g2"]])
    out <- run_sum / grp_n2
    # Return using input order
    out <- out[radix_order(df[["id"]])]
  }
  out
}
