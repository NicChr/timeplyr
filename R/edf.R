#' Grouped empirical cumulative distribution function applied to data
#'
#' @description Like `dplyr::cume_dist(x)` and `ecdf(x)(x)`
#' but with added grouping and weighting functionality.\cr
#' You can calculate the empirical distribution of x using
#' aggregated data by supplying frequency weights.
#' No expansion occurs which makes this function extremely efficient
#' for this type of data, of which plotting is a common application.
#'
#' @param x Numeric vector.
#' @param g Numeric vector of group IDs.
#' @param wt Frequency weights.
#' @examples
#' library(timeplyr)
#' library(dplyr)
#' library(ggplot2)
#' set.seed(9123812)
#' x <- sample(seq(-10, 10, 0.5), size = 10^2, replace = TRUE)
#' plot(sort(edf(x)))
#' all.equal(edf(x), ecdf(x)(x))
#' all.equal(edf(x), cume_dist(x))
#'
#' # Manual ECDF plot using only aggregate data
#' y <- rnorm(100, 10)
#' grid <- time_span(y, by = 0.1, floor_date = TRUE)
#' counts <- time_countv(y, by = 0.1, floor_date = TRUE)
#' edf <- edf(grid, wt = counts)
#' # Trivial here as this is the same
#' all.equal(unname(cumsum(counts)/sum(counts)), edf)
#'
#' # Full ecdf
#' tibble(x) %>%
#'   ggplot(aes(x = y)) +
#'   stat_ecdf()
#' # Approximation using aggregate only data
#' tibble(grid, edf) %>%
#'   ggplot(aes(x = grid, y = edf)) +
#'   geom_step()
#'
#' # Grouped example
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
  nx <- length(x)
  if (is.null(g)){
    x_order <- radix_order(x)
    x <- x[x_order]
    if (n_na > 0) x <- x[seq_len(nx - n_na)]

    ### No weights
    if (is.null(wt)){
      times <- collapse::GRPN(x, expand = FALSE)
      grpn <- times
      N <- length(x)
    } else {
      ### With weights
      if (!length(wt) %in% c(1, nx)){
        stop("wt must be of length 1 or length(x)")
      }
      if (length(wt) == 1L){
        wt <- rep_len(wt, length(x))
      } else {
        wt <- wt[x_order]
        if (n_na > 0) wt <- wt[seq_len(nx - n_na)]
      }
      g <- group2(x, group.sizes = TRUE)
      times <- attr(g, "group.sizes")
      # times <- collapse::GRPN(g, expand = FALSE)
      grpn <- collapse::fsum(wt, g = g, use.g.names = FALSE)
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
    df[, ("g") := group_id.default(get("g"), order = FALSE, as_qg = FALSE)]
    df[, ("g1") := group_id.default(get("x"), order = TRUE, as_qg = FALSE)]
    df[, ("g3") := group_id.default(mget(c("g", "g1")), order = TRUE, as_qg = FALSE)]
    # Original order
    df[, ("id") := seq_len(.N)]
    # Order if NAs are shifted to the end
    which_na <- which(is.na(x))
    df[, ("id") := data.table::fifelse(is.na(get("x")), NA_integer_,
                                       get("id"))]
    # Sort data in ascending order
    data.table::setorderv(df, cols = "g3")
    if (n_na > 0) df <- df[!is.na(get("x")), ]
    # Group sizes
    grp_n2 <- collapse::GRPN(df[["g"]], expand = TRUE)
    times <- collapse::GRPN(df[["g3"]], expand = FALSE)
    if (is.null(wt)){
      grp_n3 <- times
      N <- grp_n2
    } else {
      grp_n3 <- collapse::fsum(df[["wt"]],
                               g = df[["g3"]],
                               use.g.names = FALSE)
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
