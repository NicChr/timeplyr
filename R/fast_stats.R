fn <- function(x, g = NULL, sort = TRUE,
               expand = FALSE, use.g.names = !expand){
  x_missing <- missing(x)
  if (is.null(g)){
    if (x_missing){
      stop("when g = NULL, x must be supplied")
    }
    nobs <- cheapr::vector_length(x)
    if (expand){
      nobs <- rep_len(nobs, nobs)
    }
  } else {
    g <- GRP2(g, sort = sort, return.groups = use.g.names)
    nobs <- GRP_group_sizes(g)
    if (GRP_n_groups(g) == 0L){
      nobs <- 0L
    }
    if (use.g.names){
      names(nobs) <- GRP_names(g)
    }
    if (expand){
      nobs <- nobs[GRP_group_id(g)]
    }
    if (!x_missing){
      N <- cheapr::vector_length(x)
      if (N != GRP_data_size(g)){
        stop("g must be the same size as the data")
      }
    }
  }
  nobs
}
fnmiss <- function(x, g = NULL, sort = TRUE, use.g.names = TRUE,
                   na.rm = FALSE){
  if (is.null(x)){
    return(0L)
  }
  x <- df_ungroup(x)
  g <- GRP2(g, sort = sort)
  N <- fn(x, g = g, use.g.names = FALSE)
  nobs <- collapse::fnobs(x, g = g, use.g.names = use.g.names)
  if (!is.null(collapse::fncol(nobs))){
    nobs <- collapse::qM(nobs)
  }
  N - nobs
}
