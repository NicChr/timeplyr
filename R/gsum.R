gsum <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::fsum(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}

gmin <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::fmin(x, g = g, use.g.names = FALSE,
                  na.rm = na.rm, TRA = "replace_fill", ...)
}

gmax <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::fmax(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}

gfirst <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::ffirst(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}

glast <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::flast(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}
