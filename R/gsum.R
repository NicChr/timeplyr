gsum <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::fsum(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}


gmean <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::fmean(x, g = g, use.g.names = FALSE,
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


gsd <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::fsd(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}


gvar <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::fvar(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}


gmode <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::fmode(x, g = g, use.g.names = FALSE,
                 na.rm = na.rm, TRA = "replace_fill", ...)
}


gmedian <- function(x, g = NULL, na.rm = TRUE, ...){
  collapse::fmedian(x, g = g, use.g.names = FALSE,
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


gnobs <- function(x, g = NULL, ...){
  collapse::fnobs(x, g = g, use.g.names = FALSE,
                  TRA = "replace_fill", ...)
}
# Version 3
# gsum <- function(x, g = NULL, ...){
#   if (!is.null(g)){
#     g <- GRP2(g, sort = TRUE, na.last = TRUE,
#               return.groups = FALSE)
#     gorder <- g[["order"]]
#     if (is.null(gorder)){
#       gorder <- radix_order(g[["group.id"]])
#     }
#   }
#   out <- collapse::fsum(x,
#                          g = g,
#                          use.g.names = FALSE,
#                          ...)
#   if (length(g) == 0L){
#     rep_len(out, length(x))
#   } else {
#     out <- rep(out, times = g[["group.sizes"]])
#     out[radix_order(gorder)]
#   }
# }
# version 2
# gsum <- function(x, g = NULL, na.rm = TRUE, ...){
#   if (!is.null(g)){
#     if (!is.numeric(g)){
#       g <- collapse::group(g)
#     }
#     g <- GRP2(as.integer(g), sort = TRUE,
#                        return.order = TRUE, na.last = TRUE)
#   }
#   out <- collapse::fsum(x,
#                         g = g,
#                         use.g.names = FALSE,
#                         na.rm = na.rm,
#                         ...)
#   if (length(g) == 0L){
#     rep_len(out, length(x))
#   } else {
#     out <- rep(out, g[["group.sizes"]])
#     out[radix_order(g[["order"]])]
#   }
# }
# Version 1
# gsum <- function(x, g = NULL, na.rm = TRUE, ...){
#   if (!is.null(g)){
#     g <- collapse::qG(as.integer(g), sort = FALSE)
#   }
#   out <- collapse::fsum(x,
#                         g = g,
#                         use.g.names = FALSE,
#                         na.rm = na.rm,
#                         ...)
#   if (length(g) == 0L){
#     rep_len(out, length(x))
#   } else {
#     out[match(g, seq_len(length(out)))]
#   }
# }
# if (!is.null(g)){
#   if (collapse::is_GRP(g)){
#     g <- GRP2(g[["group.id"]], sort = TRUE,
#                        return.order = TRUE, na.last = TRUE)
#   } else {
#     g <- GRP2(as.integer(g), sort = TRUE,
#                        return.order = TRUE, na.last = TRUE)
#   }
# }
