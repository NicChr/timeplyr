#' @description
#' A framework for handling raw date & datetime data \cr
#' using tidy best-practices from the tidyverse, the efficiency of data.table,
#' and the speed of collapse.
#'
#' You can learn more about the tidyverse,
#' data.table and collapse using the links below
#'
#' \href{https://www.tidyverse.org/learn/}{tidyverse}
#'
#' \href{https://CRAN.R-project.org/package=data.table}{data.table}
#'
#' \href{https://sebkrantz.github.io/collapse/articles/collapse_intro.html}{collapse}
#'
#' @importFrom dplyr %>%
#' @importFrom data.table :=
#' @importFrom data.table .N
#' @importFrom data.table .SD
#' @importFrom data.table .GRP
#' @importFrom dplyr .data
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom timechange time_add
#' @importFrom pillar tbl_sum
#' @importFrom pillar type_sum
"_PACKAGE"
