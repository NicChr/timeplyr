library(testthat)
library(timeplyr)

# Sys.setenv("OMP_THREAD_LIMIT" = 2)
# data.table::setDTthreads(threads = 1L)
# collapse::set_collapse(nthreads = 1L)

test_check("timeplyr")
