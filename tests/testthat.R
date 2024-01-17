library(testthat)
library(timeplyr)
Sys.unsetenv("R_TESTS")
# Sys.setenv("OMP_THREAD_LIMIT" = 2)
# data.table::setDTthreads(threads = 1L)
# collapse::set_collapse(nthreads = 1L)

reset_timeplyr_options()
test_check("timeplyr", reporter = c("check", "location"))
