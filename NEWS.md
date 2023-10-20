# timeplyr (Development version)

# timeplyr 0.2.0 (15-Oct-2023)

* CRAN submission accepted.

# (In development) timeplyr 0.2.1 (16-Oct-2023)

* Fixed a bug where `sequence2` would error when `nvec` was a zero-length vector.

* Fixed a bug where `time_granularity` would error with zero-length vectors.

* `is_whole_number` is now faster and utilises relative differences. 
Also and the underlying C++ function is safer.

* Period calculations are now faster and more memory efficient and thus all the 
time functions are also faster.

* The `.keep_na` argument of `duplicate_rows` is now deprecated and replaced with
`drop_empty`.

* Most Rcpp functions are now more memory efficient due to disabling the RNGscope
where possible.

* Fixed an integer overflow bug in `sequence2`.

* The `as_period` argument in `time_diff` has been deprecated and removed.

* `time_num_gaps` and `time_has_gaps` now handle `NA` values more appropriately.

* 'collapse' `pivot` is now used for quantile summaries in `q_summarise`.

* timeplyr now utilises relative differences for all double comparisons using 
Rcpp
