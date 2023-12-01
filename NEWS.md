# timeplyr (Development version)

* New functions `roll_lag` and `roll_diff` for rolling lags and differences.

* The `time_roll_` functions are now faster due to having amended the 
time window size calculation.

* `age_years` is now much faster when there are relatively few distinct pairs
of start and end dates compared to the full data.

* Period-arithmetic is now much faster and more efficient due a new method 
for time differencing where distinct start-end values are used.

* `time_count` no longer completes implicit missing gaps in time. 
Use `time_complete` instead. 
When using `from` and `to`, `time_count` no longer removes out-of-bounds
time values and instead simply converted to `NA`. 

* tidyverse-style functions now use a new method for data-masking variables 
which aligns more closely with the tidyverse equivalents. 
The previous method evaluated the expressions supplied through `...` twice, 
once to generate the variables, and twice to extract the resulting variable
names. These are now evaluated once.

* Improved print method speed for `year_month` and `year_quarter`

# timeplyr 0.4.1

* New classes `year_month` and `year_quarter`. 
Inspired by 'zoo' and 'tsibble', these operate similarly but
the underlying data is an integer count of months for `year_month`, 
and quarters for `year_quarter`. 
This means that creating sequences is very fast and arithmetic is simpler and
more intuitive on the 'year-month' level.

* `cpp_roll_diff` and `scm` now appropriately handle `NA` values.

# timeplyr 0.4.0 (19-Nov-2023)

* New function `gcd` for fast calculation of greatest common divisor with 
tolerance. Time granularity calculations have also been sped up.

* Fixed a rare build issue using `R_SHORT_LEN_MAX` on certain systems. 
Thanks @barracuda156.

# timeplyr 0.3.0 (13-Nov-2023)

* This version brings major performance improvements, including
new algorithms for subsetting and rolling calculations.

* The `roll_na_fill` algorithm has been improved significantly.

* Calculation of row numbers are faster and more efficient.

* All 'C++' functions are now registered using the cpp11 package.

* `cpp_which` is now available as a more efficient and sometimes faster alternative
to `which`.

* The double comparison functions have been migrated to the package 'cppdoubles'.

* `roll_na_fill` has been mostly rewritten in C++ for speed and efficiency.

* `roll_growth_rate` now accepts groups through the `g` argument.

* New function `roll_across` for grouped rolling calculations.

# timeplyr 0.2.1 (31-Oct-2023)

* Fixed a bug where `sequence2` would error when `nvec` was a zero-length vector.

* Fixed a bug where `time_granularity` would error with zero-length vectors.

* `is_whole_number` is now faster and the underlying C++ function is safer.

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

* All double comparisons are now fully vectorised and recycling occurs without 
additional memory allocation.

* New function `num_na` to efficiently calculate number of missing values.

* timeplyr 0.2.1 published to CRAN

# timeplyr 0.2.0 (15-Oct-2023)

* CRAN submission accepted.
