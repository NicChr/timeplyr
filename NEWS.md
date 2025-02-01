# timeplyr 1.0.0

### Major breaking changes

- Time intervals in timeplyr have been re-imagined.
They are now fixed-width intervals which means any one time interval vector
will have a common width for all intervals it contains, e.g. 1 month.
This makes internal operations much faster and simplifies the logic
for working with these objects. The data structure for these intervals
is a vector of LHS start times with an attribute to specify the
timespan or width of the interval. All intervals remain LHS closed and
RHS open. As a side-note this new data structure makes it possible for 
time intervals to be used in `data.table`.

- Most functions which are not time related have been removed.

- Many arguments have been renamed or removed. 
  - This includes the `time_type`
  argument which is no longer used. When specifying units such as 
  days, weeks, months or years, exact timespans will be used. 
  - Most functions no longer use the args `roll_dst` and `roll_month` with the 
  exception of the sequence functions.
  - The `time_by` argument has been renamed to either `width` or `timespan`
  depending on the context.
  
- Functions `time_expandv`, `time_completev`, `time_span_size` have been 
renamed to `time_grid`, `time_complete_missing`, `time_grid_size` respectively.

- Functions `time_summarisev` and `time_aggregate` have been removed, use 
`time_cut_width` instead.

- `time_cut` has been deprecated and renamed to `time_cut_n`.

- `edf`, `asc` and `desc` have been removed as they do not fit the 
purpose of the package.

- `.roll_na_fill` has been removed.
  
### New features

- A custom timeplyr object `timespan` to create and use 
timespans for various operations.

- New functions `resolution` and `granularity` to calculate 
the resolution and granularity of a time vector respectively.
See the help page `?resolution` for more details.

# timeplyr 0.9.0

### Upcoming breaking changes

* All of the tidy functions like `fslice` that are not time-related will be
removed in the next release. These can now be found in fastplyr.

* It is likely that in the near future, objects of class 'time_interval' will
be re-imagined to be more efficient fixed-width intervals with a different 
data structure. Currently the data structure is a length-two list containing 
start and end times. Most intervals in timeplyr are fixed-width intervals, 
such as for example a vector of intervals that span a week or a month. 
A more efficient data structure for this might be to floor the object to the
start of its respective interval and to simply 
add an attribute that details the width of the interval.

* The `to` argument that allows for aggregating date and date-time variables
to a specified time is currently inclusive and will be changed 
to be non-inclusive in the near-future. This is because 
intervals in timeplyr are left-closed right-open intervals and so the `to` 
argument should reflect this.

### Breaking changes

* Many functions that were not time-related have now been deprecated, most
having been migrated over to the 'fastplyr' package.

### Bug fixes

* A C bug that in rare cases would cause R to crash has been fixed.

# timeplyr 0.8.2

* time_intervals are now used by default. Use the 'timeplyr.use_intervals' option
to control this behaviour globally.

* New ggplot2 scales for year_months and year_quarters.

* `time_ggplot` can now handle 'year_month' and 'year_quarter' objects.

* New function `time_cut_width` which is the same as `time_aggregate` but with
less arguments.

* New `roll_lag` and `roll_diff` methods for time-series objects.

* Creating 'year_months' and 'year_quarters' from numeric vectors now always coerces them to integer internally.

* Fixed a small bug in `roll_diff` where the order vector was not being 
used in the case when a vector of lags is supplied.

# timeplyr 0.8.1

* Fixed a regression where some methods stopped being exported.

* Fixed a regression where `time_complete` didn't accept a grouped_df.

* Users should now be able to replace values of a `time_interval` in the usual way.

# timeplyr 0.8.0

* `roll_diff` has been simplified and gains a new argument `differences` 
to allow for recursive differencing.

* `roll_lag` has been simplified. It internally utilises cheapr's `lag2_` with the recursive argument always set to `TRUE`.

* `time_aggregate` no longer accepts a group `g` argument.

* Removed some unnecessary arguments from `time_by`.

* Deprecated most of the data frame specific `time_` functions, with the exception of
`time_by`, `time_episodes`, `time_expand` and `time_complete`.

* Internal bug fix for period time differences. 

* `time_aggregate` gains the `from`, `to`, `time_floor` and `week_start` arguments.

* Moved much of the C++ functionality to the cheapr package, which is on CRAN.

* `roll_na_fill` can now also handle data frames.

* `time_episodes` has a custom print method displaying key summary metrics.

* New class `time_interval` to represent right-open time intervals. 

# timeplyr 0.5.0

* The internal code of `time_cut` has been simplified and improved. 
It can now also handle very large values of `n`. 
When `time_by` is left `NULL`, the maximum possible number of breaks used is
`( diff(range(x)) / gcd_diff(x) ) + 1`.

* `time_cut` and `time_summarisev` are now slightly faster.

* `scm` now handles vectors containing only `NA` values appropriately.

* Exported additional sequence functions.

* The default summary functions in `stat_summarise` should now work
for most vector types.

* `fdistinct` is now faster when `sort = TRUE`.

* In `time_episodes`, the calculation for when there is 
a mixture of events and non-events has been significantly simplified.

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

# timeplyr 0.4.1 (24-Nov-2023)

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
