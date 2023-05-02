
<!-- badges: start -->

[![R-CMD-check](https://github.com/NicChr/timeplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NicChr/timeplyr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# timeplyr

# **A date and datetime extension to dplyr**

This package provides a set of functions to make working with date and
datetime data much easier!

While most time-based packages are designed to work with clean and
pre-aggregate data, timeplyr contains a set of tidy tools to complete,
expand and summarise both raw and aggregate date/datetime data.

It introduces no new classes.

## Installation

You can install and load `timeplyr` using the below code.

``` r
remotes::install_github("NicChr/timeplyr")
```

``` r
library(timeplyr)
```

## Notes

**All functions that accept a data argument are tidyverse friendly and
can handle grouped calculations**

The `time_` functions that accept a data frame argument all operate
similarly to the tidyverse equivalents but additionally accept a `time`
argument to allow for date/datetime implicit missing value completion,
expansion and aggregation.

-   `time_count` - Time-based `dplyr::count`
-   `time_summarise` - Time-based `dplyr::summarise`
-   `time_mutate` - Time-based `dplyr::mutate`
-   `time_distinct` - Time-based `dplyr::distinct`
-   `time_expand` - Time-based `tidyr::expand`
-   `time_complete` - Time-based `tidyr::complete`

For example, `data %>% time_count(time = date, by = "month")` will:

-   Fill in missing gaps in time  
-   Count the dates by month

There are many more options to allow for more flexible time
summarisation, such as choosing start/end times, flooring dates to get
full time units (e.g months), and many others.

## Fast functions

There are also a few dplyr alternatives designed to work faster on large
numbers of groups.

-   `fcount` - `dplyr::count` alternative
-   `fdistinct` - `dplyr::distinct` alternative
-   `fduplicates` - Finds duplicate rows, similar to
    `data %>% group_by() %>% filter(n() > 1)`
-   `fslice` - `dplyr::slice` alternative

## Vector time functions

There are also vector versions of the tidy equivalents, designed for use
on atomic vectors. These include: `time_countv`, `time_expandv`,
`time_summarisev` and `time_completev`.

## `zoo::yearmon()`

There is lightweight support for zoo’s `yearmon()`.

## Some simple examples

We use flights departing from New York City in 2013.

``` r
library(tidyverse)
#> -- Attaching core tidyverse packages ------------------------ tidyverse 2.0.0 --
#> v dplyr     1.1.2     v readr     2.1.3
#> v forcats   0.5.2     v stringr   1.5.0
#> v ggplot2   3.4.2     v tibble    3.2.0
#> v lubridate 1.9.2     v tidyr     1.3.0
#> v purrr     1.0.1     
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::desc()   masks timeplyr::desc()
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
#> i Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library(lubridate)
library(nycflights13)
flights <- flights %>%
  mutate(date = as_date(time_hour))
```

## `time_count()`

#### Easily fill in implicit missing gaps and aggregate time.

``` r
flights_ts <- flights %>%
  time_count(time = time_hour)
#> Assuming a time granularity of 1 hour(s)
flights_ts
#> # A tibble: 8,755 x 2
#>    time_hour               n
#>  * <dttm>              <int>
#>  1 2013-01-01 05:00:00     6
#>  2 2013-01-01 06:00:00    52
#>  3 2013-01-01 07:00:00    49
#>  4 2013-01-01 08:00:00    58
#>  5 2013-01-01 09:00:00    56
#>  6 2013-01-01 10:00:00    39
#>  7 2013-01-01 11:00:00    37
#>  8 2013-01-01 12:00:00    56
#>  9 2013-01-01 13:00:00    54
#> 10 2013-01-01 14:00:00    48
#> # i 8,745 more rows
```

#### Dates work as easily as datetimes

``` r
flights %>%
  time_count(time = across(time_hour, as_date),
             by = "day")
#> # A tibble: 365 x 2
#>    time_hour      n
#>  * <date>     <int>
#>  1 2013-01-01   842
#>  2 2013-01-02   943
#>  3 2013-01-03   914
#>  4 2013-01-04   915
#>  5 2013-01-05   720
#>  6 2013-01-06   832
#>  7 2013-01-07   933
#>  8 2013-01-08   899
#>  9 2013-01-09   902
#> 10 2013-01-10   932
#> # i 355 more rows
```

#### Specify time aggregation through `by`

``` r
# Some examples
flights %>%
  time_count(time = across(time_hour, as_date),
             by = "7 days")
#> # A tibble: 53 x 2
#>    time_hour      n
#>  * <date>     <int>
#>  1 2013-01-01  6099
#>  2 2013-01-08  6109
#>  3 2013-01-15  6018
#>  4 2013-01-22  6060
#>  5 2013-01-29  6072
#>  6 2013-02-05  6101
#>  7 2013-02-12  6255
#>  8 2013-02-19  6394
#>  9 2013-02-26  6460
#> 10 2013-03-05  6549
#> # i 43 more rows
flights %>%
  time_count(time = across(time_hour, as_date),
             by = "2 weeks")
#> # A tibble: 27 x 2
#>    time_hour      n
#>  * <date>     <int>
#>  1 2013-01-01 12208
#>  2 2013-01-15 12078
#>  3 2013-01-29 12173
#>  4 2013-02-12 12649
#>  5 2013-02-26 13009
#>  6 2013-03-12 13100
#>  7 2013-03-26 13145
#>  8 2013-04-09 13239
#>  9 2013-04-23 13080
#> 10 2013-05-07 13019
#> # i 17 more rows
flights %>%
  time_count(time = across(time_hour, as_date),
             by = "fortnight")
#> # A tibble: 27 x 2
#>    time_hour      n
#>  * <date>     <int>
#>  1 2013-01-01 12208
#>  2 2013-01-15 12078
#>  3 2013-01-29 12173
#>  4 2013-02-12 12649
#>  5 2013-02-26 13009
#>  6 2013-03-12 13100
#>  7 2013-03-26 13145
#>  8 2013-04-09 13239
#>  9 2013-04-23 13080
#> 10 2013-05-07 13019
#> # i 17 more rows
flights %>%
  time_count(time = across(time_hour, as_date),
             by = "month")
#> # A tibble: 12 x 2
#>    time_hour      n
#>  * <date>     <int>
#>  1 2013-01-01 27004
#>  2 2013-02-01 24951
#>  3 2013-03-01 28834
#>  4 2013-04-01 28330
#>  5 2013-05-01 28796
#>  6 2013-06-01 28243
#>  7 2013-07-01 29425
#>  8 2013-08-01 29327
#>  9 2013-09-01 27574
#> 10 2013-10-01 28889
#> 11 2013-11-01 27268
#> 12 2013-12-01 28135
flights %>%
  time_count(time = across(time_hour, as_date),
             by = "quarter")
#> # A tibble: 4 x 2
#>   time_hour      n
#> * <date>     <int>
#> 1 2013-01-01 80789
#> 2 2013-04-01 85369
#> 3 2013-07-01 86326
#> 4 2013-10-01 84292
flights %>%
  time_count(time = across(time_hour, as_date),
             by = "year")
#> # A tibble: 1 x 2
#>   time_hour       n
#> * <date>      <int>
#> 1 2013-01-01 336776
```

#### Ensure full weeks/months/years by using `floor_date = TRUE`

``` r
start <- dmy("17-Jan-2013")
flights %>%
  time_count(time = across(time_hour, as_date),
             by = "week", from = start, floor_date = TRUE)
#> # A tibble: 51 x 2
#>    time_hour      n
#>  * <date>     <int>
#>  1 2013-01-14  3311
#>  2 2013-01-21  6049
#>  3 2013-01-28  6063
#>  4 2013-02-04  6104
#>  5 2013-02-11  6236
#>  6 2013-02-18  6381
#>  7 2013-02-25  6444
#>  8 2013-03-04  6546
#>  9 2013-03-11  6555
#> 10 2013-03-18  6547
#> # i 41 more rows
flights %>%
  time_count(time = across(time_hour, as_date),
             by = "month", from = start, floor_date = TRUE)
#> # A tibble: 12 x 2
#>    time_hour      n
#>  * <date>     <int>
#>  1 2013-01-01 13001
#>  2 2013-02-01 24951
#>  3 2013-03-01 28834
#>  4 2013-04-01 28330
#>  5 2013-05-01 28796
#>  6 2013-06-01 28243
#>  7 2013-07-01 29425
#>  8 2013-08-01 29327
#>  9 2013-09-01 27574
#> 10 2013-10-01 28889
#> 11 2013-11-01 27268
#> 12 2013-12-01 28135
```

## `missing_dates()`

#### Check for missing gaps in time

``` r
missing_dates(flights) # No missing dates
#> $time_hour
#> Date of length 0
#> 
#> $date
#> Date of length 0
length(time_missing(flights$time_hour, by = "hour")) # Missing hours
#> [1] 1819
time_missing(flights$date, by = "day") # Missing days
#> Date of length 0
```

#### Alternatively, counts with `time_countv()`

``` r
flight_counts <- time_countv(flights$time_hour, by = "hour")
flights_ts %>%
  mutate(n2 = flight_counts)
#> # A tibble: 8,755 x 3
#>    time_hour               n    n2
#>    <dttm>              <int> <int>
#>  1 2013-01-01 05:00:00     6     6
#>  2 2013-01-01 06:00:00    52    52
#>  3 2013-01-01 07:00:00    49    49
#>  4 2013-01-01 08:00:00    58    58
#>  5 2013-01-01 09:00:00    56    56
#>  6 2013-01-01 10:00:00    39    39
#>  7 2013-01-01 11:00:00    37    37
#>  8 2013-01-01 12:00:00    56    56
#>  9 2013-01-01 13:00:00    54    54
#> 10 2013-01-01 14:00:00    48    48
#> # i 8,745 more rows
```

## `time_expand()`

All the `time_` functions with a data argument work with groups. Here we
create a weekly time sequence for each origin and destination

``` r
origin_dest_seq <- flights %>%
  group_by(origin, dest) %>%
  time_expand(time = date, by = "week")
```

Another thing to note is that all `time_` functions have a `seq_type`
argument to let you specify if you want durations or periods. By default
it is “auto” which chooses periods for time units greater or equal to
days and durations otherwise.

``` r
start <- dmy("01-01-2023")
time_seq(start, start + years(1), 
         by = "month",
         seq_type = "period")
#>  [1] "2023-01-01" "2023-02-01" "2023-03-01" "2023-04-01" "2023-05-01"
#>  [6] "2023-06-01" "2023-07-01" "2023-08-01" "2023-09-01" "2023-10-01"
#> [11] "2023-11-01" "2023-12-01" "2024-01-01"
time_seq(start, start + years(1), 
         by = "month",
         seq_type = "duration")
#>  [1] "2023-01-01 00:00:00 UTC" "2023-01-31 10:30:00 UTC"
#>  [3] "2023-03-02 21:00:00 UTC" "2023-04-02 07:30:00 UTC"
#>  [5] "2023-05-02 18:00:00 UTC" "2023-06-02 04:30:00 UTC"
#>  [7] "2023-07-02 15:00:00 UTC" "2023-08-02 01:30:00 UTC"
#>  [9] "2023-09-01 12:00:00 UTC" "2023-10-01 22:30:00 UTC"
#> [11] "2023-11-01 09:00:00 UTC" "2023-12-01 19:30:00 UTC"
```

When you supply groups to `time_expand()` the start and end points are
unique to each group. We confirm this below

``` r
origin_dest_seq %>%
  summarise(from = min(date),
            to = max(date)) %>%
  ungroup() %>%
  distinct(from, to, .keep_all = TRUE)
#> `summarise()` has grouped output by 'origin'. You can override using the
#> `.groups` argument.
#> # A tibble: 45 x 4
#>    origin dest  from       to        
#>    <chr>  <chr> <date>     <date>    
#>  1 EWR    ALB   2013-01-01 2013-12-31
#>  2 EWR    ANC   2013-07-06 2013-08-24
#>  3 EWR    BDL   2013-01-01 2013-12-17
#>  4 EWR    BZN   2013-01-05 2013-12-28
#>  5 EWR    CAE   2013-01-03 2013-12-26
#>  6 EWR    DSM   2013-01-01 2013-12-24
#>  7 EWR    LGA   2013-07-27 2013-07-27
#>  8 EWR    MYR   2013-01-01 2013-04-02
#>  9 EWR    ORF   2013-01-02 2013-12-25
#> 10 EWR    PHL   2013-01-03 2013-04-04
#> # i 35 more rows
```

The ability to create time sequences by group is one of the most
powerful features of timeplyr.

## `time_complete()`

#### We could instead use `fcount()` and `time_complete()` to

#### count the hours with no gaps.

``` r
flights %>%
  group_by(origin, dest) %>%
  fcount(time_hour) %>%
  time_complete(time = time_hour, 
                by = "hour", 
                fill = list(n = 0))
#> # A tibble: 1,756,358 x 4
#> # Groups:   origin, dest [224]
#>    origin dest  time_hour               n
#>  * <chr>  <chr> <dttm>              <dbl>
#>  1 EWR    ALB   2013-01-01 13:00:00     1
#>  2 EWR    ALB   2013-01-01 14:00:00     0
#>  3 EWR    ALB   2013-01-01 15:00:00     0
#>  4 EWR    ALB   2013-01-01 16:00:00     1
#>  5 EWR    ALB   2013-01-01 17:00:00     0
#>  6 EWR    ALB   2013-01-01 18:00:00     0
#>  7 EWR    ALB   2013-01-01 19:00:00     0
#>  8 EWR    ALB   2013-01-01 20:00:00     1
#>  9 EWR    ALB   2013-01-01 21:00:00     0
#> 10 EWR    ALB   2013-01-01 22:00:00     0
#> # i 1,756,348 more rows
```

## `time_span()`

#### Alternatively using `time_span()` and `fcomplete()`

``` r
hour_seq <- time_span(flights$time_hour, by = "hour")
flights %>%
  group_by(origin, dest) %>%
  fcount(time_hour) %>%
  fcomplete(time_hour = hour_seq,
            fill = list(n = 0))
#> # A tibble: 1,961,120 x 4
#> # Groups:   origin, dest [224]
#>    origin dest  time_hour               n
#>  * <chr>  <chr> <dttm>              <dbl>
#>  1 EWR    ALB   2013-01-01 13:00:00     1
#>  2 EWR    ALB   2013-01-01 16:00:00     1
#>  3 EWR    ALB   2013-01-01 20:00:00     1
#>  4 EWR    ALB   2013-01-02 13:00:00     1
#>  5 EWR    ALB   2013-01-02 16:00:00     1
#>  6 EWR    ALB   2013-01-02 20:00:00     1
#>  7 EWR    ALB   2013-01-03 16:00:00     1
#>  8 EWR    ALB   2013-01-03 20:00:00     1
#>  9 EWR    ALB   2013-01-04 16:00:00     1
#> 10 EWR    ALB   2013-01-04 20:00:00     1
#> # i 1,961,110 more rows
```

#### The above has the same expanded time sequence for each group and

#### can be achieved using `time_count()` as well

``` r
flights %>%
  time_count(origin, dest, time = time_hour, by = "hour")
#> # A tibble: 1,961,120 x 4
#>    time_hour           origin dest      n
#>  * <dttm>              <chr>  <chr> <int>
#>  1 2013-01-01 05:00:00 EWR    ALB       0
#>  2 2013-01-01 05:00:00 EWR    ANC       0
#>  3 2013-01-01 05:00:00 EWR    ATL       0
#>  4 2013-01-01 05:00:00 EWR    AUS       0
#>  5 2013-01-01 05:00:00 EWR    AVL       0
#>  6 2013-01-01 05:00:00 EWR    BDL       0
#>  7 2013-01-01 05:00:00 EWR    BNA       0
#>  8 2013-01-01 05:00:00 EWR    BOS       0
#>  9 2013-01-01 05:00:00 EWR    BQN       0
#> 10 2013-01-01 05:00:00 EWR    BTV       0
#> # i 1,961,110 more rows
```

## `add_calendar()`

#### Easily join common date information to your data

``` r
flights_ts <- flights_ts %>%
  add_calendar(time_hour)
#> New columns added:
#> year, quarter, month, month_l, week, day, yday, isoyear, isoweek, isoday, epiyear, epiweek, wday, wday_l, hour, minute, second
```

Now that gaps in time have been filled and we have joined our date
table, it is easy to count by any time dimension we like

``` r
flights_ts %>% 
  fcount(isoyear, isoweek, wt = n)
#> # A tibble: 53 x 3
#>    isoyear isoweek     n
#>  *   <int>   <int> <int>
#>  1    2013       1  5166
#>  2    2013       2  6114
#>  3    2013       3  6034
#>  4    2013       4  6049
#>  5    2013       5  6063
#>  6    2013       6  6104
#>  7    2013       7  6236
#>  8    2013       8  6381
#>  9    2013       9  6444
#> 10    2013      10  6546
#> # i 43 more rows
flights_ts %>% 
  fcount(isoweek = iso_week(time_hour), wt = n)
#> # A tibble: 53 x 2
#>    isoweek      n
#>  * <chr>    <int>
#>  1 2013-W01  5166
#>  2 2013-W02  6114
#>  3 2013-W03  6034
#>  4 2013-W04  6049
#>  5 2013-W05  6063
#>  6 2013-W06  6104
#>  7 2013-W07  6236
#>  8 2013-W08  6381
#>  9 2013-W09  6444
#> 10 2013-W10  6546
#> # i 43 more rows
flights_ts %>% 
  fcount(month_l, wt = n)
#> # A tibble: 12 x 2
#>    month_l     n
#>  * <ord>   <int>
#>  1 Jan     27004
#>  2 Feb     24951
#>  3 Mar     28834
#>  4 Apr     28330
#>  5 May     28796
#>  6 Jun     28243
#>  7 Jul     29425
#>  8 Aug     29327
#>  9 Sep     27574
#> 10 Oct     28889
#> 11 Nov     27268
#> 12 Dec     28135
```

## `time_summarise()`

We can do more than just counts, actually any summary using
`time_summarise()` We can for example, calculate mean arrival and
departure times by month

``` r
flights %>%
  time_summarise(across(c(arr_time, dep_time), 
                        ~ mean(.x, na.rm = TRUE)),
                 time = time_hour, 
                 by = "month",  
                 floor_date = TRUE,
                 include_interval = TRUE)
#> # A tibble: 12 x 4
#>    time_hour           interval                                arr_time dep_time
#>    <dttm>              <Interval>                                 <dbl>    <dbl>
#>  1 2013-01-01 00:00:00 2013-01-01 EST--2013-02-01 00:00:00 EST    1523.    1347.
#>  2 2013-02-01 00:00:00 2013-02-01 EST--2013-03-01 00:00:00 EST    1522.    1348.
#>  3 2013-03-01 00:00:00 2013-03-01 EST--2013-04-01 00:00:00 EDT    1510.    1359.
#>  4 2013-04-01 00:00:00 2013-04-01 EDT--2013-05-01 00:00:00 EDT    1501.    1353.
#>  5 2013-05-01 00:00:00 2013-05-01 EDT--2013-06-01 00:00:00 EDT    1503.    1351.
#>  6 2013-06-01 00:00:00 2013-06-01 EDT--2013-07-01 00:00:00 EDT    1468.    1351.
#>  7 2013-07-01 00:00:00 2013-07-01 EDT--2013-08-01 00:00:00 EDT    1456.    1353.
#>  8 2013-08-01 00:00:00 2013-08-01 EDT--2013-09-01 00:00:00 EDT    1495.    1350.
#>  9 2013-09-01 00:00:00 2013-09-01 EDT--2013-10-01 00:00:00 EDT    1504.    1334.
#> 10 2013-10-01 00:00:00 2013-10-01 EDT--2013-11-01 00:00:00 EDT    1520.    1340.
#> 11 2013-11-01 00:00:00 2013-11-01 EDT--2013-12-01 00:00:00 EST    1523.    1344.
#> 12 2013-12-01 00:00:00 2013-12-01 EST--2013-12-31 23:00:00 EST    1505.    1357.
```

## `time_mutate()`

A time-based version of mutate

``` r
flights %>%
  time_mutate(time = date, by = "quarter",
              .keep = "none") %>%
  fcount(date)
#> # A tibble: 4 x 2
#>   date           n
#> * <date>     <int>
#> 1 2013-01-01 80789
#> 2 2013-04-01 85369
#> 3 2013-07-01 86326
#> 4 2013-10-01 84292
```

### Other convenience functions are included below

## `.time_units`

See a list of available time units

``` r
.time_units
#>  [1] "picoseconds"  "nanoseconds"  "microseconds" "milliseconds" "seconds"     
#>  [6] "minutes"      "hours"        "days"         "weeks"        "months"      
#> [11] "years"        "fortnights"   "quarters"     "semesters"    "olympiads"   
#> [16] "lustrums"     "decades"      "indictions"   "scores"       "centuries"   
#> [21] "milleniums"
```

## `age_years()`

Calculate ages (years) accurately

``` r
age_years(dmy("28-02-2000"))
#> [1] 23
```

## `time_seq()`

A lubridate version of `seq()` for dates and datetimes

``` r
start <- dmy(31012020)
end <- start + years(1)
seq(start, end, by = "month") # Base R version
#>  [1] "2020-01-31" "2020-03-02" "2020-03-31" "2020-05-01" "2020-05-31"
#>  [6] "2020-07-01" "2020-07-31" "2020-08-31" "2020-10-01" "2020-10-31"
#> [11] "2020-12-01" "2020-12-31" "2021-01-31"
time_seq(start, end, by = "month") # lubridate version
#>  [1] "2020-01-31" "2020-02-29" "2020-03-31" "2020-04-30" "2020-05-31"
#>  [6] "2020-06-30" "2020-07-31" "2020-08-31" "2020-09-30" "2020-10-31"
#> [11] "2020-11-30" "2020-12-31" "2021-01-31"
```

`time_seq()` doesn’t mind mixing dates and datetimes

``` r
time_seq(start, as_datetime(end), by = "2 weeks")
#>  [1] "2020-01-31 UTC" "2020-02-14 UTC" "2020-02-28 UTC" "2020-03-13 UTC"
#>  [5] "2020-03-27 UTC" "2020-04-10 UTC" "2020-04-24 UTC" "2020-05-08 UTC"
#>  [9] "2020-05-22 UTC" "2020-06-05 UTC" "2020-06-19 UTC" "2020-07-03 UTC"
#> [13] "2020-07-17 UTC" "2020-07-31 UTC" "2020-08-14 UTC" "2020-08-28 UTC"
#> [17] "2020-09-11 UTC" "2020-09-25 UTC" "2020-10-09 UTC" "2020-10-23 UTC"
#> [21] "2020-11-06 UTC" "2020-11-20 UTC" "2020-12-04 UTC" "2020-12-18 UTC"
#> [25] "2021-01-01 UTC" "2021-01-15 UTC" "2021-01-29 UTC"
```

## `time_seq_v()`

A vectorised version of `time_seq()` Currently it vectorised over from,
to and by

``` r
# 3 sequences
time_seq_v(from = start, 
           to = end, 
           by = list("months" = 1:3))
#>  [1] "2020-01-31" "2020-02-29" "2020-03-31" "2020-04-30" "2020-05-31"
#>  [6] "2020-06-30" "2020-07-31" "2020-08-31" "2020-09-30" "2020-10-31"
#> [11] "2020-11-30" "2020-12-31" "2021-01-31" "2020-01-31" "2020-03-31"
#> [16] "2020-05-31" "2020-07-31" "2020-09-30" "2020-11-30" "2021-01-31"
#> [21] "2020-01-31" "2020-04-30" "2020-07-31" "2020-10-31" "2021-01-31"
# Equivalent to 
c(time_seq(start, end, by = "month"),
  time_seq(start, end, by = "2 months"),
  time_seq(start, end, by = "3 months"))
#>  [1] "2020-01-31" "2020-02-29" "2020-03-31" "2020-04-30" "2020-05-31"
#>  [6] "2020-06-30" "2020-07-31" "2020-08-31" "2020-09-30" "2020-10-31"
#> [11] "2020-11-30" "2020-12-31" "2021-01-31" "2020-01-31" "2020-03-31"
#> [16] "2020-05-31" "2020-07-31" "2020-09-30" "2020-11-30" "2021-01-31"
#> [21] "2020-01-31" "2020-04-30" "2020-07-31" "2020-10-31" "2021-01-31"
```

## `time_seq_len()`

Vectorised function that calculates time sequence lengths

``` r
set.seed(42)
time_seq_len(start, start + years(1:10), 
             by = list("days" = sample(1:10)))
#>  [1]  367  147  110  183  914  549  427  325  470 1218
```

Dealing with impossible dates and datetimes is very simple

``` r
time_seq(start, end, by = "month", roll_month = "postday") # roll impossible months forward
#>  [1] "2020-01-31" "2020-03-01" "2020-03-31" "2020-05-01" "2020-05-31"
#>  [6] "2020-07-01" "2020-07-31" "2020-08-31" "2020-10-01" "2020-10-31"
#> [11] "2020-12-01" "2020-12-31" "2021-01-31"
time_seq(start, end, by = "month", roll_month = "NA") # no roll
#>  [1] "2020-01-31" NA           "2020-03-31" NA           "2020-05-31"
#>  [6] NA           "2020-07-31" "2020-08-31" NA           "2020-10-31"
#> [11] NA           "2020-12-31" "2021-01-31"

time_seq(start, end, by = "month", seq_type = "duration") # lubridate version with durations
#>  [1] "2020-01-31 00:00:00 UTC" "2020-03-01 10:30:00 UTC"
#>  [3] "2020-03-31 21:00:00 UTC" "2020-05-01 07:30:00 UTC"
#>  [5] "2020-05-31 18:00:00 UTC" "2020-07-01 04:30:00 UTC"
#>  [7] "2020-07-31 15:00:00 UTC" "2020-08-31 01:30:00 UTC"
#>  [9] "2020-09-30 12:00:00 UTC" "2020-10-30 22:30:00 UTC"
#> [11] "2020-11-30 09:00:00 UTC" "2020-12-30 19:30:00 UTC"
#> [13] "2021-01-30 06:00:00 UTC"
```

## `iso_week()`

Simple function to get formatted ISO weeks.

``` r
iso_week(today())
#> [1] "2023-W18"
iso_week(today(), day = TRUE)
#> [1] "2023-W18-2"
iso_week(today(), year = FALSE)
#> [1] "W18"
```

Helpers like `calendar()`, `create_calendar()` and `add_calendar()` can
make time sequence tibbles.

``` r
my_seq <- time_seq(start, end, by = "day")
calendar(my_seq)
#> # A tibble: 367 x 15
#>    time        year quarter month month_l  week   day  yday isoyear isoweek
#>    <date>     <int>   <int> <int> <ord>   <int> <int> <int>   <int>   <int>
#>  1 2020-01-31  2020       1     1 Jan         5    31    31    2020       5
#>  2 2020-02-01  2020       1     2 Feb         5     1    32    2020       5
#>  3 2020-02-02  2020       1     2 Feb         5     2    33    2020       5
#>  4 2020-02-03  2020       1     2 Feb         5     3    34    2020       6
#>  5 2020-02-04  2020       1     2 Feb         5     4    35    2020       6
#>  6 2020-02-05  2020       1     2 Feb         6     5    36    2020       6
#>  7 2020-02-06  2020       1     2 Feb         6     6    37    2020       6
#>  8 2020-02-07  2020       1     2 Feb         6     7    38    2020       6
#>  9 2020-02-08  2020       1     2 Feb         6     8    39    2020       6
#> 10 2020-02-09  2020       1     2 Feb         6     9    40    2020       6
#> # i 357 more rows
#> # i 5 more variables: isoday <int>, epiyear <int>, epiweek <int>, wday <int>,
#> #   wday_l <ord>
create_calendar(start, end, by = "day") # The same style as time_seq
#> # A tibble: 367 x 15
#>    time        year quarter month month_l  week   day  yday isoyear isoweek
#>    <date>     <int>   <int> <int> <ord>   <int> <int> <int>   <int>   <int>
#>  1 2020-01-31  2020       1     1 Jan         5    31    31    2020       5
#>  2 2020-02-01  2020       1     2 Feb         5     1    32    2020       5
#>  3 2020-02-02  2020       1     2 Feb         5     2    33    2020       5
#>  4 2020-02-03  2020       1     2 Feb         5     3    34    2020       6
#>  5 2020-02-04  2020       1     2 Feb         5     4    35    2020       6
#>  6 2020-02-05  2020       1     2 Feb         6     5    36    2020       6
#>  7 2020-02-06  2020       1     2 Feb         6     6    37    2020       6
#>  8 2020-02-07  2020       1     2 Feb         6     7    38    2020       6
#>  9 2020-02-08  2020       1     2 Feb         6     8    39    2020       6
#> 10 2020-02-09  2020       1     2 Feb         6     9    40    2020       6
#> # i 357 more rows
#> # i 5 more variables: isoday <int>, epiyear <int>, epiweek <int>, wday <int>,
#> #   wday_l <ord>
# Tidy version
tibble(my_seq) %>%
  add_calendar(my_seq)
#> New columns added:
#> year, quarter, month, month_l, week, day, yday, isoyear, isoweek, isoday, epiyear, epiweek, wday, wday_l
#> # A tibble: 367 x 15
#>    my_seq      year quarter month month_l  week   day  yday isoyear isoweek
#>    <date>     <int>   <int> <int> <ord>   <int> <int> <int>   <int>   <int>
#>  1 2020-01-31  2020       1     1 Jan         5    31    31    2020       5
#>  2 2020-02-01  2020       1     2 Feb         5     1    32    2020       5
#>  3 2020-02-02  2020       1     2 Feb         5     2    33    2020       5
#>  4 2020-02-03  2020       1     2 Feb         5     3    34    2020       6
#>  5 2020-02-04  2020       1     2 Feb         5     4    35    2020       6
#>  6 2020-02-05  2020       1     2 Feb         6     5    36    2020       6
#>  7 2020-02-06  2020       1     2 Feb         6     6    37    2020       6
#>  8 2020-02-07  2020       1     2 Feb         6     7    38    2020       6
#>  9 2020-02-08  2020       1     2 Feb         6     8    39    2020       6
#> 10 2020-02-09  2020       1     2 Feb         6     9    40    2020       6
#> # i 357 more rows
#> # i 5 more variables: isoday <int>, epiyear <int>, epiweek <int>, wday <int>,
#> #   wday_l <ord>
```

## `time_cut()`

Create pretty time axes using `time_cut()` and `time_breaks()`

``` r
times <- flights$time_hour
dates <- flights$date

levels(time_cut(dates, n = 10))
#> [1] "[2013-01-01, 2013-03-01)" "[2013-03-01, 2013-05-01)"
#> [3] "[2013-05-01, 2013-07-01)" "[2013-07-01, 2013-09-01)"
#> [5] "[2013-09-01, 2013-11-01)" "[2013-11-01, 2013-12-31]"
date_breaks <- time_breaks(dates, n = 12)
time_breaks <- time_breaks(times, n = 12, floor_date = TRUE)

weekly_data <- flights %>%
  time_count(time = date, by = "week",
             to = max(time_span(date, by = "week")),
             include_interval = TRUE) %>%
  # Filter full weeks
  mutate(n_days = interval/days(1)) %>%
  filter(n_days == 7)
#> data.table converted to tibble as data.table cannot include interval class
weekly_data %>%
  ggplot(aes(x = date, y = n)) + 
  geom_bar(stat = "identity", fill = "#0072B2") + 
  scale_x_date(breaks = date_breaks, labels = scales::label_date_short()) +
  theme_minimal()
```

![](man/figures/README-unnamed-chunk-26-1.png)<!-- -->

``` r
flights %>%
  ggplot(aes(x = time_hour)) + 
  geom_bar(fill = "#0072B2") + 
  scale_x_datetime(breaks = time_breaks, labels = scales::label_date_short())
```

![](man/figures/README-unnamed-chunk-26-2.png)<!-- -->

## Efficient grouped functions

## `group_collapse()`

Collapse your data into unique groups with key information

``` r
flights %>%
  group_collapse(origin, dest)
#> # A tibble: 224 x 7
#>    origin dest  .group        .loc .start   .end .size
#>  * <chr>  <chr>  <int> <list<int>>  <int>  <int> <int>
#>  1 EWR    IAH       35     [3,973]      1 336738  3973
#>  2 LGA    IAH      188     [2,951]      2 336619  2951
#>  3 JFK    MIA      123     [3,314]      3 336567  3314
#>  4 JFK    BQN       94       [599]      4 335788   599
#>  5 LGA    ATL      157    [10,263]      5 336671 10263
#>  6 EWR    ORD       56     [6,100]      6 336676  6100
#>  7 EWR    FLL       27     [3,793]      7 336707  3793
#>  8 LGA    IAD      187     [1,803]      8 336642  1803
#>  9 JFK    MCO      121     [5,464]      9 336764  5464
#> 10 LGA    ORD      205     [8,857]     10 336710  8857
#> # i 214 more rows
# Sorted (like dplyr::group_data())
flights %>%
  group_collapse(origin, dest, sort = TRUE)
#> # A tibble: 224 x 7
#>    origin dest  .group        .loc .start   .end .size
#>  * <chr>  <chr>  <int> <list<int>>  <int>  <int> <int>
#>  1 EWR    ALB        1       [439]    361 331200   439
#>  2 EWR    ANC        2         [8] 255456 302527     8
#>  3 EWR    ATL        3     [5,022]     30 336725  5022
#>  4 EWR    AUS        4       [968]    440 336715   968
#>  5 EWR    AVL        5       [265]    212 336461   265
#>  6 EWR    BDL        6       [443]    364 334280   443
#>  7 EWR    BNA        7     [2,336]    512 336618  2336
#>  8 EWR    BOS        8     [5,327]    108 336756  5327
#>  9 EWR    BQN        9       [297]    720 335753   297
#> 10 EWR    BTV       10       [931]    307 302836   931
#> # i 214 more rows
# By order of first appearance
flights %>%
  group_collapse(origin, dest, order = FALSE)
#> # A tibble: 224 x 7
#>    origin dest  .group        .loc .start   .end .size
#>  * <chr>  <chr>  <int> <list<int>>  <int>  <int> <int>
#>  1 EWR    IAH        1     [3,973]      1 336738  3973
#>  2 LGA    IAH        2     [2,951]      2 336619  2951
#>  3 JFK    MIA        3     [3,314]      3 336567  3314
#>  4 JFK    BQN        4       [599]      4 335788   599
#>  5 LGA    ATL        5    [10,263]      5 336671 10263
#>  6 EWR    ORD        6     [6,100]      6 336676  6100
#>  7 EWR    FLL        7     [3,793]      7 336707  3793
#>  8 LGA    IAD        8     [1,803]      8 336642  1803
#>  9 JFK    MCO        9     [5,464]      9 336764  5464
#> 10 LGA    ORD       10     [8,857]     10 336710  8857
#> # i 214 more rows
```

## `fcount()`/`fadd_count()`

``` r
flights %>%
  group_by(origin, dest, tailnum) %>%
  fcount(flight, carrier)
#> # A tibble: 186,870 x 6
#> # Groups:   origin, dest, tailnum [52,783]
#>    origin dest  tailnum flight carrier     n
#>  * <chr>  <chr> <chr>    <int> <chr>   <int>
#>  1 EWR    ALB   N10575    4117 EV          2
#>  2 EWR    ALB   N10575    4162 EV          2
#>  3 EWR    ALB   N10575    4309 EV          1
#>  4 EWR    ALB   N10575    4566 EV          1
#>  5 EWR    ALB   N10575    6043 EV          2
#>  6 EWR    ALB   N11113    4264 EV          1
#>  7 EWR    ALB   N11119    4093 EV          1
#>  8 EWR    ALB   N11119    4271 EV          2
#>  9 EWR    ALB   N11150    5675 EV          1
#> 10 EWR    ALB   N11164    4088 EV          1
#> # i 186,860 more rows
flights %>%
  select(origin, dest, tailnum, flight, carrier) %>%
  fadd_count(across(all_of(c("flight", "carrier"))), 
             .by = c(origin, dest, tailnum))
#> # A tibble: 336,776 x 6
#>    origin dest  tailnum flight carrier     n
#>  * <chr>  <chr> <chr>    <int> <chr>   <int>
#>  1 EWR    IAH   N14228    1545 UA          1
#>  2 LGA    IAH   N24211    1714 UA          1
#>  3 JFK    MIA   N619AA    1141 AA          2
#>  4 JFK    BQN   N804JB     725 B6          1
#>  5 LGA    ATL   N668DN     461 DL          2
#>  6 EWR    ORD   N39463    1696 UA          1
#>  7 EWR    FLL   N516JB     507 B6          3
#>  8 LGA    IAD   N829AS    5708 EV          2
#>  9 JFK    MCO   N593JB      79 B6          3
#> 10 LGA    ORD   N3ALAA     301 AA          5
#> # i 336,766 more rows
```

## `group_id()`/`add_group_id()`

This calculates sorted and non-sorted group IDs

``` r
flights %>%
  group_by(origin, dest) %>%
  group_id(order = FALSE) %>%
  unique()
#>   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
#>  [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
#>  [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
#>  [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
#>  [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
#>  [91]  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
#> [109] 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
#> [127] 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
#> [145] 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162
#> [163] 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
#> [181] 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198
#> [199] 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216
#> [217] 217 218 219 220 221 222 223 224
flights %>%
  select(origin, dest) %>%
  add_group_id(.by = everything()) %>%
  distinct(origin, dest, group_id)
#> # A tibble: 224 x 3
#>    origin dest  group_id
#>    <chr>  <chr>    <int>
#>  1 EWR    IAH         35
#>  2 LGA    IAH        188
#>  3 JFK    MIA        123
#>  4 JFK    BQN         94
#>  5 LGA    ATL        157
#>  6 EWR    ORD         56
#>  7 EWR    FLL         27
#>  8 LGA    IAD        187
#>  9 JFK    MCO        121
#> 10 LGA    ORD        205
#> # i 214 more rows
```

## `fexpand()`/`crossed_join()`

Fast cross join

``` r
# Bare-bones but fast cross join
flights %>%
    select(where(is.character)) %>%
    crossed_join(strings_as_factors = TRUE)
#>           carrier tailnum origin dest
#>        1:      UA  N14228    EWR  IAH
#>        2:      UA  N14228    EWR  MIA
#>        3:      UA  N14228    EWR  BQN
#>        4:      UA  N14228    EWR  ATL
#>        5:      UA  N14228    EWR  ORD
#>       ---                            
#> 20381756:      OO  N557AS    JFK  LEX
#> 20381757:      OO  N557AS    JFK  CHO
#> 20381758:      OO  N557AS    JFK  TVC
#> 20381759:      OO  N557AS    JFK  ANC
#> 20381760:      OO  N557AS    JFK  LGA
# Tidy version that supports groups
flights %>%
  group_by(origin) %>%
  fexpand(dest, carrier)
#> # A tibble: 2,616 x 3
#> # Groups:   origin [3]
#>    origin dest  carrier
#>  * <chr>  <chr> <chr>  
#>  1 EWR    IAH   UA     
#>  2 EWR    IAH   B6     
#>  3 EWR    IAH   AA     
#>  4 EWR    IAH   MQ     
#>  5 EWR    IAH   DL     
#>  6 EWR    IAH   US     
#>  7 EWR    IAH   EV     
#>  8 EWR    IAH   AS     
#>  9 EWR    IAH   WN     
#> 10 EWR    IAH   9E     
#> # i 2,606 more rows
```

## `fslice()`

Fast row index slicing with lots of groups

``` r
flights %>%
  group_by(origin, dest, tailnum) %>%
  fslice(1:5)
#> # A tibble: 172,983 x 20
#> # Groups:   origin, dest, tailnum [52,783]
#>     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
#>  * <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
#>  1  2013     1    30     2224           2000       144     2316           2101
#>  2  2013     2    17     2012           2010         2     2120           2114
#>  3  2013     2    26     2356           2000       236       41           2104
#>  4  2013     3    13     1958           2005        -7     2056           2109
#>  5  2013     5    16     2214           2000       134     2307           2112
#>  6  2013     9     8     2156           2159        -3     2250           2303
#>  7  2013     1    26     1614           1620        -6     1706           1724
#>  8  2013     2    11       NA           1619        NA       NA           1723
#>  9  2013     2    17     1604           1609        -5     1715           1713
#> 10  2013    11     8     2203           2159         4     2250           2301
#> # i 172,973 more rows
#> # i 12 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
#> #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
#> #   hour <dbl>, minute <dbl>, time_hour <dttm>, date <date>
flights %>%
  group_by(origin, dest, tailnum) %>%
  fslice_head(n = 5)
#> # A tibble: 172,983 x 20
#> # Groups:   origin, dest, tailnum [52,783]
#>     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
#>  * <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
#>  1  2013     1    30     2224           2000       144     2316           2101
#>  2  2013     2    17     2012           2010         2     2120           2114
#>  3  2013     2    26     2356           2000       236       41           2104
#>  4  2013     3    13     1958           2005        -7     2056           2109
#>  5  2013     5    16     2214           2000       134     2307           2112
#>  6  2013     9     8     2156           2159        -3     2250           2303
#>  7  2013     1    26     1614           1620        -6     1706           1724
#>  8  2013     2    11       NA           1619        NA       NA           1723
#>  9  2013     2    17     1604           1609        -5     1715           1713
#> 10  2013    11     8     2203           2159         4     2250           2301
#> # i 172,973 more rows
#> # i 12 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
#> #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
#> #   hour <dbl>, minute <dbl>, time_hour <dttm>, date <date>
# Use keep_order to retain the data input order
flights %>%
  group_by(origin, dest, tailnum) %>%
  fslice_tail(prop = 0.5, keep_order = TRUE)
#> # A tibble: 153,350 x 20
#> # Groups:   origin, dest, tailnum [40,633]
#>     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
#>  * <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
#>  1  2013     1     2      919            830        49     1135           1038
#>  2  2013     1     2     1126           1125         1     1333           1325
#>  3  2013     1     3      603            605        -2      709            705
#>  4  2013     1     3     1336           1340        -4     1641           1626
#>  5  2013     1     3     1348           1350        -2     1631           1640
#>  6  2013     1     4      628            630        -2     1124           1140
#>  7  2013     1     4      712            715        -3     1021           1035
#>  8  2013     1     4      716            720        -4      855            840
#>  9  2013     1     4     1101           1106        -5     1349           1404
#> 10  2013     1     4     1249           1235        14     1434           1415
#> # i 153,340 more rows
#> # i 12 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
#> #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
#> #   hour <dbl>, minute <dbl>, time_hour <dttm>, date <date>
```

## `fdistinct()`

Distinct rows

``` r
flights %>%
  group_by(origin, dest, tailnum) %>%
  fdistinct(year, month, day)
#> # A tibble: 316,477 x 6
#> # Groups:   origin, dest, tailnum [52,783]
#>    origin dest  tailnum  year month   day
#>  * <chr>  <chr> <chr>   <int> <int> <int>
#>  1 EWR    IAH   N14228   2013     1     1
#>  2 LGA    IAH   N24211   2013     1     1
#>  3 JFK    MIA   N619AA   2013     1     1
#>  4 JFK    BQN   N804JB   2013     1     1
#>  5 LGA    ATL   N668DN   2013     1     1
#>  6 EWR    ORD   N39463   2013     1     1
#>  7 EWR    FLL   N516JB   2013     1     1
#>  8 LGA    IAD   N829AS   2013     1     1
#>  9 JFK    MCO   N593JB   2013     1     1
#> 10 LGA    ORD   N3ALAA   2013     1     1
#> # i 316,467 more rows
```

## `fduplicates()`

Duplicate rows

``` r
flights %>%
  group_by(origin, dest, tailnum) %>%
  fduplicates(year, month, day)
#> # A tibble: 20,299 x 6
#> # Groups:   origin, dest, tailnum [6,122]
#>    origin dest  tailnum  year month   day
#>  * <chr>  <chr> <chr>   <int> <int> <int>
#>  1 EWR    BOS   N206JB   2013     1     1
#>  2 JFK    DCA   N846MQ   2013     1     1
#>  3 JFK    RDU   N828MQ   2013     1     1
#>  4 LGA    CMH   N739MQ   2013     1     1
#>  5 LGA    MIA   N3EMAA   2013     1     1
#>  6 EWR    FLL   N516JB   2013     1     1
#>  7 JFK    MCO   N5FMAA   2013     1     1
#>  8 LGA    DCA   N951UW   2013     1     1
#>  9 EWR    PWM   N11544   2013     1     1
#> 10 LGA    DFW   N3DUAA   2013     1     1
#> # i 20,289 more rows
```

## `row_id()`/`add_row_id()`

Fast grouped row IDs

``` r
iris <- as_tibble(iris)
range(row_id(iris))
#> [1]   1 150
iris %>%
  add_row_id()
#> # A tibble: 150 x 6
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species row_id
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>    <int>
#>  1          5.1         3.5          1.4         0.2 setosa       1
#>  2          4.9         3            1.4         0.2 setosa       2
#>  3          4.7         3.2          1.3         0.2 setosa       3
#>  4          4.6         3.1          1.5         0.2 setosa       4
#>  5          5           3.6          1.4         0.2 setosa       5
#>  6          5.4         3.9          1.7         0.4 setosa       6
#>  7          4.6         3.4          1.4         0.3 setosa       7
#>  8          5           3.4          1.5         0.2 setosa       8
#>  9          4.4         2.9          1.4         0.2 setosa       9
#> 10          4.9         3.1          1.5         0.1 setosa      10
#> # i 140 more rows
iris %>% 
  add_row_id(Species)
#> # A tibble: 150 x 6
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species row_id
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>    <int>
#>  1          5.1         3.5          1.4         0.2 setosa       1
#>  2          4.9         3            1.4         0.2 setosa       2
#>  3          4.7         3.2          1.3         0.2 setosa       3
#>  4          4.6         3.1          1.5         0.2 setosa       4
#>  5          5           3.6          1.4         0.2 setosa       5
#>  6          5.4         3.9          1.7         0.4 setosa       6
#>  7          4.6         3.4          1.4         0.3 setosa       7
#>  8          5           3.4          1.5         0.2 setosa       8
#>  9          4.4         2.9          1.4         0.2 setosa       9
#> 10          4.9         3.1          1.5         0.1 setosa      10
#> # i 140 more rows
```

## Grouped statistical functions

These functions are wrappers around the `collapse` equivalents, but the
output is always the same length as the input.

``` r
flights %>%
  add_group_id(origin, .name = "g") %>%
  mutate(nobs = gnobs(arr_time, g = g),
         sum = gsum(arr_time, g = g),
         mean = gmean(arr_time, g = g),
         min = gmin(time_hour, g = g),
         max = gmax(time_hour, g = g)) %>%
  fdistinct(origin, nobs, sum, mean, min, max)
#> # A tibble: 3 x 6
#>   origin   nobs       sum  mean min                 max                
#> * <chr>   <int>     <int> <dbl> <dttm>              <dttm>             
#> 1 EWR    117445 175213363 1492. 2013-01-01 05:00:00 2013-12-31 23:00:00
#> 2 LGA    101334 151435934 1494. 2013-01-01 05:00:00 2013-12-31 21:00:00
#> 3 JFK    109284 166119372 1520. 2013-01-01 05:00:00 2013-12-31 23:00:00
```
