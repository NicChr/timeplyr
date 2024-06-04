
<!-- badges: start -->

[![R-CMD-check](https://github.com/NicChr/timeplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NicChr/timeplyr/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/timeplyr)](https://CRAN.R-project.org/package=timeplyr)
<!-- badges: end -->

# timeplyr

# **Fast Tidy Tools for Date and Datetime Manipulation**

This package provides a set of functions to make working with date and
datetime data much easier!

While most time-based packages are designed to work with clean and
pre-aggregate data, timeplyr contains a set of tidy tools to complete,
expand and summarise both raw and aggregate date/datetime data.

Significant efforts have been made to ensure that grouped calculations
are fast and efficient thanks to the excellent functionality within the
[collapse](https://sebkrantz.github.io/collapse/reference/collapse-package.html)
package.

## Installation

You can install and load `timeplyr` using the below code.

``` r
# CRAN version
install.packages("timeplyr")

# Development version
remotes::install_github("NicChr/timeplyr")
```

``` r
library(timeplyr)
```

# Basic examples

## Convert `ts`, `mts`, `xts`, `zoo`and `timeSeries` objects using `ts_as_tibble`

``` r
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::desc()   masks timeplyr::desc()
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
eu_stock <- EuStockMarkets %>%
  ts_as_tibble()
eu_stock
#> # A tibble: 7,440 × 3
#>    group  time value
#>    <chr> <dbl> <dbl>
#>  1 DAX   1991. 1629.
#>  2 DAX   1992. 1614.
#>  3 DAX   1992. 1607.
#>  4 DAX   1992. 1621.
#>  5 DAX   1992. 1618.
#>  6 DAX   1992. 1611.
#>  7 DAX   1992. 1631.
#>  8 DAX   1992. 1640.
#>  9 DAX   1992. 1635.
#> 10 DAX   1992. 1646.
#> # ℹ 7,430 more rows
```

## Easily plot time series using `time_ggplot`

``` r
eu_stock %>%
  time_ggplot(time, value, group)
```

![](man/figures/README-unnamed-chunk-2-1.png)<!-- -->

For the next examples we use flights departing from New York City in
2013.

``` r
library(nycflights13)
library(lubridate)
flights <- flights %>%
  mutate(date = as_date(time_hour))
```

## `time_by`

### Group your time variable by any time unit

``` r
flights_monthly <- flights %>%
  select(date, arr_delay) %>%
  time_by(date, "month")

flights_monthly
#> # A tibble: 336,776 x 3
#> # Time:     time_intv_month [12]
#> # By:       month
#> # Span:     2013-01-01 - 2013-12-31
#>    date       arr_delay time_intv_month
#>    <date>         <dbl> <date>         
#>  1 2013-01-01        11 2013-01-01     
#>  2 2013-01-01        20 2013-01-01     
#>  3 2013-01-01        33 2013-01-01     
#>  4 2013-01-01       -18 2013-01-01     
#>  5 2013-01-01       -25 2013-01-01     
#>  6 2013-01-01        12 2013-01-01     
#>  7 2013-01-01        19 2013-01-01     
#>  8 2013-01-01       -14 2013-01-01     
#>  9 2013-01-01        -8 2013-01-01     
#> 10 2013-01-01         8 2013-01-01     
#> # ℹ 336,766 more rows
```

We can then use this to create a monthly summary of the number of
flights and average arrival delay

``` r
flights_monthly %>%
  summarise(n = n(),
            mean_arr_delay = mean(arr_delay, na.rm = TRUE))
#> # A tibble: 12 × 3
#>    time_intv_month     n mean_arr_delay
#>    <date>          <int>          <dbl>
#>  1 2013-01-01      27004          6.13 
#>  2 2013-02-01      24951          5.61 
#>  3 2013-03-01      28834          5.81 
#>  4 2013-04-01      28330         11.2  
#>  5 2013-05-01      28796          3.52 
#>  6 2013-06-01      28243         16.5  
#>  7 2013-07-01      29425         16.7  
#>  8 2013-08-01      29327          6.04 
#>  9 2013-09-01      27574         -4.02 
#> 10 2013-10-01      28889         -0.167
#> 11 2013-11-01      27268          0.461
#> 12 2013-12-01      28135         14.9
```

If the time unit is left unspecified, the `time` functions try to find
the highest time unit possible.

``` r
flights %>%
  time_by(time_hour)
#> # A tibble: 336,776 x 21
#> # Time:     time_intv_hour [6,936]
#> # By:       hour
#> # Span:     2013-01-01 05:00:00 - 2013-12-31 23:00:00
#>     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
#>    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
#>  1  2013     1     1      517            515         2      830            819
#>  2  2013     1     1      533            529         4      850            830
#>  3  2013     1     1      542            540         2      923            850
#>  4  2013     1     1      544            545        -1     1004           1022
#>  5  2013     1     1      554            600        -6      812            837
#>  6  2013     1     1      554            558        -4      740            728
#>  7  2013     1     1      555            600        -5      913            854
#>  8  2013     1     1      557            600        -3      709            723
#>  9  2013     1     1      557            600        -3      838            846
#> 10  2013     1     1      558            600        -2      753            745
#> # ℹ 336,766 more rows
#> # ℹ 13 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
#> #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
#> #   hour <dbl>, minute <dbl>, time_hour <dttm>, date <date>,
#> #   time_intv_hour <dttm>
```

## `time_complete()`

### Complete missing gaps in time

``` r
flights %>%
  count(time_hour) %>%
  time_complete(time_hour)
#> Assuming a time granularity of 1 hour(s)
#> # A tibble: 8,755 × 2
#>    time_hour               n
#>    <dttm>              <int>
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
#> # ℹ 8,745 more rows
```

### We can also make use of timeplyr time intervals

``` r
quarters <- time_aggregate(flights$date, time_by = "quarter", as_interval = TRUE)
interval_count(quarters)
#> # A tibble: 4 × 2
#>                   interval     n
#>                  <tm_intv> <int>
#> 1 [2013-01-01, 2013-04-01) 80789
#> 2 [2013-04-01, 2013-07-01) 85369
#> 3 [2013-07-01, 2013-10-01) 86326
#> 4 [2013-10-01, 2014-01-01) 84292

# Or simply
flights %>%
  time_by(date, time_by = "quarter", as_interval = TRUE) %>%
  count()
#> # A tibble: 4 x 2
#> # Time:     time_intv_3_months [4]
#> # By:       3 months
#> # Span:     2013-01-01 - 2013-12-31
#>         time_intv_3_months     n
#>                  <tm_intv> <int>
#> 1 [2013-01-01, 2013-04-01) 80789
#> 2 [2013-04-01, 2013-07-01) 85369
#> 3 [2013-07-01, 2013-10-01) 86326
#> 4 [2013-10-01, 2014-01-01) 84292
```

#### Ensure full weeks by setting from to the start of the week

``` r
start <- dmy("17-Jan-2013")
flights %>%
  time_by(date, "week", 
          from = floor_date(start, unit = "week")) %>%
  count()
#> # A tibble: 52 x 2
#> # Time:     time_intv_week [52]
#> # By:       week
#> # Span:     2013-01-13 - 2013-12-31
#>    time_intv_week     n
#>    <date>         <int>
#>  1 2013-01-13      6076
#>  2 2013-01-20      6012
#>  3 2013-01-27      6072
#>  4 2013-02-03      6089
#>  5 2013-02-10      6217
#>  6 2013-02-17      6349
#>  7 2013-02-24      6411
#>  8 2013-03-03      6551
#>  9 2013-03-10      6556
#> 10 2013-03-17      6549
#> # ℹ 42 more rows
```

#### Check for missing gaps in time

``` r
missing_dates(flights$date) # No missing dates
#> Date of length 0
```

``` r
time_num_gaps(flights$time_hour, time_by = "hours") # Missing hours
#> [1] 1819
```

To check for regularity use `time_is_regular`

``` r
hours <- sort(flights$time_hour)
time_is_regular(hours, time_by = "hours")
#> [1] TRUE
time_is_regular(hours, time_by = "hours", allow_gaps = FALSE)
#> [1] FALSE
time_is_regular(hours, time_by = "hours", allow_dups = FALSE)
#> [1] FALSE

# By-group
time_num_gaps(flights$time_hour, g = flights$origin, time_by = "hours")
#>  EWR  JFK  LGA 
#> 2489 1820 2468
time_is_regular(flights$time_hour, g = flights$origin, time_by = "hours")
#>   EWR   JFK   LGA 
#> FALSE FALSE FALSE
```

## `time_expand()`

Here we create monthly sequences for each destination that accounts for
the start and end dates of each destination

``` r
flights %>%
  group_by(dest) %>%
  time_expand(date, time_by = "month") %>%
  summarise(n = n(), start = min(date), end = max(date))
#> # A tibble: 105 × 4
#>    dest      n start      end       
#>    <chr> <int> <date>     <date>    
#>  1 ABQ       9 2013-04-22 2013-12-22
#>  2 ACK       6 2013-05-16 2013-10-16
#>  3 ALB      12 2013-01-01 2013-12-01
#>  4 ANC       2 2013-07-06 2013-08-06
#>  5 ATL      12 2013-01-01 2013-12-01
#>  6 AUS      12 2013-01-01 2013-12-01
#>  7 AVL      12 2013-01-01 2013-12-01
#>  8 BDL      12 2013-01-01 2013-12-01
#>  9 BGR      10 2013-03-02 2013-12-02
#> 10 BHM      12 2013-01-02 2013-12-02
#> # ℹ 95 more rows
```

To create the same grid of months for each dest, we can do the following

``` r
flights %>%
  time_expand(date, dest, time_by = "month") %>%
  summarise(n = n(), start = min(date), end = max(date), .by = dest)
#> # A tibble: 105 × 4
#>    dest      n start      end       
#>    <chr> <int> <date>     <date>    
#>  1 ABQ      12 2013-01-01 2013-12-01
#>  2 ACK      12 2013-01-01 2013-12-01
#>  3 ALB      12 2013-01-01 2013-12-01
#>  4 ANC      12 2013-01-01 2013-12-01
#>  5 ATL      12 2013-01-01 2013-12-01
#>  6 AUS      12 2013-01-01 2013-12-01
#>  7 AVL      12 2013-01-01 2013-12-01
#>  8 BDL      12 2013-01-01 2013-12-01
#>  9 BGR      12 2013-01-01 2013-12-01
#> 10 BHM      12 2013-01-01 2013-12-01
#> # ℹ 95 more rows
```

The ability to create time sequences by group is one of the most
powerful features of timeplyr.

``` r
flights %>%
  time_by(date, "month", as_interval = TRUE) %>%
  summarise(across(c(arr_time, dep_time), ~ mean(.x, na.rm = TRUE)))
#> # A tibble: 12 × 3
#>             time_intv_month arr_time dep_time
#>                   <tm_intv>    <dbl>    <dbl>
#>  1 [2013-01-01, 2013-02-01)    1523.    1347.
#>  2 [2013-02-01, 2013-03-01)    1522.    1348.
#>  3 [2013-03-01, 2013-04-01)    1510.    1359.
#>  4 [2013-04-01, 2013-05-01)    1501.    1353.
#>  5 [2013-05-01, 2013-06-01)    1503.    1351.
#>  6 [2013-06-01, 2013-07-01)    1468.    1351.
#>  7 [2013-07-01, 2013-08-01)    1456.    1353.
#>  8 [2013-08-01, 2013-09-01)    1495.    1350.
#>  9 [2013-09-01, 2013-10-01)    1504.    1334.
#> 10 [2013-10-01, 2013-11-01)    1520.    1340.
#> 11 [2013-11-01, 2013-12-01)    1523.    1344.
#> 12 [2013-12-01, 2014-01-01)    1505.    1357.
```

# Grouped rolling time functions

## By-group rolling mean over the last 3 calendar months

``` r
eu_stock <- eu_stock %>%
  mutate(date = date_decimal(time))

eu_stock %>%
    mutate(month_mean = time_roll_mean(value, window = months(3), 
                                       time = date, 
                                       g = group)) %>%
    time_ggplot(date, month_mean, group)
```

![](man/figures/README-unnamed-chunk-16-1.png)<!-- -->

## By-group rolling (locf) NA fill

``` r
# Prerequisite: Create Time series with missing values
x <- ts(c(NA, 3, 4, NA, 6, NA, NA, 8))
g <- cheapr::seq_id(c(3, 5)) # Two groups of size 3 + 5

.roll_na_fill(x) # Simple locf fill
#> Time Series:
#> Start = 1 
#> End = 8 
#> Frequency = 1 
#> [1] NA  3  4  4  6  6  6  8
roll_na_fill(x, fill_limit = 1) # Fill up to 1 NA
#> Time Series:
#> Start = 1 
#> End = 8 
#> Frequency = 1 
#> [1] NA  3  4  4  6  6 NA  8

roll_na_fill(x, g = g) # Very efficient on large data too
#> Time Series:
#> Start = 1 
#> End = 8 
#> Frequency = 1 
#> [1] NA  3  4 NA  6  6  6  8
```

## `year_month` and `year_quarter`

timeplyr has its own lightweight ‘yearmonth’ and \`yearquarter’ classes
inspired by the excellent ‘zoo’ and ‘tsibble’ packages.

``` r
today <- today()
year_month(today)
#> [1] "2024 Jun"
```

The underlying data for a `year_month` is the number of months since 1
January 1970 (epoch).

``` r
unclass(year_month("1970-01-01"))
#> [1] 0
unclass(year_month("1971-01-01"))
#> [1] 12
```

To create a sequence of ‘year_months’, one can use base arithmetic

``` r
year_month(today) + 0:12
#>  [1] "2024 Jun" "2024 Jul" "2024 Aug" "2024 Sep" "2024 Oct" "2024 Nov"
#>  [7] "2024 Dec" "2025 Jan" "2025 Feb" "2025 Mar" "2025 Apr" "2025 May"
#> [13] "2025 Jun"
year_quarter(today) + 0:4
#> [1] "2024 Q2" "2024 Q3" "2024 Q4" "2025 Q1" "2025 Q2"
```

## `time_elapsed()`

Let’s look at the time between consecutive flights for a specific flight
number

``` r
set.seed(42)
flight_201 <- flights %>%
  distinct(time_hour, flight) %>%
  filter(flight %in% sample(flight, size = 1)) %>%
  arrange(time_hour)

tail(sort(table(time_elapsed(flight_201$time_hour, "hours"))))
#> 
#>  23  25  48   6  18  24 
#>   2   3   4  33  34 218
```

Flight 201 seems to depart mostly consistently every 24 hours

We can efficiently do the same for all flight numbers

``` r
# We use fdistinct with sort as it's much faster and simpler to write
all_flights <- flights %>%
  fdistinct(flight, time_hour, sort = TRUE)
all_flights <- all_flights %>%
  mutate(elapsed = time_elapsed(time_hour, g = flight, fill = 0))
#> Assuming a time granularity of 1 hour(s)

# Flight numbers with largest relative deviation in time between flights
all_flights %>%
  q_summarise(elapsed, .by = flight) %>%
  mutate(relative_iqr = p75 / p25) %>%
  arrange(desc(relative_iqr))
#>       flight    p0   p25   p50    p75  p100 relative_iqr
#>        <int> <num> <num> <num>  <num> <num>        <num>
#>    1:   3664     0    12    24 3252.0  6480     271.0000
#>    2:   5709     0    12    24 3080.5  6137     256.7083
#>    3:    513     0    12    24 2250.5  4477     187.5417
#>    4:   3364     0    12    24 2204.5  4385     183.7083
#>    5:   1578     0    24    48 4182.5  8317     174.2708
#>   ---                                                   
#> 3840:   6114     0     0     0    0.0     0          NaN
#> 3841:   6140     0     0     0    0.0     0          NaN
#> 3842:   6165     0     0     0    0.0     0          NaN
#> 3843:   6171     0     0     0    0.0     0          NaN
#> 3844:   8500     0     0     0    0.0     0          NaN
```

`time_seq_id()` allows us to create unique IDs for regular sequences A
new ID is created every time there is a gap in the sequence

``` r
flights %>%
  select(time_hour) %>%
  arrange(time_hour) %>%
  mutate(time_id = time_seq_id(time_hour)) %>%
  filter(time_id != lag(time_id)) %>%
  count(hour(time_hour))
#> Assuming a time granularity of 1 hour(s)
#> # A tibble: 2 × 2
#>   `hour(time_hour)`     n
#>               <int> <int>
#> 1                 1     1
#> 2                 5   364
```

We can see that the gaps typically occur at 11pm and the sequence
resumes at 5am.

### Other convenience functions are included below

## `calendar()`

#### Easily join common date information to your data

``` r
flights_calendar <- flights %>%
    select(time_hour) %>%
    reframe(calendar(time_hour))
```

Now that gaps in time have been filled and we have joined our date
table, it is easy to count by any time dimension we like

``` r
flights_calendar %>% 
  fcount(isoyear, isoweek)
#> # A tibble: 53 × 3
#>    isoyear isoweek     n
#>      <int>   <int> <int>
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
#> # ℹ 43 more rows
flights_calendar %>% 
  fcount(isoweek = iso_week(time))
#> # A tibble: 53 × 2
#>    isoweek      n
#>    <chr>    <int>
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
#> # ℹ 43 more rows
flights_calendar %>% 
  fcount(month_l)
#> # A tibble: 12 × 2
#>    month_l     n
#>    <ord>   <int>
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
#> [1] 24
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
time_seq(start, end, time_by = "month") # lubridate version
#>  [1] "2020-01-31" "2020-02-29" "2020-03-31" "2020-04-30" "2020-05-31"
#>  [6] "2020-06-30" "2020-07-31" "2020-08-31" "2020-09-30" "2020-10-31"
#> [11] "2020-11-30" "2020-12-31" "2021-01-31"
```

`time_seq()` doesn’t mind mixing dates and datetimes

``` r
time_seq(start, as_datetime(end), time_by = "2 weeks")
#>  [1] "2020-01-31 UTC" "2020-02-14 UTC" "2020-02-28 UTC" "2020-03-13 UTC"
#>  [5] "2020-03-27 UTC" "2020-04-10 UTC" "2020-04-24 UTC" "2020-05-08 UTC"
#>  [9] "2020-05-22 UTC" "2020-06-05 UTC" "2020-06-19 UTC" "2020-07-03 UTC"
#> [13] "2020-07-17 UTC" "2020-07-31 UTC" "2020-08-14 UTC" "2020-08-28 UTC"
#> [17] "2020-09-11 UTC" "2020-09-25 UTC" "2020-10-09 UTC" "2020-10-23 UTC"
#> [21] "2020-11-06 UTC" "2020-11-20 UTC" "2020-12-04 UTC" "2020-12-18 UTC"
#> [25] "2021-01-01 UTC" "2021-01-15 UTC" "2021-01-29 UTC"
```

## `time_seq_v()`

A vectorised version of `time_seq()` Currently it is vectorised over
from, to and by

``` r
# 3 sequences
time_seq_v(from = start, 
           to = end, 
           time_by = list("months" = 1:3))
#>  [1] "2020-01-31" "2020-02-29" "2020-03-31" "2020-04-30" "2020-05-31"
#>  [6] "2020-06-30" "2020-07-31" "2020-08-31" "2020-09-30" "2020-10-31"
#> [11] "2020-11-30" "2020-12-31" "2021-01-31" "2020-01-31" "2020-03-31"
#> [16] "2020-05-31" "2020-07-31" "2020-09-30" "2020-11-30" "2021-01-31"
#> [21] "2020-01-31" "2020-04-30" "2020-07-31" "2020-10-31" "2021-01-31"
# Equivalent to 
c(time_seq(start, end, time_by = "month"),
  time_seq(start, end, time_by = "2 months"),
  time_seq(start, end, time_by = "3 months"))
#>  [1] "2020-01-31" "2020-02-29" "2020-03-31" "2020-04-30" "2020-05-31"
#>  [6] "2020-06-30" "2020-07-31" "2020-08-31" "2020-09-30" "2020-10-31"
#> [11] "2020-11-30" "2020-12-31" "2021-01-31" "2020-01-31" "2020-03-31"
#> [16] "2020-05-31" "2020-07-31" "2020-09-30" "2020-11-30" "2021-01-31"
#> [21] "2020-01-31" "2020-04-30" "2020-07-31" "2020-10-31" "2021-01-31"
```

## `time_seq_sizes()`

Vectorised function that calculates time sequence lengths

``` r
seq_lengths <- time_seq_sizes(start, start + days(c(1, 10, 20)), 
                              time_by = list("days" = c(1, 5, 10)))
seq_lengths
#> [1] 2 3 3

# Use time_seq_v2() if you know the sequence lengths
seqs <- time_seq_v2(seq_lengths, start, time_by = list("days" = c(1, 5, 10)))
seqs
#> [1] "2020-01-31" "2020-02-01" "2020-01-31" "2020-02-05" "2020-02-10"
#> [6] "2020-01-31" "2020-02-10" "2020-02-20"
```

Dealing with impossible dates and datetimes is very simple

``` r
time_seq(start, end, time_by = "month", roll_month = "postday") # roll impossible months forward
#>  [1] "2020-01-31" "2020-03-01" "2020-03-31" "2020-05-01" "2020-05-31"
#>  [6] "2020-07-01" "2020-07-31" "2020-08-31" "2020-10-01" "2020-10-31"
#> [11] "2020-12-01" "2020-12-31" "2021-01-31"
time_seq(start, end, time_by = "month", roll_month = "NA") # no roll
#>  [1] "2020-01-31" NA           "2020-03-31" NA           "2020-05-31"
#>  [6] NA           "2020-07-31" "2020-08-31" NA           "2020-10-31"
#> [11] NA           "2020-12-31" "2021-01-31"

time_seq(start, end, time_by = dmonths(1)) # lubridate version with durations
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
#> [1] "2024-W23"
iso_week(today(), day = TRUE)
#> [1] "2024-W23-2"
iso_week(today(), year = FALSE)
#> [1] "W23"
```

## `time_cut()`

Create pretty time axes using `time_breaks()`

``` r
times <- flights$time_hour
dates <- flights$date

date_breaks <- time_breaks(dates, n = 12)
time_breaks <- time_breaks(times, n = 12, time_floor = TRUE)

weekly_data <- flights %>%
    time_by(time = date, time_by = "week",
            to = max(time_span(date, time_by = "week")),
            .name = "date") %>%
    count()
weekly_data %>%
  ggplot(aes(x = date, y = n)) + 
  geom_bar(stat = "identity", fill = "#0072B2") + 
  scale_x_date(breaks = date_breaks, labels = scales::label_date_short())
```

![](man/figures/README-unnamed-chunk-34-1.png)<!-- -->

``` r

flights %>%
  ggplot(aes(x = time_hour)) + 
  geom_bar(fill = "#0072B2") + 
  scale_x_datetime(breaks = time_breaks, labels = scales::label_date_short())
```

![](man/figures/README-unnamed-chunk-34-2.png)<!-- -->
