# time_by

    Code
      flights %>% time_by(time_hour, time_by = "3 days", from = start, to = end,
        time_type = "period") %>% fcount()
    Output
      # A tibble: 5 x 2
      # Time:     time_intv_3_days [5]
      # By:       3 days
      # Span:     2013-03-16 07:43:48 - 2013-12-31 23:00:00
        time_intv_3_days         n
        <dttm>               <int>
      1 2013-03-16 07:43:48   2694
      2 2013-03-19 07:43:48   2918
      3 2013-03-22 07:43:48   2650
      4 2013-03-25 07:43:48    977
      5 NA                  327537

---

    Code
      flights %>% fslice(0) %>% time_by(time_hour)
    Output
      # A tibble: 0 x 20
      # Time:     time_intv_second [0]
      # By:       second
      # Span:     NA - NA
      # i 20 variables: year <int>, month <int>, day <int>, dep_time <int>,
      #   sched_dep_time <int>, dep_delay <dbl>, arr_time <int>,
      #   sched_arr_time <int>, arr_delay <dbl>, carrier <chr>, flight <int>,
      #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
      #   hour <dbl>, minute <dbl>, time_hour <dttm>, time_intv_second <dttm>

