# time_by

    Code
      flights %>% dplyr::filter(dplyr::between(time_hour, start, end)) %>% time_by(
        time_hour, "3 days") %>% fastplyr::f_count()
    Output
      # A tibble: 4 x 2
      # Time:     time_hour [4]
      # Width:    3 days
      # Range:    2013-03-16 08:00:00 -- 2013-03-28 08:00:00
        time_hour                      n
        <tm_ntrvl>                 <int>
      1 [2013-03-16 08:00:00, +3D)  2694
      2 [2013-03-19 08:00:00, +3D)  2918
      3 [2013-03-22 08:00:00, +3D)  2650
      4 [2013-03-25 08:00:00, +3D)   977

---

    Code
      flights %>% fastplyr::f_slice(0) %>% time_by(time_hour)
    Output
      # A tibble: 0 x 19
      # Time:     time_hour [0]
      # Width:    sec
      # Range:    NA -- NA
      # i 19 variables: year <int>, month <int>, day <int>, dep_time <int>,
      #   sched_dep_time <int>, dep_delay <dbl>, arr_time <int>,
      #   sched_arr_time <int>, arr_delay <dbl>, carrier <chr>, flight <int>,
      #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
      #   hour <dbl>, minute <dbl>, time_hour <tm_ntrvl>

