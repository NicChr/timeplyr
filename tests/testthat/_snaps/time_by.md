# time_by

    Code
      time_by(dplyr::filter(flights, dplyr::between(time_hour, start, end)),
      time_hour, "3 days")
    Output
      # A tibble: 9,239 x 19
      # Time:     time_hour [4]
      # Width:    3 days
      # Range:    2013-03-16 08:00:00 -- 2013-03-28 08:00:00
          year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
         <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
       1  2013     3    16       37           2358        39      408            338
       2  2013     3    16       57           2245       132      201           2356
       3  2013     3    16      753            801        -8      854            908
       4  2013     3    16      753            800        -7     1045           1116
       5  2013     3    16      753            800        -7      945           1004
       6  2013     3    16      753            800        -7     1020           1031
       7  2013     3    16      754            802        -8      907            919
       8  2013     3    16      756            800        -4      953           1000
       9  2013     3    16      757            800        -3      958           1012
      10  2013     3    16      758            805        -7     1055           1105
      # i 9,229 more rows
      # i 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
      #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
      #   hour <dbl>, minute <dbl>, time_hour <tm_ntrvl>

---

    Code
      time_by(fastplyr::f_slice(flights, 0), time_hour)
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

