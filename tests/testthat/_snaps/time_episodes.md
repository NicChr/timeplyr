# Simple episodic tests

    Code
      df %>% time_episodes(time, time_by = 1, window = 3, .add = FALSE,
        switch_on_boundary = TRUE) %>% fastplyr::f_arrange(time)
    Output
      # A tibble:        15 x 5
      # Episodes:        N: 5, Median: 5, Mean: 5 ▁▁▇▁▁▁▁
      # Time b/w events: Pooled mean: ~1.86 numeric units
      # Threshold:       3 numeric units
         time       t_elapsed ep_start   ep_id ep_id_new
         <date>         <dbl> <date>     <int>     <int>
       1 1970-01-04         0 1970-01-04     1         1
       2 1970-01-04         0 1970-01-04     1         0
       3 1970-01-07         3 1970-01-07     2         2
       4 1970-01-08         1 1970-01-07     2         0
       5 1970-01-09         1 1970-01-07     2         0
       6 1970-01-09         0 1970-01-07     2         0
       7 1970-01-15         6 1970-01-15     3         3
       8 1970-01-20         5 1970-01-20     4         4
       9 1970-01-20         0 1970-01-20     4         0
      10 1970-01-22         2 1970-01-20     4         0
      11 1970-01-24         2 1970-01-20     4         0
      12 1970-01-24         0 1970-01-20     4         0
      13 1970-01-25         1 1970-01-20     4         0
      14 1970-01-27         2 1970-01-20     4         0
      15 1970-01-30         3 1970-01-30     5         5

---

    Code
      df %>% time_episodes(time, time_by = 1, window = 3, .add = TRUE,
        switch_on_boundary = FALSE) %>% fastplyr::f_arrange(time)
    Output
      # A tibble:        15 x 6
      # Episodes:        N: 3, Median: 3, Mean: 3 ▁▁▇▁▁▁▁
      # Time b/w events: Pooled mean: ~1.86 numeric units
      # Threshold:       3 numeric units
         time       event t_elapsed ep_start   ep_id ep_id_new
         <date>     <chr>     <dbl> <date>     <int>     <int>
       1 1970-01-04 e             0 1970-01-04     1         1
       2 1970-01-04 ne            0 1970-01-04     1         0
       3 1970-01-07 e             3 1970-01-04     1         0
       4 1970-01-08 e             1 1970-01-04     1         0
       5 1970-01-09 e             1 1970-01-04     1         0
       6 1970-01-09 e             0 1970-01-04     1         0
       7 1970-01-15 ne            6 1970-01-15     2         2
       8 1970-01-20 e             5 1970-01-20     3         3
       9 1970-01-20 ne            0 1970-01-20     3         0
      10 1970-01-22 ne            2 1970-01-20     3         0
      11 1970-01-24 ne            2 1970-01-20     3         0
      12 1970-01-24 e             0 1970-01-20     3         0
      13 1970-01-25 e             1 1970-01-20     3         0
      14 1970-01-27 ne            2 1970-01-20     3         0
      15 1970-01-30 ne            3 1970-01-20     3         0

---

    Code
      df %>% time_episodes(time, time_by = 1, window = 3, .add = TRUE,
        switch_on_boundary = TRUE, event = list(event = "e")) %>% fastplyr::f_arrange(
        time)
    Output
      # A tibble:        15 x 6
      # Episodes:        N: 4, Median: 4, Mean: 4 ▁▁▇▁▁▁▁
      # Time b/w events: Pooled mean: 3 numeric units
      # Threshold:       3 numeric units
         time       event t_elapsed ep_start   ep_id ep_id_new
         <date>     <chr>     <dbl> <date>     <int>     <int>
       1 1970-01-04 e             0 1970-01-04     1         1
       2 1970-01-04 ne           NA NA            NA        NA
       3 1970-01-07 e             3 1970-01-07     2         2
       4 1970-01-08 e             1 1970-01-07     2         0
       5 1970-01-09 e             1 1970-01-07     2         0
       6 1970-01-09 e             0 1970-01-07     2         0
       7 1970-01-15 ne           NA NA            NA        NA
       8 1970-01-20 e            11 1970-01-20     3         3
       9 1970-01-20 ne           NA NA            NA        NA
      10 1970-01-22 ne           NA NA            NA        NA
      11 1970-01-24 ne           NA NA            NA        NA
      12 1970-01-24 e             4 1970-01-24     4         4
      13 1970-01-25 e             1 1970-01-24     4         0
      14 1970-01-27 ne           NA NA            NA        NA
      15 1970-01-30 ne           NA NA            NA        NA

---

    Code
      df %>% time_episodes(time, time_by = 3, window = 1, .add = FALSE,
        switch_on_boundary = FALSE, event = list(event = "e")) %>% fastplyr::f_arrange(
        time)
    Output
      # A tibble:        15 x 6
      # Episodes:        N: 3, Median: 3, Mean: 3 ▁▁▇▁▁▁▁
      # Time b/w events: Pooled mean: 3 numeric units
      # Threshold:       3 numeric units
         time       event t_elapsed ep_start   ep_id ep_id_new
         <date>     <chr>     <dbl> <date>     <int>     <int>
       1 1970-01-04 e         0     1970-01-04     1         1
       2 1970-01-04 ne       NA     NA            NA        NA
       3 1970-01-07 e         1     1970-01-04     1         0
       4 1970-01-08 e         0.333 1970-01-04     1         0
       5 1970-01-09 e         0.333 1970-01-04     1         0
       6 1970-01-09 e         0     1970-01-04     1         0
       7 1970-01-15 ne       NA     NA            NA        NA
       8 1970-01-20 e         3.67  1970-01-20     2         2
       9 1970-01-20 ne       NA     NA            NA        NA
      10 1970-01-22 ne       NA     NA            NA        NA
      11 1970-01-24 ne       NA     NA            NA        NA
      12 1970-01-24 e         1.33  1970-01-24     3         3
      13 1970-01-25 e         0.333 1970-01-24     3         0
      14 1970-01-27 ne       NA     NA            NA        NA
      15 1970-01-30 ne       NA     NA            NA        NA

---

    Code
      df %>% time_episodes(time, time_by = "days", window = 5, .add = FALSE,
        roll_episode = FALSE, switch_on_boundary = TRUE) %>% fastplyr::f_arrange(time)
    Output
      # A tibble:        15 x 5
      # Episodes:        N: 6, Median: 6, Mean: 6 ▁▁▇▁▁▁▁
      # Time b/w events: Pooled mean: ~1.92 weeks
      # Threshold:       5 days
         time       t_elapsed ep_start   ep_id ep_id_new
         <date>         <int> <date>     <int>     <int>
       1 1970-01-04         0 1970-01-04     1         1
       2 1970-01-04         0 1970-01-04     1         0
       3 1970-01-07         3 1970-01-04     1         0
       4 1970-01-08         4 1970-01-04     1         0
       5 1970-01-09         5 1970-01-09     2         2
       6 1970-01-09         5 1970-01-09     2         0
       7 1970-01-15        11 1970-01-15     3         3
       8 1970-01-20        16 1970-01-20     4         4
       9 1970-01-20        16 1970-01-20     4         0
      10 1970-01-22        18 1970-01-20     4         0
      11 1970-01-24        20 1970-01-20     4         0
      12 1970-01-24        20 1970-01-20     4         0
      13 1970-01-25        21 1970-01-25     5         5
      14 1970-01-27        23 1970-01-25     5         0
      15 1970-01-30        26 1970-01-30     6         6

---

    Code
      df %>% time_episodes(time, time_by = "5 days", window = 1, .add = FALSE,
        roll_episode = FALSE, switch_on_boundary = FALSE) %>% fastplyr::f_arrange(
        time)
    Output
      # A tibble:        15 x 5
      # Episodes:        N: 4, Median: 4, Mean: 4 ▁▁▇▁▁▁▁
      # Time b/w events: Pooled mean: ~1.92 weeks
      # Threshold:       5 days
         time       t_elapsed ep_start   ep_id ep_id_new
         <date>         <dbl> <date>     <int>     <int>
       1 1970-01-04       0   1970-01-04     1         1
       2 1970-01-04       0   1970-01-04     1         0
       3 1970-01-07       0.6 1970-01-04     1         0
       4 1970-01-08       0.8 1970-01-04     1         0
       5 1970-01-09       1   1970-01-04     1         0
       6 1970-01-09       1   1970-01-04     1         0
       7 1970-01-15       2.2 1970-01-15     2         2
       8 1970-01-20       3.2 1970-01-15     2         0
       9 1970-01-20       3.2 1970-01-15     2         0
      10 1970-01-22       3.6 1970-01-22     3         3
      11 1970-01-24       4   1970-01-22     3         0
      12 1970-01-24       4   1970-01-22     3         0
      13 1970-01-25       4.2 1970-01-22     3         0
      14 1970-01-27       4.6 1970-01-22     3         0
      15 1970-01-30       5.2 1970-01-30     4         4

