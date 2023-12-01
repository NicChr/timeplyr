# fskim

    Code
      fskim(dplyr::slice(iris, 0))
    Output
      $nrow
      [1] 0
      
      $ncol
      [1] 5
      
      $logical
      # A tibble: 0 x 9
      # i 9 variables: col <chr>, class <chr>, n_missing <int>, p_complete <dbl>,
      #   n_true <int>, n_false <int>, p_true <dbl>, head <chr>, tail <chr>
      
      $numeric
      # A tibble: 4 x 15
        col    class n_missing p_complete n_unique  mean    p0   p25   p50   p75  p100
        <chr>  <chr>     <int>      <dbl>    <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
      1 Sepal~ nume~        NA         NA       NA    NA    NA    NA    NA    NA    NA
      2 Sepal~ nume~        NA         NA       NA    NA    NA    NA    NA    NA    NA
      3 Petal~ nume~        NA         NA       NA    NA    NA    NA    NA    NA    NA
      4 Petal~ nume~        NA         NA       NA    NA    NA    NA    NA    NA    NA
      # i 4 more variables: iqr <dbl>, sd <dbl>, head <chr>, tail <chr>
      
      $date
      # A tibble: 0 x 9
      # i 9 variables: col <chr>, class <chr>, n_missing <int>, p_complete <dbl>,
      #   n_unique <int>, min <date>, max <date>, head <chr>, tail <chr>
      
      $datetime
      # A tibble: 0 x 9
      # i 9 variables: col <chr>, class <chr>, n_missing <int>, p_complete <dbl>,
      #   n_unique <int>, min <dttm>, max <dttm>, head <chr>, tail <chr>
      
      $categorical
      # A tibble: 1 x 9
        col     class  n_missing p_complete n_unique min   max   head  tail 
        <chr>   <chr>      <int>      <dbl>    <int> <chr> <chr> <chr> <chr>
      1 Species factor        NA         NA       NA <NA>  <NA>  <NA>  <NA> 
      

---

    Code
      fskim(dplyr::select(iris))
    Output
      $nrow
      [1] 150
      
      $ncol
      [1] 0
      
      $logical
      # A tibble: 0 x 9
      # i 9 variables: col <chr>, class <chr>, n_missing <int>, p_complete <dbl>,
      #   n_true <int>, n_false <int>, p_true <dbl>, head <chr>, tail <chr>
      
      $numeric
      # A tibble: 0 x 15
      # i 15 variables: col <chr>, class <chr>, n_missing <int>, p_complete <dbl>,
      #   n_unique <int>, mean <dbl>, p0 <dbl>, p25 <dbl>, p50 <dbl>, p75 <dbl>,
      #   p100 <dbl>, iqr <dbl>, sd <dbl>, head <chr>, tail <chr>
      
      $date
      # A tibble: 0 x 9
      # i 9 variables: col <chr>, class <chr>, n_missing <int>, p_complete <dbl>,
      #   n_unique <int>, min <date>, max <date>, head <chr>, tail <chr>
      
      $datetime
      # A tibble: 0 x 9
      # i 9 variables: col <chr>, class <chr>, n_missing <int>, p_complete <dbl>,
      #   n_unique <int>, min <dttm>, max <dttm>, head <chr>, tail <chr>
      
      $categorical
      # A tibble: 0 x 9
      # i 9 variables: col <chr>, class <chr>, n_missing <int>, p_complete <dbl>,
      #   n_unique <int>, min <chr>, max <chr>, head <chr>, tail <chr>
      

