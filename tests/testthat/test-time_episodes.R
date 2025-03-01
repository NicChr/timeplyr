# Set number of data.table threads to 2
data.table::setDTthreads(threads = 1L)
# Set number of collapse threads to 1
collapse::set_collapse(nthreads = 1L)

test_that("Testing time episodes", {
  flights <- dplyr::select(nycflights13::flights, origin, dest, time_hour)

  flights <- fastplyr::add_row_id(flights, .name = "id")

  na_ids <- flights %>%
    fastplyr::f_slice_sample(n = (5 * 10^4)) %>%
    cheapr::with_local_seed(.seed = 98712412) %>%
    dplyr::pull(id)

  flights <- flights %>%
    dplyr::mutate(time_hour = cheapr::cheapr_if_else(
      id %in% na_ids,
      lubridate::NA_POSIXct_, time_hour
    ))

  flights <- flights %>%
    fastplyr::add_group_id(.name = "id1") %>%
    fastplyr::add_group_id(origin, dest,
      .name = "id2"
    )
  expect_error(time_episodes(flights))
  expect_error(time_episodes(flights, window = 100))
  # expect_error(time_episodes(flights, time = time_hour))
  base1 <- structure(
    list(
      ep_id = c(
        1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L,
        5L, 5L, 6L, 6L, NA
      ),
      ep_id_new = c(0L, 1L, 0L, 2L, 0L, 3L, 0L, 4L, 0L, 5L, 0L, 6L, NA),
      n = c(198449L, 1L, 9393L, 1L, 3271L, 1L, 13361L, 1L, 16555L, 1L, 45741L, 1L, 50000L)
    ),
    row.names = c(NA, 13L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  base2 <- structure(
    list(
      ep_id = c(
        1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L,
        5L, 5L, 6L, 6L, 7L, 7L, NA
      ),
      ep_id_new = c(0L, 1L, 0L, 2L, 0L, 3L, 0L, 4L, 0L, 5L, 0L, 6L, 0L, 7L, NA),
      n = c(280789L, 224L, 3121L, 48L, 1463L, 30L, 198L, 11L, 858L, 5L, 25L, 2L, 1L, 1L, 50000L)
    ),
    row.names = c(NA, 15L), class = c("tbl_df", "tbl", "data.frame")
  )
  base3 <- structure(
    list(
      id1 = c(
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
        1L, 1L, 1L
      ),
      ep_id = c(
        1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L,
        5L, 6L, 6L, NA
      ),
      ep_id_new = c(0L, 1L, 0L, 2L, 0L, 3L, 0L, 4L, 0L, 5L, 0L, 6L, NA),
      ep_start = structure(
        c(
          1357034400, 1357034400, 1378803600, 1378803600, 1379840400, 1379840400, 1380186000,
          1380186000, 1381654800, 1381654800, 1383472800, 1383472800, NA
        ),
        tzone = "UTC",
        class = c("POSIXct", "POSIXt")
      ),
      n = c(198449L, 1L, 9393L, 1L, 3271L, 1L, 13361L, 1L, 16555L, 1L, 45741L, 1L, 50000L)
    ),
    row.names = c(NA, 13L), class = c("tbl_df", "tbl", "data.frame")
  )
  base4 <- structure(
    list(
      id2 = c(
        1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 3L,
        3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L, 6L,
        6L, 7L, 7L, 7L, 8L, 8L, 8L, 9L, 9L, 9L, 10L, 10L, 10L, 10L, 10L,
        11L, 11L, 11L, 12L, 12L, 12L, 12L, 12L, 12L, 12L, 13L, 13L, 13L,
        13L, 13L, 13L, 13L, 14L, 14L, 14L, 14L, 14L, 14L, 14L, 15L, 15L,
        15L, 16L, 16L, 16L, 17L, 17L, 17L, 18L, 18L, 18L, 19L, 19L, 19L,
        20L, 20L, 20L, 21L, 21L, 21L, 22L, 22L, 22L, 23L, 23L, 23L, 24L,
        24L, 24L, 25L, 25L, 25L, 26L, 26L, 26L, 26L, 26L, 27L, 27L, 27L,
        28L, 28L, 28L, 29L, 29L, 29L, 30L, 30L, 30L, 31L, 31L, 31L, 31L,
        31L, 31L, 32L, 32L, 32L, 33L, 33L, 33L, 34L, 34L, 34L, 35L, 35L,
        35L, 36L, 36L, 36L, 37L, 37L, 37L, 37L, 37L, 37L, 38L, 38L, 38L,
        39L, 39L, 39L, 40L, 40L, 40L, 41L, 42L, 42L, 42L, 43L, 43L, 43L,
        44L, 44L, 44L, 45L, 45L, 45L, 46L, 46L, 46L, 47L, 47L, 47L, 48L,
        48L, 48L, 49L, 49L, 49L, 50L, 50L, 50L, 51L, 51L, 51L, 52L, 52L,
        52L, 52L, 52L, 52L, 52L, 52L, 53L, 53L, 53L, 53L, 53L, 54L, 54L,
        54L, 55L, 55L, 55L, 56L, 56L, 56L, 57L, 57L, 57L, 58L, 58L, 58L,
        59L, 59L, 59L, 60L, 60L, 60L, 60L, 60L, 61L, 61L, 61L, 62L, 62L,
        62L, 63L, 63L, 63L, 63L, 63L, 63L, 63L, 64L, 64L, 64L, 65L, 65L,
        65L, 66L, 66L, 66L, 67L, 67L, 67L, 67L, 67L, 67L, 67L, 68L, 68L,
        68L, 69L, 69L, 69L, 70L, 70L, 70L, 71L, 71L, 71L, 72L, 72L, 72L,
        73L, 73L, 73L, 74L, 74L, 74L, 75L, 75L, 75L, 76L, 76L, 76L, 77L,
        77L, 77L, 78L, 78L, 78L, 79L, 79L, 79L, 80L, 80L, 80L, 80L, 80L,
        80L, 80L, 80L, 80L, 80L, 80L, 81L, 81L, 81L, 81L, 81L, 81L, 81L,
        81L, 82L, 82L, 82L, 83L, 83L, 83L, 84L, 84L, 84L, 85L, 85L, 85L,
        86L, 86L, 86L, 87L, 87L, 87L, 88L, 88L, 88L, 89L, 89L, 89L, 90L,
        90L, 90L, 91L, 92L, 92L, 92L, 93L, 93L, 93L, 94L, 94L, 94L, 95L,
        95L, 95L, 96L, 96L, 96L, 97L, 97L, 97L, 98L, 98L, 98L, 99L, 99L,
        99L, 99L, 99L, 99L, 99L, 99L, 99L, 100L, 100L, 100L, 101L, 101L,
        101L, 102L, 102L, 102L, 103L, 103L, 103L, 104L, 104L, 104L, 105L,
        105L, 105L, 106L, 106L, 106L, 107L, 107L, 107L, 108L, 108L, 108L,
        108L, 108L, 109L, 109L, 109L, 110L, 110L, 110L, 111L, 111L, 111L,
        112L, 112L, 112L, 113L, 113L, 113L, 114L, 114L, 114L, 115L, 115L,
        116L, 116L, 116L, 117L, 117L, 117L, 118L, 118L, 118L, 119L, 119L,
        119L, 120L, 120L, 120L, 120L, 121L, 121L, 121L, 122L, 123L, 123L,
        123L, 124L, 124L, 124L, 125L, 125L, 125L, 126L, 126L, 126L, 127L,
        127L, 127L, 128L, 128L, 128L, 129L, 129L, 129L, 130L, 130L, 130L,
        131L, 131L, 131L, 132L, 132L, 132L, 133L, 133L, 133L, 134L, 134L,
        134L, 135L, 135L, 135L, 136L, 136L, 136L, 137L, 137L, 137L, 137L,
        137L, 137L, 137L, 137L, 138L, 138L, 138L, 139L, 139L, 139L, 140L,
        140L, 140L, 140L, 140L, 140L, 140L, 141L, 141L, 141L, 142L, 142L,
        142L, 143L, 143L, 143L, 144L, 144L, 144L, 145L, 145L, 145L, 145L,
        145L, 145L, 145L, 146L, 146L, 146L, 147L, 147L, 147L, 148L, 148L,
        148L, 149L, 149L, 149L, 150L, 150L, 150L, 151L, 151L, 151L, 152L,
        152L, 152L, 153L, 154L, 154L, 154L, 155L, 155L, 155L, 156L, 156L,
        156L, 157L, 157L, 157L, 158L, 158L, 158L, 159L, 159L, 159L, 159L,
        160L, 160L, 160L, 160L, 160L, 161L, 161L, 161L, 162L, 162L, 162L,
        163L, 163L, 163L, 163L, 163L, 164L, 164L, 164L, 164L, 164L, 164L,
        164L, 164L, 164L, 165L, 165L, 165L, 166L, 166L, 166L, 166L, 167L,
        167L, 167L, 168L, 168L, 168L, 168L, 168L, 169L, 169L, 169L, 170L,
        170L, 170L, 171L, 171L, 171L, 172L, 172L, 172L, 173L, 173L, 173L,
        174L, 174L, 174L, 174L, 174L, 174L, 174L, 175L, 175L, 175L, 176L,
        176L, 176L, 177L, 177L, 177L, 178L, 178L, 178L, 179L, 179L, 179L,
        180L, 180L, 180L, 181L, 181L, 181L, 181L, 181L, 181L, 182L, 182L,
        182L, 183L, 183L, 183L, 183L, 183L, 183L, 184L, 184L, 184L, 184L,
        184L, 184L, 185L, 185L, 185L, 185L, 186L, 186L, 186L, 187L, 187L,
        187L, 188L, 188L, 188L, 189L, 189L, 189L, 190L, 190L, 190L, 190L,
        190L, 190L, 190L, 191L, 191L, 191L, 191L, 191L, 192L, 193L, 193L,
        193L, 193L, 193L, 193L, 193L, 194L, 194L, 194L, 195L, 195L, 195L,
        196L, 196L, 196L, 197L, 197L, 197L, 197L, 197L, 197L, 197L, 197L,
        198L, 198L, 198L, 199L, 199L, 199L, 200L, 200L, 200L, 200L, 200L,
        201L, 201L, 201L, 202L, 202L, 202L, 203L, 203L, 203L, 204L, 204L,
        204L, 204L, 204L, 205L, 205L, 205L, 206L, 206L, 206L, 206L, 206L,
        206L, 206L, 207L, 207L, 207L, 208L, 208L, 208L, 209L, 209L, 209L,
        210L, 210L, 210L, 210L, 210L, 210L, 211L, 211L, 211L, 212L, 212L,
        212L, 212L, 212L, 213L, 213L, 213L, 213L, 214L, 214L, 214L, 215L,
        215L, 215L, 215L, 215L, 215L, 215L, 215L, 215L, 215L, 215L, 215L,
        215L, 215L, 216L, 216L, 216L, 216L, 216L, 216L, 217L, 217L, 217L,
        218L, 218L, 218L, 219L, 219L, 219L, 220L, 220L, 220L, 220L, 220L,
        220L, 220L, 221L, 221L, 221L, 222L, 222L, 222L, 223L, 223L, 223L,
        224L, 224L, 224L
      ),
      ep_id = c(
        1L, 1L, 2L, 2L, 3L, 3L, NA,
        1L, 1L, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, 2L, 2L, NA, 1L, 1L, 2L,
        2L, 3L, 3L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, 2L,
        2L, NA, 1L, 1L, NA, 1L, 1L, 2L, 2L, 3L, 3L, NA, 1L, 1L, 2L, 2L,
        3L, 3L, NA, 1L, 1L, 2L, 2L, 3L, 3L, NA, 1L, 1L, NA, 1L, 1L, NA,
        1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L,
        1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, 2L, 2L, NA,
        1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, 2L, 3L,
        3L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L,
        NA, 1L, 2L, 2L, 3L, 3L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA,
        1L, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA,
        1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L,
        1L, 2L, 2L, 3L, 3L, 4L, NA, 1L, 1L, 2L, 2L, NA, 1L, 1L, NA, 1L,
        1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L,
        2L, 2L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, 2L, 2L, 3L, 3L, NA,
        1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, 2L, 2L, 3L, 3L, NA,
        1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 2L, NA, 1L,
        1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L,
        NA, 1L, 1L, NA, 1L, 1L, 2L, 2L, 3L, 4L, 5L, 5L, 6L, 6L, NA, 1L,
        1L, 2L, 2L, 3L, 4L, 4L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA,
        1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L,
        1L, NA, 1L, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L,
        1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, 2L, 3L, 4L, 4L, 5L, 5L,
        NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA,
        1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, 2L, 2L, NA, 1L, 1L,
        NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA,
        1L, 1L, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 2L,
        2L, NA, 1L, 1L, NA, 1L, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L,
        1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L,
        NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA,
        1L, 2L, 2L, 3L, 3L, 4L, 5L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L,
        2L, 2L, 3L, 3L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L,
        NA, 1L, 1L, 2L, 2L, 3L, 3L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L,
        NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, 1L,
        NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 2L, 2L,
        NA, 1L, 1L, 2L, 2L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, 2L, 2L,
        NA, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, NA, 1L, 1L, NA, 1L, 2L, 2L,
        NA, 1L, 1L, NA, 1L, 1L, 2L, 2L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L,
        1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, 2L, 2L, 3L, 3L, NA, 1L,
        1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L,
        NA, 1L, 2L, 2L, 3L, 3L, NA, 1L, 1L, NA, 1L, 1L, 2L, 3L, 4L, NA,
        1L, 2L, 2L, 3L, 3L, NA, 1L, 2L, 2L, NA, 1L, 1L, NA, 1L, 1L, NA,
        1L, 1L, NA, 1L, 1L, NA, 1L, 1L, 2L, 2L, 3L, 3L, NA, 1L, 1L, 2L,
        2L, NA, 1L, 1L, 1L, 2L, 2L, 3L, 3L, NA, 1L, 1L, NA, 1L, 1L, NA,
        1L, 1L, NA, 1L, 2L, 2L, 3L, 3L, 4L, 4L, NA, 1L, 1L, NA, 1L, 1L,
        NA, 1L, 1L, 2L, 2L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L,
        2L, 3L, 3L, NA, 1L, 1L, NA, 1L, 1L, 2L, 3L, 4L, 4L, NA, 1L, 1L,
        NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, 2L, 3L, 3L, NA, 1L, 1L, NA,
        1L, 1L, 2L, 2L, NA, 1L, 2L, 2L, NA, 1L, 1L, NA, 1L, 1L, 2L, 2L,
        3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, NA, 1L, 2L, 2L, 3L, 4L, 5L,
        1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, 2L, 2L, 3L, 3L, NA,
        1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA, 1L, 1L, NA
      ),
      ep_id_new = c(
        0L, 1L, 0L, 2L, 0L, 3L, NA, 0L, 1L, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, 0L, 2L, NA, 0L, 1L, 0L, 2L, 0L, 3L, NA, 0L, 1L, NA, 0L, 1L, NA,
        0L, 1L, NA, 0L, 1L, 0L, 2L, NA, 0L, 1L, NA, 0L, 1L, 0L, 2L, 0L,
        3L, NA, 0L, 1L, 0L, 2L, 0L, 3L, NA, 0L, 1L, 0L, 2L, 0L, 3L, NA,
        0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L,
        1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L,
        NA, 0L, 1L, 0L, 2L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L,
        1L, NA, 0L, 1L, 2L, 0L, 3L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L,
        NA, 0L, 1L, NA, 0L, 1L, NA, 1L, 0L, 2L, 0L, 3L, NA, 0L, 1L, NA,
        0L, 1L, NA, 0L, 1L, NA, 1L, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA,
        0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L,
        1L, NA, 0L, 1L, NA, 0L, 1L, 0L, 2L, 0L, 3L, 4L, NA, 0L, 1L, 0L,
        2L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L,
        NA, 0L, 1L, NA, 0L, 1L, 0L, 2L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L,
        1L, 0L, 2L, 0L, 3L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L,
        1L, 0L, 2L, 0L, 3L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L,
        1L, NA, 1L, 2L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L,
        NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, 0L, 2L, 3L, 4L,
        0L, 5L, 0L, 6L, NA, 0L, 1L, 0L, 2L, 3L, 0L, 4L, NA, 0L, 1L, NA,
        0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L,
        1L, NA, 0L, 1L, NA, 0L, 1L, NA, 1L, 0L, 1L, NA, 0L, 1L, NA, 0L,
        1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L,
        2L, 3L, 0L, 4L, 0L, 5L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA,
        0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L,
        1L, 0L, 2L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA,
        0L, 1L, NA, 0L, 1L, NA, 0L, 1L, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L,
        NA, 0L, 1L, NA, 1L, 0L, 2L, NA, 0L, 1L, NA, 1L, 0L, 1L, NA, 0L,
        1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L,
        NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA,
        0L, 1L, NA, 0L, 1L, NA, 1L, 0L, 2L, 0L, 3L, 4L, 5L, NA, 0L, 1L,
        NA, 0L, 1L, NA, 0L, 1L, 0L, 2L, 0L, 3L, NA, 0L, 1L, NA, 0L, 1L,
        NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, 0L, 2L, 0L, 3L, NA, 0L, 1L,
        NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA,
        0L, 1L, NA, 1L, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA,
        0L, 1L, NA, 1L, 0L, 2L, NA, 0L, 1L, 0L, 2L, NA, 0L, 1L, NA, 0L,
        1L, NA, 0L, 1L, 0L, 2L, NA, 0L, 1L, 0L, 2L, 0L, 3L, 0L, 4L, NA,
        0L, 1L, NA, 1L, 0L, 2L, NA, 0L, 1L, NA, 0L, 1L, 0L, 2L, NA, 0L,
        1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L,
        0L, 2L, 0L, 3L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L,
        NA, 0L, 1L, NA, 0L, 1L, NA, 1L, 0L, 2L, 0L, 3L, NA, 0L, 1L, NA,
        0L, 1L, 2L, 3L, 4L, NA, 1L, 0L, 2L, 0L, 3L, NA, 1L, 0L, 2L, NA,
        0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, 0L, 2L,
        0L, 3L, NA, 0L, 1L, 0L, 2L, NA, 1L, 0L, 1L, 0L, 2L, 0L, 3L, NA,
        0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 1L, 0L, 2L, 0L, 3L, 0L, 4L,
        NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, 0L, 2L, NA, 0L, 1L, NA, 0L,
        1L, NA, 0L, 1L, NA, 1L, 2L, 0L, 3L, NA, 0L, 1L, NA, 0L, 1L, 2L,
        3L, 0L, 4L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, 2L,
        0L, 3L, NA, 0L, 1L, NA, 0L, 1L, 0L, 2L, NA, 1L, 0L, 2L, NA, 0L,
        1L, NA, 0L, 1L, 0L, 2L, 3L, 0L, 4L, 0L, 5L, 0L, 6L, 0L, 7L, NA,
        1L, 0L, 2L, 3L, 4L, 5L, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L,
        1L, 0L, 2L, 0L, 3L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L, 1L, NA, 0L,
        1L, NA
      ),
      ep_start = structure(
        c(
          1357063200, 1357063200, 1367420400, 1367420400, 1383703200, 1383703200, NA, 1373140800, 1373140800, 1357038000, 1357038000, NA, 1357066800, 1357066800,
          NA, 1357048800, 1357048800, 1367496000, 1367496000, NA, 1357063200,
          1357063200, 1376355600, 1376355600, 1385211600, 1385211600, NA,
          1357070400, 1357070400, NA, 1357041600, 1357041600, NA, 1357084800,
          1357084800, NA, 1357059600, 1357059600, 1380628800, 1380628800,
          NA, 1357052400, 1357052400, NA, 1357059600, 1357059600, 1375405200,
          1375405200, 1387458000, 1387458000, NA, 1357390800, 1357390800,
          1371902400, 1371902400, 1387630800, 1387630800, NA, 1357254000,
          1357254000, 1369609200, 1369609200, 1387490400, 1387490400, NA,
          1357045200, 1357045200, NA, 1357066800, 1357066800, NA, 1357038000,
          1357038000, NA, 1357056000, 1357056000, NA, 1357063200, 1357063200,
          NA, 1357059600, 1357059600, NA, 1357048800, 1357048800, NA, 1357041600,
          1357041600, NA, 1357041600, 1357041600, NA, 1357084800, 1357084800,
          NA, 1357045200, 1357045200, NA, 1357048800, 1357048800, 1387465200,
          1387465200, NA, 1357038000, 1357038000, NA, 1357070400, 1357070400,
          NA, 1357063200, 1357063200, NA, 1357066800, 1357066800, NA, 1357390800,
          1357390800, 1364644800, 1387630800, 1387630800, NA, 1357063200,
          1357063200, NA, 1357063200, 1357063200, NA, 1357038000, 1357038000,
          NA, 1357034400, 1357034400, NA, 1357052400, 1357052400, NA, 1357045200,
          1361019600, 1361019600, 1387544400, 1387544400, NA, 1357045200,
          1357045200, NA, 1357038000, 1357038000, NA, 1357038000, 1357038000,
          NA, 1374901200, 1357066800, 1357066800, NA, 1357038000, 1357038000,
          NA, 1357045200, 1357045200, NA, 1357045200, 1357045200, NA, 1357063200,
          1357063200, NA, 1357038000, 1357038000, NA, 1357070400, 1357070400,
          NA, 1357063200, 1357063200, NA, 1357128000, 1357128000, NA, 1357210800,
          1357210800, NA, 1357394400, 1357394400, 1359813600, 1359813600,
          1362232800, 1362232800, 1388239200, NA, 1357045200, 1357045200,
          1362315600, 1362315600, NA, 1357084800, 1357084800, NA, 1357063200,
          1357063200, NA, 1357034400, 1357034400, NA, 1357131600, 1357131600,
          NA, 1357038000, 1357038000, NA, 1357077600, 1357077600, NA, 1357264800,
          1357264800, 1362268800, 1362268800, NA, 1357038000, 1357038000,
          NA, 1357081200, 1357081200, NA, 1357092000, 1357092000, 1367420400,
          1367420400, 1376244000, 1376244000, NA, 1357066800, 1357066800,
          NA, 1357045200, 1357045200, NA, 1357063200, 1357063200, NA, 1357056000,
          1357056000, 1362157200, 1362157200, 1383652800, 1383652800, NA,
          1357041600, 1357041600, NA, 1357045200, 1357045200, NA, 1357077600,
          1357077600, NA, 1357048800, 1357048800, NA, 1378641600, 1379851200,
          NA, 1357074000, 1357074000, NA, 1357041600, 1357041600, NA, 1357038000,
          1357038000, NA, 1357041600, 1357041600, NA, 1357164000, 1357164000,
          NA, 1357038000, 1357038000, NA, 1357048800, 1357048800, NA, 1357131600,
          1357131600, 1372510800, 1372510800, 1377349200, 1380373200, 1381582800,
          1381582800, 1383397200, 1383397200, NA, 1357131600, 1357131600,
          1378738800, 1378738800, 1383656400, 1384952400, 1384952400, NA,
          1357041600, 1357041600, NA, 1357084800, 1357084800, NA, 1373047200,
          1373047200, NA, 1357084800, 1357084800, NA, 1357131600, 1357131600,
          NA, 1366761600, 1366761600, NA, 1368716400, 1368716400, NA, 1357038000,
          1357038000, NA, 1357048800, 1357048800, NA, 1369490400, 1357074000,
          1357074000, NA, 1357034400, 1357034400, NA, 1357034400, 1357034400,
          NA, 1357048800, 1357048800, NA, 1357038000, 1357038000, NA, 1357063200,
          1357063200, NA, 1357070400, 1357070400, NA, 1357081200, 1357081200,
          1358031600, 1359241200, 1360450800, 1360450800, 1362052800, 1362052800,
          NA, 1357066800, 1357066800, NA, 1357045200, 1357045200, NA, 1357070400,
          1357070400, NA, 1357074000, 1357074000, NA, 1357045200, 1357045200,
          NA, 1357074000, 1357074000, NA, 1357074000, 1357074000, NA, 1357045200,
          1357045200, NA, 1357077600, 1357077600, 1387490400, 1387490400,
          NA, 1357038000, 1357038000, NA, 1357048800, 1357048800, NA, 1357056000,
          1357056000, NA, 1357048800, 1357048800, NA, 1364936400, 1364936400,
          NA, 1357070400, 1357070400, NA, 1387630800, 1387630800, 1357045200,
          1357045200, NA, 1357038000, 1357038000, NA, 1357038000, 1357038000,
          NA, 1357059600, 1357059600, NA, 1361055600, 1362178800, 1362178800,
          NA, 1357038000, 1357038000, NA, 1357160400, 1357041600, 1357041600,
          NA, 1362258000, 1362258000, NA, 1357045200, 1357045200, NA, 1357038000,
          1357038000, NA, 1368723600, 1368723600, NA, 1357081200, 1357081200,
          NA, 1357045200, 1357045200, NA, 1357077600, 1357077600, NA, 1357063200,
          1357063200, NA, 1357077600, 1357077600, NA, 1357088400, 1357088400,
          NA, 1357038000, 1357038000, NA, 1357045200, 1357045200, NA, 1357099200,
          1357099200, NA, 1357398000, 1358607600, 1358607600, 1363442400,
          1363442400, 1367071200, 1387638000, NA, 1357048800, 1357048800,
          NA, 1357045200, 1357045200, NA, 1357254000, 1357254000, 1372604400,
          1372604400, 1380718800, 1380718800, NA, 1357041600, 1357041600,
          NA, 1357038000, 1357038000, NA, 1357045200, 1357045200, NA, 1357084800,
          1357084800, NA, 1365883200, 1365883200, 1373227200, 1373227200,
          1387659600, 1387659600, NA, 1357041600, 1357041600, NA, 1357038000,
          1357038000, NA, 1357167600, 1357167600, NA, 1357038000, 1357038000,
          NA, 1357038000, 1357038000, NA, 1357167600, 1357167600, NA, 1357041600,
          1357041600, NA, 1383415200, 1357045200, 1357045200, NA, 1357041600,
          1357041600, NA, 1357038000, 1357038000, NA, 1357038000, 1357038000,
          NA, 1366549200, 1366549200, NA, 1362240000, 1364742000, 1364742000,
          NA, 1357340400, 1357340400, 1387584000, 1387584000, NA, 1357045200,
          1357045200, NA, 1357142400, 1357142400, NA, 1362405600, 1362405600,
          1387461600, 1387461600, NA, 1357070400, 1357070400, 1362870000,
          1362870000, 1378342800, 1378342800, 1387047600, 1387047600, NA,
          1357038000, 1357038000, NA, 1372683600, 1387497600, 1387497600,
          NA, 1357088400, 1357088400, NA, 1362186000, 1362186000, 1378861200,
          1378861200, NA, 1362315600, 1362315600, NA, 1357045200, 1357045200,
          NA, 1357038000, 1357038000, NA, 1357045200, 1357045200, NA, 1357081200,
          1357081200, NA, 1357124400, 1357124400, 1362243600, 1362243600,
          1377360000, 1377360000, NA, 1362319200, 1362319200, NA, 1357056000,
          1357056000, NA, 1357038000, 1357038000, NA, 1357038000, 1357038000,
          NA, 1370908800, 1370908800, NA, 1357038000, 1357038000, NA, 1357398000,
          1362236400, 1362236400, 1387638000, 1387638000, NA, 1357038000,
          1357038000, NA, 1357135200, 1357135200, 1372784400, 1385661600,
          1387501200, NA, 1357419600, 1359838800, 1359838800, 1362337200,
          1362337200, NA, 1373050800, 1378767600, 1378767600, NA, 1370170800,
          1370170800, NA, 1357052400, 1357052400, NA, 1357038000, 1357038000,
          NA, 1378558800, 1378558800, NA, 1369602000, 1369602000, 1378648800,
          1378648800, 1387458000, 1387458000, NA, 1359997200, 1359997200,
          1378224000, 1378224000, NA, 1385341200, 1357074000, 1357074000,
          1377979200, 1377979200, 1387458000, 1387458000, NA, 1357038000,
          1357038000, NA, 1357048800, 1357048800, NA, 1357059600, 1357059600,
          NA, 1361120400, 1367780400, 1367780400, 1370199600, 1370199600,
          1387468800, 1387468800, NA, 1357038000, 1357038000, NA, 1357041600,
          1357041600, NA, 1359759600, 1359759600, 1378242000, 1378242000,
          NA, 1357038000, 1357038000, NA, 1357059600, 1357059600, NA, 1372788000,
          1372788000, NA, 1367607600, 1372806000, 1378242000, 1378242000,
          NA, 1357038000, 1357038000, NA, 1362261600, 1362261600, 1368381600,
          1371394800, 1372604400, 1372604400, NA, 1357041600, 1357041600,
          NA, 1357048800, 1357048800, NA, 1357074000, 1357074000, NA, 1357182000,
          1357182000, 1382277600, 1387742400, 1387742400, NA, 1357045200,
          1357045200, NA, 1357243200, 1357243200, 1365555600, 1365555600,
          NA, 1361660400, 1378076400, 1378076400, NA, 1357041600, 1357041600,
          NA, 1362333600, 1362333600, 1364749200, 1364749200, 1367082000,
          1.377e+09, 1.377e+09, 1378645200, 1378645200, 1386424800, 1386424800,
          1387893600, 1387893600, NA, 1377903600, 1379718000, 1379718000,
          1382130000, 1383346800, 1385164800, 1362319200, 1362319200, NA,
          1357048800, 1357048800, NA, 1357045200, 1357045200, NA, 1362265200,
          1362265200, 1365285600, 1365285600, 1372852800, 1372852800, NA,
          1357041600, 1357041600, NA, 1371247200, 1371247200, NA, 1357174800,
          1357174800, NA, 1357041600, 1357041600, NA
        ),
        tzone = "UTC",
        class = c("POSIXct", "POSIXt")
      ), n = c(
        166L, 1L, 132L, 1L, 86L, 1L, 52L, 7L, 1L, 4281L, 1L, 740L, 820L, 1L, 147L, 1L, 1L, 221L, 1L, 41L, 266L, 1L, 90L, 1L, 26L, 1L, 58L, 1968L, 1L, 367L, 4513L, 1L, 813L, 246L, 1L, 50L, 616L, 1L, 165L, 1L, 148L, 825L, 1L, 147L, 359L, 1L, 73L,
        1L, 21L, 1L, 89L, 12L, 1L, 15L, 1L, 3L, 1L, 3L, 32L, 1L, 50L,
        1L, 3L, 1L, 16L, 1185L, 1L, 238L, 1462L, 1L, 291L, 4323L, 1L,
        702L, 605L, 1L, 118L, 2289L, 1L, 383L, 945L, 1L, 188L, 1480L,
        1L, 238L, 2420L, 1L, 438L, 2676L, 1L, 471L, 350L, 1L, 60L, 2709L,
        1L, 468L, 83L, 1L, 10L, 1L, 15L, 3217L, 1L, 575L, 609L, 1L, 109L,
        931L, 1L, 188L, 635L, 1L, 109L, 10L, 1L, 1L, 1L, 1L, 1L, 317L,
        1L, 47L, 817L, 1L, 158L, 1062L, 1L, 173L, 3388L, 1L, 584L, 1096L,
        1L, 204L, 1L, 6L, 1L, 11L, 1L, 3L, 1053L, 1L, 188L, 1717L, 1L,
        292L, 4163L, 1L, 748L, 1L, 1174L, 1L, 181L, 4200L, 1L, 740L,
        1763L, 1L, 279L, 797L, 1L, 128L, 745L, 1L, 121L, 2226L, 1L, 406L,
        937L, 1L, 156L, 306L, 1L, 47L, 2049L, 1L, 327L, 1001L, 1L, 153L,
        2L, 1L, 2L, 1L, 2L, 1L, 1L, 5L, 41L, 1L, 8L, 1L, 5L, 277L, 1L,
        68L, 640L, 1L, 113L, 5189L, 1L, 910L, 495L, 1L, 88L, 2006L, 1L,
        344L, 492L, 1L, 78L, 41L, 1L, 5L, 1L, 1L, 2313L, 1L, 409L, 462L,
        1L, 96L, 114L, 1L, 118L, 1L, 95L, 1L, 46L, 658L, 1L, 111L, 1278L,
        1L, 203L, 1425L, 1L, 241L, 16L, 1L, 286L, 1L, 127L, 1L, 81L,
        1204L, 1L, 232L, 990L, 1L, 143L, 280L, 1L, 49L, 616L, 1L, 119L,
        1L, 1L, 2L, 755L, 1L, 138L, 1566L, 1L, 264L, 4362L, 1L, 764L,
        911L, 1L, 155L, 301L, 1L, 52L, 713L, 1L, 111L, 2157L, 1L, 358L,
        106L, 1L, 10L, 1L, 1L, 1L, 1L, 1L, 24L, 1L, 42L, 99L, 1L, 4L,
        1L, 1L, 27L, 1L, 23L, 1979L, 1L, 354L, 271L, 1L, 43L, 22L, 1L,
        1L, 275L, 1L, 47L, 229L, 1L, 61L, 220L, 1L, 33L, 221L, 1L, 43L,
        1653L, 1L, 276L, 1257L, 1L, 213L, 1L, 626L, 1L, 103L, 5025L,
        1L, 872L, 519L, 1L, 79L, 1131L, 1L, 232L, 3027L, 1L, 554L, 311L,
        1L, 59L, 1058L, 1L, 162L, 1L, 1L, 1L, 1L, 1L, 1L, 819L, 1L, 134L,
        586L, 1L, 120L, 2460L, 1L, 409L, 632L, 1L, 109L, 819L, 1L, 177L,
        2765L, 1L, 504L, 588L, 1L, 114L, 635L, 1L, 96L, 986L, 1L, 179L,
        75L, 1L, 10L, 1L, 16L, 3627L, 1L, 626L, 288L, 1L, 53L, 603L,
        1L, 110L, 2287L, 1L, 373L, 242L, 1L, 31L, 582L, 1L, 106L, 1L,
        1L, 1103L, 1L, 195L, 3418L, 1L, 568L, 9573L, 1L, 1688L, 551L,
        1L, 116L, 1L, 238L, 1L, 36L, 4612L, 1L, 851L, 1L, 2799L, 1L,
        514L, 161L, 1L, 21L, 950L, 1L, 144L, 1406L, 1L, 226L, 192L, 1L,
        28L, 258L, 1L, 53L, 2002L, 1L, 323L, 630L, 1L, 115L, 1469L, 1L,
        269L, 675L, 1L, 107L, 829L, 1L, 146L, 1668L, 1L, 264L, 1063L,
        1L, 185L, 320L, 1L, 44L, 1L, 6L, 1L, 4L, 1L, 1L, 1L, 4L, 1118L,
        1L, 185L, 2647L, 1L, 452L, 128L, 1L, 29L, 1L, 59L, 1L, 30L, 1446L,
        1L, 232L, 1138L, 1L, 200L, 1353L, 1L, 249L, 299L, 1L, 56L, 3L,
        1L, 29L, 1L, 2L, 1L, 9L, 1781L, 1L, 310L, 7039L, 1L, 1164L, 274L,
        1L, 54L, 4051L, 1L, 700L, 1794L, 1L, 318L, 249L, 1L, 34L, 401L,
        1L, 72L, 1L, 287L, 1L, 45L, 1128L, 1L, 182L, 2514L, 1L, 472L,
        8709L, 1L, 1553L, 7L, 1L, 2L, 1L, 306L, 1L, 67L, 243L, 1L, 6L,
        1L, 45L, 2784L, 1L, 482L, 3679L, 1L, 603L, 224L, 1L, 22L, 1L,
        46L, 9L, 1L, 6L, 1L, 79L, 1L, 9L, 1L, 19L, 10L, 1L, 4L, 1L, 8L,
        1L, 2L, 735L, 1L, 128L, 22L, 1L, 16L, 1L, 12L, 439L, 1L, 60L,
        1823L, 1L, 288L, 5324L, 1L, 843L, 1730L, 1L, 327L, 124L, 1L,
        13L, 1L, 1L, 133L, 1L, 92L, 1L, 42L, 326L, 1L, 64L, 4010L, 1L,
        705L, 3170L, 1L, 533L, 4121L, 1L, 736L, 136L, 1L, 21L, 4323L,
        1L, 716L, 1L, 5L, 1L, 8L, 1L, 1L, 3405L, 1L, 602L, 39L, 1L, 1L,
        1L, 1L, 3L, 1L, 1L, 1L, 404L, 1L, 78L, 1L, 85L, 1L, 17L, 359L,
        1L, 65L, 1527L, 1L, 275L, 2476L, 1L, 474L, 95L, 1L, 14L, 1L,
        1L, 52L, 1L, 14L, 1L, 17L, 44L, 1L, 108L, 1L, 25L, 1L, 226L,
        1L, 70L, 1L, 7L, 1L, 70L, 3128L, 1L, 548L, 1743L, 1L, 326L, 747L,
        1L, 114L, 1L, 4L, 1L, 112L, 1L, 7L, 1L, 15L, 4876L, 1L, 904L,
        1302L, 1L, 222L, 19L, 1L, 169L, 1L, 28L, 3165L, 1L, 547L, 849L,
        1L, 161L, 1L, 1L, 1L, 1L, 1L, 80L, 1L, 12L, 7540L, 1L, 1316L,
        25L, 1L, 1L, 1L, 148L, 1L, 29L, 2096L, 1L, 367L, 519L, 1L, 87L,
        923L, 1L, 143L, 226L, 1L, 1L, 9L, 1L, 40L, 3037L, 1L, 543L, 40L,
        1L, 410L, 1L, 86L, 1L, 180L, 1L, 42L, 623L, 1L, 137L, 2L, 1L,
        2L, 1L, 1L, 6L, 1L, 38L, 1L, 1L, 1L, 1L, 1L, 11L, 1L, 1L, 1L,
        1L, 1L, 1L, 186L, 1L, 30L, 631L, 1L, 105L, 1545L, 1L, 276L, 2L,
        1L, 30L, 1L, 218L, 1L, 40L, 1801L, 1L, 343L, 57L, 1L, 19L, 259L,
        1L, 48L, 635L, 1L, 109L
      )
    ),
    row.names = c(NA, 819L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  expect_identical(
    base1,
    flights %>%
      time_episodes(
        .by = id1, time = time_hour,
        time_by = NULL,
        window = 7
      ) %>%
      suppressMessages() %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(ep_id, ep_id_new)
  )
  expect_identical(
    base1,
    flights %>%
      dplyr::mutate(event = cheapr::cheapr_if_else(is.na(time_hour), NA, 1)) %>%
      time_episodes(
        .by = id1, time = time_hour,
        time_by = NULL,
        window = 7,
        event = list(event = 1)
      ) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(ep_id, ep_id_new)
  )
  expect_identical(
    flights %>%
      time_episodes(
        .by = id2, time = time_hour,
        time_by = list("hours" = 18.5),
        window = 2
      ) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(ep_id, ep_id_new),
    flights %>%
      time_episodes(
        .by = id2, time = time_hour,
        time_by = "4 hours",
        window = 9.25
      ) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(ep_id, ep_id_new)
  )
  expect_identical(
    base1,
    flights %>%
      time_episodes(
        .by = id1, time = time_hour,
        time_by = "hour",
        window = 7
      ) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(ep_id, ep_id_new)
  )
  expect_identical(
    base2,
    flights %>%
      time_episodes(
        .by = id2, time = time_hour,
        time_by = "hour",
        window = 240
      ) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(ep_id, ep_id_new)
  )
  expect_identical(
    base3,
    flights %>%
      time_episodes(
        .by = id1, time = time_hour,
        time_by = "hour",
        window = 7
      ) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(
        id1, ep_id,
        ep_id_new, ep_start
      )
  )
  expect_identical(
    base4,
    flights %>%
      time_episodes(
        .by = id2, time = time_hour,
        time_by = "hour",
        window = 240
      ) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(
        id2, ep_id,
        ep_id_new, ep_start
      )
  )
  expect_identical(
    base1,
    flights %>%
      time_episodes(
        .by = id1, time = time_hour,
        time_by = "hour",
        window = 7,
        .add = TRUE
      ) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(ep_id, ep_id_new)
  )
  expect_identical(
    base2,
    flights %>%
      time_episodes(
        .by = id2, time = time_hour,
        time_by = "hour",
        window = 240,
        .add = TRUE
      ) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(ep_id, ep_id_new)
  )
  expect_identical(
    base3,
    flights %>%
      time_episodes(
        .by = id1, time = time_hour,
        time_by = "hour",
        window = 7,
        .add = TRUE
      ) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(
        id1, ep_id,
        ep_id_new, ep_start
      )
  )
  expect_identical(
    base4,
    flights %>%
      time_episodes(
        .by = id2, time = time_hour,
        time_by = "hour",
        window = 240,
        .add = TRUE
      ) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(
        id2, ep_id,
        ep_id_new, ep_start
      )
  )
  # Grouped
  expect_identical(
    base1,
    flights %>%
      time_episodes(
        .by = id1, time = time_hour,
        time_by = "hour",
        window = 7,
        .add = TRUE
      ) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(ep_id, ep_id_new)
  )
  expect_identical(
    base2,
    flights %>%
      time_episodes(
        .by = id2, time = time_hour,
        time_by = "hour",
        window = 240,
        .add = TRUE
      ) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(ep_id, ep_id_new)
  )
  expect_identical(
    base3,
    flights %>%
      time_episodes(
        .by = id1, time = time_hour,
        time_by = "hour",
        window = 7,
        .add = TRUE
      ) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(
        id1, ep_id,
        ep_id_new, ep_start
      )
  )
  expect_identical(
    base4,
    flights %>%
      time_episodes(
        .by = id2, time = time_hour,
        time_by = "hour",
        window = 240,
        .add = TRUE
      ) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(
        id2, ep_id,
        ep_id_new, ep_start
      )
  )

  # Check that the order hasn't changed
  expect_identical(
    flights$id,
    flights %>%
      time_episodes(
        .by = id1, time = time_hour,
        time_by = "hour",
        window = 7,
        .add = TRUE
      ) %>%
      dplyr::pull(id)
  )
  expect_identical(
    flights$id,
    flights %>%
      time_episodes(
        .by = id2, time = time_hour,
        time_by = "hour",
        window = 240,
        .add = TRUE
      ) %>%
      dplyr::pull(id)
  )
  expect_identical(
    flights$id,
    flights %>%
      time_episodes(
        .by = id1, time = time_hour,
        time_by = "hour",
        window = 7,
        .add = TRUE
      ) %>%
      dplyr::pull(id)
  )
  expect_identical(
    flights$id,
    flights %>%
      time_episodes(
        .by = id2, time = time_hour,
        time_by = "hour",
        window = 240,
        .add = TRUE
      ) %>%
      dplyr::pull(id)
  )
  expect_identical(
    flights %>%
      fastplyr::f_slice(0) %>%
      time_episodes(
        .by = id2, time = time_hour,
        time_by = "hour",
        window = 240
      ) %>%
      dplyr::as_tibble(),
    structure(
      list(
        id2 = integer(0),
        time_hour = structure(numeric(0), tzone = "UTC", class = c("POSIXct", "POSIXt")),
        t_elapsed = numeric(0),
        ep_start = structure(numeric(0), tzone = "UTC", class = c("POSIXct", "POSIXt")),
        ep_id = integer(0),
        ep_id_new = integer(0)
      ),
      row.names = integer(0), class = c("tbl_df", "tbl", "data.frame")
    )
  )
  set.seed(93)
  x <- sample(1:500, 20, T)
  g <- sample(1:2, 20, T)
  x[sample.int(20, 7)] <- NA
  names(x) <- g
  df <- fastplyr::f_enframe(x)

  df <- df %>%
    fastplyr::f_arrange(name, value) %>%
    dplyr::mutate(
      telapsed1 = time_elapsed(value, 1, rolling = TRUE, g = name),
      telapsed2 = time_elapsed(value, 1, rolling = FALSE, g = name)
    ) %>%
    fastplyr::f_group_by(name)

  # Rolling
  # When window = 100
  # res is
  df$res1 <- c(
    1, 2, 0, 0, 3, 0, NA, NA, NA, NA, NA,
    1, 2, 0, 0, 3, 0, 0, NA, NA
  )
  expect_equal(
    df %>%
      time_episodes(value, roll_episode = TRUE, window = 100, .add = TRUE) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(res1 == ep_id_new),
    fastplyr::as_tbl(cheapr::fast_df(
      "res1 == ep_id_new" = c(TRUE, NA),
      "n" = c(13L, 7L)
    ))
  )

  # Fixed episode
  # When window = 200
  # res is
  df$res2 <- c(
    1, 0, 2, 0, 0, 3, NA, NA, NA, NA, NA,
    1, 0, 2, 0, 0, 0, 3, NA, NA
  )

  expect_equal(
    df %>%
      time_episodes(value,
        roll_episode = FALSE, window = 200,
        .add = TRUE
      ) %>%
      dplyr::as_tibble() %>%
      fastplyr::f_count(res2 == ep_id_new),
    fastplyr::as_tbl(cheapr::fast_df(
      "res2 == ep_id_new" = c(TRUE, NA),
      "n" = c(13L, 7L)
    ))
  )
})

test_that("Simple episodic tests", {
  set.seed(4200)
  df <- dplyr::tibble(
    time = time_cast(sample.int(30, 15, TRUE), Sys.Date()),
    event = sample(c("e", "ne"), 15, TRUE, prob = c(0.6, 0.4))
  )

  expect_snapshot({
    df %>%
      time_episodes(time,
        time_by = 1, window = 3, .add = FALSE,
        # t >= threshold
        switch_on_boundary = TRUE
      ) %>%
      fastplyr::f_arrange(time)
  })

  expect_snapshot({
    df %>%
      time_episodes(time,
        time_by = 1, window = 3, .add = TRUE,
        # t > threshold
        switch_on_boundary = FALSE
      ) %>%
      fastplyr::f_arrange(time)
  })

  expect_snapshot({
    df %>%
      time_episodes(time,
        time_by = 1, window = 3, .add = TRUE,
        switch_on_boundary = TRUE,
        event = list(event = "e")
      ) %>%
      fastplyr::f_arrange(time)
  })

  expect_snapshot({
    df %>%
      time_episodes(time,
        time_by = 3, window = 1, .add = FALSE,
        switch_on_boundary = FALSE,
        event = list(event = "e")
      ) %>%
      fastplyr::f_arrange(time)
  })
  # Cumulative time ---------------------------------------------------------
  expect_snapshot({
    df %>%
      time_episodes(time,
        time_by = "days", window = 5, .add = FALSE,
        roll_episode = FALSE,
        switch_on_boundary = TRUE
      ) %>%
      fastplyr::f_arrange(time)
  })

  expect_snapshot({
    df %>%
      time_episodes(time,
        time_by = "5 days", window = 1, .add = FALSE,
        roll_episode = FALSE,
        switch_on_boundary = FALSE
      ) %>%
      fastplyr::f_arrange(time)
  })
})
