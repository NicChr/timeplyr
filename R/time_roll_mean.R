# In progress.
# time_roll_mean <- function(x, time, n = 1, time_by = NULL,
#                            g = NULL, partial = TRUE,
#                            time_type = c("auto", "duration", "period")){
#   time_by <- time_by_get(x, time_by = time_by)
#   window_size <- n
#   if (length(window_size) != 1L){
#     stop("time window size must be of length 1")
#   }
#   if (time_by_length(time_by) != 1L){
#     stop("time_by must consist of a single number")
#   }
#   has_groups <- !is.null(g)
#   if (is.null(g)){
#     group_id <- rep_len(1L, length(x))
#     group_id <- group_id_to_qg(group_id,
#                                n_groups = min(1L, length(x)),
#                                group_sizes = length(x))
#   } else {
#     group_id <- group_id(g, as_qg = TRUE)
#   }
#   check_is_time_or_num(time)
#   if (length(group_id) != length(x)){
#     stop("length of groups must match length of x")
#   }
#   df <- data.table::data.table(x, time, ".group.id" = group_id)
#   df[, (".row.id") := seq_len(.N)]
#   df <- farrange(df, .cols = c(".group.id", "time"))
#   sizes <- collapse::GRPN(df[[".group.id"]], expand = FALSE)
#   roll_window1 <- window_sequence(sizes, k = window_size, partial = partial)
#   time_id <- time_seq_id(df$time, time_by = time_by, g = df[[".group.id"]])
#   g2 <- GRP2(list(df$.group.id, time_id))
#   time_elapsed <- time_elapsed(df[["time"]], g = df[[".group.id"]],
#                                time_by = time_by, fill = 1)
#   df$elapsed <- time_elapsed
#   roll_window2 <- window_sequence(GRP_group_sizes(g2),
#                                   k = alloc(n, GRP_n_groups(g2)),
#                                   ascending = TRUE)
#   df$window1 <- roll_window1
#   df$window <- roll_window2
#   last_time_there_was_no_gap <- roll_lag(df$time, roll_window2)
#   last_time_there_was_no_gap[roll_window2 == n] <- NA
#   df$last_time <- last_time_there_was_no_gap
#   diff2 <- time_diff(last_time_there_was_no_gap, df$time,
#                      time_by = time_by,
#                      time_type = time_type)
#   df$diff <- diff2
#   roll_window <- roll_window2
#   # roll_window[which(diff2 < n)] <- n - diff2[which(diff2 < n)] + 1
#   adj_window <- n - diff2[which(diff2 < n)] +
#     roll_window2[which(diff2 < n)] - 1L
#   # adj_window <- pmin(adj_window, max(n - 1L, 1L))
#   roll_window[which(diff2 < n)] <- adj_window
#
#   df$final_window <- roll_window
#   df[, (".roll.mean") := frollmean3(df[["x"]],
#                                     n = roll_window,
#                                     adaptive = TRUE,
#                                     align = "right",
#                                     na.rm = TRUE)]
#   df[[".roll.mean"]][order(df[[".row.id"]])]
#
# }
# time_roll_mean <- function(x, time, n = 1, time_by = NULL,
#                            g = NULL, partial = TRUE,
#                            time_type = c("auto", "duration", "period")){
#   time_by <- time_by_get(x, time_by = time_by)
#   window_size <- n
#   if (length(window_size) != 1L){
#     stop("time window size must be of length 1")
#   }
#   if (time_by_length(time_by) != 1L){
#     stop("time_by must consist of a single number")
#   }
#   has_groups <- !is.null(g)
#   if (is.null(g)){
#     group_id <- rep_len(1L, length(x))
#     group_id <- group_id_to_qg(group_id,
#                                n_groups = min(1L, length(x)),
#                                group_sizes = length(x))
#   } else {
#     group_id <- group_id(g, as_qg = TRUE)
#   }
#   check_is_time_or_num(time)
#   if (length(group_id) != length(x)){
#     stop("length of groups must match length of x")
#   }
#   df <- data.table::data.table(x, time, ".group.id" = group_id)
#   # df[, (".temp.id") := integer(nrow2(df))]
#   # df_full <- time_complete(df, time = .data[["time"]],
#   #                          time_by = time_by, .by = dplyr::all_of(".group.id"),
#   #                          keep_class = FALSE,
#   #                          sort = FALSE)
#   # any_rows_completed <- anyNA(df_full[[".temp.id"]])
#   # df_full[, (".row.id") := data.table::fifelse(is.na(get(".temp.id")),
#   #                                              NA_integer_,
#   #                                              seq_len(.N))]
#   # setorderv2(df_full, c(".group.id", "time"))
#   df <- farrange(df, .cols = c(".group.id", "time"))
#   sizes <- collapse::GRPN(df[[".group.id"]], expand = FALSE)
#   roll_window1 <- window_sequence(sizes, k = window_size, partial = partial)
#   time_id <- time_seq_id(df$time, time_by = time_by, g = df[[".group.id"]])
#   g2 <- GRP2(list(df$.group.id, time_id))
#   time_elapsed <- time_elapsed(df[["time"]], g = df[[".group.id"]],
#                                time_by = time_by, fill = 1)
#   df$elapsed <- time_elapsed
#   # roll_window2 <- window_sequence(GRP_group_sizes(g2),
#   #                                 k = time_elapsed[GRP_starts(g2)] - 1,
#   #                                 ascending = FALSE)
#   roll_window2 <- window_sequence(GRP_group_sizes(g2),
#                                   k = alloc(n, GRP_n_groups(g2)),
#                                   ascending = TRUE)
#   last_time_there_was_no_gap <- roll_lag(df$time, roll_window2)
#   last_time_there_was_no_gap[roll_window2 == n] <- NA
#   # last_time_there_was_no_gap[is.na(last_time_there_was_no_gap)] <-
#   #   df$time[is.na(last_time_there_was_no_gap)]
#   diff2 <- time_diff(last_time_there_was_no_gap, df$time,
#                      time_by = time_by,
#                      time_type = time_type)
#   # roll_window3 <- roll_window2 + (n - diff2)
#   roll_window <- roll_window2
#   roll_window[which(diff2 < n)] <- n - diff2[which(diff2 < n)] + 1
#   # roll_window[!is.na(roll_window3)] <- roll_window2[!is.na(roll_window3)]
#   #   pmax(time_elapsed[GRP_starts(g2)] - n, 0L)
#   # time_missed <- (time_elapsed - 1)
#   # roll_window3 <- pmax(roll_window2 - time_missed, 1L)
#   # roll_window4 <- collapse::fcumsum(gduplicated(df[["time"]], g = df[[".group.id"]]))
#   # roll_window <- pmax(roll_window1 - roll_window2 + roll_window3, 1L)
#   # df$missed_days <- pmax(time_elapsed - 1L, 0L)
#   # df$window <- roll_window
#   # roll_window2 <- pmax(pmin(time_elapsed - 1, n), 0L)
#   # roll_window2 <- pmax(roll_window - time_elapsed + 1L, 1L)
#
#   df[, (".roll.mean") := frollmean3(df[["x"]],
#                                     n = roll_window,
#                                     adaptive = TRUE,
#                                     align = "right",
#                                     na.rm = TRUE)]
#   # setorderv2(df_full, ".row.id")
#   df[[".roll.mean"]]
#
# }

# time_roll_mean <- function(x, time, n = 1, time_by = NULL, g = NULL, partial = TRUE){
#   time_by <- time_by_get(x, time_by = time_by)
#   window_size <- n
#   if (length(window_size) != 1L){
#     stop("time window size must be of length 1")
#   }
#   if (time_by_length(time_by) != 1L){
#     stop("time_by must consist of a single number")
#   }
#   has_groups <- !is.null(g)
#   if (is.null(g)){
#     group_id <- rep_len(1L, length(x))
#     group_id <- group_id_to_qg(group_id,
#                                n_groups = min(1L, length(x)),
#                                group_sizes = length(x))
#   } else {
#     group_id <- group_id(g, as_qg = TRUE)
#   }
#   check_is_time_or_num(time)
#   if (length(group_id) != length(x)){
#     stop("length of groups must match length of x")
#   }
#   df <- data.table::data.table(x, time, ".group.id" = group_id)
#   # df[, (".temp.id") := integer(nrow2(df))]
#   # df_full <- time_complete(df, time = .data[["time"]],
#   #                          time_by = time_by, .by = dplyr::all_of(".group.id"),
#   #                          keep_class = FALSE,
#   #                          sort = FALSE)
#   # any_rows_completed <- anyNA(df_full[[".temp.id"]])
#   # df_full[, (".row.id") := data.table::fifelse(is.na(get(".temp.id")),
#   #                                              NA_integer_,
#   #                                              seq_len(.N))]
#   # setorderv2(df_full, c(".group.id", "time"))
#   df <- farrange(df, .cols = c(".group.id", "time"))
#   sizes <- collapse::GRPN(df[[".group.id"]], expand = FALSE)
#   # roll_window <- window_sequence(sizes, k = window_size, partial = partial)
#   time_elapsed <- time_elapsed(df[["time"]], g = df[[".group.id"]],
#                                time_by = time_by, fill = 1)
#   df$elapsed <- time_elapsed
#   df$missed_days <- pmax(time_elapsed - 1L, 0L)
#   time_id <- time_seq_id(df$time, time_by = time_by, g = df[[".group.id"]])
#   roll_window  <- row_id(df, .group.id, time_id)
#   df$window <- roll_window
#   roll_window2 <- pmax(pmin(time_elapsed - 1, n), 0L)
#   # roll_window2 <- pmax(roll_window - time_elapsed + 1L, 1L)
#
#   g2 <- group_id(df)
#   roll_window3 <- roll_window2 + collapse::fcumsum(gduplicated(df[["time"]],
#                                                                g = df[[".group.id"]]))
#   df_full[, (".roll.mean") := frollmean3(df_full[["x"]],
#                                          n = roll_window,
#                                          adaptive = TRUE,
#                                          align = "right",
#                                          na.rm = TRUE)]
#   if (any_rows_completed){
#     df_full <- df_full[!is.na(get(".row.id"))]
#   }
#   setorderv2(df_full, ".row.id")
#   df_full[[".roll.mean"]]
#
# }

# time_roll_mean <- function(x, time, n = 1, time_by = NULL, g = NULL, partial = TRUE){
#   time_by <- time_by_get(x, time_by = time_by)
#   window_size <- n
#   if (length(window_size) != 1L){
#     stop("time window size must be of length 1")
#   }
#   if (time_by_length(time_by) != 1L){
#     stop("time_by must consist of a single number")
#   }
#   has_groups <- !is.null(g)
#   if (is.null(g)){
#     group_id <- rep_len(1L, length(x))
#   } else {
#     group_id <- group_id(g)
#   }
#   check_is_time_or_num(time)
#   if (length(group_id) != length(x)){
#     stop("length of groups must match length of x")
#   }
#   df <- data.table::data.table(x, time, ".group.id" = group_id)
#   df[, (".temp.id") := integer(nrow2(df))]
#   df_full <- time_complete(df, time = .data[["time"]],
#                            time_by = time_by, .by = dplyr::all_of(".group.id"),
#                            keep_class = FALSE,
#                            sort = FALSE)
#   any_rows_completed <- anyNA(df_full[[".temp.id"]])
#   df_full[, (".row.id") := data.table::fifelse(is.na(get(".temp.id")),
#                                                NA_integer_,
#                                                seq_len(.N))]
#   setorderv2(df_full, c(".group.id", "time"))
#   sizes <- collapse::GRPN(df_full[[".group.id"]], expand = FALSE)
#   roll_window <- window_sequence(sizes, k = window_size, partial = partial)
#   df_full[, (".roll.mean") := frollmean3(df_full[["x"]],
#                                          n = roll_window,
#                                          adaptive = TRUE,
#                                          align = "right",
#                                          na.rm = TRUE)]
#   if (any_rows_completed){
#     df_full <- df_full[!is.na(get(".row.id"))]
#   }
#   setorderv2(df_full, ".row.id")
#   df_full[[".roll.mean"]]
#
# }
# time_roll_mean <- function(x, time, n, g = NULL, partial = TRUE){
#   has_groups <- !is.null(g)
#   if (has_groups){
#     g <- GRP2(g)
#     check_data_GRP_size(x, g)
#     sizes <- GRP_group_sizes(g)
#   } else {
#     sizes <- length(x)
#   }
#   check_is_time_or_num(time)
#   if (length(n) != 1L){
#     stop("n must be of length 1")
#   }
#   if (has_groups && !GRP_is_sorted(g)){
#     group_order <- GRP_order(g)
#     x <- x[group_order]
#     g2 <- GRP_group_id(g)[group_order]
#     sizes2 <- collapse::GRPN(g2, expand = FALSE)
#   } else {
#     g2 <- g
#     sizes2 <- sizes
#   }
#   group_windows <- window_sequence(sizes2, k = n, partial = partial)
# }
