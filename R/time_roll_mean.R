# #include <Rcpp.h>
# using namespace Rcpp;
#
# IntegerVector time_window_size(int n, IntegerVector time){
#   int out_length = time.length();
#   IntegerVector out(out_length);
#   for (int i = 0; i < out_length; i++) {
#     for (int j = i; j >= 0; j--) {
#       if ( ( time[i] - time(j) ) > (n - 1) ) {
#         out[i] = i - j;
#         break;
#       } else if (j == 0){
#         out[i] = NA_INTEGER;
#       }
#     }
#   }
#   return(out);
# }
# test42 <- function(){
#   set.seed(82314)
#
#   x <- today() +
#     days(c(1:5, 6, 7, 9, 10, 13, 14, 15,
#            rep(15, 5), 16:30, 34:45))
#   window_size <- 6
#
#   val <- sample.int(5, length(x), TRUE)
#
#
#   x
#
#   # true_window <- time_window_size(6, x)
#   true_window <- lengths(runner::window_run(x, k = 6, idx = x))
#
#   naive_window <- window_sequence(length(x), 6)
#   num_gaps
#   total_gaps <- runner::sum_run(num_gaps, k = 6)
#   time_elapsed(x, "days", rolling = FALSE)
#   time_lost <- integer(length(x))
#   current_window_size <- naive_window
#
#   num_gaps <- time_elapsed(x, "days", fill = 1) - 1
#   total_gaps <- runner::sum_run(num_gaps, k = 6)
#   out <- naive_window
#   dup <- as.integer(collapse::fduplicated(x))
#   gap_counter <- 0L
#   df <- tibble(x, num_gaps, total_gaps,
#                naive_window, true_window)
#   df$my_window <- df$naive_window - df$total_gaps
#   df$id <- time_seq_id(df$x, "days")
#   df$rowid <- frowid(df$id)
#   roll_window2 <- window_sequence(fn(g = df$id),
#                                   k = alloc(6, n_unique(df$id)))
#   last_time_there_was_no_gap <- roll_lag(df$time, roll_window2)
#   last_time_there_was_no_gap[roll_window2 == n] <- NA
#   last_time_there_was_gap <- if_else(df$rowid == 1 & df$total_gaps > 0,
#                                      x, NA_Date_)
#   last_time_there_was_gap <- roll_na_fill(last_time_there_was_gap)
#   # df <- df %>%
#   #   mutate(mywindow2 = if_else(naive_window == 6L & rowid > 1L & total_gaps > 0L,
#   #                              pmin(my_window - (rowid - 1L), 6L),
#   #                              my_window))
#   for (i in seq_along(x)){
#     # print(gap_counter)
#     # Minus the number of gaps
#     if (total_gaps[i] > 0){
#       out[i] <- out[i] - total_gaps[i]
#       gap_counter <- gap_counter + num_gaps[i]
#     }
#     print(out[i] - true_window[i])
#     if (gap_counter > 0){
#       gap_counter <- gap_counter - 1L
#     }
#   }
#   true_window - out
# }
# time_roll_mean <- function(x, n = length(x),
#                            time = NULL, time_by = NULL,
#                            g = NULL, partial = TRUE,
#                            na.rm = TRUE,
#                            time_type = c("auto", "duration", "period"),
#                            ...){
#   if (is.null(time)){
#     return(roll_mean(x, n = n,
#                      g = g, partial = partial,
#                      na.rm = na.rm, ...))
#   }
#   time_by <- time_by_get(time, time_by = time_by)
#   window_size <- n
#   if (length(window_size) != 1L){
#     stop("time window size must be of length 1")
#   }
#   if (time_by_length(time_by) != 1L){
#     stop("time_by must consist of a single number")
#   }
#   has_groups <- !is.null(g)
#   check_is_time_or_num(time)
#   g <- GRP2(g)
#   check_data_GRP_size(x, g)
#   group_sizes <- GRP_group_sizes(g)
#   n_groups <- GRP_n_groups(g)
#   groups_are_sorted <- !has_groups || GRP_is_sorted(g)
#   group_id <- group_id(g)
#   if (is.null(group_id)){
#     group_id <- integer(length(x))
#   }
#   if (!groups_are_sorted){
#     group_order <- GRP_order(g)
#     x <- x[group_order]
#     time <- time[group_order]
#     group_id <- group_id[group_order]
#   }
#   # roll_window <- window_sequence(group_sizes,
#   #                                k = rep.int(window_size, n_groups),
#   #                                partial = partial)
#   elapsed_time <- time_elapsed(time, time_by = time_by,
#                                time_type = time_type,
#                                g = group_id, rolling = FALSE)
#   dt <- data.table::data.table(x = x,
#                                time = time,
#                                group = group_id)
#   dt[, ("window_width") :=
#        runner::length_run(
#          idx = get("time"), k = window_size
#        ),
#      by = "group"]
#   roll_window <- dt[["window_width"]]
#   roll_window[is.na(roll_window)] <- sequence(fnmiss(dt[["window_width"]],
#                                                      g = dt[["group"]],
#                                                      use.g.names = FALSE))
#   # roll_window <- runner::runner(x, k = n, idx = elapsed_time, f = length)
#   out <- frollmean3(x, n = roll_window,
#                     # weights = weights,
#                     adaptive = TRUE, align = "right",
#                     na.rm = na.rm, ...)
#   if (!groups_are_sorted){
#     out <- collapse::greorder(out, g = g)
#   }
#   out
# }
# # This isn't great due to need to expand
# time_roll_mean2 <- function(x, n = length(x),
#                            time = NULL, time_by = NULL,
#                            g = NULL, partial = TRUE,
#                            na.rm = TRUE,
#                            time_type = c("auto", "duration", "period"),
#                            ...){
#   if (is.null(time)){
#     return(roll_mean(x, n = n,
#                      g = g, partial = partial,
#                      na.rm = na.rm, ...))
#   }
#   time_by <- time_by_get(time, time_by = time_by)
#   window_size <- n
#   if (length(window_size) != 1L){
#     stop("time window size must be of length 1")
#   }
#   if (time_by_length(time_by) != 1L){
#     stop("time_by must consist of a single number")
#   }
#   has_groups <- !is.null(g)
#   check_is_time_or_num(time)
#   # g <- GRP2(g)
#   group_id <- group_id(g)
#
#   if (is.null(group_id)){
#     group_id <- collapse::alloc(1L, length(x))
#     # groups_are_sorted <- TRUE
#   } else {
#     # groups_are_sorted <- GRP_is_sorted(g)
#   }
#   time_tbl <- as_DT(list(.x = unname(x),
#                          .time = time,
#                          .group = group_id,
#                          # .gap_id = integer(length(x)),
#                          .row_id = seq_along(x)))
#   time_full <- time_expandv(time, time_by = time_by,
#                             g = group_id, use.g.names = TRUE,
#                             time_type = time_type)
#   time_full_tbl <- data.table::data.table(.time = unname(time_full),
#                                           .group = as.integer(names(time_full)))
#   out_tbl <- merge(time_tbl,
#                    time_full_tbl,
#                    by = setdiff(names(time_tbl), c(".x", ".row_id")),
#                    all = TRUE,
#                    sort = FALSE,
#                    allow.cartesian = TRUE)
#   # data.table::setnafill(out_tbl, type = "const", fill = 1L, cols = ".gap_id")
#   setorderv2(out_tbl, cols = c(".group", ".time"))
#   out_tbl[, ("window") := window_sequence(
#     collapse::GRPN(out_tbl[[".group"]], expand = FALSE), k = window_size
#     )]
#   is_gap <- is.na(out_tbl[[".row_id"]])
#   out_tbl[, ("num_gaps") := data.table::frollsum(is_gap,
#                                                  n = get("window"),
#                                                  adaptive = TRUE,
#                                                  na.rm = FALSE,
#                                                  algo = "fast")]
#   # out_tbl[, ("num_gaps") := data.table::frollsum(get(".gap_id"),
#   #                                                n = get("window"),
#   #                                                adaptive = TRUE,
#   #                                                na.rm = FALSE,
#   #                                                algo = "fast")]
#   # out_tbl[get("window") >= 11L,
#   #         ("num_dups") :=
#   #           data.table::frollsum(
#   #             gduplicated(
#   #               get(".time"), g = get(".group")
#   #             ),
#   #             n = get("window"),
#   #             adaptive = TRUE,
#   #             na.rm = FALSE,
#   #             algo = "fast")]
#   # out_tbl[, ("num_dups") :=
#   #           data.table::frollsum(
#   #             gduplicated(
#   #               get(".time"), g = get(".group")
#   #             ),
#   #             n = flag2(get("window"), g = get(".group"), n = window_size),
#   #             adaptive = TRUE,
#   #             na.rm = FALSE,
#   #             algo = "fast")]
#   # out_tbl[, ("num_dups") :=
#   #           collapse::fcumsum(
#   #             data.table::fifelse(get("window") < window_size,
#   #                                 FALSE,
#   #                                 gduplicated(
#   #                                   get(".time"), g = get(".group")
#   #                                 ))
#   #             )]
#   out_tbl[, ("num_dups") := 0L]
#   out_tbl[get("window") == window_size, ("num_dups") :=
#             collapse::fcumsum(
#               gduplicated(out_tbl[[".time"]][out_tbl[["window"]] == window_size],
#                 g = out_tbl[[".group"]][out_tbl[["window"]] == window_size])
#               )]
#   out_tbl[, (".mean") := data.table::frollsum(get(".x"),
#                                               n = get("window"),
#                                               adaptive = TRUE,
#                                               na.rm = na.rm,
#                                               ...) /
#             (get("window") - get("num_gaps") + get("num_dups"))]
#   out_tbl$final <- out_tbl$window + out_tbl$num_dups - out_tbl$num_gaps
#   out_tbl$true <- lengths(runner::window_run(out_tbl$.time, idx = out_tbl$.time, k = 11))
#   out_tbl <- df_row_slice(out_tbl, !is_gap)
#   # out_tbl <- df_row_slice(out_tbl, out_tbl[[".gap_id"]] == 0L)
#   out <- out_tbl[[".mean"]]
#   if (is.unsorted(out_tbl[[".row_id"]])){
#     out <- out[order(out_tbl[[".row_id"]])]
#   }
#   out
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
#   df[, (".row.id") := seq_len(.N)]
#   df <- farrange(df, .cols = c(".group.id", "time"))
#   sizes <- collapse::GRPN(df[[".group.id"]], expand = FALSE)
#   roll_window1 <- window_sequence(sizes, k = window_size, partial = partial)
#   # time_id <- time_seq_id(df$time, time_by = time_by, g = df[[".group.id"]])
#   time_id <- time_seq_id(df$time, time_by = time_by,
#                          g = df[[".group.id"]],
#                          threshold = n,
#                          switch_on_boundary = TRUE,
#                          rolling = FALSE)
#   g2 <- GRP2(list(df$.group.id, time_id))
#   time_elapsed <- time_elapsed(df[["time"]], g = df[[".group.id"]],
#                                time_by = time_by, fill = 1)
#   df$elapsed <- time_elapsed
#   roll_window2 <- window_sequence(GRP_group_sizes(g2),
#                                   k = alloc(n, GRP_n_groups(g2)),
#                                   ascending = TRUE)
#   df$window1 <- roll_window1
#   df$window2 <- roll_window2
#   last_time_there_was_no_gap <- roll_lag(df$time, roll_window2)
#   last_time_there_was_no_gap[roll_window2 == n] <- NA
#   df$last_time <- last_time_there_was_no_gap
#   diff2 <- time_diff(last_time_there_was_no_gap, df$time,
#                      time_by = time_by,
#                      time_type = time_type)
#   df$diff <- diff2
#   roll_window <- roll_window2
#   adj_window <- n - diff2[which(diff2 < n)] +
#     roll_window2[which(diff2 < n)] - 1L
#   roll_window[which(diff2 < n)] <- adj_window + 1L
#
#   df$final_window <- roll_window
#   df$correct_window <- lengths(runner::window_run(x, k = n, idx = time))
#   df$time_lost <- data.table::frollsum(df$elapsed - 1L,
#                                        n = df$window1,
#                                        adaptive = TRUE,
#                                        na.rm = FALSE,
#                                        algo = "fast")
#   time_lost <- df$elapsed - 1L
#   total_time_lost <- data.table::frollsum(time_lost,
#                                     n = df$window1,
#                                     adaptive = TRUE,
#                                     na.rm = FALSE,
#                                     algo = "fast")
#   time_lost_window <- df$window1 - (data.table::frollsum(time_lost,
#                                                          n = df$window1,
#                                                          adaptive = TRUE,
#                                                          na.rm = FALSE,
#                                                          algo = "fast"))
#   total_time_lost2 <- data.table::frollsum(time_lost,
#                                            n = time_lost_window,
#                                            adaptive = TRUE,
#                                            na.rm = FALSE,
#                                            algo = "fast")
#
#   # roll_window <- roll_window1 - time_lost
#   # roll_window <- roll_window1 - (roll_window2 - time_lost)
#   # df$final_window2 <- roll_window
#   # roll_window <- roll_window1 - total_time_lost2
#   diff3 <- pmin(diff2, n)
#   roll_window[!is.na(last_time_there_was_no_gap)] <-
#     roll_window1[!is.na(last_time_there_was_no_gap)] -
#     (diff3[!is.na(last_time_there_was_no_gap)]) +
#     roll_window2[!is.na(last_time_there_was_no_gap)]
#   df$final_window <- roll_window
#   df$time_lost <- time_lost
#   df$total_lost <- total_time_lost
#   df$total_lost2 <- total_time_lost2
#   df$lost_window <- time_lost_window
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
#   df$window2 <- roll_window2
#   last_time_there_was_no_gap <- roll_lag(df$time, roll_window2)
#   last_time_there_was_no_gap[roll_window2 == n] <- NA
#   df$last_time <- last_time_there_was_no_gap
#   diff2 <- time_diff(last_time_there_was_no_gap, df$time,
#                      time_by = time_by,
#                      time_type = time_type)
#   df$diff <- diff2
#   roll_window <- roll_window2
#   adj_window <- n - diff2[which(diff2 < n)] +
#     roll_window2[which(diff2 < n)] - 1L
#   roll_window[which(diff2 < n)] <- adj_window + 1L
#
#   df$final_window <- roll_window
#   df$correct_window <- lengths(runner::window_run(x, k = n, idx = time))
#   df$time_lost <- data.table::frollsum(df$elapsed - 1L,
#                                        n = df$window1,
#                                        adaptive = TRUE,
#                                        na.rm = FALSE,
#                                        algo = "fast")
#   df <- as_tibble(df)
#   df <- df %>%
#     select(-.row.id) %>%
#     mutate(ok = if_else(row_number() %in% which(diff2 < n), TRUE, FALSE))
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
