source("social_sync.R")

fixture <- list(data = list(
  list(
    name = "page_post_engagements",
    period = "day",
    values = list(
      list(value = 12, end_time = "2026-10-19T07:00:00+0000"),
      list(value = 8, end_time = "2026-10-20T07:00:00+0000")
    )
  ),
  list(
    name = "page_media_view",
    period = "day",
    values = list(
      list(value = 100, end_time = "2026-10-19T07:00:00+0000"),
      list(value = 80, end_time = "2026-10-20T07:00:00+0000")
    )
  )
))

parsed <- meta_parse_insights(fixture, "Europe/Amsterdam")
stopifnot(nrow(parsed) == 2)
stopifnot(identical(parsed$media_views, c(100, 80)))
stopifnot(identical(parsed$post_engagements, c(12, 8)))
stopifnot(all(is.na(parsed$followers_count)))
stopifnot(identical(as.character(parsed$metric_date), c("2026-10-18", "2026-10-19")))

post <- list(attachments = list(data = list(list(media_type = "photo"))))
stopifnot(identical(meta_post_type(post), "photo"))
stopifnot(identical(meta_summary_count(list(summary = list(total_count = 5))), 5))

follower_fixture <- list(data = list(list(
  name = "page_follows", period = "day",
  values = list(list(value = 1234, end_time = "2026-10-20T07:00:00+0000"))
)))
follower_parsed <- meta_parse_insights(follower_fixture, "Europe/Amsterdam")
stopifnot(identical(follower_parsed$followers_count, 1234))
stopifnot(grepl("TOKEN VERWIJDERD", social_error_message(simpleError("token EAAabcdefghijklmnop"))))

empty_post_insights <- list(data = list(list(name = "post_media_view", values = list())))
original_request <- meta_graph_request
meta_graph_request <- function(...) empty_post_insights
empty_result <- meta_fetch_post_insights("123_456", list())
meta_graph_request <- original_request
stopifnot(is.na(empty_result$media_views), is.na(empty_result$reactions_total))

cat("Social Media-transformatietests geslaagd.\n")
