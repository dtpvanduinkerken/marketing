source("social_sync.R")

.social_api_cache <- new.env(parent = emptyenv())
.social_api_cache$value <- NULL
.social_api_cache$loaded_at <- as.POSIXct(NA)

leeg_social_live <- function(message = "Live Facebook-data is nog niet beschikbaar.") {
  list(
    available = FALSE, message = message, content_warning = NULL,
    daily = data.frame(), posts = data.frame(), sync = data.frame()
  )
}

empty_social_posts <- function() {
  data.frame(
    platform = character(), external_post_id = character(), post_type = character(),
    message = character(), permalink_url = character(), picture_url = character(),
    published_at = as.POSIXct(character(), tz = "UTC"), captured_date = as.Date(character()),
    media_views = numeric(), likes = numeric(), reactions_total = numeric(), comments = numeric(),
    shares = numeric(), engagement_total = numeric(), collected_at = as.POSIXct(character(), tz = "UTC")
  )
}

social_posts_from_api <- function(raw_posts, cfg, captured_date = Sys.Date()) {
  if (!length(raw_posts)) return(empty_social_posts())
  rows <- lapply(raw_posts, function(post) {
    insight <- meta_fetch_post_insights(post$id, cfg)
    likes <- if (is.null(post$reactions)) NA_real_ else meta_summary_count(post$reactions)
    comments <- if (is.null(post$comments)) NA_real_ else meta_summary_count(post$comments)
    shares <- suppressWarnings(as.numeric(post$shares$count %||% NA_real_))
    reactions_total <- insight$reactions_total
    reactions_for_total <- if (is.na(reactions_total)) likes else reactions_total
    engagement <- if (all(is.na(c(reactions_for_total, comments, shares)))) NA_real_ else
      sum(c(reactions_for_total, comments, shares), na.rm = TRUE)
    data.frame(
      platform = "facebook", external_post_id = as.character(post$id),
      post_type = meta_post_type(post), message = as.character(post$message %||% NA_character_),
      permalink_url = as.character(post$permalink_url %||% NA_character_),
      picture_url = as.character(post$full_picture %||% NA_character_),
      published_at = meta_epoch(post$created_time), captured_date = as.Date(captured_date),
      media_views = insight$media_views, likes = likes, reactions_total = reactions_total,
      comments = comments, shares = shares, engagement_total = engagement,
      collected_at = Sys.time(), stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

fetch_social_live_api <- function(today = Sys.Date()) {
  cfg <- meta_config()
  token_info <- meta_token_status(cfg)
  if (!is.null(token_info) && !isTRUE(token_info$is_valid)) stop("Het Meta-toegangstoken is ongeldig of verlopen.")

  fetched_at <- Sys.time()
  page <- meta_fetch_page(cfg)
  daily <- meta_fetch_daily_insights(cfg, today - 729, today)
  if (!nrow(daily)) {
    daily <- data.frame(metric_date = as.Date(today), media_views = NA_real_,
                        post_engagements = NA_real_, source_end_time = as.POSIXct(NA))
  }
  daily$platform <- "facebook"
  daily$external_account_id <- as.character(page$id)
  daily$account_name <- as.character(page$name)
  daily$account_timezone <- cfg$timezone
  daily$followers_count <- NA_real_
  daily$fan_count <- NA_real_
  daily$collected_at <- fetched_at

  today_index <- which(daily$metric_date == as.Date(today))
  if (!length(today_index)) {
    daily <- rbind(daily, data.frame(
      metric_date = as.Date(today), media_views = NA_real_, post_engagements = NA_real_,
      source_end_time = as.POSIXct(NA), platform = "facebook",
      external_account_id = as.character(page$id), account_name = as.character(page$name),
      account_timezone = cfg$timezone, followers_count = NA_real_, fan_count = NA_real_,
      collected_at = fetched_at
    ))
    today_index <- nrow(daily)
  }
  daily$followers_count[today_index] <- suppressWarnings(as.numeric(page$followers_count %||% NA_real_))
  daily$fan_count[today_index] <- suppressWarnings(as.numeric(page$fan_count %||% NA_real_))
  daily <- daily[order(daily$metric_date), , drop = FALSE]

  content_warning <- NULL
  raw_posts <- tryCatch(
    meta_fetch_posts(cfg, today - cfg$posts_lookback_days),
    meta_api_error = function(e) {
      content_warning <<- paste(
        "Facebook-berichten zijn niet beschikbaar met de huidige Meta-rechten.",
        "Volgers, weergaven en interacties worden wel geladen."
      )
      list()
    }
  )
  posts <- social_posts_from_api(raw_posts, cfg, today)
  sync <- data.frame(
    platform = "facebook", account_external_id = cfg$page_id,
    started_at = fetched_at, finished_at = Sys.time(), status = "success",
    daily_rows = nrow(daily), post_rows = nrow(posts),
    error_code = NA_character_, error_message = NA_character_
  )
  list(
    available = TRUE, message = NULL, content_warning = content_warning,
    daily = daily, posts = posts, sync = sync
  )
}

load_social_live_data <- function(cache_hours = 20) {
  cache_valid <- !is.null(.social_api_cache$value) && !is.na(.social_api_cache$loaded_at) &&
    difftime(Sys.time(), .social_api_cache$loaded_at, units = "hours") < cache_hours
  if (isTRUE(cache_valid)) return(.social_api_cache$value)
  result <- fetch_social_live_api()
  .social_api_cache$value <- result
  .social_api_cache$loaded_at <- Sys.time()
  result
}

hydrate_social_dashboard <- function(data) {
  data$social_live <- tryCatch(
    load_social_live_data(),
    error = function(e) leeg_social_live(paste("Live Facebook-data kon niet worden geladen:", social_error_message(e)))
  )
  data
}

social_number <- function(x) {
  if (!length(x) || is.na(x)) "Niet beschikbaar" else format(round(x), big.mark = ".", decimal.mark = ",")
}

social_delta <- function(current, previous) {
  if (is.na(current) || is.na(previous) || previous == 0) return(list(value = NA_real_, label = NULL, class = NULL))
  pct <- round((current - previous) / previous * 100, 1)
  list(value = pct, label = paste0(if (pct > 0) "+" else "", format(pct, decimal.mark = ","), "%"),
       class = if (pct > 0) "kpi-trend-up" else if (pct < 0) "kpi-trend-down" else NULL)
}
