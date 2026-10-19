source("social_sync.R")

.social_api_cache <- new.env(parent = emptyenv())
.social_api_cache$value <- NULL
.social_api_cache$loaded_at <- as.POSIXct(NA)

leeg_social_live <- function(message = "Live socialmediadata is nog niet beschikbaar.") {
  list(
    available = FALSE, message = message, content_warning = NULL, warnings = data.frame(),
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

fetch_facebook_live_api <- function(today = Sys.Date()) {
  cfg <- meta_config()
  token_info <- meta_token_status(cfg)
  if (!is.null(token_info) && !isTRUE(token_info$is_valid)) stop("Het Meta-toegangstoken is ongeldig of verlopen.")

  fetched_at <- Sys.time()
  page <- meta_fetch_page(cfg)
  daily <- meta_fetch_daily_insights(cfg, today - 729, today)
  follower_history <- meta_fetch_follower_history(cfg, today - 729, today)
  if (!nrow(daily)) {
    daily <- data.frame(metric_date = as.Date(today), media_views = NA_real_,
                        post_engagements = NA_real_, followers_count = NA_real_,
                        follower_change = NA_real_,
                        source_end_time = as.POSIXct(NA))
  }
  if (!"followers_count" %in% names(daily)) daily$followers_count <- NA_real_
  if (nrow(follower_history)) {
    ontbrekende_datums <- setdiff(follower_history$metric_date, daily$metric_date)
    if (length(ontbrekende_datums)) {
      daily <- rbind(daily, data.frame(
        metric_date = as.Date(ontbrekende_datums), media_views = NA_real_,
        post_engagements = NA_real_, followers_count = NA_real_,
        follower_change = NA_real_,
        source_end_time = as.POSIXct(NA)
      ))
    }
    doel <- match(follower_history$metric_date, daily$metric_date)
    daily$followers_count[doel] <- follower_history$followers_count
  }
  daily$platform <- "facebook"
  daily$external_account_id <- as.character(page$id)
  daily$account_name <- as.character(page$name)
  daily$account_timezone <- cfg$timezone
  daily$fan_count <- NA_real_
  daily$collected_at <- fetched_at

  today_index <- which(daily$metric_date == as.Date(today))
  if (!length(today_index)) {
    daily <- rbind(daily, data.frame(
      metric_date = as.Date(today), media_views = NA_real_, post_engagements = NA_real_,
      followers_count = NA_real_, follower_change = NA_real_,
      source_end_time = as.POSIXct(NA), platform = "facebook",
      external_account_id = as.character(page$id), account_name = as.character(page$name),
      account_timezone = cfg$timezone, fan_count = NA_real_,
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

instagram_posts_from_api <- function(raw_media, cfg, captured_date = Sys.Date()) {
  if (!length(raw_media)) return(empty_social_posts())
  grens <- as.Date(captured_date) - cfg$posts_lookback_days
  rows <- lapply(raw_media, function(media) {
    gepubliceerd <- meta_epoch(media$timestamp)
    publicatiedatum <- as.Date(format(gepubliceerd, tz = cfg$timezone))
    if (is.na(publicatiedatum) || publicatiedatum < grens) return(NULL)
    views <- instagram_fetch_media_metric(media$id, "views", cfg)
    total_interactions <- instagram_fetch_media_metric(media$id, "total_interactions", cfg)
    likes <- suppressWarnings(as.numeric(media$like_count %||% NA_real_))
    comments <- suppressWarnings(as.numeric(media$comments_count %||% NA_real_))
    engagement <- if (!is.na(total_interactions)) total_interactions else {
      if (all(is.na(c(likes, comments)))) NA_real_ else sum(c(likes, comments), na.rm = TRUE)
    }
    data.frame(
      platform = "instagram", external_post_id = as.character(media$id),
      post_type = tolower(as.character(media$media_product_type %||% media$media_type %||% "post")),
      message = as.character(media$caption %||% NA_character_),
      permalink_url = as.character(media$permalink %||% NA_character_),
      picture_url = as.character(media$thumbnail_url %||% media$media_url %||% NA_character_),
      published_at = gepubliceerd, captured_date = as.Date(captured_date),
      media_views = views, likes = likes, reactions_total = likes,
      comments = comments, shares = NA_real_, engagement_total = engagement,
      collected_at = Sys.time(), stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) empty_social_posts() else do.call(rbind, rows)
}

fetch_instagram_live_api <- function(today = Sys.Date()) {
  cfg <- instagram_config()
  fetched_at <- Sys.time()
  profile <- instagram_fetch_profile(cfg)
  account_id <- as.character(profile$id %||% profile$user_id %||% cfg$account_id)
  if (!nzchar(account_id)) stop("Instagram-account-ID kon niet worden bepaald.")

  daily <- instagram_fetch_reach_history(cfg, account_id, today - 89, today) |>
    instagram_parse_reach_history(cfg$timezone)
  if (!nrow(daily)) {
    daily <- data.frame(
      metric_date = as.Date(today), media_views = NA_real_, post_engagements = NA_real_,
      followers_count = NA_real_, follower_change = NA_real_, source_end_time = as.POSIXct(NA)
    )
  }
  daily$platform <- "instagram"
  daily$external_account_id <- account_id
  daily$account_name <- as.character(profile$username %||% profile$name %||% "Instagram")
  daily$account_timezone <- cfg$timezone
  daily$fan_count <- NA_real_
  daily$collected_at <- fetched_at
  today_index <- which(daily$metric_date == as.Date(today))
  if (!length(today_index)) {
    daily <- rbind(daily, data.frame(
      metric_date = as.Date(today), media_views = NA_real_, post_engagements = NA_real_,
      followers_count = NA_real_, follower_change = NA_real_, source_end_time = as.POSIXct(NA),
      platform = "instagram", external_account_id = account_id,
      account_name = as.character(profile$username %||% profile$name %||% "Instagram"),
      account_timezone = cfg$timezone, fan_count = NA_real_, collected_at = fetched_at
    ))
    today_index <- nrow(daily)
  }
  daily$followers_count[today_index] <- suppressWarnings(as.numeric(profile$followers_count %||% NA_real_))
  daily <- daily[order(daily$metric_date), , drop = FALSE]

  content_warning <- NULL
  raw_media <- tryCatch(
    instagram_fetch_media(cfg, account_id),
    instagram_api_error = function(e) {
      content_warning <<- paste(
        "Instagram-content is niet beschikbaar met de huidige rechten.",
        "Profiel- en bereikcijfers worden wel geladen."
      )
      list()
    }
  )
  posts <- instagram_posts_from_api(raw_media, cfg, today)
  sync <- data.frame(
    platform = "instagram", account_external_id = account_id,
    started_at = fetched_at, finished_at = Sys.time(), status = "success",
    daily_rows = nrow(daily), post_rows = nrow(posts),
    error_code = NA_character_, error_message = NA_character_
  )
  list(
    available = TRUE, message = NULL, content_warning = content_warning,
    daily = daily, posts = posts, sync = sync
  )
}

fetch_social_live_api <- function(today = Sys.Date()) {
  resultaten <- list()
  waarschuwingen <- list()
  bronnen <- list(
    facebook = list(enabled = nzchar(Sys.getenv("FACEBOOK_ACCESS_TOKEN")), fetch = fetch_facebook_live_api),
    instagram = list(enabled = nzchar(Sys.getenv("INSTAGRAM_ACCESS_TOKEN")), fetch = fetch_instagram_live_api)
  )
  for (platform in names(bronnen)) {
    bron <- bronnen[[platform]]
    if (!isTRUE(bron$enabled)) next
    resultaat <- tryCatch(bron$fetch(today), error = function(e) e)
    if (inherits(resultaat, "error")) {
      waarschuwingen[[length(waarschuwingen) + 1L]] <- data.frame(
        platform = platform,
        message = paste0(tools::toTitleCase(platform), " kon niet worden geladen: ", social_error_message(resultaat)),
        stringsAsFactors = FALSE
      )
    } else {
      resultaten[[platform]] <- resultaat
      if (!is.null(resultaat$content_warning) && nzchar(resultaat$content_warning)) {
        waarschuwingen[[length(waarschuwingen) + 1L]] <- data.frame(
          platform = platform, message = resultaat$content_warning, stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(resultaten)) {
    melding <- if (length(waarschuwingen)) {
      paste(vapply(waarschuwingen, function(x) x$message[1], character(1)), collapse = " ")
    } else "FACEBOOK_ACCESS_TOKEN en INSTAGRAM_ACCESS_TOKEN ontbreken."
    stop(melding)
  }
  warnings <- if (length(waarschuwingen)) do.call(rbind, waarschuwingen) else
    data.frame(platform = character(), message = character())
  list(
    available = TRUE, message = NULL, content_warning = NULL, warnings = warnings,
    daily = do.call(rbind, lapply(resultaten, `[[`, "daily")),
    posts = do.call(rbind, lapply(resultaten, `[[`, "posts")),
    sync = do.call(rbind, lapply(resultaten, `[[`, "sync"))
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
    error = function(e) leeg_social_live(paste("Live socialmediadata kon niet worden geladen:", social_error_message(e)))
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
