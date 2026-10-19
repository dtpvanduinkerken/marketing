library(httr2)
library(jsonlite)
source("social_database.R")

meta_config <- function() {
  api_version <- Sys.getenv("FACEBOOK_API_VERSION", unset = "")
  if (!nzchar(api_version)) api_version <- "v26.0"
  lookback <- Sys.getenv("FACEBOOK_POSTS_LOOKBACK_DAYS", unset = "")
  if (!nzchar(lookback)) lookback <- "180"
  cfg <- list(
    page_id = Sys.getenv("FACEBOOK_PAGE_ID", unset = "135617146515071"),
    access_token = Sys.getenv("FACEBOOK_ACCESS_TOKEN"),
    api_version = api_version,
    app_id = Sys.getenv("META_APP_ID"),
    app_secret = Sys.getenv("META_APP_SECRET"),
    timezone = Sys.getenv("SOCIAL_TIMEZONE", unset = "Europe/Amsterdam"),
    posts_lookback_days = as.integer(lookback)
  )
  if (!grepl("^v[0-9]+\\.[0-9]+$", cfg$api_version)) stop("Ongeldige FACEBOOK_API_VERSION.")
  if (!nzchar(cfg$page_id) || !grepl("^[0-9]+$", cfg$page_id)) stop("Ongeldige FACEBOOK_PAGE_ID.")
  if (!nzchar(cfg$access_token)) stop("FACEBOOK_ACCESS_TOKEN ontbreekt.")
  cfg
}

meta_graph_request <- function(path, query = list(), cfg = meta_config()) {
  url <- paste0("https://graph.facebook.com/", cfg$api_version, "/", sub("^/", "", path))
  response <- request(url) |>
    req_auth_bearer_token(cfg$access_token) |>
    req_url_query(!!!query) |>
    req_user_agent("VanDuinkerkenSocialDashboard/1.0") |>
    req_retry(max_tries = 4, backoff = ~ min(30, 2 ^ .x)) |>
    req_timeout(60) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  body <- resp_body_json(response, simplifyVector = FALSE)
  if (resp_status(response) >= 400 || !is.null(body$error)) {
    err <- body$error %||% list()
    code <- as.character(err$code %||% resp_status(response))
    message <- as.character(err$message %||% "Onbekende Meta Graph API-fout")
    condition <- structure(
      list(message = message, call = NULL, code = code, subcode = err$error_subcode %||% NULL),
      class = c("meta_api_error", "error", "condition")
    )
    stop(condition)
  }
  body
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

meta_epoch <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
}

meta_metric_date <- function(end_time, timezone = "Europe/Amsterdam") {
  moment <- meta_epoch(end_time)
  as.Date(format(moment, tz = timezone, usetz = FALSE)) - 1
}

meta_parse_insights <- function(response, timezone = "Europe/Amsterdam") {
  metrics <- response$data %||% list()
  rows <- list()
  for (metric in metrics) {
    naam <- metric$name %||% ""
    for (waarde in metric$values %||% list()) {
      datum <- meta_metric_date(waarde$end_time, timezone)
      sleutel <- as.character(datum)
      if (is.null(rows[[sleutel]])) {
        rows[[sleutel]] <- list(
          metric_date = datum,
          media_views = NA_real_,
          post_engagements = NA_real_,
          source_end_time = meta_epoch(waarde$end_time)
        )
      }
      getal <- suppressWarnings(as.numeric(waarde$value))
      if (identical(naam, "page_media_view")) rows[[sleutel]]$media_views <- getal
      if (identical(naam, "page_post_engagements")) rows[[sleutel]]$post_engagements <- getal
    }
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, lapply(rows, function(x) data.frame(
    metric_date = as.Date(x$metric_date),
    media_views = x$media_views,
    post_engagements = x$post_engagements,
    source_end_time = as.POSIXct(x$source_end_time, origin = "1970-01-01", tz = "UTC")
  )))
}

meta_fetch_page <- function(cfg) {
  meta_graph_request(cfg$page_id, list(fields = "id,name,followers_count,fan_count"), cfg)
}

meta_fetch_daily_insights <- function(cfg, since, until) {
  starts <- seq(as.Date(since), as.Date(until), by = 90)
  delen <- lapply(starts, function(start) {
    einde <- min(start + 89, as.Date(until))
    response <- meta_graph_request(
      paste0(cfg$page_id, "/insights"),
      list(
        metric = "page_post_engagements,page_media_view",
        period = "day",
        since = format(start, "%Y-%m-%d"),
        until = format(einde + 1, "%Y-%m-%d")
      ),
      cfg
    )
    meta_parse_insights(response, cfg$timezone)
  })
  delen <- delen[vapply(delen, nrow, integer(1)) > 0]
  if (!length(delen)) return(data.frame())
  resultaat <- do.call(rbind, delen)
  resultaat[!duplicated(resultaat$metric_date, fromLast = TRUE), , drop = FALSE]
}

meta_fetch_posts <- function(cfg, since) {
  query <- list(
    fields = paste(
      "id", "message", "created_time", "permalink_url", "full_picture",
      "attachments{media_type,type}", "shares", "comments.limit(0).summary(true)",
      "reactions.type(LIKE).limit(0).summary(true)", sep = ","
    ),
    since = format(as.Date(since), "%Y-%m-%d"),
    limit = 100
  )
  response <- meta_graph_request(paste0(cfg$page_id, "/posts"), query, cfg)
  posts <- response$data %||% list()
  next_url <- (response$paging %||% list())[["next"]] %||% ""
  while (nzchar(next_url) && length(posts) < 1000) {
    volgende <- request(next_url) |>
      req_user_agent("VanDuinkerkenSocialDashboard/1.0") |>
      req_retry(max_tries = 4) |>
      req_timeout(60) |>
      req_perform() |>
      resp_body_json(simplifyVector = FALSE)
    posts <- c(posts, volgende$data %||% list())
    next_url <- (volgende$paging %||% list())[["next"]] %||% ""
  }
  posts
}

meta_post_type <- function(post) {
  attachments <- (post$attachments %||% list())$data %||% list()
  attachment <- if (length(attachments)) attachments[[1]] else list()
  type <- attachment$media_type %||% attachment$type %||% "status"
  tolower(as.character(type))
}

meta_summary_count <- function(x) {
  waarde <- x$summary$total_count %||% NA_real_
  suppressWarnings(as.numeric(waarde))
}

meta_fetch_post_insights <- function(post_id, cfg) {
  response <- tryCatch(
    meta_graph_request(
      paste0(post_id, "/insights"),
      list(metric = "post_media_view,post_reactions_by_type_total", period = "lifetime"),
      cfg
    ),
    meta_api_error = function(e) list(data = list())
  )
  waarden <- setNames(lapply(response$data %||% list(), function(x) {
    values <- x$values %||% list()
    value <- if (length(values)) values[[1]]$value %||% NA else NA
    value
  }), vapply(response$data %||% list(), function(x) x$name %||% "", character(1)))
  media_views <- suppressWarnings(as.numeric(waarden$post_media_view %||% NA_real_))
  reacties <- waarden$post_reactions_by_type_total %||% NA
  reactions_total <- if (is.list(reacties)) {
    aantallen <- suppressWarnings(as.numeric(unlist(reacties)))
    if (!length(aantallen) || all(is.na(aantallen))) NA_real_ else sum(aantallen, na.rm = TRUE)
  } else suppressWarnings(as.numeric(reacties))
  list(media_views = media_views, reactions_total = reactions_total)
}

meta_token_status <- function(cfg) {
  if (!nzchar(cfg$app_id) || !nzchar(cfg$app_secret)) return(NULL)
  inspector <- paste0(cfg$app_id, "|", cfg$app_secret)
  response <- request(paste0("https://graph.facebook.com/", cfg$api_version, "/debug_token")) |>
    req_auth_bearer_token(inspector) |>
    req_url_query(input_token = cfg$access_token) |>
    req_timeout(30) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)
  response$data %||% NULL
}

social_error_message <- function(e) {
  tekst <- conditionMessage(e)
  tekst <- gsub("EA[A-Za-z0-9]+", "[TOKEN VERWIJDERD]", tekst)
  substr(tekst, 1, 1000)
}

sync_facebook <- function(con = NULL, cfg = meta_config(), today = Sys.Date()) {
  eigen_con <- is.null(con)
  if (eigen_con) con <- social_db_connect()
  if (eigen_con) on.exit(DBI::dbDisconnect(con), add = TRUE)
  apply_social_migrations(con)

  start <- Sys.time()
  log_id <- DBI::dbGetQuery(con, "SELECT 0 AS id")$id[1]
  DBI::dbExecute(con, paste(
    "INSERT INTO social_sync_logs",
    "(platform, account_external_id, started_at, status)",
    "VALUES ('facebook', ?, ?, 'running')"
  ), params = list(cfg$page_id, format(start, "%Y-%m-%d %H:%M:%S")))
  log_id <- DBI::dbGetQuery(con, "SELECT LAST_INSERT_ID() AS id")$id[1]

  dagelijkse_rijen <- 0L
  post_rijen <- 0L
  tryCatch({
    token_info <- meta_token_status(cfg)
    if (!is.null(token_info) && !isTRUE(token_info$is_valid)) stop("Het Meta-toegangstoken is ongeldig of verlopen.")

    page <- meta_fetch_page(cfg)
    social_db_upsert(con, paste(
      "INSERT INTO social_accounts (platform, external_account_id, account_name, account_timezone)",
      "VALUES ('facebook', ?, ?, ?)",
      "ON DUPLICATE KEY UPDATE account_name=VALUES(account_name), account_timezone=VALUES(account_timezone), is_active=1"
    ), c(page$id, page$name, cfg$timezone))
    account_id <- DBI::dbGetQuery(con,
      "SELECT id FROM social_accounts WHERE platform='facebook' AND external_account_id=?",
      params = list(cfg$page_id)
    )$id[1]

    laatste <- DBI::dbGetQuery(con,
      "SELECT MAX(metric_date) AS laatste FROM social_daily_stats WHERE account_id=?",
      params = list(account_id)
    )$laatste[1]
    since <- if (is.na(laatste)) today - 729 else max(as.Date(laatste) - 7, today - 729)
    daily <- meta_fetch_daily_insights(cfg, since, today)

    DBI::dbBegin(con)
    if (nrow(daily)) {
      for (i in seq_len(nrow(daily))) {
        social_db_upsert(con, paste(
          "INSERT INTO social_daily_stats",
          "(account_id, metric_date, media_views, post_engagements, metric_period, source_end_time, collected_at)",
          "VALUES (?, ?, ?, ?, 'day', ?, CURRENT_TIMESTAMP)",
          "ON DUPLICATE KEY UPDATE media_views=VALUES(media_views), post_engagements=VALUES(post_engagements),",
          "source_end_time=VALUES(source_end_time), collected_at=CURRENT_TIMESTAMP"
        ), list(
          account_id, format(daily$metric_date[i], "%Y-%m-%d"), daily$media_views[i],
          daily$post_engagements[i], format(daily$source_end_time[i], "%Y-%m-%d %H:%M:%S", tz = "UTC")
        ))
      }
      dagelijkse_rijen <- nrow(daily)
    }
    social_db_upsert(con, paste(
      "INSERT INTO social_daily_stats",
      "(account_id, metric_date, followers_count, fan_count, metric_period, collected_at)",
      "VALUES (?, ?, ?, ?, 'snapshot', CURRENT_TIMESTAMP)",
      "ON DUPLICATE KEY UPDATE followers_count=VALUES(followers_count), fan_count=VALUES(fan_count), collected_at=CURRENT_TIMESTAMP"
    ), list(account_id, format(today, "%Y-%m-%d"), page$followers_count %||% NA, page$fan_count %||% NA))

    posts <- meta_fetch_posts(cfg, today - cfg$posts_lookback_days)
    if (length(posts)) {
      for (post in posts) {
        published <- meta_epoch(post$created_time)
        social_db_upsert(con, paste(
          "INSERT INTO social_posts",
          "(account_id, external_post_id, post_type, message, permalink_url, picture_url, published_at)",
          "VALUES (?, ?, ?, ?, ?, ?, ?)",
          "ON DUPLICATE KEY UPDATE post_type=VALUES(post_type), message=VALUES(message),",
          "permalink_url=VALUES(permalink_url), picture_url=VALUES(picture_url), published_at=VALUES(published_at)"
        ), list(
          account_id, post$id, meta_post_type(post), post$message %||% NA,
          post$permalink_url %||% NA, post$full_picture %||% NA,
          format(published, "%Y-%m-%d %H:%M:%S", tz = "UTC")
        ))
        post_id <- DBI::dbGetQuery(con,
          "SELECT id FROM social_posts WHERE account_id=? AND external_post_id=?",
          params = list(account_id, post$id)
        )$id[1]
        insight <- meta_fetch_post_insights(post$id, cfg)
        likes <- meta_summary_count(post$reactions %||% list())
        comments <- meta_summary_count(post$comments %||% list())
        shares <- suppressWarnings(as.numeric(post$shares$count %||% NA_real_))
        reactions_for_total <- if (is.na(insight$reactions_total)) likes else insight$reactions_total
        engagement <- if (all(is.na(c(reactions_for_total, comments, shares)))) {
          NA_real_
        } else {
          sum(c(reactions_for_total, comments, shares), na.rm = TRUE)
        }
        social_db_upsert(con, paste(
          "INSERT INTO social_post_stats",
          "(post_id, captured_date, media_views, likes, reactions_total, comments, shares, engagement_total, collected_at)",
          "VALUES (?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)",
          "ON DUPLICATE KEY UPDATE media_views=VALUES(media_views), likes=VALUES(likes),",
          "reactions_total=VALUES(reactions_total), comments=VALUES(comments), shares=VALUES(shares),",
          "engagement_total=VALUES(engagement_total), collected_at=CURRENT_TIMESTAMP"
        ), list(
          post_id, format(today, "%Y-%m-%d"), insight$media_views, likes,
          insight$reactions_total, comments, shares, engagement
        ))
      }
      post_rijen <- length(posts)
    }
    DBI::dbCommit(con)
    DBI::dbExecute(con, paste(
      "UPDATE social_sync_logs SET finished_at=?, status='success', daily_rows=?, post_rows=? WHERE id=?"
    ), params = list(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), dagelijkse_rijen, post_rijen, log_id))
    list(daily_rows = dagelijkse_rijen, post_rows = post_rijen)
  }, error = function(e) {
    if (DBI::dbIsValid(con)) try(DBI::dbRollback(con), silent = TRUE)
    code <- if (inherits(e, "meta_api_error")) e$code else class(e)[1]
    DBI::dbExecute(con, paste(
      "UPDATE social_sync_logs SET finished_at=?, status='error', error_code=?, error_message=? WHERE id=?"
    ), params = list(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), code, social_error_message(e), log_id))
    stop(e)
  })
}

