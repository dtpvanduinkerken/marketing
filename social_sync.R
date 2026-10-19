library(httr2)
library(jsonlite)

meta_config <- function() {
  api_version <- Sys.getenv("FACEBOOK_API_VERSION", unset = "")
  if (!nzchar(api_version)) api_version <- "v26.0"
  lookback <- Sys.getenv("FACEBOOK_POSTS_LOOKBACK_DAYS", unset = "")
  if (!nzchar(lookback)) lookback <- "180"
  post_limit <- Sys.getenv("FACEBOOK_POST_LIMIT", unset = "")
  if (!nzchar(post_limit)) post_limit <- "30"
  cfg <- list(
    page_id = Sys.getenv("FACEBOOK_PAGE_ID", unset = "135617146515071"),
    access_token = Sys.getenv("FACEBOOK_ACCESS_TOKEN"),
    api_version = api_version,
    app_id = Sys.getenv("META_APP_ID"),
    app_secret = Sys.getenv("META_APP_SECRET"),
    timezone = Sys.getenv("SOCIAL_TIMEZONE", unset = "Europe/Amsterdam"),
    posts_lookback_days = as.integer(lookback),
    post_limit = as.integer(post_limit)
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
      "attachments{media_type,type}", sep = ","
    ),
    since = format(as.Date(since), "%Y-%m-%d"),
    limit = min(100L, cfg$post_limit)
  )
  response <- meta_graph_request(paste0(cfg$page_id, "/posts"), query, cfg)
  posts <- response$data %||% list()
  next_url <- (response$paging %||% list())[["next"]] %||% ""
  while (nzchar(next_url) && length(posts) < cfg$post_limit) {
    volgende <- request(next_url) |>
      req_user_agent("VanDuinkerkenSocialDashboard/1.0") |>
      req_retry(max_tries = 4) |>
      req_timeout(60) |>
      req_perform() |>
      resp_body_json(simplifyVector = FALSE)
    posts <- c(posts, volgende$data %||% list())
    next_url <- (volgende$paging %||% list())[["next"]] %||% ""
  }
  head(posts, cfg$post_limit)
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

