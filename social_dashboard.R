source("social_database.R")

leeg_social_live <- function(message = "Live Facebook-data is nog niet beschikbaar.") {
  list(
    available = FALSE,
    message = message,
    daily = data.frame(),
    posts = data.frame(),
    sync = data.frame()
  )
}

load_social_live_data <- function() {
  if (!social_db_configured()) return(leeg_social_live("De Aiven MySQL-verbinding is niet ingesteld."))
  con <- social_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  vereist <- c("social_accounts", "social_daily_stats", "social_posts", "social_post_stats", "social_sync_logs")
  ontbrekend <- setdiff(vereist, DBI::dbListTables(con))
  if (length(ontbrekend)) {
    return(leeg_social_live("De Social Media-databasemigratie is nog niet uitgevoerd."))
  }

  daily <- DBI::dbGetQuery(con, paste(
    "SELECT a.platform, a.external_account_id, a.account_name, a.account_timezone,",
    "d.metric_date, d.followers_count, d.fan_count, d.media_views, d.post_engagements,",
    "d.source_end_time, d.collected_at",
    "FROM social_accounts a JOIN social_daily_stats d ON d.account_id=a.id",
    "WHERE a.is_active=1 ORDER BY d.metric_date"
  ))
  posts <- DBI::dbGetQuery(con, paste(
    "SELECT a.platform, p.external_post_id, p.post_type, p.message, p.permalink_url,",
    "p.picture_url, p.published_at, s.captured_date, s.media_views, s.likes,",
    "s.reactions_total, s.comments, s.shares, s.engagement_total, s.collected_at",
    "FROM social_accounts a JOIN social_posts p ON p.account_id=a.id",
    "LEFT JOIN social_post_stats s ON s.post_id=p.id",
    "AND NOT EXISTS (SELECT 1 FROM social_post_stats newer",
    "WHERE newer.post_id=s.post_id AND (newer.captured_date>s.captured_date",
    "OR (newer.captured_date=s.captured_date AND newer.collected_at>s.collected_at)))",
    "WHERE a.is_active=1 ORDER BY p.published_at DESC"
  ))
  sync <- DBI::dbGetQuery(con, paste(
    "SELECT platform, account_external_id, started_at, finished_at, status,",
    "daily_rows, post_rows, error_code, error_message",
    "FROM social_sync_logs ORDER BY started_at DESC LIMIT 20"
  ))
  if (nrow(daily)) daily$metric_date <- as.Date(daily$metric_date)
  if (nrow(posts)) {
    posts$published_at <- as.POSIXct(posts$published_at, tz = "UTC")
    posts$captured_date <- as.Date(posts$captured_date)
  }
  list(available = nrow(daily) > 0, message = NULL, daily = daily, posts = posts, sync = sync)
}

hydrate_social_dashboard <- function(data) {
  data$social_live <- tryCatch(
    load_social_live_data(),
    error = function(e) leeg_social_live(paste("Live Facebook-data kon niet worden geladen:", conditionMessage(e)))
  )
  data
}

social_number <- function(x) {
  if (!length(x) || is.na(x)) "Niet beschikbaar" else format(round(x), big.mark = ".", decimal.mark = ",")
}

social_delta <- function(current, previous) {
  if (is.na(current) || is.na(previous) || previous == 0) return(list(value = NA_real_, label = NULL, class = NULL))
  pct <- round((current - previous) / previous * 100, 1)
  list(
    value = pct,
    label = paste0(if (pct > 0) "+" else "", format(pct, decimal.mark = ","), "%"),
    class = if (pct > 0) "kpi-trend-up" else if (pct < 0) "kpi-trend-down" else NULL
  )
}

