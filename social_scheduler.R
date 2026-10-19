source("social_sync.R")

.social_scheduler_state <- new.env(parent = emptyenv())
.social_scheduler_state$running <- FALSE
.social_scheduler_state$started <- FALSE

social_sync_configured <- function() {
  social_db_configured() && nzchar(Sys.getenv("FACEBOOK_ACCESS_TOKEN"))
}

social_last_success <- function() {
  con <- social_db_connect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (!DBI::dbExistsTable(con, "social_sync_logs")) return(as.POSIXct(NA))
  value <- DBI::dbGetQuery(
    con,
    "SELECT MAX(finished_at) AS finished_at FROM social_sync_logs WHERE platform='facebook' AND status='success'"
  )$finished_at[1]
  if (is.na(value)) as.POSIXct(NA) else as.POSIXct(value, tz = "UTC")
}

social_sync_is_due <- function(max_age_hours = 20) {
  last_success <- social_last_success()
  is.na(last_success) || difftime(Sys.time(), last_success, units = "hours") >= max_age_hours
}

run_social_sync_if_due <- function() {
  if (!social_sync_configured() || isTRUE(.social_scheduler_state$running)) return(invisible(FALSE))
  .social_scheduler_state$running <- TRUE
  on.exit(.social_scheduler_state$running <- FALSE, add = TRUE)
  tryCatch({
    if (!social_sync_is_due()) return(invisible(FALSE))
    result <- sync_facebook()
    message("Social Media-synchronisatie via Render voltooid: ", result$daily_rows,
            " dagrijen en ", result$post_rows, " berichten.")
    invisible(TRUE)
  }, error = function(e) {
    message("Social Media-synchronisatie via Render mislukt: ", social_error_message(e))
    invisible(FALSE)
  })
}

start_social_scheduler <- function(first_delay_seconds = 30, check_interval_seconds = 3600) {
  if (isTRUE(.social_scheduler_state$started) || !social_sync_configured()) return(invisible(FALSE))
  .social_scheduler_state$started <- TRUE
  tick <- function() {
    run_social_sync_if_due()
    later::later(tick, delay = check_interval_seconds)
  }
  later::later(tick, delay = first_delay_seconds)
  invisible(TRUE)
}
