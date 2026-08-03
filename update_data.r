# ==================================================
# UPDATE_DATA.R
# Herlaadt de actuele 2Factors-couponexport en bouwt
# daarna alle staging- en mart-tabellen opnieuw op.
# ==================================================

source("update_database.R")

database_pad <- Sys.getenv("COUPON_DB_PATH", unset = "bedrijf.duckdb")
staging_map <- Sys.getenv("COUPON_STAGING_MAP", unset = "sql/staging")
mart_map <- Sys.getenv("COUPON_MART_MAP", unset = "sql/mart")

# app.R maakt in een interactieve RStudio-sessie een globale verbinding
# met de naam `con`. Sluit die eerst, anders kan DuckDB hetzelfde bestand
# niet exclusief openen voor de import.
bestaande_con <- get0("con", envir = .GlobalEnv, inherits = FALSE)
if (inherits(bestaande_con, "DBIConnection") && DBI::dbIsValid(bestaande_con)) {
  message("Bestaande DuckDB-verbinding sluiten voor de couponupdate.")
  DBI::dbDisconnect(bestaande_con, shutdown = TRUE)
  rm("con", envir = .GlobalEnv)
}

con <- connect_database(database_pad)

tryCatch(
  {
    update_csv(
      con = con,
      bestand = file.path("data", "raw", "coupons.csv"),
      tabel = "coupons",
      schema = "raw",
      datum_kolommen = "datum",
      staging_map = staging_map,
      mart_map = mart_map
    )
  },
  finally = {
    if (DBI::dbIsValid(con)) {
      disconnect_database(con)
    }
  }
)
