# ==================================================
# UPDATE_DATA.R
# Herlaadt de actuele 2Factors-couponexport en bouwt
# daarna alle staging- en mart-tabellen opnieuw op.
# ==================================================

source("update_database.R")

database_pad <- Sys.getenv("COUPON_DB_PATH", unset = "bedrijf.duckdb")
staging_map <- Sys.getenv("COUPON_STAGING_MAP", unset = "sql/staging")
mart_map <- Sys.getenv("COUPON_MART_MAP", unset = "sql/mart")

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
