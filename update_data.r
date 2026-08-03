# ==================================================
# UPDATE_DATA.R
# Herlaadt de actuele 2Factors-couponexport en bouwt
# daarna alle staging- en mart-tabellen opnieuw op.
# ==================================================

source("update_database.R")

con <- connect_database("bedrijf.duckdb")
on.exit(disconnect_database(con), add = TRUE)

update_csv(
  con = con,
  bestand = file.path("data", "raw", "coupons.csv"),
  tabel = "coupons",
  schema = "raw",
  datum_kolommen = "datum"
)
