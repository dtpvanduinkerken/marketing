# ==================================================
# UPDATE_DATA.R
# Importeert members, personal_pricing, afspraken en social media
# en vernieuwt alleen de daarvan afhankelijke marts.
# ==================================================

source("update_database.R")
source("update_render_snapshot.R")

database_pad <- Sys.getenv("DASHBOARD_DB_PATH", unset = "bedrijf.duckdb")
snapshot_pad <- Sys.getenv("DASHBOARD_SNAPSHOT_PATH", unset = "render_snapshot.duckdb")
lege_sql_map <- file.path(tempdir(), "geen_sql_bestanden")

# app.R maakt in een interactieve RStudio-sessie een globale verbinding
# met de naam `con`. Sluit die eerst, anders kan DuckDB hetzelfde bestand
# niet exclusief openen voor de import.
bestaande_con <- get0("con", envir = .GlobalEnv, inherits = FALSE)
if (inherits(bestaande_con, "DBIConnection") && DBI::dbIsValid(bestaande_con)) {
  message("Bestaande DuckDB-verbinding sluiten voor de data-update.")
  DBI::dbDisconnect(bestaande_con, shutdown = TRUE)
  rm("con", envir = .GlobalEnv)
}

con <- connect_database(database_pad)
transactie_actief <- FALSE

import_raw <- function(bestand, tabel, datum_kolommen) {
  update_csv(
    con = con,
    bestand = file.path("data", "raw", bestand),
    tabel = tabel,
    schema = "raw",
    datum_kolommen = datum_kolommen,
    staging_map = lege_sql_map,
    mart_map = lege_sql_map,
    controle_objecten = character()
  )
}

tryCatch(
  {
    DBI::dbBegin(con)
    transactie_actief <- TRUE

    import_raw(
      bestand = "members.csv",
      tabel = "members",
      datum_kolommen = c(
        "aanmelddatum",
        "eerste_aankoop",
        "laatste_aankoop",
        "geboortedatum"
      )
    )

    import_raw(
      bestand = "personal_pricing.csv",
      tabel = "personal_pricing",
      datum_kolommen = "datum"
    )

    import_raw(
      bestand = "afspraken.csv",
      tabel = "afspraken",
      datum_kolommen = "datum"
    )

    import_raw(
      bestand = "social_media.csv",
      tabel = "social_media",
      datum_kolommen = "datum"
    )

    import_raw(
      bestand = "social_media_volgers.csv",
      tabel = "social_media_volgers",
      datum_kolommen = "datum"
    )

    cat("\n==============================\n")
    cat("Afhankelijke marts vernieuwen\n")
    cat("==============================\n")

    run_sql_folder(con, "sql/mart", "members", "^members_.*\\.sql$")
    run_sql_folder(
      con,
      "sql/mart",
      "personal_pricing",
      "^(pricing_performance|omzet_per_maand)\\.sql$"
    )
    run_sql_folder(
      con,
      "sql/mart",
      "social_media",
      "^(social_media_.*|post_.*)\\.sql$"
    )

    controles <- c(
      "mart.members_activiteit",
      "mart.members_groei",
      "mart.members_kpis",
      "mart.members_woonplaats",
      "mart.pricing_performance",
      "mart.omzet_per_maand",
      "mart.afspraken_kpis",
      "mart.social_media_kpis",
      "mart.social_media_platform",
      "mart.social_media_volgergroei",
      "mart.social_media_volgers",
      "mart.post_type_performance",
      "mart.post_performance"
    )

    for (object in controles) {
      dbGetQuery(con, paste0("SELECT * FROM ", object, " LIMIT 1"))
    }

    aantallen <- dbGetQuery(con, "
      SELECT 'members' AS bron, COUNT(*) AS records FROM raw.members
      UNION ALL
      SELECT 'personal_pricing', COUNT(*) FROM raw.personal_pricing
      UNION ALL
      SELECT 'afspraken', COUNT(*) FROM raw.afspraken
      UNION ALL
      SELECT 'social_media', COUNT(*) FROM raw.social_media
      UNION ALL
      SELECT 'social_media_volgers', COUNT(*) FROM raw.social_media_volgers
      ORDER BY bron
    ")

    cat("\n==============================\n")
    cat("Importcontrole\n")
    cat("==============================\n")
    print(aantallen, row.names = FALSE)

    DBI::dbCommit(con)
    transactie_actief <- FALSE
    disconnect_database(con)

    update_render_snapshot(
      bron_pad = database_pad,
      snapshot_pad = snapshot_pad
    )

    cat("\n✔ Members, personal pricing, afspraken, social media en het dashboard-snapshot zijn bijgewerkt.\n")
  },
  error = function(e) {
    if (transactie_actief && DBI::dbIsValid(con)) {
      DBI::dbRollback(con)
      transactie_actief <- FALSE
    }
    stop(e)
  },
  finally = {
    if (DBI::dbIsValid(con)) {
      disconnect_database(con)
    }
  }
)
