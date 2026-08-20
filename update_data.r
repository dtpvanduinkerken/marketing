# ==================================================
# UPDATE_DATA.R
# Importeert members, personal_pricing en afspraken
# en vernieuwt alleen de daarvan afhankelijke marts.
# ==================================================

source("update_database.R")

database_pad <- Sys.getenv("DASHBOARD_DB_PATH", unset = "bedrijf.duckdb")
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

    controles <- c(
      "mart.members_activiteit",
      "mart.members_groei",
      "mart.members_kpis",
      "mart.members_woonplaats",
      "mart.pricing_performance",
      "mart.omzet_per_maand",
      "mart.afspraken_kpis"
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
      ORDER BY bron
    ")

    cat("\n==============================\n")
    cat("Importcontrole\n")
    cat("==============================\n")
    print(aantallen, row.names = FALSE)

    DBI::dbCommit(con)
    transactie_actief <- FALSE
    cat("\n✔ Members, personal pricing en afspraken zijn bijgewerkt.\n")
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
