#==============================================================
# Import script voor dashboard data
# Leest CSV-bestanden in, schrijft naar DuckDB, vernieuwt
# staging- en mart-laag, en controleert de mart-views.
#==============================================================

library(DBI)
library(duckdb)

#--------------------------------------------------------------
# Database verbinding
#--------------------------------------------------------------

connect_database <- function(pad = "bedrijf.duckdb") {
  dbConnect(duckdb::duckdb(), pad)
}

disconnect_database <- function(con) {
  dbDisconnect(con, shutdown = TRUE)
}

#--------------------------------------------------------------
# Hulpfuncties
#--------------------------------------------------------------

# Voert alle .sql bestanden in een map uit, in alfabetische volgorde.
# Stopt direct (met duidelijke melding) als een bestand een fout geeft.
run_sql_folder <- function(con, map, label) {
  
  if (!dir.exists(map)) {
    cat("  (map", map, "bestaat niet, overgeslagen)\n")
    return(invisible(NULL))
  }
  
  bestanden <- sort(list.files(map, pattern = "\\.sql$", full.names = TRUE))
  
  if (length(bestanden) == 0) {
    cat("  (geen .sql bestanden gevonden in", map, ")\n")
    return(invisible(NULL))
  }
  
  for (f in bestanden) {
    cat("->", basename(f), "\n")
    
    sql <- paste(readLines(f, warn = FALSE), collapse = "\n")
    
    resultaat <- tryCatch({
      dbExecute(con, sql)
      TRUE
    }, error = function(e) {
      cat("\n✘ FOUT in", basename(f), "(", label, "):\n   ", e$message, "\n")
      FALSE
    })
    
    if (isFALSE(resultaat)) {
      stop(sprintf("Stoppen: '%s' is mislukt in %s.", basename(f), map))
    }
  }
}

# Herkent of een character-kolom eigenlijk numeriek is.
# Negeert kolommen met leading zeros (bijv. postcodes, klantnummers),
# want die moeten als tekst blijven staan.
is_eigenlijk_numeriek <- function(x) {
  
  x_clean <- trimws(x)
  niet_leeg <- x_clean[!is.na(x_clean) & x_clean != ""]
  
  if (length(niet_leeg) == 0) return(FALSE)
  
  # Leading zero zoals "0123" of "00" telt niet als numeriek (waarschijnlijk een code)
  heeft_leading_zero <- grepl("^0[0-9]", niet_leeg)
  if (any(heeft_leading_zero)) return(FALSE)
  
  tmp <- gsub(",", ".", niet_leeg)
  all(grepl("^-?[0-9]+\\.?[0-9]*$", tmp))
}

#--------------------------------------------------------------
# Hoofdfunctie: CSV importeren en hele pipeline vernieuwen
#--------------------------------------------------------------

update_csv <- function(
    con,
    bestand,
    tabel,
    schema = "raw",
    datum_kolommen = NULL,
    staging_map = "sql/staging",
    mart_map = "sql/mart"
) {
  
  cat("\n==============================\n")
  cat("CSV import:", basename(bestand), "\n")
  cat("==============================\n")
  
  if (!file.exists(bestand)) {
    stop("Bestand niet gevonden: ", bestand)
  }
  
  df <- tryCatch(
    read.csv2(bestand, stringsAsFactors = FALSE),
    error = function(e) {
      stop("Kon CSV niet lezen (", bestand, "): ", e$message)
    }
  )
  
  if (nrow(df) == 0) {
    stop("Bestand '", bestand, "' bevat geen records. Import gestopt om lege tabel te voorkomen.")
  }
  
  cat("Records:", nrow(df), "| Kolommen:", ncol(df), "\n")
  
  # Lege Excel-kolommen verwijderen (zoals "X", "X.1", "X.2", ...)
  df <- df[, !grepl("^X(\\.[0-9]+)?$", names(df)), drop = FALSE]
  
  # Datums omzetten
  if (!is.null(datum_kolommen)) {
    for (kolom in datum_kolommen) {
      if (kolom %in% names(df)) {
        omgezet <- as.Date(df[[kolom]], format = "%d-%m-%Y")
        n_mislukt <- sum(is.na(omgezet) & !is.na(df[[kolom]]) & df[[kolom]] != "")
        if (n_mislukt > 0) {
          cat("⚠ Let op:", n_mislukt, "waarde(n) in kolom '", kolom,
              "' konden niet als datum (dd-mm-jjjj) worden geïnterpreteerd.\n")
        }
        df[[kolom]] <- omgezet
      } else {
        cat("⚠ Let op: datumkolom '", kolom, "' niet gevonden in bestand.\n")
      }
    }
  }
  
  # Automatisch numerieke kolommen herkennen (met leading-zero bescherming)
  for (kolom in names(df)) {
    if (is.character(df[[kolom]]) && is_eigenlijk_numeriek(df[[kolom]])) {
      df[[kolom]] <- as.numeric(gsub(",", ".", df[[kolom]]))
    }
  }
  
  # Schema aanmaken indien nodig
  dbExecute(con, paste0("CREATE SCHEMA IF NOT EXISTS ", schema))
  
  # Veilig schrijven: eerst naar tijdelijke tabel, dan pas vervangen.
  # Zo blijft de oude tabel intact als er iets misgaat.
  tmp_tabel <- paste0(tabel, "__tmp_import")
  
  dbExecute(con, sprintf("DROP TABLE IF EXISTS %s.%s", schema, tmp_tabel))
  
  schrijf_resultaat <- tryCatch({
    dbWriteTable(con, DBI::Id(schema = schema, table = tmp_tabel), df, overwrite = TRUE)
    TRUE
  }, error = function(e) {
    cat("✘ Schrijven naar tijdelijke tabel mislukt:", e$message, "\n")
    FALSE
  })
  
  if (isFALSE(schrijf_resultaat)) {
    stop("Import van '", bestand, "' afgebroken: tabel ", schema, ".", tabel, " is niet aangepast.")
  }
  
  dbExecute(con, sprintf("DROP TABLE IF EXISTS %s.%s", schema, tabel))
  dbExecute(con, sprintf("ALTER TABLE %s.%s RENAME TO %s", schema, tmp_tabel, tabel))
  
  cat("\n✔ Tabel", paste0(schema, ".", tabel), "bijgewerkt\n\n")
  
  print(dbGetQuery(con, paste0("DESCRIBE ", schema, ".", tabel)))
  
  #
  # STAGING
  #
  cat("\n==============================\n")
  cat("Staging vernieuwen\n")
  cat("==============================\n")
  run_sql_folder(con, staging_map, "staging")
  
  #
  # MART
  #
  cat("\n==============================\n")
  cat("Mart vernieuwen\n")
  cat("==============================\n")
  run_sql_folder(con, mart_map, "mart")
  
  #
  # Controle van mart-views
  #
  cat("\n==============================\n")
  cat("Controle views\n")
  cat("==============================\n")
  
  views <- dbGetQuery(con, "
      SELECT view_name
      FROM duckdb_views()
      WHERE schema_name = 'mart'
      ORDER BY view_name
  ")
  
  fouten <- character()
  
  for (v in views$view_name) {
    ok <- tryCatch({
      dbGetQuery(con, paste0("SELECT * FROM mart.", v, " LIMIT 1"))
      TRUE
    }, error = function(e) {
      fouten <<- c(fouten, paste(v, "-", e$message))
      FALSE
    })
  }
  
  if (length(fouten) == 0) {
    cat("\n✔ Alle mart-views werken.\n")
  } else {
    cat("\n⚠ Problemen gevonden:\n\n")
    cat(paste(fouten, collapse = "\n"), "\n")
  }
  
  invisible(df)
}