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
run_sql_folder <- function(con, map, label, patroon = "\\.sql$") {
  
  if (!dir.exists(map)) {
    cat("  (map", map, "bestaat niet, overgeslagen)\n")
    return(invisible(NULL))
  }
  
  bestanden <- sort(list.files(map, pattern = patroon, full.names = TRUE))
  
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
    mart_map = "sql/mart",
    staging_patroon = "\\.sql$",
    mart_patroon = "\\.sql$",
    controle_objecten = NULL
) {
  
  cat("\n==============================\n")
  cat("CSV import:", basename(bestand), "\n")
  cat("==============================\n")
  
  if (!file.exists(bestand)) {
    stop("Bestand niet gevonden: ", bestand)
  }
  
  df <- tryCatch(
    # Eerst alles als tekst lezen. De gecontroleerde conversies hieronder
    # zetten datums en bedragen om, terwijl identifiers zoals receipt_id
    # hun voorloopnullen behouden.
    read.csv2(bestand, stringsAsFactors = FALSE, colClasses = "character"),
    error = function(e) {
      stop("Kon CSV niet lezen (", bestand, "): ", e$message)
    }
  )

  # Couponexports van 2Factors gebruiken Engelstalige kolomnamen met
  # spaties. Normaliseer deze hier naar het vaste raw-contract waarop de
  # marts en het dashboard bouwen. Zo werkt dezelfde pipeline ook wanneer
  # een volgende export meerdere couponcodes bevat.
  if (identical(tabel, "coupons")) {
    coupon_kolommen <- c(
      "Customer.number" = "customer_number",
      "Coupon.Code" = "coupon_code",
      "Date" = "datum",
      "Receipt.id" = "receipt_id",
      "Discount" = "discount",
      "Turnover" = "omzet"
    )
    hernoemen <- intersect(names(coupon_kolommen), names(df))
    names(df)[match(hernoemen, names(df))] <- unname(coupon_kolommen[hernoemen])

    vereist <- unname(coupon_kolommen)
    ontbrekend <- setdiff(vereist, names(df))
    if (length(ontbrekend) > 0) {
      stop("Couponexport mist verplichte kolommen: ", paste(ontbrekend, collapse = ", "))
    }
    df <- df[, vereist, drop = FALSE]
    datum_kolommen <- unique(c(datum_kolommen, "datum"))
  }

  # De members-export bevat een geboortedatum. Behandel die kolom altijd
  # als datum, ook wanneer de aanroeper geen datum_kolommen meegeeft.
  if ("geboortedatum" %in% names(df)) {
    datum_kolommen <- unique(c(datum_kolommen, "geboortedatum"))
  }
  
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
        waarde <- trimws(as.character(df[[kolom]]))
        waarde[waarde == ""] <- NA_character_
        omgezet <- as.Date(rep(NA_character_, length(waarde)))
        for (datum_formaat in c("%d-%m-%Y", "%Y-%m-%d", "%d/%m/%Y")) {
          nog_leeg <- is.na(omgezet) & !is.na(waarde)
          omgezet[nog_leeg] <- as.Date(waarde[nog_leeg], format = datum_formaat)
        }

        # Excel kan datums als serienummer exporteren (bijv. 46207).
        # Zet alleen nog niet herkende, zuiver numerieke waarden om met
        # de Excel-origin; gewone klant- en bonnummers komen hier niet
        # terecht omdat alleen expliciete datumkolommen worden verwerkt.
        excel_serial <- is.na(omgezet) & !is.na(waarde) & grepl("^[0-9]{4,5}$", waarde)
        omgezet[excel_serial] <- as.Date(
          as.numeric(waarde[excel_serial]),
          origin = "1899-12-30"
        )
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
    is_identifier <- kolom %in% c("customer_number", "receipt_id", "klantnummer")
    if (!is_identifier && is.character(df[[kolom]]) && is_eigenlijk_numeriek(df[[kolom]])) {
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

  # De SQL-bestanden schrijven naar deze vaste lagen. Maak de schema's
  # expliciet aan zodat ook een nieuwe of tijdelijke database werkt.
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS staging")
  dbExecute(con, "CREATE SCHEMA IF NOT EXISTS mart")
  
  cat("\n✔ Tabel", paste0(schema, ".", tabel), "bijgewerkt\n\n")
  
  print(dbGetQuery(con, paste0("DESCRIBE ", schema, ".", tabel)))
  
  #
  # STAGING
  #
  cat("\n==============================\n")
  cat("Staging vernieuwen\n")
  cat("==============================\n")
  run_sql_folder(con, staging_map, "staging", staging_patroon)
  
  #
  # MART
  #
  cat("\n==============================\n")
  cat("Mart vernieuwen\n")
  cat("==============================\n")
  run_sql_folder(con, mart_map, "mart", mart_patroon)
  
  #
  # Controle van mart-views
  #
  cat("\n==============================\n")
  cat("Controle views\n")
  cat("==============================\n")
  
  if (is.null(controle_objecten)) {
    controle_objecten <- dbGetQuery(con, "
        SELECT view_name
        FROM duckdb_views()
        WHERE schema_name = 'mart'
        ORDER BY view_name
    ")$view_name
  }
  
  fouten <- character()
  
  for (v in controle_objecten) {
    ok <- tryCatch({
      dbGetQuery(con, paste0("SELECT * FROM mart.", v, " LIMIT 1"))
      TRUE
    }, error = function(e) {
      fouten <<- c(fouten, paste(v, "-", e$message))
      FALSE
    })
  }
  
  if (length(fouten) == 0) {
    cat("\n✔ Alle geselecteerde mart-objecten werken.\n")
  } else {
    cat("\n⚠ Problemen gevonden:\n\n")
    cat(paste(fouten, collapse = "\n"), "\n")
  }
  
  invisible(df)
}
