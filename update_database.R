library(DBI)
library(duckdb)

con <- dbConnect(
  duckdb::duckdb(),
  "bedrijf.duckdb"
)

update_csv <- function(
    bestand,
    tabel,
    schema = "raw",
    datum_kolommen = NULL
) {
  
  cat("\n==============================\n")
  cat("CSV import\n")
  cat("==============================\n")
  
  df <- read.csv2(
    bestand,
    stringsAsFactors = FALSE
  )
  
  cat("Records:", nrow(df), "\n")
  
  # Lege Excel-kolommen verwijderen
  df <- df[, !grepl("^X(\\.|$)", names(df))]
  
  # Datums omzetten
  if (!is.null(datum_kolommen)) {
    
    for (kolom in datum_kolommen) {
      
      if (kolom %in% names(df)) {
        
        df[[kolom]] <- as.Date(
          df[[kolom]],
          format = "%d-%m-%Y"
        )
        
      }
      
    }
    
  }
  
  # Automatisch numerieke kolommen herkennen
  for (kolom in names(df)) {
    
    if (is.character(df[[kolom]])) {
      
      tmp <- gsub(",", ".", df[[kolom]])
      
      if (all(
        is.na(tmp) |
        tmp == "" |
        grepl("^-?[0-9]+\\.?[0-9]*$", tmp)
      )) {
        
        df[[kolom]] <- as.numeric(tmp)
        
      }
      
    }
    
  }
  
  dbWriteTable(
    con,
    DBI::Id(schema = schema, table = tabel),
    df,
    overwrite = TRUE
  )
  
  cat("\n✔ Tabel bijgewerkt\n\n")
  
  print(
    dbGetQuery(
      con,
      paste0(
        "DESCRIBE ",
        schema,
        ".",
        tabel
      )
    )
  )
  
  #
  # STAGING
  #
  
  if (dir.exists("sql/staging")) {
    
    cat("\n==============================\n")
    cat("Staging vernieuwen\n")
    cat("==============================\n")
    
    bestanden <- list.files(
      "sql/staging",
      pattern="\\.sql$",
      full.names=TRUE
    )
    
    for (f in bestanden) {
      
      cat("->", basename(f), "\n")
      
      sql <- paste(
        readLines(f, warn = FALSE),
        collapse="\n"
      )
      
      dbExecute(con, sql)
      
    }
    
  }
  
  #
  # MART
  #
  
  cat("\n==============================\n")
  cat("Mart vernieuwen\n")
  cat("==============================\n")
  
  bestanden <- list.files(
    "sql/mart",
    pattern="\\.sql$",
    full.names=TRUE
  )
  
  for (f in bestanden) {
    
    cat("->", basename(f), "\n")
    
    sql <- paste(
      readLines(f, warn = FALSE),
      collapse="\n"
    )
    
    dbExecute(con, sql)
    
  }
  
  #
  # Controle
  #
  
  cat("\n==============================\n")
  cat("Controle views\n")
  cat("==============================\n")
  
  views <- dbGetQuery(con, "
      SELECT view_name
      FROM duckdb_views()
      WHERE schema_name='mart'
      ORDER BY view_name
  ")
  
  fouten <- character()
  
  for(v in views$view_name){
    
    ok <- tryCatch({
      
      dbGetQuery(
        con,
        paste0(
          "SELECT * FROM mart.",
          v,
          " LIMIT 1"
        )
      )
      
      TRUE
      
    }, error=function(e){
      
      fouten <<- c(
        fouten,
        paste(v, "-", e$message)
      )
      
      FALSE
      
    })
    
  }
  
  if(length(fouten)==0){
    
    cat("\n✔ Alle mart-views werken.\n")
    
  } else {
    
    cat("\n⚠ Problemen gevonden:\n\n")
    
    cat(
      paste(
        fouten,
        collapse="\n"
      )
    )
    
  }
  
  invisible(df)
  
}

disconnect_database <- function(){
  
  dbDisconnect(
    con,
    shutdown = TRUE
  )
  
}