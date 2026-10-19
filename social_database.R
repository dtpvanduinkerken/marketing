library(DBI)

social_env <- function(primary, aliases = character(), default = "") {
  for (naam in c(primary, aliases)) {
    waarde <- Sys.getenv(naam, unset = "")
    if (nzchar(waarde)) return(waarde)
  }
  default
}

social_db_configured <- function() {
  all(nzchar(c(
    social_env("SOCIAL_DB_HOST", c("AIVEN_MYSQL_HOST", "MYSQL_HOST", "DB_HOST")),
    social_env("SOCIAL_DB_NAME", c("AIVEN_MYSQL_DATABASE", "MYSQL_DATABASE", "DB_NAME")),
    social_env("SOCIAL_DB_USER", c("AIVEN_MYSQL_USER", "MYSQL_USER", "DB_USER")),
    social_env("SOCIAL_DB_PASSWORD", c("AIVEN_MYSQL_PASSWORD", "MYSQL_PASSWORD", "DB_PASSWORD"))
  )))
}

social_db_connect <- function() {
  if (!requireNamespace("RMariaDB", quietly = TRUE)) {
    stop("Het pakket RMariaDB ontbreekt; installeer de projectdependencies opnieuw.")
  }
  if (!social_db_configured()) {
    stop("De Aiven MySQL-omgevingsvariabelen voor Social Media zijn niet volledig ingesteld.")
  }

  ssl_ca <- social_env("SOCIAL_DB_SSL_CA", c("AIVEN_MYSQL_SSL_CA"))
  ssl_pem <- social_env("SOCIAL_DB_SSL_CA_PEM", c("AIVEN_MYSQL_SSL_CA_PEM"))
  if (!nzchar(ssl_ca) && nzchar(ssl_pem)) {
    ssl_ca <- tempfile("aiven-ca-", fileext = ".pem")
    writeLines(gsub("\\\\n", "\n", ssl_pem), ssl_ca, useBytes = TRUE)
  }

  args <- list(
    drv = RMariaDB::MariaDB(),
    host = social_env("SOCIAL_DB_HOST", c("AIVEN_MYSQL_HOST", "MYSQL_HOST", "DB_HOST")),
    port = as.integer(social_env("SOCIAL_DB_PORT", c("AIVEN_MYSQL_PORT", "MYSQL_PORT", "DB_PORT"), "3306")),
    dbname = social_env("SOCIAL_DB_NAME", c("AIVEN_MYSQL_DATABASE", "MYSQL_DATABASE", "DB_NAME")),
    user = social_env("SOCIAL_DB_USER", c("AIVEN_MYSQL_USER", "MYSQL_USER", "DB_USER")),
    password = social_env("SOCIAL_DB_PASSWORD", c("AIVEN_MYSQL_PASSWORD", "MYSQL_PASSWORD", "DB_PASSWORD")),
    timeout = 15
  )
  if (nzchar(ssl_ca)) args$ssl.ca <- ssl_ca
  do.call(DBI::dbConnect, args)
}

apply_social_migrations <- function(con, pad = "sql/mysql/001_social_media.sql") {
  if (!file.exists(pad)) stop("Social Media-migratiebestand ontbreekt: ", pad)
  sql <- paste(readLines(pad, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  statements <- strsplit(sql, "-- statement-break", fixed = TRUE)[[1]]
  statements <- trimws(statements)
  statements <- statements[nzchar(statements)]
  for (statement in statements) DBI::dbExecute(con, statement)
  invisible(TRUE)
}

social_db_upsert <- function(con, sql, params) {
  DBI::dbExecute(con, sql, params = unname(as.list(params)))
}

