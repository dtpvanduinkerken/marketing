library(httr2)

# Leest afspraken uit de SimplyBook.me Admin API en zet ze om naar het
# bestaande raw.afspraken-contract van het dashboard. Inloggegevens worden
# uitsluitend via omgevingsvariabelen gelezen en nooit opgeslagen.

simplybook_rpc <- function(url, method, params = list(), headers = list()) {
  verzoek <- request(url) |>
    req_headers(!!!headers) |>
    req_body_json(list(
      jsonrpc = "2.0",
      method = method,
      params = params,
      id = 1L
    ), auto_unbox = TRUE) |>
    req_retry(max_tries = 3) |>
    req_timeout(60)

  antwoord <- req_perform(verzoek) |>
    resp_body_json(simplifyVector = FALSE)

  if (!is.null(antwoord$error)) {
    stop(
      "SimplyBook API-fout bij ", method, ": ",
      antwoord$error$message %||% "onbekende fout"
    )
  }
  antwoord$result
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

simplybook_waarde <- function(record, namen, standaard = NA_character_) {
  for (naam in namen) {
    waarde <- record[[naam]]
    if (!is.null(waarde) && length(waarde) > 0 && !is.list(waarde)) {
      tekst <- as.character(waarde[[1]])
      if (!is.na(tekst) && nzchar(tekst)) return(tekst)
    }
  }
  standaard
}

simplybook_dienst <- function(record) {
  direct <- simplybook_waarde(
    record,
    c("event_name", "service_name", "event", "service"),
    standaard = NA_character_
  )
  if (!is.na(direct)) return(direct)

  for (naam in c("event", "service")) {
    object <- record[[naam]]
    if (is.list(object)) {
      genest <- simplybook_waarde(object, c("name", "title"), NA_character_)
      if (!is.na(genest)) return(genest)
    }
  }

  event_id <- simplybook_waarde(record, c("event_id", "service_id"), "onbekend")
  paste("Dienst", event_id)
}

simplybook_is_geannuleerd <- function(record) {
  type <- tolower(simplybook_waarde(
    record,
    c("booking_type", "status", "approve_status"),
    ""
  ))
  if (grepl("cancel", type, fixed = TRUE)) return(TRUE)

  bevestigd <- record$is_confirmed
  if (is.null(bevestigd)) return(FALSE)
  bevestigd_tekst <- tolower(as.character(bevestigd[[1]]))
  bevestigd_tekst %in% c("0", "false", "no")
}

fetch_simplybook_afspraken <- function(
    company_login = Sys.getenv("SIMPLYBOOK_COMPANY_LOGIN"),
    user_login = Sys.getenv("SIMPLYBOOK_USER_LOGIN"),
    user_key = Sys.getenv("SIMPLYBOOK_USER_KEY"),
    date_from = Sys.getenv("SIMPLYBOOK_DATE_FROM", unset = "2010-01-01"),
    date_to = Sys.getenv(
      "SIMPLYBOOK_DATE_TO",
      unset = format(seq(Sys.Date(), by = "5 years", length.out = 2)[2], "%Y-%m-%d")
    )
) {
  ontbrekend <- c(
    SIMPLYBOOK_COMPANY_LOGIN = company_login,
    SIMPLYBOOK_USER_LOGIN = user_login,
    SIMPLYBOOK_USER_KEY = user_key
  )
  ontbrekend <- names(ontbrekend)[!nzchar(ontbrekend)]
  if (length(ontbrekend) > 0) {
    stop("Ontbrekende SimplyBook-instellingen: ", paste(ontbrekend, collapse = ", "))
  }

  token <- simplybook_rpc(
    "https://user-api.simplybook.me/login",
    "getUserToken",
    list(company_login, user_login, user_key)
  )
  if (!is.character(token) || length(token) != 1 || !nzchar(token)) {
    stop("SimplyBook heeft geen geldig gebruikerstoken teruggegeven.")
  }

  records <- simplybook_rpc(
    "https://user-api.simplybook.me/admin",
    "getBookings",
    list(list(
      date_from = date_from,
      date_to = date_to,
      booking_type = "all",
      order = "date_start_asc"
    )),
    headers = list(
      `X-Company-Login` = company_login,
      `X-User-Token` = token
    )
  )

  # Sommige API-configuraties retourneren de records onder `data`.
  if (is.list(records) && !is.null(records$data)) records <- records$data
  if (!is.list(records) || length(records) == 0) {
    return(data.frame(
      klantnummer = character(), datum = as.Date(character()),
      dienst = character(), geannuleerd = character(), categorie = character(),
      stringsAsFactors = FALSE
    ))
  }

  resultaat <- do.call(rbind, lapply(records, function(record) {
    datum_tekst <- simplybook_waarde(
      record,
      c("date_start", "start_date", "date", "booking_date"),
      NA_character_
    )
    data.frame(
      # Het dashboard telt afspraken en heeft geen klantidentificatie nodig.
      klantnummer = "",
      datum = as.Date(substr(datum_tekst, 1, 10)),
      dienst = simplybook_dienst(record),
      geannuleerd = if (simplybook_is_geannuleerd(record)) "Ja" else "Nee",
      categorie = simplybook_waarde(record, c("category_name", "category"), "Onbekend"),
      stringsAsFactors = FALSE
    )
  }))

  resultaat <- resultaat[!is.na(resultaat$datum), , drop = FALSE]
  resultaat <- resultaat[order(resultaat$datum, decreasing = TRUE), , drop = FALSE]
  rownames(resultaat) <- NULL
  resultaat
}
