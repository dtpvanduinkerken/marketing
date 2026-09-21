library(httr2)

`%||%` <- function(x, y) if (is.null(x)) y else x

# Haalt verzonden campagnerapporten uit Mailchimp op en zet ze om naar
# het bestaande raw.newsletters-contract van het dashboard.
fetch_mailchimp_newsletters <- function(
    api_key = Sys.getenv("MAILCHIMP_API_KEY"),
    server = Sys.getenv("MAILCHIMP_SERVER")
) {
  if (!nzchar(api_key) || !nzchar(server)) {
    stop("MAILCHIMP_API_KEY en MAILCHIMP_SERVER moeten beide zijn ingesteld.")
  }
  if (!grepl("^[a-z]{2}[0-9]+$", server)) {
    stop("Ongeldige Mailchimp-serverprefix: ", server)
  }

  api_url <- paste0("https://", server, ".api.mailchimp.com/3.0/reports")
  page_size <- 1000L
  offset <- 0L
  alle_reports <- list()
  totaal <- Inf

  while (offset < totaal) {
    response <- request(api_url) |>
      req_auth_basic("dashboard", api_key) |>
      req_url_query(count = page_size, offset = offset) |>
      req_retry(max_tries = 3) |>
      req_timeout(60) |>
      req_perform()

    pagina <- resp_body_json(response, simplifyVector = FALSE)
    totaal <- as.integer(pagina$total_items)
    reports <- pagina$reports
    if (length(reports) == 0) break

    alle_reports <- c(alle_reports, reports)
    offset <- offset + length(reports)
  }

  if (length(alle_reports) == 0) {
    stop("Mailchimp heeft geen verzonden campagnerapporten teruggegeven.")
  }

  get_num <- function(x, naam) {
    waarde <- x[[naam]]
    if (is.null(waarde)) 0 else as.numeric(waarde)
  }
  get_text <- function(x, naam, standaard = "") {
    waarde <- x[[naam]]
    if (is.null(waarde) || !nzchar(waarde)) standaard else as.character(waarde)
  }

  resultaat <- do.call(rbind, lapply(alle_reports, function(report) {
    opens <- report$opens %||% list()
    clicks <- report$clicks %||% list()
    bounces <- report$bounces %||% list()

    data.frame(
      datum = as.Date(substr(get_text(report, "send_time"), 1, 10)),
      campagne = get_text(
        report,
        "campaign_title",
        get_text(report, "subject_line", "Naamloze campagne")
      ),
      verzonden = get_num(report, "emails_sent"),
      geopend = get_num(opens, "unique_opens"),
      clicks = get_num(clicks, "unique_subscriber_clicks"),
      bounces = get_num(bounces, "hard_bounces") + get_num(bounces, "soft_bounces"),
      unsubscribes = get_num(report, "unsubscribed"),
      stringsAsFactors = FALSE
    )
  }))

  resultaat <- resultaat[!is.na(resultaat$datum), , drop = FALSE]
  resultaat <- resultaat[order(resultaat$datum, decreasing = TRUE), , drop = FALSE]
  rownames(resultaat) <- NULL
  resultaat
}
