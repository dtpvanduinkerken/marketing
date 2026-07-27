library(shiny)
library(shinydashboard)
library(DBI)
library(duckdb)
library(plotly)
library(dplyr)
library(googleAnalyticsR)

# --------------------------------------------------
# CONSTANTEN
# --------------------------------------------------

KLEUR_PRIMAIR   <- "#8cbe26"
KLEUR_SECUNDAIR <- "#6ca61c"
KLEUR_TERTIAIR  <- "#4d7e12"
KLEUR_LICHT     <- "#c5e07a"

`%||%` <- function(a, b) if (is.null(a)) b else a

# Pad naar de database. shiny::runApp('/srv/shiny-server') zet de working
# directory automatisch naar de app-map, dus een relatief pad is hier
# voldoende en het meest robuust (geen afhankelijkheid van sys.frame()-
# trucs die breken zodra het script niet via source() gestart wordt).
conn <- dbConnect(duckdb::duckdb())
tryCatch({
  dbExecute(conn, "INSTALL icu")
  dbExecute(conn, "LOAD icu")
}, error = function(e) {
  warning("Could not install/load ICU extension: ", e$message)
})


# --------------------------------------------------
# GOOGLE ANALYTICS AUTHENTICATIE (SERVICE ACCOUNT)
# --------------------------------------------------
# Op een server (Render, Docker, etc.) is geen browser beschikbaar, dus
# interactieve ga_auth() werkt niet en doet de hele app crashen bij opstart.
#
# Aanpak: de service account JSON-key wordt als Secret File op een vast
# pad aangeleverd (rauw JSON-bestand, geen base64-encoding nodig omdat
# Secret Files op Render het volledige bestand al ongewijzigd opslaan).
# googleAnalyticsR leest dat bestand rechtstreeks in via ga_auth(json_file=).
#
# Belangrijk: het service account moet in de GA4-property zelf zijn
# toegevoegd als gebruiker (Admin > Property Access Management) met
# minimaal "Viewer"-rechten, anders krijg je een 403 op de ga_data()-calls.
#
# Verwacht: Secret File op /etc/secrets/ga_service_account.json met de
# inhoud van de service-account JSON-key uit Google Cloud Console.
# Fallback: environment variable GA_AUTH_JSON_PATH die naar een eigen pad
# wijst, voor het geval het Secret File-pad afwijkt.

website_data_beschikbaar <- FALSE
ga4_fout_melding <- NULL

# Primaire bron: Secret File op een vast pad.
ga_service_account_secret_pad <- "/etc/secrets/ga4-service-account.json"

message("Debug: verwacht bestand op: ", ga_service_account_secret_pad)
message("Debug: bestaat bestand? ", file.exists(ga_service_account_secret_pad))

if (file.exists(ga_service_account_secret_pad)) {
  message("Debug: bestandsgrootte = ",
          file.size(ga_service_account_secret_pad), " bytes")
}

# Fallback: environment variable met een (eventueel afwijkend) pad naar
# het service-account JSON-bestand.
ga_auth_json <- Sys.getenv("GA_AUTH_JSON_PATH", unset = "")

ga_service_account_pad <- ""

if (file.exists(ga_service_account_secret_pad)) {
  ga_service_account_pad <- ga_service_account_secret_pad
  message("Debug: service-account JSON gevonden via Secret File (", ga_service_account_secret_pad, ").")
} else if (nzchar(ga_auth_json) && file.exists(ga_auth_json)) {
  ga_service_account_pad <- ga_auth_json
  message("Debug: Secret File niet gevonden op ", ga_service_account_secret_pad,
          ", terugvallen op GA_AUTH_JSON_PATH (", ga_auth_json, ").")
} else {
  message("Debug: geen service-account JSON gevonden op Secret File-pad (",
          ga_service_account_secret_pad, ") en GA_AUTH_JSON_PATH is leeg of bestaat niet.")
}

# Debug: toont alleen pad en bestandsgrootte, nooit de inhoud van de key.
if (nzchar(ga_service_account_pad)) {
  message("Debug: service-account bestandsgrootte = ",
          file.size(ga_service_account_pad), " bytes.")
}

# Extra debug: toon ALLE environment variable NAMEN (niet de waarden) die
# 'GA' of 'GOOGLE' bevatten, om te zien hoe Render variabelen daadwerkelijk
# doorgeeft aan dit proces.
alle_env_namen <- names(Sys.getenv())
relevante_namen <- alle_env_namen[grepl("GA|GOOGLE", alle_env_namen, ignore.case = TRUE)]
message("Debug: gevonden environment variable NAMEN die mogelijk relevant zijn: ",
        if (length(relevante_namen) > 0) paste(relevante_namen, collapse = ", ") else "(geen gevonden)")
message("Debug: bestaat /etc/secrets? ", dir.exists("/etc/secrets"),
        " | inhoud: ", if (dir.exists("/etc/secrets")) paste(list.files("/etc/secrets"), collapse = ", ") else "n.v.t.")

if (nzchar(ga_service_account_pad)) {
  
  tryCatch({
    # Geen schijf-cache nodig/gewenst bij service accounts; voorkomt dat
    # gargle een cache-bestand probeert te schrijven op een read-only
    # filesystem (zoals het mountpoint van een Secret File).
    options(gargle_oauth_cache = FALSE)
    googleAnalyticsR::ga_auth(json_file = ga_service_account_pad)
    website_data_beschikbaar <- TRUE
    message("Google Analytics authenticatie gelukt via service account.")
  }, error = function(e) {
    ga4_fout_melding <<- paste(capture.output(str(e)), collapse = " ")
    message("VOLLEDIGE GA4 FOUT:")
    print(e)
  })
  
} else {
  ga4_fout_melding <- "Geen service-account JSON gevonden op de server (Secret File of GA_AUTH_JSON_PATH)."
  message("Geen service-account JSON gevonden. ",
          "Website-tab draait zonder live GA-data.")
}

# --------------------------------------------------
# DATABASE
# --------------------------------------------------

# DB_PAD: relatief pad naar het DuckDB-bestand in de app-map. Kan via de
# environment variable DB_PAD overschreven worden (bv. op een server met
# een ander deploy-pad), met "warehouse.duckdb" als standaardwaarde.
# DB_PAD: pad naar het DuckDB-bestand. Als dit al ergens anders is
# gedefinieerd (bv. in een global.R die Shiny automatisch vóór app.R
# inleest), laten we die waarde ongemoeid. Alleen als DB_PAD nog
# nergens bestaat, vallen we terug op de environment variable of een
# relatief standaardpad.
if (!exists("DB_PAD", inherits = FALSE) || is.null(DB_PAD) || !nzchar(DB_PAD)) {
  DB_PAD <- Sys.getenv("DB_PAD", unset = "bedrijf.duckdb")
}
if (!file.exists(DB_PAD)) {
  stop("Databasebestand niet gevonden op pad: '", DB_PAD,
       "'. Werkdirectory is: ", getwd(),
       ". Zet evt. de environment variable DB_PAD naar het juiste pad.")
}
message("Debug: DuckDB-bestand wordt geladen vanaf: ", normalizePath(DB_PAD))

con <- dbConnect(duckdb::duckdb(), DB_PAD)
onStop(function() dbDisconnect(con, shutdown = TRUE))

# Expliciet de icu-extensie laden (nodig voor sommige datum/locale-functies
# in de mart-views). Wordt al tijdens de Docker build geïnstalleerd onder
# dezelfde HOME als waarmee de app draait, zodat dit hier alleen het laden
# is, zonder netwerktoegang nodig te hebben.
tryCatch({
  dbExecute(con, "LOAD icu")
}, error = function(e) {
  message("Waarschuwing: kon DuckDB-extensie 'icu' niet laden: ", conditionMessage(e))
})

# --------------------------------------------------
# HELPER: OPMAAK
# --------------------------------------------------

format_euro <- function(x) {
  paste0("\u20ac ", format(round(x, 0), big.mark = ".", decimal.mark = ","))
}

format_number <- function(x) {
  format(round(x, 0), big.mark = ".", decimal.mark = ",")
}

format_percentage <- function(x) {
  paste0(format(round(x, 1), big.mark = ".", decimal.mark = ","), "%")
}

format_euro_precies <- function(x) {
  paste0("\u20ac ", format(round(x, 2), nsmall = 2, big.mark = ".", decimal.mark = ","))
}

# --------------------------------------------------
# HELPER: PLOT STIJL
# --------------------------------------------------

basis_layout <- function(p,
                         x_titel  = "",
                         y_titel  = "",
                         y2_titel = NULL,
                         legenda   = FALSE) {
  
  p <- p |> layout(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font = list(
      family = "Inter, sans-serif",
      color  = "#374151",
      size   = 12
    ),
    xaxis = list(
      title    = x_titel,
      showgrid = FALSE,
      zeroline = FALSE,
      showline = FALSE,
      tickfont = list(size = 12, color = "#6b7280"),
      tickangle = if (x_titel == "") -30 else 0
    ),
    yaxis = list(
      title     = y_titel,
      showgrid  = TRUE,
      gridcolor = "#f0f3f6",
      gridwidth = 1,
      zeroline  = FALSE,
      showline  = FALSE,
      tickfont  = list(size = 12, color = "#6b7280")
    ),
    margin = list(l = 8, r = 8, t = 12, b = 8),
    hoverlabel = list(
      bgcolor     = "#111827",
      bordercolor = "#111827",
      font        = list(family = "Inter, sans-serif",
                         color  = "#ffffff", size = 13)
    )
  )
  
  if (!is.null(y2_titel)) {
    p <- p |> layout(
      yaxis2 = list(
        title      = y2_titel,
        overlaying = "y",
        side       = "right",
        showgrid   = FALSE,
        zeroline   = FALSE,
        showline   = FALSE,
        tickfont   = list(size = 12, color = "#6b7280")
      )
    )
  }
  
  if (legenda) {
    p <- p |> layout(
      legend = list(
        orientation = "h",
        y           = -0.2,
        font        = list(size = 12, color = "#374151"),
        bgcolor     = "rgba(0,0,0,0)"
      )
    )
  }
  
  p
}

maak_bar_plot <- function(data, x_col, y_col, y_label,
                          kleuren = KLEUR_PRIMAIR) {
  plot_ly(
    data   = data,
    x      = as.formula(paste0("~", x_col)),
    y      = as.formula(paste0("~", y_col)),
    type   = "bar",
    marker = list(
      color   = kleuren,
      opacity = 0.90,
      line    = list(color = "rgba(0,0,0,0)", width = 0)
    ),
    hovertemplate = "<b>%{x}</b><br>%{y:,.0f}<extra></extra>"
  ) |>
    basis_layout(y_titel = y_label) |>
    layout(bargap = 0.38)
}

maak_lijn_plot <- function(data, x_col, y_col, y_label,
                           lijn_kleur = KLEUR_PRIMAIR,
                           fill = TRUE) {
  p <- plot_ly(
    data          = data,
    x             = as.formula(paste0("~", x_col)),
    y             = as.formula(paste0("~", y_col)),
    type          = "scatter",
    mode          = "lines+markers",
    line          = list(color = lijn_kleur, width = 2.5, shape = "spline"),
    marker        = list(
      color = "#ffffff",
      size  = 7,
      line  = list(color = lijn_kleur, width = 2)
    ),
    hovertemplate = "<b>%{x}</b><br>%{y:,.0f}<extra></extra>"
  )
  if (fill) {
    p <- p |> add_trace(
      x             = as.formula(paste0("~", x_col)),
      y             = as.formula(paste0("~", y_col)),
      type          = "scatter",
      mode          = "none",
      fill          = "tozeroy",
      fillcolor     = "rgba(140,190,38,0.08)",
      showlegend    = FALSE,
      hoverinfo     = "skip"
    )
  }
  p |> basis_layout(y_titel = y_label)
}

maak_donut_plot <- function(data, label_col, value_col,
                            kleuren = NULL) {
  args <- list(
    data   = data,
    labels = as.formula(paste0("~", label_col)),
    values = as.formula(paste0("~", value_col)),
    type   = "pie",
    hole   = 0.55,
    textinfo      = "label+percent",
    textfont      = list(size = 13, family = "Inter, sans-serif"),
    hovertemplate = "<b>%{label}</b><br>%{value:,.0f} (%{percent})<extra></extra>"
  )
  if (!is.null(kleuren)) args$marker <- list(colors = kleuren)
  p <- do.call(plot_ly, args)
  p |> basis_layout() |> layout(showlegend = TRUE,
                                legend = list(font = list(size = 12), bgcolor = "rgba(0,0,0,0)"))
}

# --------------------------------------------------
# HELPER: KPI CARD
# --------------------------------------------------

kpi_card <- function(titel, waarde, trend_class = NULL,
                     trend_label = NULL, subtitel = NULL) {
  div(
    class = "kpi-card",
    div(
      class = "kpi-header",
      div(class = "kpi-title", titel),
      if (!is.null(trend_class)) div(class = trend_class, trend_label)
    ),
    div(class = "kpi-value", waarde),
    if (!is.null(subtitel)) div(class = "kpi-subtitel", subtitel)
  )
}

# --------------------------------------------------
# LEGE FALLBACK DATAFRAMES VOOR GA-DATA
# --------------------------------------------------
# Als GA-authenticatie niet beschikbaar is, geven we lege/placeholder
# dataframes terug met de juiste kolomnamen, zodat de UI niet crasht.

leeg_website_kpis <- data.frame(
  activeUsers = 0, sessions = 0, screenPageViews = 0, engagementRate = 0
)
leeg_website_dagelijks <- data.frame(
  date = as.character(Sys.Date()), activeUsers = 0, sessions = 0
)
leeg_website_paginas <- data.frame(pagePath = character(0), screenPageViews = numeric(0))
leeg_website_bronnen <- data.frame(sessionSource = character(0), sessions = numeric(0))
leeg_website_devices <- data.frame(deviceCategory = c("desktop", "mobile", "tablet"),
                                   activeUsers = c(0, 0, 0))
leeg_website_search_terms <- data.frame(searchTerm = character(0), eventCount = numeric(0))
leeg_website_funnel <- data.frame(
  eventName = c("page_view", "view_item", "add_to_cart", "begin_checkout", "purchase"),
  eventCount = c(0, 0, 0, 0, 0)
)

veilige_ga_data <- function(expr) {
  # Voert een ga_data(...) aanroep uit; geeft fallback terug bij fouten
  # of als er geen GA-auth beschikbaar is.
  if (!website_data_beschikbaar) return(NULL)
  tryCatch(expr, error = function(e) {
    ga4_fout_melding <<- conditionMessage(e)
    message("GA-call mislukt: ", conditionMessage(e))
    NULL
  })
}

# --------------------------------------------------
# DATA LADEN
# --------------------------------------------------

load_data <- function() {
  
  pricing <- dbGetQuery(con, "SELECT * FROM mart.kpi_personal_pricing")
  klanten <- dbGetQuery(con, "SELECT * FROM mart.klant_kpis")
  marketing <- dbGetQuery(con, "SELECT * FROM mart.newsletter_kpis")
  afspraken <- dbGetQuery(con, "SELECT * FROM mart.afspraken_kpis")
  woonplaats <- dbGetQuery(con,
                           "SELECT * FROM mart.omzet_per_woonplaats ORDER BY omzet DESC LIMIT 10")
  omzet_per_maand <- dbGetQuery(con,
                                "SELECT * FROM mart.omzet_per_maand ORDER BY maand")
  pricing_performance <- dbGetQuery(con,
                                    "SELECT * FROM mart.pricing_performance ORDER BY omzet DESC")
  members_kpis <- dbGetQuery(con, "SELECT * FROM mart.members_kpis")
  klantgedrag <- dbGetQuery(con, "SELECT * FROM mart.klantgedrag")
  woonplaats_members <- dbGetQuery(con,
                                   "SELECT woonplaats, klanten, omzet, omzet_per_klant
     FROM mart.omzet_per_woonplaats ORDER BY omzet DESC LIMIT 10")
  newsletter_campagnes <- dbGetQuery(con,
                                     "SELECT * FROM staging.newsletters ORDER BY datum DESC")
  afspraken_per_dienst <- dbGetQuery(con,
                                     "SELECT dienst, COUNT(*) AS totaal FROM raw.afspraken
     GROUP BY dienst ORDER BY totaal DESC")
  afspraken_kpis_detail <- dbGetQuery(con, "
SELECT
  COUNT(*) AS totaal_afspraken,
  COUNT(DISTINCT dienst) AS aantal_diensten
FROM raw.afspraken
")
  members_groei <- dbGetQuery(con,
                              "SELECT * FROM mart.members_groei ORDER BY maand")
  afspraken_over_tijd <- dbGetQuery(con,
                                    "SELECT DATE_TRUNC('month', datum) AS maand, COUNT(*) AS totaal
     FROM raw.afspraken GROUP BY DATE_TRUNC('month', datum) ORDER BY maand")
  social_media_kpis <- dbGetQuery(con, "SELECT * FROM mart.social_media_kpis")
  social_media_platform <- dbGetQuery(con, "SELECT * FROM mart.social_media_platform")
  social_media_volgergroei <- dbGetQuery(con,
                                         "SELECT * FROM mart.social_media_volgergroei")
  social_media_volgers <- dbGetQuery(con,
                                     "SELECT * FROM mart.social_media_volgers ORDER BY datum")
  post_type_performance <- dbGetQuery(con,
                                      "SELECT * FROM mart.post_type_performance ORDER BY views DESC")
  post_performance <- dbGetQuery(con,
                                 "SELECT * FROM mart.post_performance ORDER BY datum DESC")
  coupon_kpis <- dbGetQuery(con, "SELECT * FROM mart.coupon_kpis")
  coupon_performance <- dbGetQuery(con,
                                   "SELECT * FROM mart.coupon_performance ORDER BY omzet DESC")
  coupon_maand <- dbGetQuery(con, "SELECT * FROM mart.coupon_maand ORDER BY maand")
  coupon_detail <- dbGetQuery(con, "SELECT * FROM raw.coupons")

  meta_bestand <- file.path("data", "raw", "meta_advertenties.csv")
  if (!file.exists(meta_bestand)) {
    stop("Meta-export niet gevonden op pad: '", meta_bestand, "'.")
  }
  meta_advertenties <- read.csv(
    meta_bestand,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    encoding = "UTF-8"
  )
  namen_meta <- c(
    "Campagnenaam", "Naam advertentieset", "Weergavestatus", "Bereik",
    "Weergaven", "Frequentie", "Resultaattype", "Resultaten",
    "Besteed bedrag (EUR)", "Kosten per resultaat", "Begint op",
    "Eindigt op", "Start rapportage", "Einde rapportage"
  )
  ontbrekend_meta <- setdiff(namen_meta, names(meta_advertenties))
  if (length(ontbrekend_meta) > 0) {
    stop("Meta-export mist kolommen: ", paste(ontbrekend_meta, collapse = ", "))
  }
  meta_advertenties <- meta_advertenties |>
    mutate(
      Bereik = as.numeric(Bereik),
      Weergaven = as.numeric(Weergaven),
      Frequentie = as.numeric(Frequentie),
      Resultaten = as.numeric(Resultaten),
      `Besteed bedrag (EUR)` = as.numeric(`Besteed bedrag (EUR)`),
      `Kosten per resultaat` = as.numeric(`Kosten per resultaat`),
      `Begint op` = as.Date(`Begint op`),
      `Eindigt op` = as.Date(`Eindigt op`),
      `Start rapportage` = as.Date(`Start rapportage`),
      `Einde rapportage` = as.Date(`Einde rapportage`)
    )

  meta_numerieke_kolommen <- c(
    "Aankopen", "Kosten per aankoop", "Conversiewaarde aankopen",
    "Conversiewaarde directe aankopen op website", "Aankopen op website",
    "Directe aankopen op website", "Conversiewaarde door winkels ondersteunde aankopen",
    "Door winkels ondersteunde aankopen",
    "ROAS (rendement op advertentie-uitgaven) voor aankoop",
    "In-app-aankopen", "Offline aankopen", "Aankopen op Meta",
    "Conversiewaarde offline aankopen", "Conversiewaarde aankopen in mobiele app",
    "Conversiewaarde aankopen op Meta", "ROAS voor aankoop voor website",
    "ROAS voor aankoop in de app"
  )
  for (kolom in intersect(meta_numerieke_kolommen, names(meta_advertenties))) {
    meta_advertenties[[kolom]] <- suppressWarnings(as.numeric(meta_advertenties[[kolom]]))
  }
  
  members_nieuw_7d <- dbGetQuery(con, "
    SELECT
      SUM(CASE WHEN aanmelddatum > CURRENT_DATE - INTERVAL 7 DAY
                AND aanmelddatum <= CURRENT_DATE THEN 1 ELSE 0 END) AS afgelopen_7d,
      SUM(CASE WHEN aanmelddatum > CURRENT_DATE - INTERVAL 14 DAY
                AND aanmelddatum <= CURRENT_DATE - INTERVAL 7 DAY THEN 1 ELSE 0 END) AS vorige_7d
    FROM raw.members
  ")
  
  members_actief_slapend <- dbGetQuery(con, "
    SELECT
      CASE WHEN laatste_aankoop IS NULL THEN 'Slapend'
           WHEN laatste_aankoop < CURRENT_DATE - INTERVAL 90 DAY THEN 'Slapend'
           ELSE 'Actief'
      END AS status,
      COUNT(*) AS aantal
    FROM raw.members
    GROUP BY status
  ")
  
  # --------------------------------------------------
  # GOOGLE ANALYTICS DATA (met veilige fallback)
  # --------------------------------------------------
  
  website_kpis <- veilige_ga_data(ga_data(
    propertyId = 314034198,
    date_range = c("30daysAgo", "today"),
    metrics    = c("activeUsers", "sessions", "screenPageViews", "engagementRate")
  )) %||% leeg_website_kpis
  
  website_dagelijks <- veilige_ga_data(ga_data(
    propertyId = 314034198, date_range = c("30daysAgo", "today"),
    dimensions = "date", metrics = c("activeUsers", "sessions")
  )) %||% leeg_website_dagelijks
  
  website_paginas <- veilige_ga_data(ga_data(
    propertyId = 314034198, date_range = c("30daysAgo", "today"),
    dimensions = "pagePath", metrics = c("screenPageViews"), limit = 20
  )) %||% leeg_website_paginas
  
  website_bronnen <- veilige_ga_data(ga_data(
    propertyId = 314034198, date_range = c("30daysAgo", "today"),
    dimensions = "sessionSource", metrics = c("sessions"), limit = 20
  )) %||% leeg_website_bronnen
  
  website_devices <- veilige_ga_data(ga_data(
    propertyId = 314034198, date_range = c("30daysAgo", "today"),
    dimensions = "deviceCategory", metrics = c("activeUsers")
  )) %||% leeg_website_devices
  
  website_search_terms <- veilige_ga_data(ga_data(
    propertyId = 314034198,
    date_range = c("30daysAgo", "today"),
    dimensions = "searchTerm",
    metrics = c("eventCount"),
    limit = 10
  )) %||% leeg_website_search_terms
  
  website_checkout_funnel_raw <- veilige_ga_data(ga_data(
    propertyId = 314034198, date_range = c("30daysAgo", "today"),
    dimensions = "eventName", metrics = c("eventCount")
  ))
  website_checkout_funnel <- if (!is.null(website_checkout_funnel_raw)) {
    website_checkout_funnel_raw |>
      dplyr::filter(eventName %in% c(
        "page_view", "view_item", "add_to_cart", "begin_checkout", "purchase"
      ))
  } else {
    leeg_website_funnel
  }
  
  list(
    pricing = pricing, klanten = klanten, marketing = marketing,
    afspraken = afspraken, woonplaats = woonplaats,
    omzet_per_maand = omzet_per_maand,
    pricing_performance = pricing_performance,
    members_kpis = members_kpis, klantgedrag = klantgedrag,
    woonplaats_members = woonplaats_members,
    newsletter_campagnes = newsletter_campagnes,
    afspraken_per_dienst = afspraken_per_dienst,
    members_groei = members_groei, afspraken_over_tijd = afspraken_over_tijd,
    coupon_kpis = coupon_kpis, coupon_performance = coupon_performance,
    coupon_maand = coupon_maand, coupon_detail = coupon_detail,
    social_media_kpis = social_media_kpis,
    social_media_platform = social_media_platform,
    social_media_volgergroei = social_media_volgergroei,
    social_media_volgers = social_media_volgers,
    post_type_performance = post_type_performance,
    post_performance = post_performance,
    website_kpis = website_kpis, website_dagelijks = website_dagelijks,
    website_paginas = website_paginas, website_bronnen = website_bronnen,
    website_devices = website_devices,
    website_checkout_funnel = website_checkout_funnel,
    website_search_terms = website_search_terms,
    afspraken_kpis_detail = afspraken_kpis_detail,
    members_nieuw_7d = members_nieuw_7d,
    members_actief_slapend = members_actief_slapend,
    meta_advertenties = meta_advertenties
  )
}

data <- load_data()

# --------------------------------------------------
# TREND BEREKENING
# --------------------------------------------------

bereken_trend <- function(omzet_per_maand) {
  laatste_omzet <- tail(omzet_per_maand$omzet, 1)
  vorige_omzet  <- tail(omzet_per_maand$omzet, 2)[1]
  trend <- round((laatste_omzet - vorige_omzet) / vorige_omzet * 100, 1)
  list(
    waarde = trend,
    class  = ifelse(trend >= 0, "kpi-trend-up", "kpi-trend-down"),
    label  = paste0(ifelse(trend >= 0, "+", ""), trend, "%")
  )
}

omzet_trend <- bereken_trend(data$omzet_per_maand)

bereken_members_7d_trend <- function(members_nieuw_7d) {
  afgelopen <- members_nieuw_7d$afgelopen_7d[1]
  vorige    <- members_nieuw_7d$vorige_7d[1]
  if (is.na(afgelopen)) afgelopen <- 0
  if (is.na(vorige)) vorige <- 0
  
  if (vorige == 0) {
    trend <- if (afgelopen > 0) 100 else 0
  } else {
    trend <- round((afgelopen - vorige) / vorige * 100, 1)
  }
  
  list(
    waarde = afgelopen,
    class  = ifelse(trend >= 0, "kpi-trend-up", "kpi-trend-down"),
    label  = paste0(ifelse(trend >= 0, "+", ""), trend, "%")
  )
}

members_7d_trend <- bereken_members_7d_trend(data$members_nieuw_7d)

# --------------------------------------------------
# HOME PAGINA: INZICHTEN & ACTIEPUNTEN
# --------------------------------------------------
# Genereert automatisch een korte samenvatting van de belangrijkste
# signalen uit de data voor het MT, elk gekoppeld aan een concreet en
# relevant actiepunt. Dit zijn GEEN vaste, standaard teksten: alles
# wordt berekend op basis van de actuele cijfers in de database, dus
# de inhoud verandert mee zodra de data verandert. Elke afzonderlijke
# check is met tryCatch afgeschermd, zodat een ontbrekende of
# onverwachte waarde in één datamart nooit de hele homepage laat
# crashen; die ene inzicht wordt dan gewoon overgeslagen.

voeg_inzicht_toe <- function(lijst, type, categorie, titel, tekst, actie) {
  # type: "kritiek" | "waarschuwing" | "positief"
  append(lijst, list(list(type = type, categorie = categorie, titel = titel, tekst = tekst, actie = actie)))
}

# Nette weergave voor kleine percentages: format_percentage() rondt af op
# 1 decimaal, waardoor een waarde als 0,03% als "0%" getoond zou worden en
# dat oogt als een fout. Bij een waarde die niet nul is maar wel afrondt
# naar 0,0 tonen we daarom "<0,1%" in plaats van een kale "0%".
format_percentage_precies <- function(x) {
  if (is.na(x)) return("n.v.t.")
  if (x > 0 && round(x, 1) == 0) return("<0,1%")
  format_percentage(x)
}

genereer_inzichten <- function(data, omzet_trend, members_7d_trend,
                               website_data_beschikbaar) {
  
  inzichten <- list()
  
  # 1. OMZETONTWIKKELING -------------------------------------------------
  tryCatch({
    trend <- omzet_trend$waarde
    if (!is.na(trend)) {
      if (trend <= -5) {
        inzichten <- voeg_inzicht_toe(inzichten, "kritiek", "Omzet",
                                      "Omzet daalt significant",
                                      paste0("De omzet is de afgelopen maand met ", format_percentage(abs(trend)),
                                             " gedaald ten opzichte van de maand ervoor."),
                                      "Analyseer per pricing-code en woonplaats welk segment de daling veroorzaakt en bespreek dit in het eerstvolgende MT-overleg.")
      } else if (trend < 0) {
        inzichten <- voeg_inzicht_toe(inzichten, "waarschuwing", "Omzet",
                                      "Omzet licht gedaald",
                                      paste0("De omzet daalde met ", format_percentage(abs(trend)), " ten opzichte van de vorige maand."),
                                      "Houd de ontwikkeling de komende weken in de gaten en vergelijk met hetzelfde seizoen vorig jaar voordat je ingrijpt.")
      } else if (trend >= 10) {
        inzichten <- voeg_inzicht_toe(inzichten, "positief", "Omzet",
                                      "Sterke omzetgroei",
                                      paste0("De omzet steeg met ", format_percentage(trend), " ten opzichte van de vorige maand."),
                                      "Breng in kaart welke actie (campagne, coupon, seizoen) deze groei verklaart, zodat dit herhaald kan worden.")
      }
    }
  }, error = function(e) NULL)
  
  # 2. NIEUWE MEMBERS (7 DAGEN) ------------------------------------------
  tryCatch({
    afgelopen <- data$members_nieuw_7d$afgelopen_7d[1]
    vorige    <- data$members_nieuw_7d$vorige_7d[1]
    if (!is.na(afgelopen) && !is.na(vorige) && vorige > 0) {
      trend <- round((afgelopen - vorige) / vorige * 100, 1)
      if (trend <= -25) {
        inzichten <- voeg_inzicht_toe(inzichten, "kritiek", "Members",
                                      "Aanmeldingen van nieuwe members vallen sterk terug",
                                      paste0("In de afgelopen 7 dagen meldden zich ", format_number(afgelopen),
                                             " nieuwe members aan, tegenover ", format_number(vorige),
                                             " in de week ervoor (", format_percentage(trend), ")."),
                                      "Controleer of wervingskanalen (social, website, verenigingen) nog goed lopen en overweeg een korte wervingsactie.")
      } else if (trend >= 25) {
        inzichten <- voeg_inzicht_toe(inzichten, "positief", "Members",
                                      "Piek in nieuwe aanmeldingen",
                                      paste0("Het aantal nieuwe members steeg deze week naar ", format_number(afgelopen),
                                             ", een stijging van ", format_percentage(trend), " t.o.v. vorige week."),
                                      "Onderzoek welke actie of kanaal hiervoor zorgde en zet hier bewust extra budget of aandacht op.")
      }
    }
  }, error = function(e) NULL)
  
  # 3. KLANTRETENTIE -------------------------------------------------------
  tryCatch({
    terugkerend <- data$klantgedrag$terugkerende_klanten[1]
    uniek       <- data$klantgedrag$unieke_klanten[1]
    if (!is.na(terugkerend) && !is.na(uniek) && uniek > 0) {
      retentie_pct <- round(terugkerend / uniek * 100, 1)
      if (retentie_pct < 25) {
        inzichten <- voeg_inzicht_toe(inzichten, "waarschuwing", "Klanten",
                                      "Laag aandeel terugkerende klanten",
                                      paste0("Slechts ", format_percentage(retentie_pct), " van de klanten (",
                                             format_number(terugkerend), " van de ", format_number(uniek), ") komt terug voor een herhaalaankoop."),
                                      "Zet een opvolgcampagne of loyaliteitsactie op voor eenmalige klanten, bijvoorbeeld via een gerichte nieuwsbrief of coupon.")
      } else if (retentie_pct >= 50) {
        inzichten <- voeg_inzicht_toe(inzichten, "positief", "Klanten",
                                      "Sterke klantloyaliteit",
                                      paste0(format_percentage(retentie_pct), " van de klanten keert terug voor een herhaalaankoop."),
                                      "Onderzoek wat deze groep drijft (dienst, coupon, moment) en gebruik dit als basis voor de werving van nieuwe klanten.")
      }
    }
  }, error = function(e) NULL)
  
  # 4. MEMBERS: ACTIEF VS SLAPEND -----------------------------------------
  # Let op: dit gaat over het totale ledenbestand in raw.members (alle
  # ooit aangemelde members), niet over de "unieke klanten" uit de
  # klantenkaart bovenaan de pagina — dat is een kleinere, andere groep
  # (mensen met minimaal 1 transactie). Vandaar dat de aantallen sterk
  # kunnen verschillen; dit is geen rekenfout.
  tryCatch({
    df <- data$members_actief_slapend
    slapend <- sum(df$aantal[df$status == "Slapend"], na.rm = TRUE)
    totaal  <- sum(df$aantal, na.rm = TRUE)
    if (totaal > 0) {
      slapend_pct <- round(slapend / totaal * 100, 1)
      if (slapend_pct >= 50) {
        inzichten <- voeg_inzicht_toe(inzichten, "kritiek", "Members",
                                      "Meer dan helft van de leden is slapend",
                                      paste0(format_percentage(slapend_pct), " van alle leden in het memberprogramma (",
                                             format_number(slapend), " van de ", format_number(totaal),
                                             ") heeft al 90+ dagen niets gekocht."),
                                      "Start een reactivatiecampagne met een gerichte coupon of nieuwsbrief specifiek voor slapende members.")
      } else if (slapend_pct >= 35) {
        inzichten <- voeg_inzicht_toe(inzichten, "waarschuwing", "Members",
                                      "Aandeel slapende leden loopt op",
                                      paste0(format_percentage(slapend_pct), " van de leden heeft al 90+ dagen niets gekocht."),
                                      "Overweeg een reactivatie-actie voordat dit aandeel verder oploopt.")
      }
    }
  }, error = function(e) NULL)
  
  # 5. COUPONS: VERZILVERING --------------------------------------------
  # Optelling over álle coupons en alle datums samen (raw.coupons bevat
  # per coupon meerdere datumregels), dezelfde rekenwijze als de
  # KPI-kaarten op de Coupons-pagina zelf.
  tryCatch({
    df <- data$coupon_detail
    verzonden  <- sum(df$verzonden, na.rm = TRUE)
    ingeleverd <- sum(df$ingeleverd, na.rm = TRUE)
    openstaand <- sum(df$openstaand, na.rm = TRUE)
    if (verzonden > 0) {
      inlever_pct <- round(ingeleverd / verzonden * 100, 1)
      open_pct    <- round(openstaand / verzonden * 100, 1)
      if (inlever_pct < 15) {
        inzichten <- voeg_inzicht_toe(inzichten, "waarschuwing", "Coupons",
                                      "Coupons worden nauwelijks verzilverd",
                                      paste0("Over alle coupons samen wordt slechts ", format_percentage_precies(inlever_pct),
                                             " van de verzonden coupons daadwerkelijk ingeleverd (", format_number(ingeleverd),
                                             " van de ", format_number(verzonden), "); ", format_percentage_precies(open_pct), " staat nog open."),
                                      "Herzie de voorwaarden (kortingshoogte, geldigheidsduur) of promoot lopende coupons opnieuw via de nieuwsbrief.")
      } else if (open_pct >= 40) {
        inzichten <- voeg_inzicht_toe(inzichten, "waarschuwing", "Coupons",
                                      "Veel coupons staan nog open",
                                      paste0(format_percentage(open_pct), " van de verzonden coupons is nog niet gebruikt."),
                                      "Stuur een herinnering naar klanten met een openstaande coupon, zeker als de vervaldatum nadert.")
      }
    }
  }, error = function(e) NULL)
  
  # 6. NIEUWSBRIEF PERFORMANCE ---------------------------------------------
  tryCatch({
    df <- data$newsletter_campagnes
    verzonden <- sum(df$sent, na.rm = TRUE)
    if (verzonden > 0 && nrow(df) > 0) {
      open_rate_gem <- mean(df$opens / df$sent * 100, na.rm = TRUE)
      unsub_rate_gem <- mean(df$unsubscribers / df$sent * 100, na.rm = TRUE)
      laatste <- df[which.max(as.Date(df$datum)), ]
      laatste_open_rate <- laatste$opens / laatste$sent * 100
      
      if (open_rate_gem < 15) {
        inzichten <- voeg_inzicht_toe(inzichten, "waarschuwing", "Nieuwsbrief",
                                      "Open rate nieuwsbrief onder benchmark",
                                      paste0("De gemiddelde open rate over alle campagnes is ", format_percentage(open_rate_gem),
                                             ", terwijl 20-25% gebruikelijk is in deze sector."),
                                      "Test andere onderwerpregels en verzendmomenten, en overweeg de lijst op te schonen van inactieve ontvangers.")
      }
      if (!is.na(laatste_open_rate) && laatste_open_rate < open_rate_gem * 0.7) {
        inzichten <- voeg_inzicht_toe(inzichten, "waarschuwing", "Nieuwsbrief",
                                      "Laatste nieuwsbrief presteerde ondermaats",
                                      paste0("De laatste campagne (", format(as.Date(laatste$datum), "%d-%m-%Y"),
                                             ") haalde een open rate van ", format_percentage(laatste_open_rate),
                                             ", ruim onder het gemiddelde van ", format_percentage(open_rate_gem), "."),
                                      "Bekijk onderwerpregel en verzendmoment van deze specifieke campagne en pas dit aan voor de volgende editie.")
      }
      if (unsub_rate_gem >= 1) {
        inzichten <- voeg_inzicht_toe(inzichten, "waarschuwing", "Nieuwsbrief",
                                      "Verhoogd aantal afmeldingen bij nieuwsbrief",
                                      paste0("Gemiddeld meldt ", format_percentage(unsub_rate_gem), " van de ontvangers zich per verzending af."),
                                      "Controleer de verzendfrequentie en relevantie van de inhoud; overweeg segmentatie op interesse.")
      }
    }
  }, error = function(e) NULL)
  
  # 7. SOCIAL MEDIA ---------------------------------------------------------
  tryCatch({
    eng_rate <- data$social_media_kpis$engagement_rate[1]
    if (!is.na(eng_rate)) {
      if (eng_rate < 1) {
        inzichten <- voeg_inzicht_toe(inzichten, "waarschuwing", "Social Media",
                                      "Lage engagement rate op social media",
                                      paste0("De gemiddelde engagement rate is ", format_percentage_precies(eng_rate), "."),
                                      "Vergelijk contentformats via de post type performance en zet vaker in op het best presterende type.")
      }
    }
    pt <- data$post_type_performance
    if (nrow(pt) >= 2) {
      beste <- pt[which.max(pt$gemiddelde_engagement), ]
      inzichten <- voeg_inzicht_toe(inzichten, "positief", "Social Media",
                                    paste0("Post type '", beste$post_type, "' presteert het best"),
                                    paste0("Dit type post scoort gemiddeld de hoogste engagement (",
                                           format_number(beste$gemiddelde_engagement), " per post)."),
                                    paste0("Plan de komende periode vaker content van het type '", beste$post_type, "' in.")
      )
    }
  }, error = function(e) NULL)
  
  # 8. AFSPRAKEN: CONCENTRATIE OP ÉÉN DIENST -------------------------------
  tryCatch({
    df <- data$afspraken_per_dienst
    totaal <- sum(df$totaal, na.rm = TRUE)
    if (totaal > 0 && nrow(df) > 1) {
      top <- df[which.max(df$totaal), ]
      aandeel <- round(top$totaal / totaal * 100, 1)
      if (aandeel >= 50) {
        inzichten <- voeg_inzicht_toe(inzichten, "waarschuwing", "Afspraken",
                                      "Sterke afhankelijkheid van één dienst",
                                      paste0("'", top$dienst, "' is goed voor ", format_percentage(aandeel),
                                             " van alle geboekte afspraken."),
                                      "Onderzoek of overige diensten meer promotie nodig hebben om de afhankelijkheid van deze ene dienst te verkleinen.")
      }
    }
  }, error = function(e) NULL)
  
  # 9. MEMBERDEALS / PRICING: ZWAKSTE CODE ---------------------------------
  tryCatch({
    df <- data$pricing_performance
    if (nrow(df) >= 2) {
      zwakste <- df[which.min(df$omzet), ]
      totaal_omzet <- sum(df$omzet, na.rm = TRUE)
      if (totaal_omzet > 0) {
        aandeel <- round(zwakste$omzet / totaal_omzet * 100, 1)
        if (aandeel < 3) {
          inzichten <- voeg_inzicht_toe(inzichten, "waarschuwing", "Memberdeals",
                                        paste0("Pricing-code '", zwakste$pricing_code, "' presteert nauwelijks"),
                                        paste0("Deze code is verantwoordelijk voor slechts ", format_percentage_precies(aandeel),
                                               " van de omzet uit memberdeals (", format_euro(zwakste$omzet), " op een totaal van ",
                                               format_euro(totaal_omzet), ")."),
                                        paste0("Evalueer of '", zwakste$pricing_code, "' nog relevant is, of dat de voorwaarden/zichtbaarheid aangepast moeten worden.")
          )
        }
      }
    }
  }, error = function(e) NULL)
  
  # 10. WEBSITE: GROOTSTE UITVAL IN CHECKOUT-FUNNEL ------------------------
  tryCatch({
    if (website_data_beschikbaar) {
      stappen <- c("page_view", "view_item", "add_to_cart", "begin_checkout", "purchase")
      labels  <- c("Paginabezoek", "Product bekeken", "Winkelwagen", "Checkout", "Aankoop")
      df <- data$website_checkout_funnel
      counts <- sapply(stappen, function(s) {
        w <- df$eventCount[df$eventName == s]
        if (length(w) == 0) NA else w[1]
      })
      if (all(!is.na(counts)) && counts[1] > 0) {
        uitval_pct <- round(100 - (counts[-1] / counts[-length(counts)] * 100), 1)
        i <- which.max(uitval_pct)
        if (!is.na(uitval_pct[i]) && uitval_pct[i] >= 60) {
          inzichten <- voeg_inzicht_toe(inzichten, "waarschuwing", "Website",
                                        "Grootste uitval in de checkout-funnel",
                                        paste0("Van '", labels[i], "' naar '", labels[i + 1], "' haakt ",
                                               format_percentage(uitval_pct[i]), " van de bezoekers af."),
                                        paste0("Bekijk deze stap in de funnel op de Website-pagina en onderzoek drempels zoals laadtijd, verzendkosten of het aantal stappen."))
        }
      }
    }
  }, error = function(e) NULL)
  
  # Sorteer op belang: kritiek eerst, dan waarschuwing, dan positief.
  # Beperk tot maximaal 5 signalen zodat de homepage een korte
  # samenvatting blijft in plaats van een volledige data-dump.
  if (length(inzichten) > 0) {
    volgorde <- sapply(inzichten, function(x) switch(x$type, kritiek = 1, waarschuwing = 2, positief = 3, 4))
    inzichten <- inzichten[order(volgorde)]
    if (length(inzichten) > 5) inzichten <- inzichten[1:5]
  } else {
    inzichten <- list(list(
      type      = "positief",
      categorie = "Algemeen",
      titel     = "Geen bijzonderheden",
      tekst     = "Op basis van de huidige data wijkt niets significant af van de verwachte bandbreedtes.",
      actie     = "Geen directe actie nodig; blijf de KPI's hieronder volgen."
    ))
  }
  
  inzichten
}

inzichten_home <- genereer_inzichten(data, omzet_trend, members_7d_trend,
                                     website_data_beschikbaar)

# --------------------------------------------------
# HOME PAGINA: WEERGAVE VAN EEN INZICHT-KAART
# --------------------------------------------------

inzicht_stijl <- list(
  kritiek      = list(bg = "#fdecea", rand = "#f5c2c0", tekst = "#7a1f1f", icoon = "triangle-exclamation", label = "Kritiek"),
  waarschuwing = list(bg = "#fff7e6", rand = "#f5cf87", tekst = "#8a5a00", icoon = "circle-exclamation", label = "Aandachtspunt"),
  positief     = list(bg = "#eaf5d8", rand = "#c5e07a", tekst = "#3d5c0f", icoon = "circle-check", label = "Positief")
)

# Compacte kaart: categorie-pill + status-pill boven de titel, actiepunt
# visueel losgekoppeld van de constatering zodat het onderscheid tussen
# "wat zien we" en "wat doen we ermee" in één oogopslag duidelijk is.
inzicht_kaart_ui <- function(inzicht) {
  stijl <- inzicht_stijl[[inzicht$type]]
  div(
    class = paste0("home-insight home-insight--", inzicht$type),
    div(
      class = "home-insight__icon",
      icon(stijl$icoon)
    ),
    div(
      class = "home-insight__content",
      div(class = "home-insight__meta", inzicht$categorie, " · ", stijl$label),
      div(class = "home-insight__title", inzicht$titel),
      div(class = "home-insight__text", inzicht$tekst),
      tags$details(
        class = "home-insight__details",
        tags$summary("Bekijk actie"),
        div(class = "home-insight__action", inzicht$actie)
      )
    )
  )
}

# Samenvattingsbalk boven de kaarten: in één oogopslag zien hoeveel
# kritieke, aandachts- en positieve signalen er zijn, zonder alles te
# hoeven lezen.
inzicht_samenvatting_ui <- function(inzichten) {
  types <- vapply(inzichten, function(x) x$type, character(1))
  telling <- table(factor(types, levels = c("kritiek", "waarschuwing", "positief")))
  
  pil <- function(type, label) {
    stijl <- inzicht_stijl[[type]]
    n <- unname(telling[type])
    div(
      style = paste0("display:inline-flex; align-items:center; gap:6px; background:", stijl$bg,
                     "; color:", stijl$tekst, "; border-radius:20px; padding:5px 12px; font-size:12.5px; font-weight:600;"),
      icon(stijl$icoon, style = "font-size:11px;"),
      paste0(n, " ", label)
    )
  }
  
  div(
    style = "display:flex; gap:8px; flex-wrap:wrap; margin-bottom:14px;",
    pil("kritiek", if (unname(telling["kritiek"]) == 1) "kritiek punt" else "kritieke punten"),
    pil("waarschuwing", if (unname(telling["waarschuwing"]) == 1) "aandachtspunt" else "aandachtspunten"),
    pil("positief", if (unname(telling["positief"]) == 1) "positief signaal" else "positieve signalen")
  )
}

# --------------------------------------------------
# UI
# --------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "Data Platform"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home",          tabName = "home",          icon = icon("house")),
      menuItem("Memberdeals",   tabName = "memberdeals",   icon = icon("tags")),
      menuItem("Coupons",       tabName = "coupons",       icon = icon("ticket")),
      menuItem("Members",       tabName = "members",       icon = icon("users")),
      menuItem("Nieuwsbrieven", tabName = "nieuwsbrieven", icon = icon("envelope")),
      menuItem("Social Media",  tabName = "social_media",  icon = icon("hashtag")),
      menuItem("Advertenties",  tabName = "advertenties",  icon = icon("bullhorn")),
      menuItem("Verenigingen",  tabName = "verenigingen",  icon = icon("people-group")),
      menuItem("Website",       tabName = "website",       icon = icon("globe")),
      menuItem("Afspraken",     tabName = "afspraken",     icon = icon("calendar")),
      menuItem("Cohortanalyse", tabName = "cohortanalyse", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    tabItems(
      
      # HOME
      tabItem(tabName = "home",
              div(
                class = "home-heading",
                div(
                  h2("Managementoverzicht"),
                  p("De belangrijkste prestaties, signalen en acties op één plek.")
                ),
                div(class = "home-heading__date",
                    icon("calendar-day"),
                    format(Sys.Date(), "%d %B %Y"))
              ),
              fluidRow(
                class = "home-kpis",
                column(3, kpi_card("Omzet",
                                   format_euro(data$pricing$totale_omzet),
                                   omzet_trend$class, omzet_trend$label, "t.o.v. vorige maand")),
                column(3, kpi_card("Klanten",
                                   format_number(data$klanten$unieke_klanten), subtitel = "unieke klanten")),
                column(3, kpi_card("Open Rate",
                                   paste0(round(data$marketing$open_rate, 1), "%"),
                                   subtitel = "nieuwsbrief gemiddeld")),
                column(3, kpi_card("Afspraken",
                                   format_number(data$afspraken$totaal_afspraken), subtitel = "totaal geboekt"))
              ),
              fluidRow(
                box(width = 7, title = "Prioriteiten voor het MT", class = "home-priorities",
                    inzicht_samenvatting_ui(inzichten_home),
                    div(class = "home-insights",
                      do.call(tagList, lapply(inzichten_home, inzicht_kaart_ui))
                    )
                ),
                box(width = 5, title = "Omzetontwikkeling", class = "home-trend",
                    div(class = "home-box-subtitle", "Maandelijkse omzet en richting van de laatste periode"),
                    plotlyOutput("omzet_per_maand", height = "360px"))
              ),
              fluidRow(
                box(width = 12, title = "Waar komt de omzet vandaan?", class = "home-locations",
                    div(class = "home-box-subtitle", "Top 10 woonplaatsen op totale omzet"),
                    plotlyOutput("omzet_woonplaats", height = "360px"))
              )
      ),
      
      # MEMBERDEALS
      tabItem(tabName = "memberdeals",
              h2("Memberdeals"),
              fluidRow(
                column(4, kpi_card("Totale omzet",
                                   format_euro(sum(data$pricing_performance$omzet)))),
                column(4, kpi_card("Aantal uses",
                                   format_number(sum(data$pricing_performance$aantal_gebruikt)))),
                column(4, kpi_card("Totale korting",
                                   format_euro(abs(sum(data$pricing_performance$discount)))))
              ),
              br(),
              fluidRow(box(width = 12, title = "Omzet per pricing code",
                           plotlyOutput("pricing_omzet", height = "380px"))),
              fluidRow(box(width = 12, title = "Performance overzicht",
                           tableOutput("pricing_tabel")))
      ),
      
      # COUPONS
      tabItem(tabName = "coupons",
              h2("Coupons"),
              selectInput("coupon_select", "Selecteer coupon",
                          choices = sort(unique(data$coupon_detail$coupon_code))),
              br(),
              fluidRow(
                column(3, kpi_card("Omzet",      textOutput("coupon_omzet"))),
                column(3, kpi_card("Ingeleverd", textOutput("coupon_ingeleverd"))),
                column(3, kpi_card("Verzonden",  textOutput("coupon_verzonden"))),
                column(3, kpi_card("Openstaand", textOutput("coupon_openstaand")))
              ),
              br(),
              fluidRow(
                box(width = 6, title = "Gebruik per dag",
                    plotlyOutput("coupon_gebruik_plot", height = "340px")),
                box(width = 6, title = "Omzet per dag",
                    plotlyOutput("coupon_omzet_plot", height = "340px"))
              ),
              fluidRow(
                box(width = 12, title = "Coupon Performance Analyse",
                    fluidRow(
                      column(3, kpi_card("Inleverpercentage",
                                         textOutput("coupon_inleverpercentage"),
                                         subtitel = "Ingeleverd / Verzonden")),
                      column(3, kpi_card("Omzet per coupon",
                                         textOutput("coupon_omzet_per_coupon"),
                                         subtitel = "Omzet / Ingeleverd")),
                      column(3, kpi_card("Gem. korting",
                                         textOutput("coupon_gem_korting"),
                                         subtitel = "Discount / Ingeleverd")),
                      column(3, kpi_card("Openstaand %",
                                         textOutput("coupon_openstaand_pct"),
                                         subtitel = "Openstaand / Verzonden"))
                    ))
              )
      ),
      
      # MEMBERS
      tabItem(tabName = "members",
              fluidRow(
                column(3, kpi_card("Totaal Members",
                                   format_number(data$members_kpis$totaal_members))),
                column(3, kpi_card("Actieve members",
                                   format_number(data$members_kpis$actieve_members_90d),
                                   subtitel = "afgelopen 90 dagen")),
                column(3, kpi_card("Nieuwe members",
                                   format_number(members_7d_trend$waarde),
                                   members_7d_trend$class, members_7d_trend$label,
                                   "afgelopen 7 dagen")),
                column(3, kpi_card("Terugkerende klanten",
                                   format_number(data$klantgedrag$terugkerende_klanten)))
              ),
              br(),
              fluidRow(
                box(width = 8, title = "Aanmeldingen per maand",
                    plotlyOutput("members_aanmeldingen_plot", height = "380px")),
                box(width = 4, title = "Top 10 woonplaatsen",
                    plotlyOutput("members_woonplaats", height = "380px"))
              ),
              fluidRow(
                box(width = 12, title = "Actieve vs slapende members",
                    plotlyOutput("members_actief_slapend_plot", height = "340px"))
              )
      ),
      
      # NIEUWSBRIEVEN
      tabItem(
        tabName = "nieuwsbrieven",
        
        h2("Nieuwsbrief Analytics"),
        
        fluidRow(
          column(3, kpi_card("Gem. Open Rate", textOutput("nieuwsbrief_openrate"))),
          column(3, kpi_card("Gem. CTR", textOutput("nieuwsbrief_ctr"))),
          column(3, kpi_card("Totaal verzonden", textOutput("nieuwsbrief_verzonden"))),
          column(3, kpi_card("Totaal clicks", textOutput("nieuwsbrief_clicks")))
        ),
        
        br(),
        
        fluidRow(
          box(width = 12, title = "Open Rate ontwikkeling",
              plotlyOutput("nieuwsbrief_trend_plot", height = "350px"))
        ),
        
        fluidRow(
          box(width = 6, title = "Top campagnes", tableOutput("nieuwsbrief_top")),
          box(width = 6, title = "Verbeterpunten", tableOutput("nieuwsbrief_bottom"))
        ),
        
        fluidRow(
          box(width = 12, title = "Open Rate vs CTR",
              plotlyOutput("nieuwsbrief_bubble_plot", height = "450px"))
        ),
        
        fluidRow(
          box(width = 12, title = "Campagne overzicht", tableOutput("nieuwsbrief_tabel"))
        )
        
      ),
      
      # SOCIAL MEDIA
      tabItem(tabName = "social_media",
              h2("Social Media"),
              fluidRow(
                column(3, kpi_card("Views",
                                   format_number(data$social_media_kpis$totaal_views))),
                column(3, kpi_card("Engagement",
                                   format_number(data$social_media_kpis$totaal_engagement))),
                column(3, kpi_card("Engagement Rate",
                                   paste0(data$social_media_kpis$engagement_rate, "%"))),
                column(3, kpi_card("Watch Time",
                                   round(data$social_media_kpis$gemiddelde_watchtime, 1)))
              ),
              br(),
              fluidRow(
                box(width = 6, title = "Platform prestaties",
                    plotlyOutput("social_platform_plot", height = "340px")),
                box(width = 6, title = "Volgers per platform",
                    plotlyOutput("social_volgers_plot", height = "340px"))
              ),
              
              h3("Volgers per platform"),
              fluidRow(uiOutput("social_volgers_kpis")),
              
              br(),
              h3("Post Type Performance"),
              fluidRow(
                box(width = 6, title = "Gemiddelde views per post type",
                    plotlyOutput("post_type_plot", height = "340px")),
                box(width = 6, title = "Overzicht per post type",
                    tableOutput("post_type_tabel"))
              ),
              
              br(),
              h3("Post Performance"),
              fluidRow(
                column(12,
                       selectInput("post_select", "Selecteer post",
                                   choices = setNames(
                                     seq_len(nrow(data$post_performance)),
                                     paste0(data$post_performance$titel, " (",
                                            format(as.Date(data$post_performance$datum), "%d-%m-%Y"), ")")
                                   )))
              ),
              fluidRow(
                column(2, kpi_card("Views",    textOutput("post_views"))),
                column(2, kpi_card("Likes",    textOutput("post_likes"))),
                column(2, kpi_card("Comments", textOutput("post_comments"))),
                column(2, kpi_card("Shares",   textOutput("post_shares"))),
                column(2, kpi_card("Saves",    textOutput("post_saves"))),
                column(2, kpi_card("Engagement", textOutput("post_engagement")))
              )
      ),
      
      # META ADVERTENTIES
      tabItem(
        tabName = "advertenties",
        h2("Social media advertentie"),
        uiOutput("meta_campagne_header"),
        fluidRow(
          column(4, kpi_card("Bereik", textOutput("meta_bereik"), subtitel = "unieke personen")),
          column(4, kpi_card("Vertoningen", textOutput("meta_weergaven"), subtitel = "totaal weergegeven")),
          column(4, kpi_card("Landingspaginaweergaven", textOutput("meta_resultaten"), subtitel = "door Meta toegeschreven"))
        ),
        br(),
        fluidRow(
          column(4, kpi_card("Besteed bedrag", textOutput("meta_spend"), subtitel = "inclusief campagnelooptijd")),
          column(4, kpi_card("Kosten per resultaat", textOutput("meta_kosten_resultaat"), subtitel = "spend / landingspaginaweergave")),
          column(4, kpi_card("Landingspaginapercentage", textOutput("meta_resultaat_rate"), subtitel = "resultaten / vertoningen"))
        ),
        br(),
        h3("Omzetmeting"),
        fluidRow(
          column(4, kpi_card("Directe websiteaankopen", textOutput("meta_aankopen"), subtitel = "door Meta gerapporteerd")),
          column(4, kpi_card("Toegeschreven omzet", textOutput("meta_omzet"), subtitel = "aankoopwaarde in Meta")),
          column(4, kpi_card("ROAS", textOutput("meta_roas"), subtitel = "omzet / advertentiekosten"))
        ),
        br(),
        fluidRow(
          box(width = 7, title = "Van vertoning naar landingspagina",
              plotlyOutput("meta_funnel_plot", height = "380px")),
          box(width = 5, title = "Campagnedetails",
              uiOutput("meta_details"))
        ),
        fluidRow(
          box(width = 12, title = "Interpretatie",
              uiOutput("meta_inzichten"))
        )
      ),

      # VERENIGINGEN
      tabItem(tabName = "verenigingen",
              h2("Verenigingen"),
              div(class = "placeholder-melding",
                  icon("clock"), p("Data wordt binnenkort beschikbaar gesteld."))
      ),
      
      # WEBSITE
      tabItem(
        tabName = "website",
        
        h2("Website Analytics"),
        uiOutput("ga4_status_banner"),
        
        fluidRow(
          column(3, kpi_card("Gebruikers", format_number(data$website_kpis$activeUsers))),
          column(3, kpi_card("Sessies", format_number(data$website_kpis$sessions))),
          column(3, kpi_card("Pageviews", format_number(data$website_kpis$screenPageViews))),
          column(3, kpi_card("Engagement",
                             paste0(round(data$website_kpis$engagementRate * 100, 1), "%")))
        ),
        
        br(),
        
        fluidRow(
          box(width = 8, title = "Verkeer per dag",
              plotlyOutput("website_bezoekers_plot", height = "340px")),
          box(width = 4, title = "Apparaten",
              plotlyOutput("website_devices_plot", height = "340px"))
        ),
        
        fluidRow(
          box(width = 7, title = "Checkout funnel (laatste 30 dagen)",
              uiOutput("website_checkout_funnel")),
          box(width = 5, title = "Top zoektermen (laatste 30 dagen)",
              plotlyOutput("website_search_plot", height = "500px"))
        ),
        
        fluidRow(
          box(width = 6, title = "Top pagina's", tableOutput("website_paginas_tabel")),
          box(width = 6, title = "Verkeersbronnen", tableOutput("website_bronnen_tabel"))
        )
        
      ),
      
      # AFSPRAKEN
      tabItem(
        
        tabName = "afspraken",
        
        h2("Afspraken"),
        
        fluidRow(
          
          column(3, kpi_card("Totaal afspraken",
                             format_number(data$afspraken_kpis_detail$totaal_afspraken),
                             subtitel = "totaal geboekt")),
          
          column(3, kpi_card("Aantal diensten",
                             format_number(data$afspraken_kpis_detail$aantal_diensten),
                             subtitel = "unieke diensten")),
          
          column(3, kpi_card("Gem. afspraken per maand",
                             format_number(mean(data$afspraken_over_tijd$totaal, na.rm = TRUE)),
                             subtitel = "gemiddeld per maand")),
          
          column(3, kpi_card("Top dienst",
                             data$afspraken_per_dienst$dienst[which.max(data$afspraken_per_dienst$totaal)],
                             subtitel = paste0(format_number(max(data$afspraken_per_dienst$totaal)), " afspraken")))
          
        ),
        
        br(),
        
        fluidRow(
          box(width = 8, title = "Afspraken door de tijd",
              plotlyOutput("afspraken_tijd_plot", height = "350px")),
          box(width = 4, title = "Top diensten",
              plotlyOutput("afspraken_per_dienst_plot", height = "350px"))
        ),
        
        fluidRow(
          box(width = 12, title = "Overzicht per dienst", tableOutput("afspraken_tabel"))
        )
        
      ),
      
      # COHORTANALYSE
      tabItem(tabName = "cohortanalyse",
              h2("Cohortanalyse"),
              fluidRow(
                column(4, kpi_card(
                  titel    = "Nieuwe members deze maand",
                  waarde   = format_number(tail(data$members_groei$nieuwe_members, 1)),
                  subtitel = format(as.Date(tail(data$members_groei$maand, 1)), "%B %Y")
                )),
                column(4, kpi_card(
                  titel  = "Retentieratio",
                  waarde = paste0(round(
                    data$klantgedrag$terugkerende_klanten /
                      data$klantgedrag$unieke_klanten * 100, 1), "%"),
                  subtitel = paste0(data$klantgedrag$terugkerende_klanten,
                                    " van ", data$klantgedrag$unieke_klanten, " klanten keren terug")
                )),
                column(4, kpi_card(
                  titel    = "Afspraken deze maand",
                  waarde   = format_number(tail(data$afspraken_over_tijd$totaal, 1)),
                  subtitel = format(as.Date(tail(data$afspraken_over_tijd$maand, 1)), "%B %Y")
                ))
              ),
              br(),
              fluidRow(box(width = 12, title = "Nieuwe members per maand",
                           plotlyOutput("members_groei_plot", height = "340px"))),
              fluidRow(
                box(width = 6, title = "Retentie: klanttype",
                    plotlyOutput("retentie_plot", height = "340px")),
                box(width = 6, title = "Afspraken over tijd",
                    plotlyOutput("cohort_afspraken_plot", height = "340px"))
              )
      )
      
    ) # einde tabItems
  ) # einde dashboardBody
) # einde dashboardPage

# --------------------------------------------------
# SERVER
# --------------------------------------------------

server <- function(input, output, session) {
  
  # HOME
  output$omzet_woonplaats <- renderPlotly({
    maak_bar_plot(data$woonplaats, "woonplaats", "omzet", "Omzet (\u20ac)")
  })
  
  output$omzet_per_maand <- renderPlotly({
    plot_ly(
      data   = data$omzet_per_maand,
      x      = ~maand, y = ~omzet,
      type   = "scatter", mode = "lines+markers",
      line   = list(color = KLEUR_PRIMAIR, width = 2.5, shape = "spline"),
      marker = list(color = "#ffffff", size = 7,
                    line = list(color = KLEUR_PRIMAIR, width = 2)),
      fill          = "tozeroy",
      fillcolor     = "rgba(140,190,38,0.09)",
      hovertemplate = "<b>%{x}</b><br>\u20ac%{y:,.0f}<extra></extra>"
    ) |>
      basis_layout(y_titel = "Omzet (\u20ac)")
  })
  
  # MEMBERDEALS
  output$pricing_omzet <- renderPlotly({
    maak_bar_plot(data$pricing_performance, "pricing_code", "omzet", "Omzet (\u20ac)")
  })
  
  output$pricing_tabel <- renderTable({ data$pricing_performance })
  
  # MEMBERS
  output$members_aanmeldingen_plot <- renderPlotly({
    
    plot_data <- data$members_groei |>
      arrange(maand) |>
      mutate(
        groei_pct = round(
          (nieuwe_members - lag(nieuwe_members)) / lag(nieuwe_members) * 100, 1
        )
      )
    
    plot_ly(
      data   = plot_data,
      x      = ~as.Date(maand), y = ~nieuwe_members,
      type   = "bar", name = "Nieuwe aanmeldingen",
      marker = list(color = KLEUR_PRIMAIR, opacity = 0.90,
                    line = list(color = "rgba(0,0,0,0)")),
      hovertemplate = "<b>%{x}</b><br>Aanmeldingen: %{y}<extra></extra>"
    ) |>
      add_trace(
        y             = ~groei_pct,
        type          = "scatter", mode = "lines+markers",
        name          = "Groei t.o.v. vorige maand (%)",
        line          = list(color = KLEUR_TERTIAIR, width = 2.5),
        marker        = list(color = KLEUR_TERTIAIR, size = 6),
        yaxis         = "y2",
        hovertemplate = "<b>%{x}</b><br>Groei: %{y}%<extra></extra>"
      ) |>
      basis_layout(
        y_titel  = "Nieuwe aanmeldingen",
        y2_titel = "Groei (%)",
        legenda  = TRUE
      ) |>
      layout(bargap = 0.35)
  })
  
  output$members_woonplaats <- renderPlotly({
    maak_bar_plot(data$woonplaats_members, "woonplaats", "omzet", "Omzet (\u20ac)")
  })
  
  output$members_actief_slapend_plot <- renderPlotly({
    
    kleuren <- setNames(
      c(KLEUR_PRIMAIR, "#9ca3af"),
      c("Actief", "Slapend")
    )
    
    plot_data <- data$members_actief_slapend
    
    plot_ly(
      data   = plot_data,
      x      = ~status, y = ~aantal,
      type   = "bar",
      text         = ~format_number(aantal),
      textposition = "outside",
      textfont     = list(size = 13, color = "#374151"),
      marker = list(
        color = kleuren[plot_data$status],
        opacity = 0.92,
        line = list(color = "rgba(0,0,0,0)")
      ),
      hovertemplate = "<b>%{x}</b><br>%{y:,.0f} members<extra></extra>"
    ) |>
      basis_layout(y_titel = "Aantal members") |>
      layout(bargap = 0.5)
  })
  
  # NIEUWSBRIEVEN
  
  nieuwsbrief_data <- reactive({
    
    data$newsletter_campagnes |>
      mutate(
        open_rate = round(opens / sent * 100, 1),
        ctr = round(clicks / sent * 100, 1),
        bounce_rate = round(bounces / sent * 100, 1),
        unsubscribe_rate = round(unsubscribers / sent * 100, 1)
      )
    
  })
  
  output$nieuwsbrief_openrate <- renderText({
    paste0(round(mean(nieuwsbrief_data()$open_rate, na.rm = TRUE), 1), "%")
  })
  
  output$nieuwsbrief_ctr <- renderText({
    paste0(round(mean(nieuwsbrief_data()$ctr, na.rm = TRUE), 1), "%")
  })
  
  output$nieuwsbrief_verzonden <- renderText({
    format_number(sum(nieuwsbrief_data()$sent, na.rm = TRUE))
  })
  
  output$nieuwsbrief_clicks <- renderText({
    format_number(sum(nieuwsbrief_data()$clicks, na.rm = TRUE))
  })
  
  output$nieuwsbrief_trend_plot <- renderPlotly({
    
    plot_ly(
      data = nieuwsbrief_data(),
      x = ~datum,
      y = ~open_rate,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = KLEUR_PRIMAIR, width = 3),
      marker = list(
        color = "#ffffff",
        size = 7,
        line = list(color = KLEUR_PRIMAIR, width = 2)
      ),
      hovertemplate = "<b>%{x}</b><br>Open Rate: %{y}%<extra></extra>"
    ) |>
      basis_layout(y_titel = "Open Rate (%)")
    
  })
  
  output$nieuwsbrief_top <- renderTable({
    nieuwsbrief_data() |>
      arrange(desc(open_rate)) |>
      select(campagne, open_rate, ctr) |>
      head(5)
  })
  
  output$nieuwsbrief_bottom <- renderTable({
    nieuwsbrief_data() |>
      arrange(open_rate) |>
      select(campagne, open_rate, ctr) |>
      head(5)
  })
  
  output$nieuwsbrief_bubble_plot <- renderPlotly({
    
    plot_ly(
      data = nieuwsbrief_data(),
      x = ~open_rate,
      y = ~ctr,
      type = "scatter",
      mode = "markers",
      text = ~campagne,
      size = ~sent,
      color = I(KLEUR_PRIMAIR),
      hovertemplate = paste(
        "<b>%{text}</b><br>",
        "Open Rate: %{x}%<br>",
        "CTR: %{y}%<br>",
        "Verzonden: %{marker.size}<extra></extra>"
      )
    ) |>
      basis_layout(x_titel = "Open Rate (%)", y_titel = "CTR (%)")
    
  })
  
  output$nieuwsbrief_tabel <- renderTable({
    nieuwsbrief_data() |>
      select(datum, campagne, sent, opens, clicks, open_rate, ctr, unsubscribers) |>
      arrange(desc(datum))
  })
  
  # AFSPRAKEN
  
  output$afspraken_per_dienst_plot <- renderPlotly({
    
    top_diensten <- data$afspraken_per_dienst |> head(10)
    
    plot_ly(
      data = top_diensten,
      x = ~totaal,
      y = ~reorder(dienst, totaal),
      type = "bar",
      orientation = "h",
      marker = list(color = KLEUR_PRIMAIR),
      hovertemplate = "<b>%{y}</b><br>%{x} afspraken<extra></extra>"
    ) |>
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 120),
        xaxis = list(title = "Aantal afspraken"),
        yaxis = list(title = "")
      )
    
  })
  
  output$afspraken_tabel <- renderTable({
    
    totaal_afspraken <- sum(data$afspraken_per_dienst$totaal)
    
    data$afspraken_per_dienst |>
      mutate(percentage = round(totaal / totaal_afspraken * 100, 1)) |>
      rename(Dienst = dienst, Afspraken = totaal, Percentage = percentage)
    
  })
  
  output$afspraken_tijd_plot <- renderPlotly({
    plot_ly(
      data   = data$afspraken_over_tijd,
      x      = ~as.Date(maand), y = ~totaal,
      type   = "scatter", mode = "lines+markers",
      line   = list(color = KLEUR_PRIMAIR, width = 2.5, shape = "spline"),
      marker = list(color = "#ffffff", size = 7,
                    line = list(color = KLEUR_PRIMAIR, width = 2)),
      fill          = "tozeroy",
      fillcolor     = "rgba(140,190,38,0.09)",
      hovertemplate = "<b>%{x}</b><br>Afspraken: %{y}<extra></extra>"
    ) |>
      basis_layout(y_titel = "Aantal afspraken")
  })
  
  # COUPONS
  coupon_selected <- reactive({
    req(input$coupon_select)
    data$coupon_detail |> dplyr::filter(coupon_code == input$coupon_select)
  })
  
  output$coupon_omzet      <- renderText(format_euro(sum(coupon_selected()$omzet, na.rm = TRUE)))
  output$coupon_ingeleverd <- renderText(format_number(sum(coupon_selected()$ingeleverd, na.rm = TRUE)))
  output$coupon_verzonden  <- renderText(format_number(sum(coupon_selected()$verzonden, na.rm = TRUE)))
  output$coupon_openstaand <- renderText(format_number(sum(coupon_selected()$openstaand, na.rm = TRUE)))
  
  output$coupon_gebruik_plot <- renderPlotly({
    plot_ly(
      data   = coupon_selected(),
      x      = ~datum, y = ~ingeleverd,
      type   = "bar",
      marker = list(color = KLEUR_PRIMAIR, opacity = 0.90,
                    line = list(color = "rgba(0,0,0,0)")),
      hovertemplate = "<b>%{x}</b><br>Gebruikt: %{y}<extra></extra>"
    ) |>
      basis_layout(y_titel = "Aantal gebruikt") |>
      layout(bargap = 0.35)
  })
  
  output$coupon_omzet_plot <- renderPlotly({
    plot_ly(
      data   = coupon_selected(),
      x      = ~datum, y = ~omzet,
      type   = "scatter", mode = "lines+markers",
      line   = list(color = KLEUR_PRIMAIR, width = 2.5, shape = "spline"),
      marker = list(color = "#ffffff", size = 7,
                    line = list(color = KLEUR_PRIMAIR, width = 2)),
      fill          = "tozeroy",
      fillcolor     = "rgba(140,190,38,0.09)",
      hovertemplate = "<b>%{x}</b><br>\u20ac%{y:,.0f}<extra></extra>"
    ) |>
      basis_layout(y_titel = "Omzet (\u20ac)")
  })
  
  coupon_kpi_data <- reactive({
    
    df <- coupon_selected()
    
    totaal_omzet      <- sum(df$omzet, na.rm = TRUE)
    totaal_ingeleverd  <- sum(df$ingeleverd, na.rm = TRUE)
    totaal_verzonden   <- sum(df$verzonden, na.rm = TRUE)
    totaal_openstaand  <- sum(df$openstaand, na.rm = TRUE)
    totaal_discount    <- if ("discount" %in% names(df)) sum(df$discount, na.rm = TRUE) else NA
    
    list(
      inleverpercentage = if (totaal_verzonden > 0) totaal_ingeleverd / totaal_verzonden * 100 else NA,
      omzet_per_coupon  = if (totaal_ingeleverd > 0) totaal_omzet / totaal_ingeleverd else NA,
      gem_korting       = if (!is.na(totaal_discount) && totaal_ingeleverd > 0) abs(totaal_discount) / totaal_ingeleverd else NA,
      openstaand_pct    = if (totaal_verzonden > 0) totaal_openstaand / totaal_verzonden * 100 else NA
    )
    
  })
  
  output$coupon_inleverpercentage <- renderText({
    waarde <- coupon_kpi_data()$inleverpercentage
    if (is.na(waarde)) "n.v.t." else format_percentage(waarde)
  })
  
  output$coupon_omzet_per_coupon <- renderText({
    waarde <- coupon_kpi_data()$omzet_per_coupon
    if (is.na(waarde)) "n.v.t." else format_euro(waarde)
  })
  
  output$coupon_gem_korting <- renderText({
    waarde <- coupon_kpi_data()$gem_korting
    if (is.na(waarde)) "n.v.t." else format_euro(waarde)
  })
  
  output$coupon_openstaand_pct <- renderText({
    waarde <- coupon_kpi_data()$openstaand_pct
    if (is.na(waarde)) "n.v.t." else format_percentage(waarde)
  })
  
  # SOCIAL MEDIA
  output$social_platform_plot <- renderPlotly({
    maak_bar_plot(data$social_media_platform, "platform", "views", "Views")
  })
  
  output$social_volgers_plot <- renderPlotly({
    kleuren <- c(KLEUR_PRIMAIR, KLEUR_SECUNDAIR, KLEUR_TERTIAIR, KLEUR_LICHT,
                 "#3a5e0d", "#b8d96e")
    maak_donut_plot(data$social_media_volgergroei, "platform",
                    "huidige_volgers", kleuren)
  })
  
  output$social_volgers_kpis <- renderUI({
    
    df <- data$social_media_volgers |>
      mutate(datum = as.Date(datum))
    
    platforms <- sort(unique(df$platform))
    
    kaarten <- lapply(platforms, function(plt) {
      
      df_plt <- df |> filter(platform == plt) |> arrange(datum)
      
      laatste_datum  <- max(df_plt$datum, na.rm = TRUE)
      huidige_volgers <- df_plt |> filter(datum == laatste_datum) |> pull(volgers) |> tail(1)
      
      referentie_datum <- laatste_datum - 30
      vergelijk_df <- df_plt |> filter(datum <= referentie_datum)
      
      if (nrow(vergelijk_df) > 0) {
        vorige_volgers <- vergelijk_df |> filter(datum == max(datum)) |> pull(volgers) |> tail(1)
        groei_pct <- round((huidige_volgers - vorige_volgers) / vorige_volgers * 100, 1)
        trend_class <- ifelse(groei_pct >= 0, "kpi-trend-up", "kpi-trend-down")
        trend_label <- paste0(ifelse(groei_pct >= 0, "+", ""), groei_pct, "%")
      } else {
        trend_class <- NULL
        trend_label <- NULL
      }
      
      column(3, kpi_card(
        titel       = plt,
        waarde      = format_number(huidige_volgers),
        trend_class = trend_class,
        trend_label = trend_label,
        subtitel    = "t.o.v. 30 dagen terug"
      ))
      
    })
    
    do.call(tagList, kaarten)
    
  })
  
  # POST TYPE PERFORMANCE
  output$post_type_plot <- renderPlotly({
    maak_bar_plot(data$post_type_performance, "post_type", "gemiddelde_views", "Gemiddelde views")
  })
  
  output$post_type_tabel <- renderTable({
    data$post_type_performance |>
      rename(
        "Post type"          = post_type,
        "Aantal posts"       = aantal_posts,
        "Views"               = views,
        "Likes"               = likes,
        "Comments"            = comments,
        "Shares"              = shares,
        "Saves"               = saves,
        "Engagement"          = engagement,
        "Engagement rate (%)" = engagement_rate,
        "Gem. views"          = gemiddelde_views,
        "Gem. engagement"     = gemiddelde_engagement
      )
  })

  # META ADVERTENTIES
  meta_campagne <- reactive({
    req(nrow(data$meta_advertenties) > 0)
    data$meta_advertenties[1, , drop = FALSE]
  })

  output$meta_campagne_header <- renderUI({
    x <- meta_campagne()
    div(
      class = "campaign-banner",
      div(class = "campaign-banner__eyebrow", "META CAMPAGNE"),
      div(class = "campaign-banner__title", x$Campagnenaam),
      div(
        class = "campaign-banner__meta",
        span(icon("calendar"), paste(format(x$`Begint op`, "%d-%m-%Y"), "t/m", format(x$`Eindigt op`, "%d-%m-%Y"))),
        span(class = paste0("campaign-status campaign-status--", tolower(x$Weergavestatus)), x$Weergavestatus),
        span(paste("Rapportage t/m", format(x$`Einde rapportage`, "%d-%m-%Y")))
      )
    )
  })

  output$meta_bereik <- renderText(format_number(meta_campagne()$Bereik))
  output$meta_weergaven <- renderText(format_number(meta_campagne()$Weergaven))
  output$meta_resultaten <- renderText(format_number(meta_campagne()$Resultaten))
  output$meta_spend <- renderText(format_euro_precies(meta_campagne()$`Besteed bedrag (EUR)`))
  output$meta_kosten_resultaat <- renderText(format_euro_precies(meta_campagne()$`Kosten per resultaat`))
  output$meta_resultaat_rate <- renderText({
    x <- meta_campagne()
    format_percentage(x$Resultaten / x$Weergaven * 100)
  })
  output$meta_aankopen <- renderText({
    x <- meta_campagne()
    waarde <- if ("Directe aankopen op website" %in% names(x)) x$`Directe aankopen op website` else NA_real_
    if (is.na(waarde)) "Niet gemeten" else format_number(waarde)
  })
  output$meta_omzet <- renderText({
    x <- meta_campagne()
    waarde <- if ("Conversiewaarde aankopen" %in% names(x)) x$`Conversiewaarde aankopen` else NA_real_
    if (is.na(waarde)) "Niet gemeten" else format_euro_precies(waarde)
  })
  output$meta_roas <- renderText({
    x <- meta_campagne()
    waarde <- if ("ROAS (rendement op advertentie-uitgaven) voor aankoop" %in% names(x)) {
      x$`ROAS (rendement op advertentie-uitgaven) voor aankoop`
    } else {
      NA_real_
    }
    if (is.na(waarde)) "Niet berekenbaar" else paste0(format(round(waarde, 2), nsmall = 2, decimal.mark = ","), "x")
  })

  output$meta_funnel_plot <- renderPlotly({
    x <- meta_campagne()
    funnel <- data.frame(
      stap = factor(c("Vertoningen", "Bereikte personen", "Landingspaginaweergaven"),
                    levels = rev(c("Vertoningen", "Bereikte personen", "Landingspaginaweergaven"))),
      aantal = c(x$Weergaven, x$Bereik, x$Resultaten),
      kleur = c(KLEUR_LICHT, KLEUR_PRIMAIR, KLEUR_TERTIAIR)
    )
    plot_ly(
      funnel,
      x = ~aantal,
      y = ~stap,
      type = "bar",
      orientation = "h",
      text = ~format_number(aantal),
      textposition = "outside",
      cliponaxis = FALSE,
      marker = list(color = funnel$kleur),
      hovertemplate = "<b>%{y}</b><br>%{x:,.0f}<extra></extra>"
    ) |>
      basis_layout(x_titel = "Aantal", y_titel = "") |>
      layout(margin = list(l = 155, r = 50, t = 12, b = 45), bargap = 0.42)
  })

  output$meta_details <- renderUI({
    x <- meta_campagne()
    detail <- function(label, waarde) {
      div(class = "campaign-detail", span(class = "campaign-detail__label", label), span(class = "campaign-detail__value", waarde))
    }
    tagList(
      detail("Advertentieset", x$`Naam advertentieset`),
      detail("Resultaattype", x$Resultaattype),
      detail("Frequentie", format(round(x$Frequentie, 2), nsmall = 2, decimal.mark = ",")),
      detail("Toeschrijving", x$Toeschrijvingsinstelling),
      detail("Rapportageperiode", paste(format(x$`Start rapportage`, "%d-%m-%Y"), "t/m", format(x$`Einde rapportage`, "%d-%m-%Y")))
    )
  })

  output$meta_inzichten <- renderUI({
    x <- meta_campagne()
    bereik_rate <- x$Resultaten / x$Bereik * 100
    impressie_rate <- x$Resultaten / x$Weergaven * 100
    tagList(
      div(class = "campaign-insight",
          icon("arrow-pointer"),
          div(strong(paste0(format_percentage(impressie_rate), " van de vertoningen leidde tot een landingspaginaweergave.")),
              span(paste0("Dat is ", format_percentage(bereik_rate), " ten opzichte van het unieke bereik.")))),
      div(class = "campaign-insight",
          icon("repeat"),
          div(strong(paste0("Gemiddelde frequentie: ", format(round(x$Frequentie, 2), nsmall = 2, decimal.mark = ","), " keer.")),
              span("De doelgroep zag de advertentie gemiddeld minder dan twee keer; er is op basis van deze export geen duidelijk signaal van advertentiemoeheid."))),
      div(class = "campaign-note",
          icon("circle-info"),
          "Meta rapporteert voor deze advertentieset 0 directe websiteaankopen, maar laat de totale aankoopwaarde leeg. Dat betekent dat Meta geen omzet aan de campagne heeft toegeschreven; het bewijst niet dat bezoekers nooit later of via een ander kanaal hebben gekocht. Zonder gemeten aankoopwaarde kan ROAS niet worden berekend.")
    )
  })
  
  # POST PERFORMANCE
  post_selected <- reactive({
    req(input$post_select)
    data$post_performance[as.integer(input$post_select), ]
  })
  
  output$post_views     <- renderText({ format_number(post_selected()$views) })
  output$post_likes     <- renderText({ format_number(post_selected()$likes) })
  output$post_comments  <- renderText({ format_number(post_selected()$comments) })
  output$post_shares    <- renderText({ format_number(post_selected()$shares) })
  output$post_saves     <- renderText({ format_number(post_selected()$saves) })
  output$post_engagement <- renderText({ format_number(post_selected()$engagement) })
  
  # WEBSITE
  
  output$website_search_plot <- renderPlotly({
    
    zoekdata <- data$website_search_terms |>
      filter(
        !is.na(searchTerm),
        trimws(searchTerm) != "",
        searchTerm != "(not set)"
      ) |>
      arrange(desc(eventCount))
    
    plot_ly(
      data = zoekdata,
      x = ~eventCount,
      y = ~searchTerm,
      type = "bar",
      orientation = "h",
      marker = list(color = KLEUR_PRIMAIR),
      hovertemplate = "<b>%{y}</b><br>%{x} zoekopdrachten<extra></extra>"
    ) |>
      layout(
        margin = list(l = 120),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        xaxis = list(title = "Aantal zoekopdrachten"),
        yaxis = list(title = "")
      )
  })
  
  # GA4 STATUSBALK
  # Toont direct in het dashboard of GA4 live-data beschikbaar is, in
  # plaats van dat je dit alleen in de server-logs kunt zien.
  ga4_status_ui <- renderUI({
    if (website_data_beschikbaar) {
      div(style = "padding:10px 15px;margin-bottom:15px;border-radius:6px;
                   background:#eaf5d8;color:#3d5c0f;border:1px solid #c5e07a;",
          icon("circle-check"), " GA4-data succesvol opgehaald.")
    } else {
      div(style = "padding:10px 15px;margin-bottom:15px;border-radius:6px;
                   background:#fdecea;color:#7a1f1f;border:1px solid #f5c2c0;",
          icon("triangle-exclamation"),
          " Geen live GA4-data beschikbaar. ",
          if (!is.null(ga4_fout_melding)) paste0("Oorzaak: ", ga4_fout_melding) else "")
    }
  })
  output$ga4_status_banner <- ga4_status_ui
  
  output$website_bezoekers_plot <- renderPlotly({
    
    plot_data <- data$website_dagelijks |>
      mutate(date = as.Date(date)) |>
      arrange(date)
    
    plot_ly(
      data   = plot_data,
      x      = ~date, y = ~sessions,
      type   = "scatter", mode = "lines",
      name   = "Sessies",
      line   = list(color = KLEUR_PRIMAIR, width = 3, shape = "linear"),
      fill          = "tozeroy",
      fillcolor     = "rgba(140,190,38,0.10)",
      hovertemplate = "<b>%{x|%d %b}</b><br>Sessies: %{y:,.0f}<extra></extra>"
    ) |>
      add_trace(
        y = ~activeUsers,
        name = "Gebruikers",
        mode = "lines",
        line = list(color = "#9ca3af", width = 1.5, shape = "linear", dash = "dot"),
        hovertemplate = "<b>%{x|%d %b}</b><br>Gebruikers: %{y:,.0f}<extra></extra>"
      ) |>
      basis_layout(y_titel = "Aantal", legenda = TRUE) |>
      layout(
        xaxis = list(
          title    = "",
          showgrid = FALSE,
          zeroline = FALSE,
          showline = FALSE,
          tickfont = list(size = 12, color = "#6b7280"),
          tickformat = "%d %b",
          nticks   = 8,
          tickangle = 0
        ),
        hovermode = "x unified"
      )
  })
  
  output$website_devices_plot <- renderPlotly({
    maak_donut_plot(
      data$website_devices,
      "deviceCategory",
      "activeUsers",
      c(KLEUR_PRIMAIR, KLEUR_TERTIAIR, KLEUR_LICHT)
    )
  })
  
  output$website_checkout_funnel <- renderUI({
    
    stappen <- data.frame(
      event = c("page_view", "view_item", "add_to_cart", "begin_checkout", "purchase"),
      label = c("Paginabezoek", "Product bekeken", "Winkelwagen", "Checkout", "Aankoop"),
      stringsAsFactors = FALSE
    )
    
    funnel_data <- stappen |>
      left_join(data$website_checkout_funnel, by = c("event" = "eventName")) |>
      mutate(eventCount = ifelse(is.na(eventCount), 0, eventCount))
    
    kleuren <- c("#8cbe26", "#7ab221", "#6ca61c", "#5b9218", "#4d7e12")
    max_count <- max(funnel_data$eventCount, na.rm = TRUE)
    if (!is.finite(max_count) || max_count == 0) max_count <- 1
    items <- list()
    
    for (i in seq_len(nrow(funnel_data))) {
      
      aantal <- format(funnel_data$eventCount[i], big.mark = ".", decimal.mark = ",")
      breedte <- max(35, round(funnel_data$eventCount[i] / max_count * 100))
      
      blok <- div(
        style = paste0(
          "background:", kleuren[i], ";",
          "width:", breedte, "%;",
          "margin:0 auto 12px auto;",
          "padding:18px 24px;",
          "border-radius:14px;",
          "color:white;",
          "display:flex;",
          "justify-content:space-between;",
          "align-items:center;",
          "font-weight:600;",
          "font-size:15px;",
          "box-shadow:0 3px 12px rgba(0,0,0,.08);"
        ),
        span(funnel_data$label[i]),
        span(aantal)
      )
      
      items <- append(items, list(blok))
      
      if (i < nrow(funnel_data)) {
        conv <- ifelse(
          funnel_data$eventCount[i] == 0, 0,
          round(funnel_data$eventCount[i + 1] / funnel_data$eventCount[i] * 100, 1)
        )
        uitval <- round(100 - conv, 1)
        
        pijl <- div(
          style = "text-align:center; margin:4px 0 14px 0; color:#6b7280;",
          div(style = "font-size:20px; font-weight:700;", paste0("\u2193 ", conv, "%")),
          div(style = "font-size:12px;", paste0(uitval, "% uitval"))
        )
        
        items <- append(items, list(pijl))
      }
    }
    
    div(
      style = "background:#f8fafc; border-radius:16px; padding:25px; max-width:700px; margin:0 auto;",
      do.call(tagList, items)
    )
  })
  
  output$website_paginas_tabel <- renderTable({ data$website_paginas })
  output$website_bronnen_tabel <- renderTable({ data$website_bronnen })
  
  # COHORTANALYSE
  output$members_groei_plot <- renderPlotly({
    plot_ly(
      data   = data$members_groei,
      x      = ~as.Date(maand), y = ~nieuwe_members,
      type   = "bar", name = "Nieuwe members",
      marker = list(color = KLEUR_PRIMAIR, opacity = 0.90,
                    line = list(color = "rgba(0,0,0,0)")),
      hovertemplate = "<b>%{x}</b><br>Nieuwe members: %{y}<extra></extra>"
    ) |>
      add_trace(
        y             = ~cumsum(nieuwe_members),
        type          = "scatter", mode = "lines",
        name          = "Cumulatief",
        line          = list(color = KLEUR_TERTIAIR, width = 2.5),
        marker        = list(size = 0),
        yaxis         = "y2",
        hovertemplate = "<b>%{x}</b><br>Cumulatief: %{y}<extra></extra>"
      ) |>
      basis_layout(
        y_titel  = "Nieuwe members",
        y2_titel = "Cumulatief totaal",
        legenda  = TRUE
      ) |>
      layout(bargap = 0.35)
  })
  
  output$retentie_plot <- renderPlotly({
    retentie_df <- data.frame(
      klanttype = c("Eenmalig", "Terugkerend"),
      aantal    = c(data$klantgedrag$eenmalige_klanten,
                    data$klantgedrag$terugkerende_klanten)
    )
    plot_ly(
      data         = retentie_df,
      x            = ~klanttype, y = ~aantal,
      type         = "bar",
      text         = ~format_number(aantal),
      textposition = "outside",
      textfont     = list(size = 13, color = "#374151"),
      marker       = list(
        color   = c(KLEUR_LICHT, KLEUR_PRIMAIR),
        opacity = 0.92,
        line    = list(color = "rgba(0,0,0,0)")
      ),
      hovertemplate = "<b>%{x}</b><br>%{y:,.0f} klanten<extra></extra>"
    ) |>
      basis_layout(y_titel = "Aantal klanten") |>
      layout(bargap = 0.5)
  })
  
  output$cohort_afspraken_plot <- renderPlotly({
    plot_ly(
      data   = data$afspraken_over_tijd,
      x      = ~as.Date(maand), y = ~totaal,
      type   = "scatter", mode = "lines+markers",
      line   = list(color = KLEUR_PRIMAIR, width = 2.5, shape = "spline"),
      marker = list(color = "#ffffff", size = 7,
                    line = list(color = KLEUR_PRIMAIR, width = 2)),
      fill          = "tozeroy",
      fillcolor     = "rgba(140,190,38,0.09)",
      hovertemplate = "<b>%{x}</b><br>Afspraken: %{y}<extra></extra>"
    ) |>
      basis_layout(y_titel = "Aantal afspraken")
  })
  
}

# --------------------------------------------------
# APP
# --------------------------------------------------

shinyApp(ui = ui, server = server)
