library(shiny)
library(shinydashboard)
library(DBI)
library(duckdb)
library(plotly)
library(dplyr)
library(googleAnalyticsR)

if (file.exists("simplybook.R")) source("simplybook.R")

# --------------------------------------------------
# CONSTANTEN
# --------------------------------------------------

KLEUR_PRIMAIR   <- "#8cbe26"
KLEUR_SECUNDAIR <- "#6ca61c"
KLEUR_TERTIAIR  <- "#4d7e12"
KLEUR_LICHT     <- "#c5e07a"

`%||%` <- function(a, b) if (is.null(a)) b else a

# --------------------------------------------------
# DESKTOP DATABASE (LOKAAL)
# --------------------------------------------------

DB_PAD <- Sys.getenv("DESKTOP_DB_PATH", unset = "bedrijf.duckdb")
if (!file.exists(DB_PAD)) {
  stop("Desktop database niet gevonden op pad: '", DB_PAD,
       "'. Werkdirectory is: ", getwd(),
       ". Zet evt. de environment variable DESKTOP_DB_PATH naar het juiste pad.")
}
message("Desktop database wordt geladen vanaf: ", normalizePath(DB_PAD))

con <- dbConnect(duckdb::duckdb(), DB_PAD, read_only = TRUE)
onStop(function() dbDisconnect(con, shutdown = TRUE))

# --------------------------------------------------
# GOOGLE ANALYTICS AUTHENTICATIE (LOCAL)
# --------------------------------------------------

website_data_beschikbaar <- FALSE
ga4_fout_melding <- NULL

tryCatch({
  if (file.exists("ga_token.rds")) {
    googleAnalyticsR::ga_auth(token = "ga_token.rds")
    website_data_beschikbaar <- TRUE
    message("Google Analytics authenticatie gelukt via lokale token.")
  }
}, error = function(e) {
  ga4_fout_melding <- "Geen GA-token gevonden. Website-tab draait zonder live GA-data."
  message("Geen GA-token gevonden. Website-tab draait zonder live GA-data.")
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

leeg_verenigingen <- data.frame(
  vereniging_id = character(0), vereniging = character(0), sport = character(0),
  aantal_leden = numeric(0), aantal_members = numeric(0), datum = as.Date(character(0)),
  pricing_code = character(0), vereniging_deal = character(0),
  discount = numeric(0), omzet = numeric(0)
)

veilige_ga_data <- function(expr) {
  if (!website_data_beschikbaar) return(NULL)
  tryCatch(expr, error = function(e) {
    ga4_fout_melding <<- conditionMessage(e)
    message("GA-call mislukt: ", conditionMessage(e))
    NULL
  })
}

laad_website_data <- function() {
  website_kpis <- veilige_ga_data(ga_data(
    propertyId = 314034198,
    date_range = c("30daysAgo", "today"),
    metrics = c("activeUsers", "sessions", "screenPageViews", "engagementRate")
  )) %||% leeg_website_kpis

  website_dagelijks <- veilige_ga_data(ga_data(
    propertyId = 314034198, date_range = c("30daysAgo", "today"),
    dimensions = "date", metrics = c("activeUsers", "sessions")
  )) %||% leeg_website_dagelijks

  website_paginas <- veilige_ga_data(ga_data(
    propertyId = 314034198, date_range = c("30daysAgo", "today"),
    dimensions = "pagePath", metrics = "screenPageViews", limit = 20
  )) %||% leeg_website_paginas

  website_bronnen <- veilige_ga_data(ga_data(
    propertyId = 314034198, date_range = c("30daysAgo", "today"),
    dimensions = "sessionSource", metrics = "sessions", limit = 20
  )) %||% leeg_website_bronnen

  website_devices <- veilige_ga_data(ga_data(
    propertyId = 314034198, date_range = c("30daysAgo", "today"),
    dimensions = "deviceCategory", metrics = "activeUsers"
  )) %||% leeg_website_devices

  website_search_terms <- veilige_ga_data(ga_data(
    propertyId = 314034198, date_range = c("30daysAgo", "today"),
    dimensions = "searchTerm", metrics = "eventCount", limit = 10
  )) %||% leeg_website_search_terms

  funnel_raw <- veilige_ga_data(ga_data(
    propertyId = 314034198, date_range = c("30daysAgo", "today"),
    dimensions = "eventName", metrics = "eventCount"
  ))
  website_checkout_funnel <- if (is.null(funnel_raw)) leeg_website_funnel else {
    funnel_raw |>
      dplyr::filter(eventName %in% c(
        "page_view", "view_item", "add_to_cart", "begin_checkout", "purchase"
      ))
  }

  list(
    kpis = website_kpis,
    dagelijks = website_dagelijks,
    paginas = website_paginas,
    bronnen = website_bronnen,
    devices = website_devices,
    search_terms = website_search_terms,
    checkout_funnel = website_checkout_funnel
  )
}

normaliseer_vereniging_code <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- sub("^twv[_ -]*", "", x)
  gsub("[^a-z0-9]", "", x)
}

maak_verenigingen_dataset <- function(verenigingen, verenigingen_pricing) {
  if (nrow(verenigingen) == 0) return(leeg_verenigingen)
  names(verenigingen) <- gsub("\\.", " ", names(verenigingen))
  verenigingen <- verenigingen |>
    transmute(
      vereniging_id = as.character(.data[["vereniging_id"]]),
      vereniging = trimws(as.character(.data[["vereniging"]])),
      sport = trimws(as.character(.data[["sport"]])),
      aantal_leden = suppressWarnings(as.numeric(.data[["aantal_leden"]])),
      aantal_members = suppressWarnings(as.numeric(.data[["aantal_members"]]))
    ) |>
    mutate(
      aantal_leden = ifelse(is.na(aantal_leden), 0, aantal_leden),
      aantal_members = ifelse(is.na(aantal_members), 0, aantal_members),
      vereniging_id_match = normaliseer_vereniging_code(vereniging_id),
      vereniging_match = normaliseer_vereniging_code(vereniging)
    )
  if (nrow(verenigingen_pricing) == 0) {
    return(verenigingen |>
      transmute(vereniging_id, vereniging, sport, aantal_leden, aantal_members,
                datum = as.Date(NA), pricing_code = NA_character_,
                vereniging_deal = NA_character_, discount = 0, omzet = 0))
  }
  names(verenigingen_pricing) <- gsub("\\.", "_", names(verenigingen_pricing))
  pricing <- verenigingen_pricing |>
    transmute(
      datum = as.Date(.data[["datum"]], format = "%d-%m-%Y"),
      pricing_code = trimws(as.character(.data[["pricing_code"]])),
      vereniging_deal = trimws(as.character(.data[["vereniging_deal"]])),
      discount = suppressWarnings(as.numeric(gsub(",", ".", .data[["discount"]]))),
      omzet = suppressWarnings(as.numeric(gsub(",", ".", .data[["omzet"]]))),
      pricing_match = normaliseer_vereniging_code(pricing_code)
    ) |>
    mutate(discount = ifelse(is.na(discount), 0, discount),
           omzet = ifelse(is.na(omzet), 0, omzet))
  match_index <- vapply(pricing$pricing_match, function(code) {
    hits <- which(verenigingen$vereniging_id_match == code |
                    verenigingen$vereniging_match == code |
                    startsWith(verenigingen$vereniging_id_match, code) |
                    startsWith(code, verenigingen$vereniging_id_match) |
                    startsWith(verenigingen$vereniging_match, code) |
                    startsWith(code, verenigingen$vereniging_match))
    if (length(hits) > 0) hits[1] else NA_integer_
  }, integer(1))
  pricing_gematcht <- bind_cols(
    verenigingen[match_index, c("vereniging_id", "vereniging", "sport", "aantal_leden", "aantal_members")],
    pricing |> select(datum, pricing_code, vereniging_deal, discount, omzet)
  )
  zonder_pricing <- verenigingen |>
    filter(!vereniging_id %in% pricing_gematcht$vereniging_id) |>
    transmute(vereniging_id, vereniging, sport, aantal_leden, aantal_members,
              datum = as.Date(NA), pricing_code = NA_character_,
              vereniging_deal = NA_character_, discount = 0, omzet = 0)
  bind_rows(pricing_gematcht, zonder_pricing) |> select(names(leeg_verenigingen))
}

laad_verenigingen_data <- function(con) {
  verenigingen <- if (dbExistsTable(con, "verenigingen")) {
    dbGetQuery(con, "SELECT * FROM verenigingen")
  } else if (file.exists("data/raw/Verenigingen.csv")) {
    read.csv2("data/raw/Verenigingen.csv", stringsAsFactors = FALSE, check.names = FALSE)
  } else data.frame()
  pricing <- if (dbExistsTable(con, "verenigingen_pricing")) {
    dbGetQuery(con, "SELECT * FROM verenigingen_pricing")
  } else if (file.exists("data/raw/verenigingen_pricing.csv")) {
    read.csv2("data/raw/verenigingen_pricing.csv", stringsAsFactors = FALSE, check.names = FALSE)
  } else data.frame()
  maak_verenigingen_dataset(verenigingen, pricing)
}

maak_leeg_plot <- function() {
  plot_ly() |> layout(
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
    annotations = list(list(text = "Geen data voor deze selectie", showarrow = FALSE,
                            font = list(color = "#9ca3af", size = 13)))
  )
}

# --------------------------------------------------
# DESKTOP OPTIMIZED DATA LOADING (LAZY LOADING)
# --------------------------------------------------

desktop_data <- reactiveValues(
  pricing = NULL,
  klanten = NULL,
  marketing = NULL,
  afspraken = NULL,
  woonplaats = NULL,
  omzet_per_maand = NULL,
  pricing_performance = NULL,
  members_kpis = NULL,
  klantgedrag = NULL,
  woonplaats_members = NULL,
  newsletter_campagnes = NULL,
  afspraken_per_dienst = NULL,
  members_groei = NULL,
  afspraken_over_tijd = NULL,
  coupon_kpis = NULL,
  coupon_performance = NULL,
  coupon_maand = NULL,
  coupon_detail = NULL,
  social_media_kpis = NULL,
  social_media_platform = NULL,
  social_media_volgergroei = NULL,
  social_media_volgers = NULL,
  post_type_performance = NULL,
  post_performance = NULL,
  website_kpis = leeg_website_kpis,
  website_dagelijks = leeg_website_dagelijks,
  website_paginas = leeg_website_paginas,
  website_bronnen = leeg_website_bronnen,
  website_devices = leeg_website_devices,
  website_search_terms = leeg_website_search_terms,
  website_checkout_funnel = leeg_website_funnel,
  afspraken_kpis_detail = NULL,
  members_nieuw_7d = NULL,
  members_actief_slapend = NULL,
  members_leeftijd_kpis = NULL,
  members_leeftijd = NULL,
  meta_advertenties = NULL,
  verenigingen = NULL
)

# Laad alleen homepagina data bij startup
load_home_data <- function() {
  message("Desktop: Laden homepagina data...")
  desktop_data$omzet_per_maand <- dbGetQuery(con, "SELECT * FROM mart.omzet_per_maand ORDER BY maand")
  desktop_data$members_kpis <- dbGetQuery(con, "SELECT * FROM mart.members_kpis")
  desktop_data$pricing_performance <- dbGetQuery(con, "SELECT * FROM mart.pricing_performance ORDER BY omzet DESC")
  
  tryCatch({
    desktop_data$members_nieuw_7d <- dbGetQuery(con, "
      SELECT
        SUM(CASE WHEN aanmelddatum > today() - 7
                  AND aanmelddatum <= today() THEN 1 ELSE 0 END) AS afgelopen_7d,
        SUM(CASE WHEN aanmelddatum > today() - 14
                  AND aanmelddatum <= today() - 7 THEN 1 ELSE 0 END) AS vorige_7d
      FROM members
    ")
  }, error = function(e) {
    desktop_data$members_nieuw_7d <- data.frame(afgelopen_7d = 0, vorige_7d = 0)
  })
  
  tryCatch({
    desktop_data$members_actief_slapend <- dbGetQuery(con, "
      SELECT
        CASE
          WHEN laatste_aankoop IS NULL OR laatste_aankoop < today() - 90
            THEN 'Slapend'
          ELSE 'Actief'
        END AS status,
        COUNT(*) AS aantal
      FROM members
      GROUP BY 1
      ORDER BY 1
    ")
  }, error = function(e) {
    desktop_data$members_actief_slapend <- data.frame(status = c("Actief", "Slapend"), aantal = c(0, 0))
  })
  
  desktop_data$klantgedrag <- dbGetQuery(con, "SELECT * FROM mart.klantgedrag")
  desktop_data$pricing <- dbGetQuery(con, "SELECT * FROM mart.kpi_personal_pricing")
  desktop_data$klanten <- dbGetQuery(con, "SELECT * FROM mart.klant_kpis")
  desktop_data$afspraken <- dbGetQuery(con, "SELECT * FROM mart.afspraken_kpis")
  desktop_data$marketing <- dbGetQuery(con, "SELECT * FROM mart.newsletter_kpis")
  desktop_data$social_media_kpis <- dbGetQuery(con, "SELECT * FROM mart.social_media_kpis")
  desktop_data$coupon_kpis <- dbGetQuery(con, "SELECT * FROM mart.coupon_kpis")
  message("Desktop: Homepagina data geladen.")
}

# Lazy loading functies per tabblad
load_pricing_data <- function() {
  if (is.null(desktop_data$woonplaats)) {
    message("Desktop: Laden pricing detail data...")
    desktop_data$woonplaats <- dbGetQuery(con, "SELECT * FROM mart.omzet_per_woonplaats ORDER BY omzet DESC LIMIT 10")
    desktop_data$woonplaats_members <- dbGetQuery(con, "SELECT woonplaats, klanten, omzet, omzet_per_klant FROM mart.omzet_per_woonplaats ORDER BY omzet DESC LIMIT 10")
    message("Desktop: Pricing detail data geladen.")
  }
}

load_members_data <- function() {
  if (is.null(desktop_data$members_groei)) {
    message("Desktop: Laden members data...")
    desktop_data$members_groei <- dbGetQuery(con, "SELECT * FROM mart.members_groei ORDER BY maand")
    
    tryCatch({
      geboortedata <- dbGetQuery(con, "SELECT geboortedatum FROM members WHERE geboortedatum IS NOT NULL")$geboortedatum
      geboortedata <- as.Date(geboortedata)
      vandaag <- Sys.Date()
      geldig <- !is.na(geboortedata) &
        geboortedata >= as.Date("1900-01-01") & geboortedata <= vandaag
      geboortedata <- geboortedata[geldig]
      leeftijden <- as.integer(format(vandaag, "%Y")) -
        as.integer(format(geboortedata, "%Y")) -
        as.integer(format(vandaag, "%m%d") < format(geboortedata, "%m%d"))
      leeftijden <- leeftijden[leeftijden >= 0 & leeftijden <= 120]

      leeftijd_labels <- c(
        "Jonger dan 18", "18–24", "25–34", "35–44",
        "45–54", "55–64", "65+"
      )
      groepen <- cut(
        leeftijden,
        breaks = c(-Inf, 17, 24, 34, 44, 54, 64, Inf),
        labels = leeftijd_labels,
        right = TRUE
      )
      aantallen <- table(factor(groepen, levels = leeftijd_labels))
      
      desktop_data$members_leeftijd <- data.frame(
        leeftijdsgroep = leeftijd_labels,
        aantal = as.numeric(aantallen),
        stringsAsFactors = FALSE
      )
      desktop_data$members_leeftijd_kpis <- data.frame(
        gemiddelde_leeftijd = if (length(leeftijden)) round(mean(leeftijden), 1) else NA_real_,
        members_met_geboortedatum = length(leeftijden)
      )
    }, error = function(e) {
      desktop_data$members_leeftijd <- data.frame(
        leeftijdsgroep = leeftijd_labels,
        aantal = rep(0, length(leeftijd_labels)),
        stringsAsFactors = FALSE
      )
      desktop_data$members_leeftijd_kpis <- data.frame(
        gemiddelde_leeftijd = NA_real_,
        members_met_geboortedatum = 0
      )
    })
    
    desktop_data$verenigingen <- laad_verenigingen_data(con)
    message("Desktop: Members data geladen.")
  }
}

load_marketing_data <- function() {
  if (is.null(desktop_data$newsletter_campagnes)) {
    message("Desktop: Laden marketing data...")
    tryCatch({
      desktop_data$newsletter_campagnes <- dbGetQuery(con, "SELECT * FROM raw.newsletters ORDER BY datum DESC")
    }, error = function(e) {
      tryCatch({
        desktop_data$newsletter_campagnes <- dbGetQuery(con, "SELECT * FROM newsletters ORDER BY datum DESC")
      }, error = function(e2) {
        desktop_data$newsletter_campagnes <- data.frame()
      })
    })
    desktop_data$social_media_platform <- dbGetQuery(con, "SELECT * FROM mart.social_media_platform")
    desktop_data$social_media_volgergroei <- dbGetQuery(con, "SELECT * FROM mart.social_media_volgergroei")
    desktop_data$social_media_volgers <- dbGetQuery(con, "SELECT * FROM mart.social_media_volgers ORDER BY datum")
    desktop_data$post_type_performance <- dbGetQuery(con, "SELECT * FROM mart.post_type_performance ORDER BY views DESC")
    desktop_data$post_performance <- dbGetQuery(con, "SELECT * FROM mart.post_performance ORDER BY datum DESC")
    message("Desktop: Marketing data geladen.")
  }
}

load_afspraken_data <- function() {
  if (is.null(desktop_data$afspraken_per_dienst)) {
    message("Desktop: Laden afspraken data...")
    tryCatch({
      desktop_data$afspraken_per_dienst <- dbGetQuery(con, "
        SELECT dienst, COUNT(*) AS totaal
        FROM afspraken
        GROUP BY dienst
        ORDER BY totaal DESC
      ")
      desktop_data$afspraken_kpis_detail <- dbGetQuery(con, "
        SELECT
          COUNT(*) AS totaal_afspraken,
          COUNT(DISTINCT dienst) AS aantal_diensten
        FROM afspraken
      ")
      desktop_data$afspraken_over_tijd <- dbGetQuery(con, "
        SELECT DATE_TRUNC('month', datum) AS maand, COUNT(*) AS totaal
        FROM afspraken
        WHERE datum IS NOT NULL
        GROUP BY 1
        ORDER BY 1
      ")
    }, error = function(e) {
      desktop_data$afspraken_per_dienst <- data.frame(dienst = character(0), totaal = numeric(0))
      desktop_data$afspraken_kpis_detail <- data.frame(totaal_afspraken = 0, aantal_diensten = 0)
      desktop_data$afspraken_over_tijd <- data.frame(maand = as.Date(character(0)), totaal = numeric(0))
    })
    message("Desktop: Afspraken data geladen.")
  }
}

load_coupons_data <- function() {
  if (is.null(desktop_data$coupon_performance)) {
    message("Desktop: Laden coupons data...")
    desktop_data$coupon_performance <- dbGetQuery(con, "SELECT * FROM mart.coupon_performance ORDER BY omzet DESC")
    desktop_data$coupon_maand <- dbGetQuery(con, "SELECT * FROM mart.coupon_maand ORDER BY maand")
    desktop_data$coupon_detail <- dbGetQuery(con, "SELECT * FROM coupons")
    message("Desktop: Coupons data geladen.")
  }
}

load_website_data <- function() {
  if (is.null(desktop_data$meta_advertenties)) {
    message("Desktop: Laden website/advertentie data...")
    tryCatch({
      if (file.exists("data/raw/meta_advertenties.csv")) {
        meta <- read.csv("data/raw/meta_advertenties.csv", stringsAsFactors = FALSE, check.names = FALSE)
        meta <- meta[!apply(meta, 1, function(regel) {
          all(is.na(regel) | trimws(as.character(regel)) == "")
        }), , drop = FALSE]
        desktop_data$meta_advertenties <- meta
      } else {
        desktop_data$meta_advertenties <- data.frame()
      }
    }, error = function(e) {
      desktop_data$meta_advertenties <- data.frame()
    })
    
    website_data <- laad_website_data()
    desktop_data$website_kpis <- website_data$kpis
    desktop_data$website_dagelijks <- website_data$dagelijks
    desktop_data$website_paginas <- website_data$paginas
    desktop_data$website_bronnen <- website_data$bronnen
    desktop_data$website_devices <- website_data$devices
    desktop_data$website_search_terms <- website_data$search_terms
    desktop_data$website_checkout_funnel <- website_data$checkout_funnel
    message("Desktop: Website/advertentie data geladen.")
  }
}

# Initialisatie
load_home_data()

# --------------------------------------------------
# TREND BEREKENING
# --------------------------------------------------

bereken_trend <- function(omzet_per_maand) {
  if (is.null(omzet_per_maand) || nrow(omzet_per_maand) < 2) {
    return(list(waarde = 0, class = "kpi-trend-neutral", label = "0%"))
  }
  laatste_omzet <- tail(omzet_per_maand$omzet, 1)
  vorige_omzet  <- tail(omzet_per_maand$omzet, 2)[1]
  if (is.na(laatste_omzet) || is.na(vorige_omzet) || vorige_omzet == 0) {
    return(list(waarde = 0, class = "kpi-trend-neutral", label = "0%"))
  }
  trend <- round((laatste_omzet - vorige_omzet) / vorige_omzet * 100, 1)
  list(
    waarde = trend,
    class  = ifelse(trend >= 0, "kpi-trend-up", "kpi-trend-down"),
    label  = paste0(ifelse(trend >= 0, "+", ""), trend, "%")
  )
}

omzet_trend <- reactive(bereken_trend(desktop_data$omzet_per_maand))

bereken_members_7d_trend <- function(members_nieuw_7d) {
  if (is.null(members_nieuw_7d) || nrow(members_nieuw_7d) == 0) {
    return(list(waarde = 0, class = "kpi-trend-neutral", label = "0%"))
  }
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

members_7d_trend <- reactive(bereken_members_7d_trend(desktop_data$members_nieuw_7d))

retentie_pct <- reactive({
  if (!is.null(desktop_data$klantgedrag) && 
      !is.na(desktop_data$klantgedrag$unieke_klanten[1]) &&
      desktop_data$klantgedrag$unieke_klanten[1] > 0) {
    desktop_data$klantgedrag$terugkerende_klanten[1] /
      desktop_data$klantgedrag$unieke_klanten[1] * 100
  } else {
    0
  }
})

# --------------------------------------------------
# INZICHTEN GENERATOR (ZELFDE ALS ORIGINEEL)
# --------------------------------------------------

inzicht_stijl <- list(
  kritiek      = list(bg = "#fdecea", rand = "#f5c2c0", tekst = "#7a1f1f", icoon = "triangle-exclamation", label = "Kritiek"),
  waarschuwing = list(bg = "#fff7e6", rand = "#f5cf87", tekst = "#8a5a00", icoon = "circle-exclamation", label = "Aandachtspunt"),
  positief     = list(bg = "#eaf5d8", rand = "#c5e07a", tekst = "#3d5c0f", icoon = "circle-check", label = "Positief")
)

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

voeg_inzicht_toe <- function(lijst, type, categorie, titel, tekst, actie) {
  append(lijst, list(list(type = type, categorie = categorie, titel = titel, tekst = tekst, actie = actie)))
}

format_percentage_precies <- function(x) {
  if (is.na(x)) return("n.v.t.")
  if (x > 0 && round(x, 1) == 0) return("<0,1%")
  format_percentage(x)
}

genereer_inzichten <- function(data, omzet_trend, members_7d_trend,
                               website_data_beschikbaar) {
  
  inzichten <- list()
  
  # 1. OMZETONTWIKKELING
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
  
  # 2. NIEUWE MEMBERS (7 DAGEN)
  tryCatch({
    if (!is.null(data$members_nieuw_7d) && nrow(data$members_nieuw_7d) > 0) {
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
    }
  }, error = function(e) NULL)
  
  # 3. KLANTRETENTIE
  tryCatch({
    if (!is.null(data$klantgedrag) && nrow(data$klantgedrag) > 0) {
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
    }
  }, error = function(e) NULL)
  
  # 4. MEMBERS: ACTIEF VS SLAPEND
  tryCatch({
    if (!is.null(data$members_actief_slapend) && nrow(data$members_actief_slapend) > 0) {
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
    }
  }, error = function(e) NULL)
  
  # 5. COUPONS: TRANSACTIEKWALITEIT
  tryCatch({
    if (!is.null(data$coupon_detail) && nrow(data$coupon_detail) > 0) {
      df <- data$coupon_detail
      retouren <- sum(df$omzet < 0, na.rm = TRUE)
      transacties <- nrow(df)
      if (transacties > 0 && retouren > 0) {
        retour_pct <- retouren / transacties * 100
        inzichten <- voeg_inzicht_toe(inzichten, "waarschuwing", "Coupons",
                                      "Coupontransacties met negatieve omzet",
                                      paste0(format_number(retouren), " van de ", format_number(transacties),
                                             " coupontransacties heeft negatieve omzet (", format_percentage_precies(retour_pct), ")."),
                                      "Controleer of dit retouren of correcties zijn en sluit ze zo nodig uit bij campagne-evaluaties.")
      }
    }
  }, error = function(e) NULL)
  
  # 6. NIEUWSBRIEF PERFORMANCE
  tryCatch({
    if (!is.null(data$newsletter_campagnes) && nrow(data$newsletter_campagnes) > 0) {
      df <- data$newsletter_campagnes
      verzonden_col <- if ("verzonden" %in% names(df)) "verzonden" else "sent"
      geopend_col <- if ("geopend" %in% names(df)) "geopend" else "opens"
      unsub_col <- if ("unsubscribes" %in% names(df)) "unsubscribes" else "unsubscribers"
      
      verzonden <- sum(df[[verzonden_col]], na.rm = TRUE)
      if (verzonden > 0) {
        open_rate_gem <- mean(df[[geopend_col]] / df[[verzonden_col]] * 100, na.rm = TRUE)
        unsub_rate_gem <- mean(df[[unsub_col]] / df[[verzonden_col]] * 100, na.rm = TRUE)
        laatste <- df[which.max(as.Date(df$datum)), ]
        laatste_open_rate <- laatste[[geopend_col]] / laatste[[verzonden_col]] * 100
        
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
    }
  }, error = function(e) NULL)
  
  # 7. SOCIAL MEDIA
  tryCatch({
    if (!is.null(data$social_media_kpis) && nrow(data$social_media_kpis) > 0) {
      eng_rate <- data$social_media_kpis$engagement_rate[1]
      if (!is.na(eng_rate)) {
        if (eng_rate < 1) {
          inzichten <- voeg_inzicht_toe(inzichten, "waarschuwing", "Social Media",
                                        "Lage engagement rate op social media",
                                        paste0("De gemiddelde engagement rate is ", format_percentage_precies(eng_rate), "."),
                                        "Vergelijk contentformats via de post type performance en zet vaker in op het best presterende type.")
        }
      }
      if (!is.null(data$post_type_performance) && nrow(data$post_type_performance) >= 2) {
        pt <- data$post_type_performance
        beste <- pt[which.max(pt$gemiddelde_engagement), ]
        inzichten <- voeg_inzicht_toe(inzichten, "positief", "Social Media",
                                      paste0("Post type '", beste$post_type, "' presteert het best"),
                                      paste0("Dit type post scoort gemiddeld de hoogste engagement (",
                                             format_number(beste$gemiddelde_engagement), " per post)."),
                                      paste0("Plan de komende periode vaker content van het type '", beste$post_type, "' in."))
      }
    }
  }, error = function(e) NULL)
  
  # 8. AFSPRAKEN: CONCENTRATIE OP ÉÉN DIENST
  tryCatch({
    if (!is.null(data$afspraken_per_dienst) && nrow(data$afspraken_per_dienst) > 0) {
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
    }
  }, error = function(e) NULL)
  
  # 9. MEMBERDEALS / PRICING: ZWAKSTE CODE
  tryCatch({
    if (!is.null(data$pricing_performance) && nrow(data$pricing_performance) >= 2) {
      zwakste <- data$pricing_performance[which.min(data$pricing_performance$omzet), ]
      totaal_omzet <- sum(data$pricing_performance$omzet, na.rm = TRUE)
      if (totaal_omzet > 0) {
        aandeel <- round(zwakste$omzet / totaal_omzet * 100, 1)
        if (aandeel < 3) {
          inzichten <- voeg_inzicht_toe(inzichten, "waarschuwing", "Memberdeals",
                                        paste0("Pricing-code '", zwakste$pricing_code, "' presteert nauwelijks"),
                                        paste0("Deze code is verantwoordelijk voor slechts ", format_percentage_precies(aandeel),
                                               " van de totale omzet."),
                                        "Overweeg om deze code te herpositioneren of te stoppen.")
        }
      }
    }
  }, error = function(e) NULL)
  
  inzichten
}

inzichten <- reactive(genereer_inzichten(desktop_data, omzet_trend(), members_7d_trend(), website_data_beschikbaar))

# --------------------------------------------------
# UI (ZELFDE ALS ORIGINEEL)
# --------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "Data Platform Desktop"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Home",          tabName = "home",          icon = icon("house")),
      menuItem("Memberdeals",   tabName = "memberdeals",   icon = icon("tags")),
      menuItem("Coupons",       tabName = "coupons",       icon = icon("ticket")),
      menuItem("Members",       tabName = "members",       icon = icon("users")),
      menuItem("Nieuwsbrieven", tabName = "nieuwsbrieven", icon = icon("envelope")),
      menuItem("Social Media",  tabName = "social_media",  icon = icon("hashtag")),
      menuItem("Advertenties",  tabName = "advertenties",  icon = icon("bullhorn")),
      menuItem("Verenigingen",  tabName = "verenigingen",  icon = icon("people-group")),
      menuItem("Website",       tabName = "website",       icon = icon("globe")),
      menuItem("Afspraken",     tabName = "afspraken",     icon = icon("calendar"))
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=20260921-1")
    ),
    
    tabItems(
      
      # HOME
      tabItem(tabName = "home",
              div(
                class = "home-heading",
                div(
                  h2("Overzicht"),
                  p("Kernprestaties en commerciële ontwikkeling in één oogopslag.")
                ),
                div(class = "home-heading__date",
                    icon("calendar-day"),
                    format(Sys.Date(), "%d %B %Y"))
              ),
              div(
                class = "home-kpis",
                fluidRow(
                  column(3, kpi_card("Omzet",
                                     uiOutput("omzet_kpi"),
                                     uiOutput("omzet_trend_class"),
                                     uiOutput("omzet_trend_label"), "t.o.v. vorige maand")),
                  column(3, kpi_card("Actieve members",
                                     uiOutput("actieve_members_kpi"),
                                     subtitel = "minimaal één aankoop in 90 dagen")),
                  column(3, kpi_card("Afspraken",
                                     uiOutput("afspraken_kpi"), subtitel = "totaal geboekt")),
                  column(3, kpi_card("Klantretentie",
                                     uiOutput("retentie_kpi"),
                                     subtitel = "aandeel terugkerende klanten"))
                ),
                div(style = "height:32px; clear:both;", `aria-hidden` = "true")
              ),
              div(
                class = "home-charts",
                fluidRow(
                  box(width = 7, title = "Omzetontwikkeling", class = "home-trend",
                      div(class = "home-box-subtitle", "Trend per maand; gebruik dit om structurele beweging te beoordelen"),
                      plotlyOutput("omzet_per_maand", height = "330px")),
                  box(width = 5, title = "Omzet per memberdeal", class = "home-locations",
                      div(class = "home-box-subtitle", "Verdeling over pricingcodes"),
                      plotlyOutput("home_pricing_omzet", height = "330px"))
                ),
                div(style = "height:24px; clear:both;", `aria-hidden` = "true")
              ),
              div(
                class = "home-navigation",
                div(class = "home-navigation__heading",
                    h3("Verdiep de analyse"),
                    span("Open een detailpagina voor oorzaken, segmenten en campagnes")),
                div(
                  class = "home-navigation__grid",
                  actionLink("home_go_members", "Members", icon = icon("users"), class = "home-nav-card"),
                  actionLink("home_go_memberdeals", "Memberdeals", icon = icon("tags"), class = "home-nav-card"),
                  actionLink("home_go_marketing", "Nieuwsbrieven", icon = icon("envelope"), class = "home-nav-card"),
                  actionLink("home_go_website", "Website", icon = icon("globe"), class = "home-nav-card"),
                  actionLink("home_go_afspraken", "Afspraken", icon = icon("calendar"), class = "home-nav-card")
                )
              )
      ),
      
      # MEMBERDEALS
      tabItem(tabName = "memberdeals",
              h2("Memberdeals"),
              fluidRow(
                column(4, kpi_card("Totale omzet",
                                   uiOutput("memberdeals_omzet"))),
                column(4, kpi_card("Aantal uses",
                                   uiOutput("memberdeals_uses"))),
                column(4, kpi_card("Totale korting",
                                   uiOutput("memberdeals_korting")))
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
                          choices = c("Alle coupons"), selected = "Alle coupons"),
              br(),
              fluidRow(
                column(3, kpi_card("Omzet",      textOutput("coupon_omzet"))),
                column(3, kpi_card("Ingeleverd", textOutput("coupon_ingeleverd"))),
                column(3, kpi_card("Unieke klanten", textOutput("coupon_klanten"))),
                column(3, kpi_card("Totale korting", textOutput("coupon_korting")))
              ),
              br(),
              fluidRow(
                box(width = 6, title = "Ingeleverd per maand",
                    plotlyOutput("coupon_gebruik_plot", height = "340px")),
                box(width = 6, title = "Omzet per maand",
                    plotlyOutput("coupon_omzet_plot", height = "340px"))
              ),
              fluidRow(
                box(width = 12, title = "Coupon Performance Analyse",
                    fluidRow(
                      column(3, kpi_card("Gem. bonwaarde",
                                         textOutput("coupon_gem_bonwaarde"),
                                         subtitel = "Omzet / Ingeleverd")),
                      column(3, kpi_card("Gem. korting",
                                         textOutput("coupon_gem_korting"),
                                         subtitel = "Korting / Ingeleverd")),
                      column(3, kpi_card("Korting / omzet",
                                         textOutput("coupon_korting_pct"),
                                         subtitel = "Totale korting / omzet")),
                      column(3, kpi_card("Retourtransacties",
                                         textOutput("coupon_retouren"),
                                         subtitel = "Transacties met negatieve omzet"))
                    ))
              ),
              fluidRow(
                box(width = 12, title = "Vergelijking per coupon",
                    tableOutput("coupon_performance_tabel"))
              )
      ),
      
      # MEMBERS
      tabItem(tabName = "members",
              fluidRow(
                column(2, kpi_card("Totaal Members",
                                   uiOutput("totaal_members_kpi"))),
                column(2, kpi_card("Actieve members",
                                   uiOutput("actieve_members_90d_kpi"),
                                   subtitel = "afgelopen 90 dagen")),
                column(2, kpi_card("Nieuwe members",
                                   uiOutput("nieuwe_members_kpi"),
                                   uiOutput("nieuwe_members_trend_class"),
                                   uiOutput("nieuwe_members_trend_label"),
                                   "afgelopen 7 dagen")),
                column(2, kpi_card("Terugkerende klanten",
                                   uiOutput("terugkerende_klanten_kpi"))),
                column(2, kpi_card("Gem. leeftijd",
                                   uiOutput("gem_leeftijd_kpi"),
                                   subtitel = "op basis van geboortedatum")),
                column(2, kpi_card("Geboortedatum bekend",
                                   uiOutput("geboortedatum_bekend_kpi"),
                                   subtitel = "geldige datums"))
              ),
              br(),
              fluidRow(
                box(width = 8, title = "Aanmeldingen per maand",
                    plotlyOutput("members_aanmeldingen_plot", height = "380px")),
                box(width = 4, title = "Top 10 woonplaatsen",
                    plotlyOutput("members_woonplaats", height = "380px"))
              ),
              fluidRow(
                box(width = 6, title = "Actieve vs slapende members",
                    plotlyOutput("members_actief_slapend_plot", height = "340px")),
                box(width = 6, title = "Leeftijdsverdeling",
                    plotlyOutput("members_leeftijd_plot", height = "340px"))
              )
      ),
      
      # NIEUWSBRIEVEN
      tabItem(
        tabName = "nieuwsbrieven",
        
        h2("Nieuwsbrief Analytics"),
        
        fluidRow(
          class = "newsletter-kpis",
          column(3, kpi_card("Gem. Open Rate", textOutput("nieuwsbrief_openrate"))),
          column(3, kpi_card("Gem. CTR", textOutput("nieuwsbrief_ctr"))),
          column(3, kpi_card("Totaal verzonden", textOutput("nieuwsbrief_verzonden"))),
          column(3, kpi_card("Totaal clicks", textOutput("nieuwsbrief_clicks")))
        ),
        
        fluidRow(
          box(width = 12, title = "Nieuwsbrief Performance",
              tableOutput("nieuwsbrief_tabel"))
        )
      ),
      
      # SOCIAL MEDIA
      tabItem(tabName = "social_media",
              h2("Social Media Analytics"),
              fluidRow(
                column(3, kpi_card("Volgers", textOutput("social_volgers"))),
                column(3, kpi_card("Engagement Rate", textOutput("social_engagement"))),
                column(3, kpi_card("Gem. likes", textOutput("social_likes"))),
                column(3, kpi_card("Posts", textOutput("social_posts")))
              ),
              br(),
              fluidRow(
                box(width = 12, title = "Social Media Performance",
                    tableOutput("social_tabel"))
              )
      ),
      
      # ADVERTENTIES
      tabItem(tabName = "advertenties",
              h2("Meta Advertenties"),
              if (!is.null(ga4_fout_melding)) {
                div(class = "alert alert-warning", ga4_fout_melding)
              },
              fluidRow(
                box(width = 12, title = "Advertentie Performance",
                    tableOutput("meta_tabel"))
              )
      ),
      
      # VERENIGINGEN
      tabItem(tabName = "verenigingen",
              h2("Verenigingen"),
              fluidRow(
                box(width = 12, title = "Verenigingen Overzicht",
                    tableOutput("verenigingen_tabel"))
              )
      ),
      
      # WEBSITE
      tabItem(tabName = "website",
              h2("Website Analytics"),
              if (!is.null(ga4_fout_melding)) {
                div(class = "alert alert-warning", ga4_fout_melding)
              },
              fluidRow(
                column(3, kpi_card("Bezoekers", textOutput("website_bezoekers"))),
                column(3, kpi_card("Paginaweergaven", textOutput("website_weergaven"))),
                column(3, kpi_card("Sessions", textOutput("website_sessions"))),
                column(3, kpi_card("Engagement Rate", textOutput("website_engagement")))
              ),
              br(),
              fluidRow(
                box(width = 12, title = "Website Traffic",
                    tableOutput("website_tabel"))
              )
      ),
      
      # AFSPRAKEN
      tabItem(tabName = "afspraken",
              h2("Afspraken Analytics"),
              fluidRow(
                column(3, kpi_card("Totaal afspraken", textOutput("afspraken_totaal"))),
                column(3, kpi_card("Unieke diensten", textOutput("afspraken_diensten"))),
                column(3, kpi_card("Gem. per dag", textOutput("afspraken_gem_dag"))),
                column(3, kpi_card("Top dienst", textOutput("afspraken_top_dienst")))
              ),
              br(),
              fluidRow(
                box(width = 12, title = "Afspraken Performance",
                    tableOutput("afspraken_tabel"))
              )
      )
    )
  )
)

# --------------------------------------------------
# SERVER
# --------------------------------------------------

server <- function(input, output, session) {
  
  # Lazy loading triggers
  observeEvent(input$sidebar, {
    if (input$sidebar == "memberdeals") load_pricing_data()
    if (input$sidebar == "members") load_members_data()
    if (input$sidebar == "nieuwsbrieven" || input$sidebar == "social_media") load_marketing_data()
    if (input$sidebar == "afspraken") load_afspraken_data()
    if (input$sidebar == "coupons") load_coupons_data()
    if (input$sidebar == "website" || input$sidebar == "advertenties") load_website_data()
  })
  
  # Update coupon choices when data is loaded
  observeEvent(input$sidebar, {
    if (input$sidebar == "coupons" && !is.null(desktop_data$coupon_detail)) {
      choices <- c("Alle coupons", sort(unique(desktop_data$coupon_detail$coupon_code)))
      updateSelectInput(session, "coupon_select", choices = choices)
    }
  })
  
  # Navigation links
  observeEvent(input$home_go_members, {
    updateTabItems(session, "sidebar", "members")
  })
  observeEvent(input$home_go_memberdeals, {
    updateTabItems(session, "sidebar", "memberdeals")
  })
  observeEvent(input$home_go_marketing, {
    updateTabItems(session, "sidebar", "nieuwsbrieven")
  })
  observeEvent(input$home_go_website, {
    updateTabItems(session, "sidebar", "website")
  })
  observeEvent(input$home_go_afspraken, {
    updateTabItems(session, "sidebar", "afspraken")
  })
  
  # Home KPI outputs
  output$omzet_kpi <- renderText({
    if (!is.null(desktop_data$omzet_per_maand) && nrow(desktop_data$omzet_per_maand) > 0) {
      format_euro(tail(desktop_data$omzet_per_maand$omzet, 1))
    } else {
      "€ 0"
    }
  })
  
  output$omzet_trend_class <- renderText({
    omzet_trend()$class
  })
  
  output$omzet_trend_label <- renderText({
    omzet_trend()$label
  })
  
  output$actieve_members_kpi <- renderText({
    if (!is.null(desktop_data$members_kpis) && nrow(desktop_data$members_kpis) > 0) {
      format_number(desktop_data$members_kpis$actieve_members_90d[1])
    } else {
      "0"
    }
  })
  
  output$afspraken_kpi <- renderText({
    if (!is.null(desktop_data$afspraken) && nrow(desktop_data$afspraken) > 0) {
      format_number(desktop_data$afspraken$totaal_afspraken[1])
    } else {
      "0"
    }
  })
  
  output$retentie_kpi <- renderText({
    format_percentage(retentie_pct())
  })
  
  # Home charts
  output$omzet_per_maand <- renderPlotly({
    if (!is.null(desktop_data$omzet_per_maand) && nrow(desktop_data$omzet_per_maand) > 0) {
      maak_lijn_plot(desktop_data$omzet_per_maand, "maand", "omzet", "Omzet")
    } else {
      maak_leeg_plot()
    }
  })
  
  output$home_pricing_omzet <- renderPlotly({
    if (!is.null(desktop_data$pricing_performance) && nrow(desktop_data$pricing_performance) > 0) {
      # Gebruik pricing_performance voor de home chart
      if ("pricing_code" %in% names(desktop_data$pricing_performance) && "omzet" %in% names(desktop_data$pricing_performance)) {
        maak_bar_plot(head(desktop_data$pricing_performance, 10), "pricing_code", "omzet", "Omzet")
      } else {
        maak_leeg_plot()
      }
    } else {
      maak_leeg_plot()
    }
  })
  
  # Memberdeals outputs
  output$memberdeals_omzet <- renderText({
    if (!is.null(desktop_data$pricing_performance) && nrow(desktop_data$pricing_performance) > 0) {
      format_euro(sum(desktop_data$pricing_performance$omzet))
    } else {
      "€ 0"
    }
  })
  
  output$memberdeals_uses <- renderText({
    if (!is.null(desktop_data$pricing_performance) && nrow(desktop_data$pricing_performance) > 0) {
      format_number(sum(desktop_data$pricing_performance$aantal_gebruikt))
    } else {
      "0"
    }
  })
  
  output$memberdeals_korting <- renderText({
    if (!is.null(desktop_data$pricing_performance) && nrow(desktop_data$pricing_performance) > 0) {
      format_euro(abs(sum(desktop_data$pricing_performance$discount)))
    } else {
      "€ 0"
    }
  })
  
  output$pricing_omzet <- renderPlotly({
    if (!is.null(desktop_data$pricing_performance) && nrow(desktop_data$pricing_performance) > 0) {
      maak_bar_plot(desktop_data$pricing_performance, "pricing_code", "omzet", "Omzet")
    } else {
      maak_leeg_plot()
    }
  })
  
  output$pricing_tabel <- renderTable({
    if (!is.null(desktop_data$pricing_performance)) {
      desktop_data$pricing_performance
    } else {
      data.frame()
    }
  })
  
  # Members outputs
  output$totaal_members_kpi <- renderText({
    if (!is.null(desktop_data$members_kpis) && nrow(desktop_data$members_kpis) > 0) {
      format_number(desktop_data$members_kpis$totaal_members[1])
    } else {
      "0"
    }
  })
  
  output$actieve_members_90d_kpi <- renderText({
    if (!is.null(desktop_data$members_kpis) && nrow(desktop_data$members_kpis) > 0) {
      format_number(desktop_data$members_kpis$actieve_members_90d[1])
    } else {
      "0"
    }
  })
  
  output$nieuwe_members_kpi <- renderText({
    if (!is.null(desktop_data$members_nieuw_7d) && nrow(desktop_data$members_nieuw_7d) > 0) {
      format_number(desktop_data$members_nieuw_7d$afgelopen_7d[1])
    } else {
      "0"
    }
  })
  
  output$nieuwe_members_trend_class <- renderText({
    members_7d_trend()$class
  })
  
  output$nieuwe_members_trend_label <- renderText({
    members_7d_trend()$label
  })
  
  output$terugkerende_klanten_kpi <- renderText({
    if (!is.null(desktop_data$klantgedrag) && nrow(desktop_data$klantgedrag) > 0) {
      format_number(desktop_data$klantgedrag$terugkerende_klanten[1])
    } else {
      "0"
    }
  })
  
  output$gem_leeftijd_kpi <- renderText({
    if (!is.null(desktop_data$members_leeftijd_kpis) && nrow(desktop_data$members_leeftijd_kpis) > 0) {
      if (is.na(desktop_data$members_leeftijd_kpis$gemiddelde_leeftijd[1])) "–"
      else paste0(format(desktop_data$members_leeftijd_kpis$gemiddelde_leeftijd[1],
                      decimal.mark = ",", nsmall = 1), " jaar")
    } else {
      "–"
    }
  })
  
  output$geboortedatum_bekend_kpi <- renderText({
    if (!is.null(desktop_data$members_leeftijd_kpis) && nrow(desktop_data$members_leeftijd_kpis) > 0) {
      format_number(desktop_data$members_leeftijd_kpis$members_met_geboortedatum[1])
    } else {
      "0"
    }
  })
  
  output$members_aanmeldingen_plot <- renderPlotly({
    if (!is.null(desktop_data$members_groei) && nrow(desktop_data$members_groei) > 0) {
      maak_lijn_plot(desktop_data$members_groei, "maand", "nieuwe_members", "Nieuwe members")
    } else {
      maak_leeg_plot()
    }
  })
  
  output$members_woonplaats <- renderPlotly({
    if (!is.null(desktop_data$woonplaats_members) && nrow(desktop_data$woonplaats_members) > 0) {
      maak_bar_plot(desktop_data$woonplaats_members, "woonplaats", "omzet", "Omzet")
    } else {
      maak_leeg_plot()
    }
  })
  
  output$members_actief_slapend_plot <- renderPlotly({
    if (!is.null(desktop_data$members_actief_slapend) && nrow(desktop_data$members_actief_slapend) > 0) {
      maak_donut_plot(desktop_data$members_actief_slapend, "status", "aantal")
    } else {
      maak_leeg_plot()
    }
  })
  
  output$members_leeftijd_plot <- renderPlotly({
    if (!is.null(desktop_data$members_leeftijd) && nrow(desktop_data$members_leeftijd) > 0) {
      maak_bar_plot(desktop_data$members_leeftijd, "leeftijdsgroep", "aantal", "Aantal")
    } else {
      maak_leeg_plot()
    }
  })
  
  # Coupon outputs met echte data
  output$coupon_omzet <- renderText({
    if (!is.null(desktop_data$coupon_performance) && nrow(desktop_data$coupon_performance) > 0) {
      format_euro(sum(desktop_data$coupon_performance$omzet))
    } else {
      "€ 0"
    }
  })
  
  output$coupon_ingeleverd <- renderText({
    if (!is.null(desktop_data$coupon_performance) && nrow(desktop_data$coupon_performance) > 0) {
      format_number(sum(desktop_data$coupon_performance$ingeleverd))
    } else {
      "0"
    }
  })
  
  output$coupon_klanten <- renderText({
    if (!is.null(desktop_data$coupon_performance) && nrow(desktop_data$coupon_performance) > 0) {
      format_number(sum(desktop_data$coupon_performance$unieke_klanten))
    } else {
      "0"
    }
  })
  
  output$coupon_korting <- renderText({
    if (!is.null(desktop_data$coupon_performance) && nrow(desktop_data$coupon_performance) > 0) {
      format_euro(abs(sum(desktop_data$coupon_performance$korting)))
    } else {
      "€ 0"
    }
  })
  
  output$coupon_gebruik_plot <- renderPlotly({
    if (!is.null(desktop_data$coupon_maand) && nrow(desktop_data$coupon_maand) > 0) {
      maak_lijn_plot(desktop_data$coupon_maand, "maand", "ingeleverd", "Ingeleverd")
    } else {
      maak_leeg_plot()
    }
  })
  
  output$coupon_omzet_plot <- renderPlotly({
    if (!is.null(desktop_data$coupon_maand) && nrow(desktop_data$coupon_maand) > 0) {
      maak_lijn_plot(desktop_data$coupon_maand, "maand", "omzet", "Omzet")
    } else {
      maak_leeg_plot()
    }
  })
  
  output$coupon_gem_bonwaarde <- renderText({
    if (!is.null(desktop_data$coupon_performance) && nrow(desktop_data$coupon_performance) > 0) {
      gemiddelde <- mean(desktop_data$coupon_performance$gemiddelde_bonwaarde, na.rm = TRUE)
      format_euro_precies(gemiddelde)
    } else {
      "€ 0"
    }
  })
  
  output$coupon_gem_korting <- renderText({
    if (!is.null(desktop_data$coupon_performance) && nrow(desktop_data$coupon_performance) > 0) {
      gemiddelde <- mean(desktop_data$coupon_performance$gemiddelde_korting, na.rm = TRUE)
      format_euro_precies(gemiddelde)
    } else {
      "€ 0"
    }
  })
  
  output$coupon_korting_pct <- renderText({
    if (!is.null(desktop_data$coupon_performance) && nrow(desktop_data$coupon_performance) > 0) {
      gemiddelde <- mean(desktop_data$coupon_performance$korting_omzet_pct, na.rm = TRUE)
      format_percentage(gemiddelde)
    } else {
      "0%"
    }
  })
  
  output$coupon_retouren <- renderText({
    if (!is.null(desktop_data$coupon_performance) && nrow(desktop_data$coupon_performance) > 0) {
      format_number(sum(desktop_data$coupon_performance$retourtransacties))
    } else {
      "0"
    }
  })
  
  output$coupon_performance_tabel <- renderTable({
    if (!is.null(desktop_data$coupon_performance)) {
      desktop_data$coupon_performance
    } else {
      data.frame()
    }
  })
  
  # Newsletter outputs met echte data
  output$nieuwsbrief_openrate <- renderText({
    if (!is.null(desktop_data$newsletter_campagnes) && nrow(desktop_data$newsletter_campagnes) > 0) {
      # Check of 'geopend' kolom bestaat (raw.newsletters) of 'opens' (mart.newsletters)
      geopend_col <- if ("geopend" %in% names(desktop_data$newsletter_campagnes)) "geopend" else "opens"
      verzonden_col <- if ("verzonden" %in% names(desktop_data$newsletter_campagnes)) "verzonden" else "sent"
      
      open_rate <- mean(desktop_data$newsletter_campagnes[[geopend_col]] / desktop_data$newsletter_campagnes[[verzonden_col]] * 100, na.rm = TRUE)
      format_percentage(open_rate)
    } else {
      "0%"
    }
  })
  
  output$nieuwsbrief_ctr <- renderText({
    if (!is.null(desktop_data$newsletter_campagnes) && nrow(desktop_data$newsletter_campagnes) > 0) {
      verzonden_col <- if ("verzonden" %in% names(desktop_data$newsletter_campagnes)) "verzonden" else "sent"
      ctr <- mean(desktop_data$newsletter_campagnes$clicks / desktop_data$newsletter_campagnes[[verzonden_col]] * 100, na.rm = TRUE)
      format_percentage(ctr)
    } else {
      "0%"
    }
  })
  
  output$nieuwsbrief_verzonden <- renderText({
    if (!is.null(desktop_data$newsletter_campagnes) && nrow(desktop_data$newsletter_campagnes) > 0) {
      verzonden_col <- if ("verzonden" %in% names(desktop_data$newsletter_campagnes)) "verzonden" else "sent"
      format_number(sum(desktop_data$newsletter_campagnes[[verzonden_col]]))
    } else {
      "0"
    }
  })
  
  output$nieuwsbrief_clicks <- renderText({
    if (!is.null(desktop_data$newsletter_campagnes) && nrow(desktop_data$newsletter_campagnes) > 0) {
      format_number(sum(desktop_data$newsletter_campagnes$clicks))
    } else {
      "0"
    }
  })
  
  output$nieuwsbrief_tabel <- renderTable({
    if (!is.null(desktop_data$newsletter_campagnes)) {
      desktop_data$newsletter_campagnes
    } else {
      data.frame()
    }
  })
  
  # Social media outputs met echte data
  output$social_volgers <- renderText({
    if (!is.null(desktop_data$social_media_volgers) && nrow(desktop_data$social_media_volgers) > 0) {
      laatste <- tail(desktop_data$social_media_volgers, 1)
      format_number(laatste$volgers[1])
    } else {
      "0"
    }
  })
  
  output$social_engagement <- renderText({
    if (!is.null(desktop_data$social_media_kpis) && nrow(desktop_data$social_media_kpis) > 0) {
      format_percentage(desktop_data$social_media_kpis$engagement_rate[1])
    } else {
      "0%"
    }
  })
  
  output$social_likes <- renderText({
    if (!is.null(desktop_data$social_media_kpis) && nrow(desktop_data$social_media_kpis) > 0) {
      # Gebruik engagement als fallback als likes niet beschikbaar is
      if ("gemiddelde_likes" %in% names(desktop_data$social_media_kpis)) {
        format_number(desktop_data$social_media_kpis$gemiddelde_likes[1])
      } else {
        format_number(desktop_data$social_media_kpis$totaal_engagement[1])
      }
    } else {
      "0"
    }
  })
  
  output$social_posts <- renderText({
    if (!is.null(desktop_data$post_performance) && nrow(desktop_data$post_performance) > 0) {
      format_number(nrow(desktop_data$post_performance))
    } else {
      "0"
    }
  })
  
  output$social_tabel <- renderTable({
    if (!is.null(desktop_data$social_media_platform)) {
      desktop_data$social_media_platform
    } else {
      data.frame()
    }
  })
  
  output$meta_tabel <- renderTable({
    if (!is.null(desktop_data$meta_advertenties) && nrow(desktop_data$meta_advertenties) > 0) {
      desktop_data$meta_advertenties
    } else {
      data.frame()
    }
  })
  
  output$verenigingen_tabel <- renderTable({
    if (!is.null(desktop_data$verenigingen) && nrow(desktop_data$verenigingen) > 0) {
      desktop_data$verenigingen
    } else {
      data.frame()
    }
  })
  
  output$website_bezoekers <- renderText({
    if (!is.null(desktop_data$website_kpis) && nrow(desktop_data$website_kpis) > 0) {
      format_number(desktop_data$website_kpis$activeUsers)
    } else {
      "0"
    }
  })
  
  output$website_weergaven <- renderText({
    if (!is.null(desktop_data$website_kpis) && nrow(desktop_data$website_kpis) > 0) {
      format_number(desktop_data$website_kpis$screenPageViews)
    } else {
      "0"
    }
  })
  
  output$website_sessions <- renderText({
    if (!is.null(desktop_data$website_kpis) && nrow(desktop_data$website_kpis) > 0) {
      format_number(desktop_data$website_kpis$sessions)
    } else {
      "0"
    }
  })
  
  output$website_engagement <- renderText({
    if (!is.null(desktop_data$website_kpis) && nrow(desktop_data$website_kpis) > 0) {
      format_percentage(desktop_data$website_kpis$engagementRate)
    } else {
      "0%"
    }
  })
  
  output$website_tabel <- renderTable({
    if (!is.null(desktop_data$website_bronnen) && nrow(desktop_data$website_bronnen) > 0) {
      desktop_data$website_bronnen
    } else {
      data.frame()
    }
  })
  
  output$afspraken_totaal <- renderText({
    if (!is.null(desktop_data$afspraken_kpis_detail) && nrow(desktop_data$afspraken_kpis_detail) > 0) {
      format_number(desktop_data$afspraken_kpis_detail$totaal_afspraken[1])
    } else {
      "0"
    }
  })
  
  output$afspraken_diensten <- renderText({
    if (!is.null(desktop_data$afspraken_kpis_detail) && nrow(desktop_data$afspraken_kpis_detail) > 0) {
      format_number(desktop_data$afspraken_kpis_detail$aantal_diensten[1])
    } else {
      "0"
    }
  })
  
  output$afspraken_gem_dag <- renderText({
    if (!is.null(desktop_data$afspraken_over_tijd) && nrow(desktop_data$afspraken_over_tijd) > 0) {
      gemiddelde <- mean(desktop_data$afspraken_over_tijd$totaal, na.rm = TRUE)
      format_number(round(gemiddelde / 30, 1))
    } else {
      "0"
    }
  })
  
  output$afspraken_top_dienst <- renderText({
    if (!is.null(desktop_data$afspraken_per_dienst) && nrow(desktop_data$afspraken_per_dienst) > 0) {
      top <- desktop_data$afspraken_per_dienst[which.max(desktop_data$afspraken_per_dienst$totaal), ]
      top$dienst[1]
    } else {
      "–"
    }
  })
  
  output$afspraken_tabel <- renderTable({
    if (!is.null(desktop_data$afspraken_per_dienst)) {
      desktop_data$afspraken_per_dienst
    } else {
      data.frame()
    }
  })
}

shinyApp(ui = ui, server = server)