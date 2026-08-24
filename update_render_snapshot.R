library(DBI)
library(duckdb)

update_render_snapshot <- function(
    bron_pad = "bedrijf.duckdb",
    snapshot_pad = "render_snapshot.duckdb",
    meta_bestand = file.path("data", "raw", "meta_advertenties.csv")
) {
  if (!file.exists(bron_pad)) stop("Brondatabase niet gevonden: ", bron_pad)
  if (!file.exists(snapshot_pad)) stop("Render-snapshot niet gevonden: ", snapshot_pad)

  tijdelijk_pad <- file.path(
    dirname(snapshot_pad),
    paste0(".", basename(snapshot_pad), ".tmp")
  )
  if (file.exists(tijdelijk_pad)) unlink(tijdelijk_pad)
  if (!file.copy(snapshot_pad, tijdelijk_pad, overwrite = TRUE)) {
    stop("Kon geen tijdelijke kopie van het Render-snapshot maken.")
  }

  bron <- dbConnect(duckdb::duckdb(), bron_pad, read_only = TRUE)
  snapshot <- dbConnect(duckdb::duckdb(), tijdelijk_pad)

  sluit_verbindingen <- function() {
    if (DBI::dbIsValid(bron)) dbDisconnect(bron, shutdown = TRUE)
    if (DBI::dbIsValid(snapshot)) dbDisconnect(snapshot, shutdown = TRUE)
  }

  schrijf_tabel <- function(schema, tabel, data) {
    dbWriteTable(
      snapshot,
      DBI::Id(schema = schema, table = tabel),
      data,
      overwrite = TRUE
    )
  }

  geslaagd <- FALSE
  tryCatch(
    {
      dbBegin(snapshot)

      mart_tabellen <- c(
        "afspraken_kpis",
        "klantgedrag",
        "kpi_personal_pricing",
        "members_groei",
        "members_kpis",
        "newsletter_kpis",
        "omzet_per_maand",
        "omzet_per_woonplaats",
        "pricing_performance",
        "social_media_kpis",
        "social_media_platform",
        "social_media_volgergroei",
        "social_media_volgers",
        "post_type_performance",
        "post_performance"
      )
      for (tabel in mart_tabellen) {
        schrijf_tabel(
          "mart",
          tabel,
          dbGetQuery(bron, paste0("SELECT * FROM mart.", tabel))
        )
      }

      # Coupontransacties zijn nodig voor de filters en grafieken, maar het
      # klantnummer blijft bewust buiten het deploysnapshot.
      schrijf_tabel(
        "raw",
        "coupons",
        dbGetQuery(bron, "
          SELECT coupon_code, datum, receipt_id, discount, omzet
          FROM raw.coupons
        ")
      )

      schrijf_tabel(
        "raw",
        "verenigingen",
        dbGetQuery(bron, "
          SELECT
            vereniging_id,
            vereniging,
            sport,
            CAST(aantal_leden AS INTEGER) AS aantal_leden,
            CAST(aantal_members AS INTEGER) AS aantal_members
          FROM raw.verenigingen
        ")
      )
      schrijf_tabel(
        "raw",
        "verenigingen_pricing",
        dbGetQuery(bron, "
          SELECT datum, pricing_code, vereniging_deal, discount, omzet
          FROM raw.verenigingen_pricing
        ")
      )

      if (!file.exists(meta_bestand)) {
        stop("Meta-advertentiebestand niet gevonden: ", meta_bestand)
      }
      meta <- read.csv(
        meta_bestand,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        colClasses = "character",
        encoding = "UTF-8"
      )
      meta <- meta[!apply(meta, 1, function(regel) {
        all(is.na(regel) | trimws(as.character(regel)) == "")
      }), , drop = FALSE]

      vereist_meta <- c(
        "Campagnenaam", "Naam advertentieset", "Weergavestatus", "Weergavelevel",
        "Bereik", "Weergaven", "Frequentie", "Toeschrijvingsinstelling",
        "Resultaattype", "Resultaten", "Besteed bedrag (EUR)", "Kosten per resultaat",
        "Begint op", "Eindigt op", "Aankopen", "Kosten per aankoop",
        "Conversiewaarde aankopen", "Aankopen op website",
        "Directe aankopen op website",
        "ROAS (rendement op advertentie-uitgaven) voor aankoop",
        "Start rapportage", "Einde rapportage"
      )
      ontbrekend_meta <- setdiff(vereist_meta, names(meta))
      if (length(ontbrekend_meta) > 0) {
        stop("Meta-export mist verplichte kolommen: ", paste(ontbrekend_meta, collapse = ", "))
      }

      als_getal <- function(x) suppressWarnings(as.numeric(gsub(",", ".", x)))
      meta_snapshot <- data.frame(
        campagnenaam = meta[["Campagnenaam"]],
        advertentieset = meta[["Naam advertentieset"]],
        weergavestatus = meta[["Weergavestatus"]],
        weergavelevel = meta[["Weergavelevel"]],
        bereik = als_getal(meta[["Bereik"]]),
        weergaven = als_getal(meta[["Weergaven"]]),
        frequentie = als_getal(meta[["Frequentie"]]),
        toeschrijvingsinstelling = meta[["Toeschrijvingsinstelling"]],
        resultaattype = meta[["Resultaattype"]],
        resultaten = als_getal(meta[["Resultaten"]]),
        besteed_bedrag = als_getal(meta[["Besteed bedrag (EUR)"]]),
        kosten_per_resultaat = als_getal(meta[["Kosten per resultaat"]]),
        begint_op = as.Date(meta[["Begint op"]]),
        eindigt_op = as.Date(meta[["Eindigt op"]]),
        aankopen = als_getal(meta[["Aankopen"]]),
        kosten_per_aankoop = als_getal(meta[["Kosten per aankoop"]]),
        conversiewaarde_aankopen = als_getal(meta[["Conversiewaarde aankopen"]]),
        aankopen_website = als_getal(meta[["Aankopen op website"]]),
        directe_aankopen_website = als_getal(meta[["Directe aankopen op website"]]),
        roas = als_getal(meta[["ROAS (rendement op advertentie-uitgaven) voor aankoop"]]),
        start_rapportage = as.Date(meta[["Start rapportage"]]),
        einde_rapportage = as.Date(meta[["Einde rapportage"]]),
        stringsAsFactors = FALSE
      )
      schrijf_tabel("dashboard", "meta_advertenties", meta_snapshot)

      schrijf_tabel(
        "dashboard",
        "newsletter_campagnes",
        dbGetQuery(bron, "SELECT * FROM staging.newsletters ORDER BY datum DESC")
      )

      schrijf_tabel(
        "dashboard",
        "afspraken_per_dienst",
        dbGetQuery(bron, "
          SELECT dienst, COUNT(*) AS totaal
          FROM raw.afspraken
          GROUP BY dienst
          ORDER BY totaal DESC
        ")
      )
      schrijf_tabel(
        "dashboard",
        "afspraken_kpis_detail",
        dbGetQuery(bron, "
          SELECT
            COUNT(*) AS totaal_afspraken,
            COUNT(DISTINCT dienst) AS aantal_diensten
          FROM raw.afspraken
        ")
      )
      schrijf_tabel(
        "dashboard",
        "afspraken_over_tijd",
        dbGetQuery(bron, "
          SELECT DATE_TRUNC('month', datum) AS maand, COUNT(*) AS totaal
          FROM raw.afspraken
          WHERE datum IS NOT NULL
          GROUP BY 1
          ORDER BY 1
        ")
      )
      schrijf_tabel(
        "dashboard",
        "members_nieuw_7d",
        dbGetQuery(bron, "
          SELECT
            SUM(CASE WHEN aanmelddatum > today() - 7
                      AND aanmelddatum <= today() THEN 1 ELSE 0 END) AS afgelopen_7d,
            SUM(CASE WHEN aanmelddatum > today() - 14
                      AND aanmelddatum <= today() - 7 THEN 1 ELSE 0 END) AS vorige_7d
          FROM raw.members
        ")
      )
      schrijf_tabel(
        "dashboard",
        "members_actief_slapend",
        dbGetQuery(bron, "
          SELECT
            CASE
              WHEN laatste_aankoop IS NULL OR laatste_aankoop < today() - 90
                THEN 'Slapend'
              ELSE 'Actief'
            END AS status,
            COUNT(*) AS aantal
          FROM raw.members
          GROUP BY 1
          ORDER BY 1
        ")
      )

      geboortedata <- dbGetQuery(
        bron,
        "SELECT geboortedatum FROM raw.members WHERE geboortedatum IS NOT NULL"
      )$geboortedatum
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
      schrijf_tabel(
        "dashboard",
        "members_leeftijd",
        data.frame(
          leeftijdsgroep = leeftijd_labels,
          aantal = as.numeric(aantallen),
          stringsAsFactors = FALSE
        )
      )
      schrijf_tabel(
        "dashboard",
        "members_leeftijd_kpis",
        data.frame(
          gemiddelde_leeftijd = if (length(leeftijden)) round(mean(leeftijden), 1) else NA_real_,
          members_met_geboortedatum = length(leeftijden)
        )
      )
      schrijf_tabel(
        "dashboard",
        "snapshot_metadata",
        data.frame(
          snapshot_gemaakt_op = Sys.time(),
          bron_database = "van_duinkerken",
          snapshot_versie = 1L
        )
      )

      dbCommit(snapshot)
      geslaagd <- TRUE
    },
    error = function(e) {
      if (DBI::dbIsValid(snapshot)) try(dbRollback(snapshot), silent = TRUE)
      stop(e)
    },
    finally = sluit_verbindingen()
  )

  if (!geslaagd) stop("Render-snapshot kon niet worden opgebouwd.")
  if (!file.rename(tijdelijk_pad, snapshot_pad)) {
    unlink(tijdelijk_pad)
    stop("Kon het gevalideerde Render-snapshot niet activeren.")
  }

  cat("✔ render_snapshot.duckdb is bijgewerkt met geaggregeerde dashboarddata.\n")
  invisible(snapshot_pad)
}
