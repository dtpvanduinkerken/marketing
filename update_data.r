# ==================================================
# UPDATE_DATA.R
# Workflow: nieuwe CSV-data toevoegen aan raw.* en
# vervolgens alle afhankelijke mart-tabellen herbouwen.
# ==================================================

library(duckdb)

con <- dbConnect(duckdb::duckdb(), "bedrijf.duckdb")

# --------------------------------------------------
# STAP 1: NIEUWE DATA TOEVOEGEN AAN RAW TABEL
# --------------------------------------------------
# Pas de tabelnaam en het CSV-pad aan naar jouw situatie.
# Kolommen in de CSV moeten exact overeenkomen (naam + type + volgorde)
# met de bestaande raw-tabel.

dbExecute(con, "
  INSERT INTO raw.coupons
  SELECT * FROM data/raw('afspraken.csv')
")

# Voeg hier extra INSERT's toe als je meerdere raw-tabellen update,
# bijv. raw.members, raw.social_media, raw.personal_pricing, etc.


# --------------------------------------------------
# STAP 2: MART-TABELLEN HERBOUWEN
# --------------------------------------------------
# Let op volgorde: mart.customer_360 (ontbreekt nog, zie chat) moet
# vóór mart.members_kpis gebouwd worden, want members_kpis leest uit
# customer_360.

# ---- COUPONS ----
dbExecute(con, "
CREATE OR REPLACE TABLE mart.coupon_kpis AS
SELECT
  SUM(verzonden) AS totaal_verzonden,
  SUM(ingeleverd) AS totaal_ingeleverd,
  SUM(verlopen) AS totaal_verlopen,
  SUM(openstaand) AS totaal_openstaand,
  ROUND(SUM(ingeleverd) * 100.0 / NULLIF(SUM(verzonden), 0), 1) AS inwisselpercentage,
  ROUND(SUM(discount), 2) AS totale_korting,
  ROUND(SUM(omzet), 2) AS totale_omzet
FROM raw.coupons;
")

dbExecute(con, "
CREATE OR REPLACE TABLE mart.coupon_maand AS
SELECT
  DATE_TRUNC('month', STRPTIME(datum, '%d-%m-%Y')) AS maand,
  SUM(ingeleverd) AS ingeleverd,
  ROUND(SUM(omzet), 2) AS omzet
FROM raw.coupons
GROUP BY 1
ORDER BY 1;
")

dbExecute(con, "
CREATE OR REPLACE TABLE mart.coupon_performance AS
SELECT
  coupon_code, campagne,
  SUM(verzonden) AS verzonden,
  SUM(ingeleverd) AS ingeleverd,
  SUM(verlopen) AS verlopen,
  SUM(openstaand) AS openstaand,
  ROUND(SUM(ingeleverd) * 100.0 / NULLIF(SUM(verzonden), 0), 1) AS inwisselpercentage,
  ROUND(SUM(discount), 2) AS korting,
  ROUND(SUM(omzet), 2) AS omzet
FROM raw.coupons
GROUP BY coupon_code, campagne
ORDER BY omzet DESC;
")

# ---- MEMBERS ----
dbExecute(con, "
CREATE OR REPLACE TABLE mart.members_activiteit AS
SELECT
  CASE
    WHEN laatste_aankoop IS NULL THEN 'Nooit actief'
    WHEN DATE_DIFF('day', laatste_aankoop, CURRENT_DATE) <= 365 THEN 'Actief'
    ELSE 'Slapend'
  END AS status,
  COUNT(*) AS aantal
FROM raw.members
GROUP BY 1;
")

dbExecute(con, "
CREATE OR REPLACE TABLE mart.members_groei AS
SELECT
  DATE_TRUNC('month', aanmelddatum) AS maand,
  COUNT(*) AS nieuwe_members
FROM raw.members
WHERE aanmelddatum IS NOT NULL
GROUP BY 1
ORDER BY 1;
")

dbExecute(con, "
CREATE OR REPLACE TABLE mart.members_woonplaats AS
SELECT
  woonplaats,
  COUNT(*) AS members
FROM raw.members
WHERE woonplaats IS NOT NULL
GROUP BY 1
ORDER BY members DESC;
")

# !! LET OP: mart.customer_360 ontbreekt nog -- members_kpis hieronder
# zal FALEN totdat je die query erbij zet (zie chat voor de ontbrekende
# mart-lijst). Zet die hier tussen vóór members_kpis.

dbExecute(con, "
CREATE OR REPLACE VIEW mart.members_kpis AS
SELECT
    COUNT(*) AS totaal_members,
    COUNT(CASE WHEN eerste_aankoop IS NOT NULL THEN 1 END) AS members_met_aanmelddatum,
    COUNT(DISTINCT woonplaats) AS aantal_woonplaatsen,
    COUNT(CASE WHEN laatste_aankoop >= CURRENT_DATE - INTERVAL 90 DAY THEN 1 END) AS actieve_members_90d,
    ROUND(AVG(totale_omzet), 2) AS gemiddelde_omzet,
    ROUND(AVG(pricing_uses), 2) AS gemiddelde_aankopen
FROM mart.customer_360;
")

# ---- PRICING ----
dbExecute(con, "
CREATE OR REPLACE VIEW mart.pricing_performance AS
SELECT
    pricing_code, categorie,
    COUNT(*) AS aantal_gebruikt,
    ROUND(SUM(COALESCE(omzet, 0)), 2) AS omzet,
    ROUND(SUM(COALESCE(discount, 0)), 2) AS discount,
    ROUND(SUM(COALESCE(omzet, 0)) / NULLIF(COUNT(*), 0), 2) AS omzet_per_gebruik
FROM raw.personal_pricing
WHERE pricing_code IS NOT NULL AND pricing_code <> ''
GROUP BY pricing_code, categorie
ORDER BY omzet DESC;
")

# ---- SOCIAL MEDIA ----
dbExecute(con, "
CREATE OR REPLACE TABLE mart.social_media_kpis AS
SELECT
  SUM(views) AS totaal_views,
  SUM(likes + comments + shares + saves) AS totaal_engagement,
  ROUND(SUM(likes + comments + shares + saves) * 100.0 / NULLIF(SUM(views), 0), 2) AS engagement_rate,
  AVG(watchTime) AS gemiddelde_watchtime,
  AVG(skipRate) AS gemiddelde_skiprate
FROM raw.social_media;
")

dbExecute(con, "
CREATE OR REPLACE TABLE mart.social_media_maand AS
SELECT
  DATE_TRUNC('month', STRPTIME(datum, '%d-%m-%Y')) AS maand,
  SUM(views) AS views,
  SUM(likes + comments + shares + saves) AS engagement
FROM raw.social_media
GROUP BY 1
ORDER BY 1;
")

dbExecute(con, "
CREATE OR REPLACE TABLE mart.social_media_platform AS
SELECT
  platform,
  COUNT(*) AS posts,
  SUM(views) AS views,
  SUM(likes + comments + shares + saves) AS engagement,
  ROUND(SUM(likes + comments + shares + saves) * 100.0 / NULLIF(SUM(views), 0), 2) AS engagement_rate
FROM raw.social_media
GROUP BY platform
ORDER BY views DESC;
")

dbExecute(con, "
CREATE OR REPLACE VIEW mart.post_performance AS
SELECT
    datum, platform, post_type, titel, categorie,
    views, likes, comments, shares, saves,
    (likes + comments + shares + saves) AS engagement,
    ROUND((likes + comments + shares + saves) * 100.0 / NULLIF(views, 0), 2) AS engagement_rate,
    watchTime, skipRate
FROM raw.social_media
ORDER BY engagement_rate DESC;
")

dbExecute(con, "
CREATE OR REPLACE VIEW mart.post_type_performance AS
SELECT
    post_type,
    COUNT(*) AS aantal_posts,
    SUM(views) AS views,
    SUM(likes) AS likes,
    SUM(comments) AS comments,
    SUM(shares) AS shares,
    SUM(saves) AS saves,
    SUM(likes + comments + shares + saves) AS engagement,
    ROUND(SUM(likes + comments + shares + saves) * 100.0 / NULLIF(SUM(views), 0), 2) AS engagement_rate,
    ROUND(AVG(views), 0) AS gemiddelde_views,
    ROUND(AVG(likes + comments + shares + saves), 0) AS gemiddelde_engagement
FROM raw.social_media
WHERE post_type IS NOT NULL AND post_type <> ''
GROUP BY post_type
ORDER BY engagement_rate DESC;
")

dbExecute(con, "
CREATE OR REPLACE TABLE mart.social_media_top_posts AS
SELECT
  datum, platform, post_type, titel, categorie,
  views, likes, comments, shares, saves,
  (likes + comments + shares + saves) AS engagement
FROM raw.social_media
ORDER BY views DESC;
")

dbExecute(con, "
CREATE OR REPLACE TABLE mart.social_media_volgergroei AS
SELECT
  platform,
  MAX(volgers) AS huidige_volgers,
  MAX(volgers) - MIN(volgers) AS groei
FROM raw.social_media_volgers
GROUP BY platform;
")

dbExecute(con, "
CREATE OR REPLACE TABLE mart.social_media_volgers AS
SELECT * FROM raw.social_media_volgers;
")

# ---- OVERIG ----
dbExecute(con, "
CREATE OR REPLACE VIEW mart.omzet_per_segment AS
SELECT
    k.segment,
    SUM(v.omzet) AS totale_omzet
FROM verkopen v
LEFT JOIN klanten k ON v.id = k.id
GROUP BY k.segment;
")

# !! TODO: nog ontbrekende marts toevoegen zodra je de SQL hebt:
#   mart.customer_360 (vereist voor members_kpis!)
#   mart.kpi_personal_pricing
#   mart.klant_kpis
#   mart.newsletter_kpis
#   mart.afspraken_kpis
#   mart.omzet_per_woonplaats
#   mart.omzet_per_maand
#   mart.klantgedrag

dbDisconnect(con, shutdown = TRUE)

message("Update voltooid. Vergeet niet: git add bedrijf.duckdb && git commit && git push")