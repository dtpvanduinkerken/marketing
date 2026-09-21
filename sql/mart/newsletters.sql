CREATE OR REPLACE VIEW staging.newsletters AS
SELECT
  datum,
  campagne,
  verzonden     AS sent,
  geopend       AS opens,
  clicks,
  bounces,
  unsubscribes  AS unsubscribers
FROM raw.newsletters
WHERE EXTRACT(YEAR FROM datum) = EXTRACT(YEAR FROM CURRENT_DATE)
  AND LOWER(COALESCE(campagne, '')) NOT LIKE '%test%';

CREATE OR REPLACE VIEW mart.newsletter_kpis AS
SELECT
  SUM(sent) AS totaal_verzonden,
  SUM(opens) AS totaal_opens,
  SUM(clicks) AS totaal_clicks,
  SUM(bounces) AS totaal_bounces,
  SUM(unsubscribers) AS totaal_unsubscribers,
  ROUND(SUM(opens) * 100.0 / NULLIF(SUM(sent), 0), 2) AS open_rate,
  ROUND(SUM(clicks) * 100.0 / NULLIF(SUM(sent), 0), 2) AS ctr,
  ROUND(SUM(clicks) * 100.0 / NULLIF(SUM(opens), 0), 2) AS ctor
FROM staging.newsletters;
