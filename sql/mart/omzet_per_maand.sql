CREATE OR REPLACE VIEW mart.omzet_per_maand AS

WITH pricing_met_datum AS (
  SELECT
    COALESCE(
      TRY_CAST(datum AS DATE),
      CAST(TRY_STRPTIME(datum, '%d-%m-%Y') AS DATE),
      CAST(TRY_STRPTIME(datum, '%d/%m/%Y') AS DATE)
    ) AS verkoopdatum,
    omzet,
    discount
  FROM staging.personal_pricing
)

SELECT
  DATE_TRUNC('month', verkoopdatum) AS maand,
  ROUND(SUM(omzet), 2) AS omzet,
  ROUND(SUM(discount), 2) AS discount,
  COUNT(*) AS aantal_gebruikt

FROM pricing_met_datum

WHERE verkoopdatum IS NOT NULL

GROUP BY 1

ORDER BY 1;
