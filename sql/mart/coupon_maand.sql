CREATE OR REPLACE TABLE mart.coupon_maand AS

SELECT
  DATE_TRUNC('month', datum) AS maand,
  coupon_code,
  COUNT(*) AS ingeleverd,
  COUNT(DISTINCT customer_number) AS unieke_klanten,
  ROUND(SUM(ABS(discount)), 2) AS korting,
  ROUND(SUM(omzet), 2) AS omzet,
  ROUND(AVG(omzet), 2) AS gemiddelde_bonwaarde

FROM raw.coupons

GROUP BY 1, 2

ORDER BY 1, 2;
