CREATE OR REPLACE TABLE mart.coupon_performance AS

SELECT
  coupon_code,
  COUNT(*) AS ingeleverd,
  COUNT(DISTINCT customer_number) AS unieke_klanten,
  ROUND(SUM(ABS(discount)), 2) AS korting,
  ROUND(SUM(omzet), 2) AS omzet,
  ROUND(AVG(omzet), 2) AS gemiddelde_bonwaarde,
  ROUND(AVG(ABS(discount)), 2) AS gemiddelde_korting,
  ROUND(SUM(ABS(discount)) * 100.0 / NULLIF(SUM(omzet), 0), 1) AS korting_omzet_pct,
  SUM(CASE WHEN omzet < 0 THEN 1 ELSE 0 END) AS retourtransacties,
  MIN(datum) AS eerste_inleverdatum,
  MAX(datum) AS laatste_inleverdatum

FROM raw.coupons

GROUP BY coupon_code

ORDER BY omzet DESC;
