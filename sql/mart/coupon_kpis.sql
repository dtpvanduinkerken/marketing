CREATE OR REPLACE TABLE mart.coupon_kpis AS

SELECT
  COUNT(*) AS totaal_ingeleverd,
  COUNT(DISTINCT customer_number) AS unieke_klanten,
  COUNT(DISTINCT coupon_code) AS actieve_coupons,
  ROUND(SUM(ABS(discount)), 2) AS totale_korting,
  ROUND(SUM(omzet), 2) AS totale_omzet,
  ROUND(AVG(omzet), 2) AS gemiddelde_bonwaarde,
  ROUND(AVG(ABS(discount)), 2) AS gemiddelde_korting,
  ROUND(SUM(ABS(discount)) * 100.0 / NULLIF(SUM(omzet), 0), 1) AS korting_omzet_pct,
  SUM(CASE WHEN omzet < 0 THEN 1 ELSE 0 END) AS retourtransacties,
  MIN(datum) AS eerste_inleverdatum,
  MAX(datum) AS laatste_inleverdatum

FROM raw.coupons;
