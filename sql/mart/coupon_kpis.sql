CREATE OR REPLACE TABLE mart.coupon_kpis AS

SELECT

  SUM(verzonden) AS totaal_verzonden,
  SUM(ingeleverd) AS totaal_ingeleverd,
  SUM(verlopen) AS totaal_verlopen,
  SUM(openstaand) AS totaal_openstaand,

  ROUND(
    SUM(ingeleverd) * 100.0 /
    NULLIF(SUM(verzonden), 0),
    1
  ) AS inwisselpercentage,

  ROUND(
    SUM(discount),
    2
  ) AS totale_korting,

  ROUND(
    SUM(omzet),
    2
  ) AS totale_omzet

FROM raw.coupons;