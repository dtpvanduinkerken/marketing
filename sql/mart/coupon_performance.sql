CREATE OR REPLACE TABLE mart.coupon_performance AS

SELECT

  coupon_code,
  campagne,

  SUM(verzonden) AS verzonden,
  SUM(ingeleverd) AS ingeleverd,
  SUM(verlopen) AS verlopen,
  SUM(openstaand) AS openstaand,

  ROUND(
    SUM(ingeleverd) * 100.0 /
    NULLIF(SUM(verzonden), 0),
    1
  ) AS inwisselpercentage,

  ROUND(
    SUM(discount),
    2
  ) AS korting,

  ROUND(
    SUM(omzet),
    2
  ) AS omzet

FROM raw.coupons

GROUP BY
  coupon_code,
  campagne

ORDER BY omzet DESC;