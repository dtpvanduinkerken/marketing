CREATE OR REPLACE TABLE mart.coupon_maand AS

SELECT

  DATE_TRUNC(
    'month',
    STRPTIME(
      datum,
      '%d-%m-%Y'
    )
  ) AS maand,

  SUM(ingeleverd) AS ingeleverd,

  ROUND(
    SUM(omzet),
    2
  ) AS omzet

FROM raw.coupons

GROUP BY 1

ORDER BY 1;