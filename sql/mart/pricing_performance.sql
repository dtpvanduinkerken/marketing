CREATE OR REPLACE VIEW mart.pricing_performance AS

SELECT

    pricing_code,

    categorie,

    COUNT(*) AS aantal_gebruikt,

    ROUND(
        SUM(COALESCE(omzet, 0)),
        2
    ) AS omzet,

    ROUND(
        SUM(COALESCE(discount, 0)),
        2
    ) AS discount,

    ROUND(
        SUM(COALESCE(omzet, 0))
        /
        NULLIF(COUNT(*), 0),
        2
    ) AS omzet_per_gebruik

FROM raw.personal_pricing

WHERE pricing_code IS NOT NULL
  AND pricing_code <> ''

GROUP BY
    pricing_code,
    categorie

ORDER BY omzet DESC;