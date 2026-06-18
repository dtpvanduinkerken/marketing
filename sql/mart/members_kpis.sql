CREATE OR REPLACE VIEW mart.members_kpis AS

SELECT
    COUNT(*) AS totaal_members,

    COUNT(
        CASE
            WHEN eerste_aankoop IS NOT NULL
            THEN 1
        END
    ) AS members_met_aanmelddatum,

    COUNT(DISTINCT woonplaats) AS aantal_woonplaatsen,

    COUNT(
        CASE
            WHEN laatste_aankoop >= CURRENT_DATE - INTERVAL 90 DAY
            THEN 1
        END
    ) AS actieve_members_90d,

    ROUND(
        AVG(totale_omzet),
        2
    ) AS gemiddelde_omzet,

    ROUND(
        AVG(pricing_uses),
        2
    ) AS gemiddelde_aankopen

FROM mart.customer_360;