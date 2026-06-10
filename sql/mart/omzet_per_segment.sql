CREATE OR REPLACE VIEW mart.omzet_per_segment AS

SELECT
    k.segment,
    SUM(v.omzet) AS totale_omzet
FROM verkopen v
LEFT JOIN klanten k
    ON v.id = k.id
GROUP BY k.segment;