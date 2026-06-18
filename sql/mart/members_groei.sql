CREATE OR REPLACE TABLE mart.members_groei AS

SELECT
  DATE_TRUNC('month', aanmelddatum) AS maand,
  COUNT(*) AS nieuwe_members
FROM raw.members
WHERE aanmelddatum IS NOT NULL
GROUP BY 1
ORDER BY 1;