CREATE OR REPLACE TABLE mart.members_woonplaats AS

SELECT
  woonplaats,
  COUNT(*) AS members
FROM raw.members
WHERE woonplaats IS NOT NULL
GROUP BY 1
ORDER BY members DESC;