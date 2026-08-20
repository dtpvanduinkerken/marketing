CREATE OR REPLACE TABLE mart.members_activiteit AS

SELECT

  CASE

    WHEN laatste_aankoop IS NULL
      THEN 'Nooit actief'

    WHEN today() - laatste_aankoop <= 365
      THEN 'Actief'

    ELSE 'Slapend'

  END AS status,

  COUNT(*) AS aantal

FROM raw.members

GROUP BY 1;
