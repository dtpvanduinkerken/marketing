CREATE OR REPLACE TABLE mart.social_media_kpis AS

SELECT

  SUM(views) AS totaal_views,

  SUM(
    likes +
    comments +
    shares +
    saves
  ) AS totaal_engagement,

  ROUND(
    SUM(
      likes +
      comments +
      shares +
      saves
    ) * 100.0 /
    NULLIF(SUM(views), 0),
    2
  ) AS engagement_rate,

  AVG(watchTime) AS gemiddelde_watchtime,

  AVG(skipRate) AS gemiddelde_skiprate

FROM raw.social_media;