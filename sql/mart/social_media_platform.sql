CREATE OR REPLACE TABLE mart.social_media_platform AS

SELECT

  platform,

  COUNT(*) AS posts,

  SUM(views) AS views,

  SUM(
    likes +
    comments +
    shares +
    saves
  ) AS engagement,

  ROUND(
    SUM(
      likes +
      comments +
      shares +
      saves
    ) * 100.0 /
    NULLIF(SUM(views), 0),
    2
  ) AS engagement_rate

FROM raw.social_media

GROUP BY platform

ORDER BY views DESC;