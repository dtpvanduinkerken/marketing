CREATE OR REPLACE TABLE mart.social_media_top_posts AS

SELECT

  datum,
  platform,
  post_type,
  titel,
  categorie,

  views,

  likes,
  comments,
  shares,
  saves,

  (
    likes +
    comments +
    shares +
    saves
  ) AS engagement

FROM raw.social_media

ORDER BY views DESC;