CREATE OR REPLACE VIEW mart.post_performance AS

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

    (likes + comments + shares + saves) AS engagement,

    ROUND(
        (likes + comments + shares + saves) * 100.0
        / NULLIF(views, 0),
        2
    ) AS engagement_rate,

    watchTime,
    skipRate

FROM raw.social_media

ORDER BY engagement_rate DESC;