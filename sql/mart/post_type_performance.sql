CREATE OR REPLACE VIEW mart.post_type_performance AS

SELECT
    post_type,

    COUNT(*) AS aantal_posts,

    SUM(views) AS views,
    SUM(likes) AS likes,
    SUM(comments) AS comments,
    SUM(shares) AS shares,
    SUM(saves) AS saves,

    SUM(likes + comments + shares + saves) AS engagement,

    ROUND(
        SUM(likes + comments + shares + saves) * 100.0
        / NULLIF(SUM(views), 0),
        2
    ) AS engagement_rate,

    ROUND(AVG(views), 0) AS gemiddelde_views,

    ROUND(
        AVG(likes + comments + shares + saves),
        0
    ) AS gemiddelde_engagement

FROM raw.social_media

WHERE post_type IS NOT NULL
  AND post_type <> ''

GROUP BY post_type

ORDER BY engagement_rate DESC;