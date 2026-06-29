CREATE OR REPLACE TABLE mart.social_media_maand AS

SELECT

    DATE_TRUNC(
        'month',
        datum
    ) AS maand,

    SUM(views) AS views,

    SUM(
        likes +
        comments +
        shares +
        saves
    ) AS engagement

FROM raw.social_media

GROUP BY 1

ORDER BY 1;