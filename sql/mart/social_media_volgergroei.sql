CREATE OR REPLACE TABLE mart.social_media_volgergroei AS

SELECT

  platform,

  MAX(volgers) AS huidige_volgers,

  MAX(volgers) - MIN(volgers) AS groei

FROM raw.social_media_volgers

GROUP BY platform;