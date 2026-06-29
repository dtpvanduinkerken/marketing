CREATE OR REPLACE VIEW staging.newsletters AS
SELECT
  datum,
  campagne,
  verzonden     AS sent,
  geopend       AS opens,
  clicks,
  bounces,
  unsubscribes  AS unsubscribers
FROM raw.newsletters;