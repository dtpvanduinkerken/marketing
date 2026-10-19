source("social_sync.R")

resultaat <- sync_facebook()
cat(
  "Social Media-synchronisatie voltooid | dagelijkse rijen:", resultaat$daily_rows,
  "| berichten:", resultaat$post_rows, "\n"
)

