# Social Media-koppeling zonder extra database

Het dashboard haalt Facebook-statistieken rechtstreeks op uit de Meta Graph API. Er is geen Aiven-, MySQL- of betaalde cronservice nodig. De gegevens worden maximaal twintig uur in het geheugen van de bestaande Render-webservice bewaard om onnodige API-aanvragen te voorkomen.

## Environment variables in Render

Voeg aan de bestaande dashboard-webservice uitsluitend deze variabelen toe:

| Naam | Inhoud |
|---|---|
| `FACEBOOK_PAGE_ID` | `135617146515071` |
| `FACEBOOK_ACCESS_TOKEN` | Nieuw Page Access Token |
| `FACEBOOK_API_VERSION` | `v26.0` |
| `META_APP_ID` | ID van de Meta-app |
| `META_APP_SECRET` | App secret |
| `FACEBOOK_POSTS_LOOKBACK_DAYS` | Optioneel, standaard `180` |
| `FACEBOOK_POST_LIMIT` | Optioneel, standaard `30` |

Sla de wijzigingen in Render op en herstart of deploy de bestaande webservice. Er zijn geen `SOCIAL_DB_*`-variabelen nodig.

## Token en rechten

Het Page Access Token moet voor pagina `135617146515071` minimaal beschikken over:

- `read_insights`
- `pages_read_engagement`
- de taak `ANALYZE` op de pagina

De app-ID en app-secret worden gebruikt om de geldigheid van het token te controleren. Zet tokens nooit in broncode, screenshots, logs of chatberichten. Een token dat zichtbaar is geweest moet worden ingetrokken en vervangen.

## Beschikbare geschiedenis

- Dagelijkse weergaven en interacties worden voor de door Meta ondersteunde historische periode rechtstreeks opgehaald.
- Het actuele volgersaantal komt rechtstreeks van de pagina.
- Meta levert via deze koppeling geen betrouwbare historische reeks van dagelijkse volgersmomenten. Daarom toont het dashboard geen verzonnen volgersgroei.
- Berichtstatistieken worden live opgehaald voor maximaal het ingestelde aantal recente berichten.
- Een herstart van Render wist alleen de tijdelijke cache. De cijfers worden daarna opnieuw bij Meta opgehaald.

## Controleren

Na een nieuwe deploy open je **Social Media**. De eerste aanvraag kan langer duren doordat Meta de historische dagwaarden en berichtstatistieken levert. Daarna gebruikt de app de cache.

De lokale transformatietests gebruiken geen echte token:

```sh
Rscript --vanilla tests/test_social_sync.R
```
