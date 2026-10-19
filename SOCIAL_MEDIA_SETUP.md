# Social Media-koppeling zonder extra database

Het dashboard haalt Facebook- en Instagram-statistieken rechtstreeks op uit de officiële API's van Meta. Er is geen Aiven-, MySQL- of betaalde cronservice nodig. De gegevens worden maximaal twintig uur in het geheugen van de bestaande Render-webservice bewaard om onnodige API-aanvragen te voorkomen.

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
| `INSTAGRAM_ACCESS_TOKEN` | Token uit **API setup with Instagram login** |
| `INSTAGRAM_APP_ID` | Instagram App ID uit dezelfde Meta-pagina |
| `INSTAGRAM_APP_SECRET` | Instagram App Secret uit dezelfde Meta-pagina |
| `INSTAGRAM_ACCOUNT_ID` | Optioneel; zonder deze variabele wordt het account via `/me` bepaald |
| `INSTAGRAM_API_VERSION` | Optioneel, standaard `v26.0` |
| `INSTAGRAM_POSTS_LOOKBACK_DAYS` | Optioneel, standaard `180` |
| `INSTAGRAM_POST_LIMIT` | Optioneel, standaard `30` |

Sla de wijzigingen in Render op en herstart of deploy de bestaande webservice. Er zijn geen `SOCIAL_DB_*`-variabelen nodig.

Voor Instagram Login zijn minimaal `instagram_business_basic` en `instagram_business_manage_insights` nodig. Het Instagram-account moet een professioneel Business- of Creator-account zijn. De koppeling gebruikt `graph.instagram.com` en vereist geen Business Portfolio of gekoppelde Facebook-pagina.

## Token en rechten

Het Page Access Token moet voor pagina `135617146515071` minimaal beschikken over:

- `read_insights`
- `pages_read_engagement`
- de taak `ANALYZE` op de pagina

De app-ID en app-secret worden gebruikt om de geldigheid van het token te controleren. Zet tokens nooit in broncode, screenshots, logs of chatberichten. Een token dat zichtbaar is geweest moet worden ingetrokken en vervangen.

## Beschikbare geschiedenis

- Dagelijkse weergaven en interacties worden voor de door Meta ondersteunde historische periode rechtstreeks opgehaald.
- Het actuele volgersaantal komt rechtstreeks van de pagina.
- De historische volgersreeks komt uit de Page Insights-metric `page_follows`. Als Meta voor een periode geen oudere meting teruggeeft, blijft groei voor die selectie **Niet beschikbaar**.
- Voor Instagram toont de volgersgrafiek geen misleidende lijn wanneer Meta alleen de actuele totaalstand levert. De KPI **Groei in volgers** gebruikt, indien beschikbaar, het aantal nieuwe volgers minus het aantal ontvolgers binnen de geselecteerde periode.
- Instagram-interacties worden door Meta als totaal over de geselecteerde periode geleverd. Daarom kan de KPI wel gevuld zijn terwijl de dagelijkse interactiegrafiek uitlegt dat er geen dagreeks beschikbaar is.
- Berichtstatistieken worden live opgehaald voor maximaal het ingestelde aantal recente berichten.
- Likes en opmerkingen worden alleen getoond als het token daarvoor toestemming heeft. Zonder `pages_read_user_content` blijven deze velden **Niet beschikbaar**; de rest van het dashboard blijft werken.
- Als Meta ook de berichtenlijst blokkeert, slaat het dashboard dit onderdeel over. De beschikbare pagina-KPI's blijven dan zichtbaar met een waarschuwing bij de synchronisatiestatus.
- Een herstart van Render wist alleen de tijdelijke cache. De cijfers worden daarna opnieuw bij Meta opgehaald.

## Controleren

Na een nieuwe deploy open je **Social Media**. De eerste aanvraag kan langer duren doordat Meta de historische dagwaarden en berichtstatistieken levert. Daarna gebruikt de app de cache.

De lokale transformatietests gebruiken geen echte token:

```sh
Rscript --vanilla tests/test_social_sync.R
```
