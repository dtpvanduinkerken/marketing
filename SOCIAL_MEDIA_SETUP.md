# Social Media-koppeling

De Facebook-koppeling bestaat uit drie gescheiden onderdelen:

1. `sync_social_media.R` haalt statistieken op uit de Meta Graph API.
2. Aiven MySQL bewaart dagwaarden, berichten, metingen per bericht en synchronisatielogs.
3. Het Shiny-dashboard leest de opgeslagen gegevens alleen. Een tijdelijke API-storing wist dus geen historische cijfers.

De dagelijkse taak staat in `.github/workflows/social-media-sync.yml` en draait om 06:15 uur in `Europe/Amsterdam`, inclusief zomer- en wintertijd. De taak kan ook handmatig worden gestart via **GitHub → Actions → Social media synchroniseren → Run workflow**. Het dashboard hoeft daarvoor niet geopend te zijn.

## 1. Aiven MySQL

Maak of gebruik een Aiven MySQL-service en noteer host, poort, database, gebruiker, wachtwoord en het CA-certificaat. De eerste synchronisatie voert `sql/mysql/001_social_media.sql` automatisch uit. De tabellen zijn voorbereid op meerdere platforms en accounts; momenteel wordt alleen Facebook getoond.

Voeg deze geheimen toe aan de GitHub-repository via **Settings → Secrets and variables → Actions → Repository secrets**:

| Naam | Inhoud |
|---|---|
| `SOCIAL_DB_HOST` | Aiven-hostnaam |
| `SOCIAL_DB_PORT` | Aiven-poort |
| `SOCIAL_DB_NAME` | Databasenaam, vaak `defaultdb` |
| `SOCIAL_DB_USER` | Aiven-gebruiker |
| `SOCIAL_DB_PASSWORD` | Aiven-wachtwoord |
| `SOCIAL_DB_SSL_CA_PEM` | Volledige inhoud van het Aiven CA-certificaat |

Zet dezelfde zes waarden als geheime environment variables in Posit Connect Cloud of Render. Alleen de GitHub-taak heeft het Meta-token nodig; het dashboard niet.

## 2. Meta-token veilig vernieuwen

Gebruik voor de Facebook-pagina `135617146515071` een Page Access Token met minimaal:

- `read_insights`
- `pages_read_engagement`
- de taak `ANALYZE` op de pagina

Maak een nieuw gebruikerstoken via Meta Graph API Explorer onder de juiste Meta-app, wissel dit via de officiële OAuth-endpoint om voor een langlevend gebruikerstoken en vraag vervolgens `/me/accounts?fields=id,name,access_token,tasks` op. Bewaar het Page Access Token uit het antwoord als GitHub-secret `FACEBOOK_ACCESS_TOKEN`.

Voeg daarnaast toe:

| Naam | Type | Inhoud |
|---|---|---|
| `FACEBOOK_PAGE_ID` | GitHub secret | `135617146515071` |
| `FACEBOOK_ACCESS_TOKEN` | GitHub secret | Nieuw Page Access Token |
| `META_APP_ID` | GitHub secret | ID van de Meta-app |
| `META_APP_SECRET` | GitHub secret | App secret |
| `FACEBOOK_API_VERSION` | GitHub variable | `v26.0` |

De app-ID en app-secret worden alleen gebruikt om vóór de synchronisatie de geldigheid van het token te controleren. Zet tokens nooit in broncode, issues, screenshots of chatberichten. Trek een token dat ergens zichtbaar is geweest direct in en maak een nieuw token.

Een Page Access Token uit een langlevend gebruikerstoken heeft niet dezelfde levenscyclusgarantie als een System User-token. Zonder Business Portfolio kan het onder andere ongeldig worden door een wachtwoordwijziging, ingetrokken app-toegang of gewijzigde paginarechten. Controleer daarom de synchronisatiestatus in het dashboard en vernieuw het token zodra de GitHub-taak een authenticatiefout meldt.

## 3. Eerste ingebruikname

1. Voeg alle GitHub-secrets en de API-versie toe.
2. Start de workflow één keer handmatig.
3. Controleer in de workflowlog alleen de aantallen; er hoort nooit een token te worden afgedrukt.
4. Controleer in Aiven of de vijf `social_*`-tabellen zijn aangemaakt.
5. Voeg de Aiven-variabelen toe aan het dashboardplatform en publiceer de app opnieuw.
6. Open **Social Media** en controleer de regel **Laatst bijgewerkt**.

## Meetdefinities en beperkingen

- Weergaven en interacties zijn dagelijkse Page Insights en worden binnen de gekozen periode opgeteld.
- De vergelijking gebruikt de direct voorafgaande periode met hetzelfde aantal dagen.
- Volgers zijn momentopnamen. Historische volgersgroei ontstaat vanaf de eerste succesvolle dagelijkse synchronisatie; Meta levert hiervoor niet altijd een volledige historische reeks.
- Berichtstatistieken zijn de laatst opgeslagen lifetime-meting van een bericht. De standaard terugkijkperiode is 180 dagen en kan via `FACEBOOK_POSTS_LOOKBACK_DAYS` worden aangepast.
- Niet elk berichttype levert elke metric. Een ontbrekende metric verschijnt als **Niet beschikbaar**, niet als nul.
- Page Insights kan maximaal de door Meta beschikbare historische periode terughalen. De synchronisatie probeert maximaal twee jaar en herleest de laatste zeven dagen om late correcties mee te nemen.
- Instagram staat bewust nog niet in de interface. De database kan later een Instagram-account als apart platform opslaan.

## Lokaal controleren

De transformatietests gebruiken geen echte token of database:

```sh
Rscript --vanilla tests/test_social_sync.R
```

Een echte synchronisatie vereist alle Meta- en Aiven-variabelen:

```sh
Rscript sync_social_media.R
```
