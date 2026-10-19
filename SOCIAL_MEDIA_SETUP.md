# Social Media-koppeling

De Facebook-koppeling bestaat uit drie onderdelen:

1. `social_scheduler.R` controleert vanuit de bestaande Render-webservice ieder uur of een synchronisatie nodig is; `sync_social_media.R` haalt de statistieken op.
2. Aiven MySQL bewaart dagwaarden, berichten, metingen per bericht en synchronisatielogs.
3. Het Shiny-dashboard leest de opgeslagen gegevens alleen. Een tijdelijke API-storing wist dus geen historische cijfers.

Er is geen extra betaalde Render Cron Job nodig. Bij een continu actieve webservice loopt de controle automatisch. Een gratis Render-webservice kan na inactiviteit slapen; zodra de service weer wordt gewekt, wordt een gemiste synchronisatie automatisch ingehaald. Render garandeert op het gratis webserviceplan daarom geen uitvoering op een exact dagelijks tijdstip.

## 1. Aiven MySQL

Maak of gebruik een Aiven MySQL-service en noteer host, poort, database, gebruiker, wachtwoord en het CA-certificaat. De eerste synchronisatie voert `sql/mysql/001_social_media.sql` automatisch uit. De tabellen zijn voorbereid op meerdere platforms en accounts; momenteel wordt alleen Facebook getoond.

Voeg deze variabelen toe aan de bestaande Render-webservice:

| Naam | Inhoud |
|---|---|
| `SOCIAL_DB_HOST` | Aiven-hostnaam |
| `SOCIAL_DB_PORT` | Aiven-poort |
| `SOCIAL_DB_NAME` | Databasenaam, vaak `defaultdb` |
| `SOCIAL_DB_USER` | Aiven-gebruiker |
| `SOCIAL_DB_PASSWORD` | Aiven-wachtwoord |
| `SOCIAL_DB_SSL_CA_PEM` | Volledige inhoud van het Aiven CA-certificaat |

Dezelfde Render-webservice gebruikt deze waarden voor zowel het dashboard als de synchronisatie.

## 2. Meta-token veilig vernieuwen

Gebruik voor de Facebook-pagina `135617146515071` een Page Access Token met minimaal:

- `read_insights`
- `pages_read_engagement`
- de taak `ANALYZE` op de pagina

Maak een nieuw gebruikerstoken via Meta Graph API Explorer onder de juiste Meta-app, wissel dit via de officiële OAuth-endpoint om voor een langlevend gebruikerstoken en vraag vervolgens `/me/accounts?fields=id,name,access_token,tasks` op. Bewaar het Page Access Token uitsluitend als Render environment variable `FACEBOOK_ACCESS_TOKEN`.

Voeg daarnaast toe:

| Naam | Inhoud |
|---|---|
| `FACEBOOK_PAGE_ID` | `135617146515071` |
| `FACEBOOK_ACCESS_TOKEN` | Nieuw Page Access Token |
| `META_APP_ID` | ID van de Meta-app |
| `META_APP_SECRET` | App secret |
| `FACEBOOK_API_VERSION` | `v26.0` |

De app-ID en app-secret worden alleen gebruikt om vóór de synchronisatie de geldigheid van het token te controleren. Zet tokens nooit in broncode, issues, screenshots of chatberichten. Trek een token dat ergens zichtbaar is geweest direct in en maak een nieuw token.

Een Page Access Token uit een langlevend gebruikerstoken heeft niet dezelfde levenscyclusgarantie als een System User-token. Zonder Business Portfolio kan het onder andere ongeldig worden door een wachtwoordwijziging, ingetrokken app-toegang of gewijzigde paginarechten. Controleer daarom de synchronisatiestatus en de Render-log en vernieuw het token zodra daar een authenticatiefout verschijnt.

## 3. Eerste ingebruikname

1. Voeg alle Meta- en Aiven-variabelen toe aan de bestaande Render-webservice.
2. Publiceer of herstart de webservice. De eerste controle start ongeveer dertig seconden na het opstarten.
3. Controleer in de Render-log alleen de aantallen; er hoort nooit een token te worden afgedrukt.
4. Controleer in Aiven of de vijf `social_*`-tabellen zijn aangemaakt.
5. Open **Social Media** en controleer de regel **Laatst bijgewerkt**.

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
