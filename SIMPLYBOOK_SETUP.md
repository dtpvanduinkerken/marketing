# SimplyBook-koppeling

Het dashboard kan afspraken rechtstreeks uit SimplyBook.me laden. Zonder
SimplyBook-instellingen blijft de bestaande `data/raw/afspraken.csv` als bron
werken.

## Benodigde instellingen

Activeer in SimplyBook.me de API Custom Feature en maak onder **Settings > API
User Keys** een aparte API User Key voor het dashboard. Stel daarna in de
omgeving waarin `update_data.r` draait deze variabelen in:

- `SIMPLYBOOK_COMPANY_LOGIN`: de bedrijfslogin van SimplyBook
- `SIMPLYBOOK_USER_LOGIN`: de gebruikerslogin met leesrechten op afspraken
- `SIMPLYBOOK_USER_KEY`: de API User Key (`api_user_key_...`), niet het gewone wachtwoord
- `SIMPLYBOOK_DATE_FROM`: optioneel; standaard `2010-01-01`
- `SIMPLYBOOK_DATE_TO`: optioneel; standaard vijf jaar vanaf vandaag

Sla deze waarden niet op in Git of in een CSV-bestand. Bij iedere uitvoering
van `update_data.r` worden de afspraken opnieuw opgehaald en worden de
dashboardtabellen en het veilige Render-snapshot bijgewerkt.

De import bewaart alleen de velden die het dashboard nodig heeft:
afspraakdatum, dienst, annuleringsstatus en categorie. Klant-ID's, namen,
e-mailadressen en telefoonnummers worden niet opgeslagen.
