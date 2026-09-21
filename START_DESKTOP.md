# Desktop App Startinstructies

## Wat is dit?

Dit is een **desktop versie** van je dashboard die alleen lokaal draait op jouw computer. Het is geoptimaliseerd voor snelheid en gebruikt de volledige lokale database (`bedrijf.duckdb`) in plaats van de Render snapshot.

## Belangrijkste verschillen met Render versie

1. **Geen wijzigingen aan Render setup** - `app.R` blijft volledig intact
2. **Snellere performance** - Geen cold starts, directe disk access, volledige CPU/RAM
3. **Lazy loading** - Data wordt pas geladen wanneer een tabblad wordt geopend
4. **Lokale database** - Gebruikt `bedrijf.duckdb` in plaats van `render_snapshot.duckdb`
5. **GA4 optie** - Kan lokale token gebruiken voor Google Analytics

## Hoe te starten

### Optie 1: Via RStudio

1. Open RStudio
2. Open het project: `DataPlatform.Rproj`
3. Run het desktop script:
```r
source("app_desktop.R")
```

### Optie 2: Via command line

```bash
cd /Users/dtp/DataPlatform
Rscript -e "shiny::runApp('app_desktop.R', launch.browser = TRUE)"
```

### Optie 3: Maak een start script (aanbevolen)

Ik heb een start script voor je gemaakt:

```bash
./start_desktop.sh
```

## Environment Variables (optioneel)

Je kunt het database pad aanpassen via environment variable:

```bash
export DESKTOP_DB_PATH="/pad/naar/jouw/bedrijf.duckdb"
./start_desktop.sh
```

Standaard gebruikt het: `bedrijf.duckdb` in de project directory.

## Performance Tips

De desktop versie is al geoptimaliseerd, maar je kunt nog meer snelheid halen:

1. **Zorg dat je database up-to-date is**:
```bash
Rscript update_data.r
```

2. **Sluit andere zware applicaties** tijdens gebruik voor maximale CPU

3. **Gebruik een SSD** als je database op een HDD staat (groot verschil)

## Troubleshooting

### "Database niet gevonden"
Zorg dat `bedrijf.duckdb` in de project directory staat of stel `DESKTOP_DB_PATH` in.

### "GA authentication failed"
De desktop versie werkt prima zonder GA data. Zet `ga_token.rds` in de project directory als je live GA data wilt.

### App start traag
De eerste keer is wat trager door R package loading. Daarna is het veel sneller dan Render.

## Data Update

Om je desktop database bij te werken:

```bash
Rscript update_data.r
```

Dit update zowel `bedrijf.duckdb` (desktop) als `render_snapshot.duckdb` (Render).

## Veiligheid

- Desktop versie heeft **geen** API keys of secrets nodig
- Draait volledig lokaal, geen data verlaat jouw computer
- Geen internet connectie nodig (behalve voor optionele GA data)