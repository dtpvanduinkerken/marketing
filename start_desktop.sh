#!/bin/bash

# Desktop Dashboard Start Script
# Start de desktop geoptimaliseerde versie van het dashboard

echo "🚀 Starten DataPlatform Desktop App..."
echo "======================================"

# Check of R is geïnstalleerd
if ! command -v R &> /dev/null; then
    echo "❌ Fout: R is niet geïnstalleerd of niet in PATH"
    echo "Installeer R via: brew install r  (macOS)"
    exit 1
fi

# Check of app_desktop.R bestaat
if [ ! -f "app_desktop.R" ]; then
    echo "❌ Fout: app_desktop.R niet gevonden in huidige directory"
    echo "Zorg dat je in de DataPlatform directory bent: cd /Users/dtp/DataPlatform"
    exit 1
fi

# Check of database bestaat
if [ ! -f "bedrijf.duckdb" ]; then
    echo "⚠️  Waarschuwing: bedrijf.duckdb niet gevonden"
    echo "Zorg dat je eerst 'Rscript update_data.r' hebt gedraaid"
    echo "Of stel DESKTOP_DB_PATH environment variable in"
fi

# Start de desktop app
echo "✅ Starten desktop app op http://127.0.0.1:3838"
echo "📱 Druk Ctrl+C om te stoppen"
echo ""

Rscript -e "shiny::runApp('app_desktop.R', host='127.0.0.1', port=3838, launch.browser=TRUE)"