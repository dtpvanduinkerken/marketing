# --------------------------------------------------
# Dockerfile voor Shiny dashboard op Render
# --------------------------------------------------

FROM rocker/shiny:4.4.1

# Systeemafhankelijkheden die sommige R-packages nodig hebben
# (curl/openssl/xml2 voor googleAnalyticsR/httr, libpq niet nodig hier maar
# duckdb heeft soms build-tools nodig)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# R packages installeren
# (let op: dit kan een paar minuten duren bij de eerste build)
RUN R -e "install.packages(c( \
    'shiny', \
    'shinydashboard', \
    'DBI', \
    'duckdb', \
    'plotly', \
    'dplyr', \
    'googleAnalyticsR', \
    'base64enc' \
    ), repos='https://packagemanager.posit.co/cran/__linux__/jammy/latest')"

# Expliciete, vaste HOME zetten voor zowel build als runtime. DuckDB slaat
# extensies standaard op in $HOME/.duckdb/extensions/... — als HOME tijdens
# runtime verschilt van tijdens de build (wat op sommige platforms gebeurt),
# vindt DuckDB de vooraf geïnstalleerde extensie niet terug en proberen
# queries 'm opnieuw te downloaden, wat zonder netwerktoegang faalt.
ENV HOME=/root
RUN mkdir -p ${HOME}

# DuckDB-extensie 'icu' (nodig voor datum/locale-functies in de mart-views)
# vooraf installeren tijdens de build, zodat tijdens runtime geen netwerk-
# toegang nodig is om 'm te downloaden.
RUN R -e "con <- DBI::dbConnect(duckdb::duckdb()); \
    DBI::dbExecute(con, 'INSTALL icu'); \
    DBI::dbExecute(con, 'LOAD icu'); \
    DBI::dbDisconnect(con, shutdown = TRUE)"

# App-bestanden kopiëren
# Verwacht structuur: app.R, bedrijf.duckdb, www/styles.css (indien gebruikt)
COPY . /srv/shiny-server/

# Eigenaarschap zetten zodat de shiny-server user erbij kan
RUN chown -R shiny:shiny /srv/shiny-server

# Render geeft de poort door via de PORT env var; shiny-server luistert
# standaard op 3838, dus we zetten Shiny direct via R op de juiste poort.
EXPOSE 3838

# We draaien de app rechtstreeks met R in plaats van shiny-server,
# zodat we de PORT environment variable van Render kunnen gebruiken.
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 3838)))"]