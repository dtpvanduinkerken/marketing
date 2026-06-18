FROM rocker/shiny:4.4.0

# Systeemdependencies die duckdb / curl-gebaseerde packages nodig hebben
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server/app

# Eerst alleen install.R kopiëren zodat Docker de package-laag kan cachen
COPY install.R .
RUN Rscript install.R

# Rest van de app kopiëren
COPY . .

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp(appDir='/srv/shiny-server/app', host='0.0.0.0', port=3838)"]