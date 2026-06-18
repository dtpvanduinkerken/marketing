FROM rocker/shiny:4.4.0

RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server/app

COPY install.R .
RUN Rscript install.R

COPY . .

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp(appDir='/srv/shiny-server/app', host='0.0.0.0', port=3838)"]