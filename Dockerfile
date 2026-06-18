FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git

RUN R -e "install.packages(c('DBI','duckdb','dplyr','plotly','shinydashboard','googleAnalyticsR'), repos='https://cloud.r-project.org')"

COPY . /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]