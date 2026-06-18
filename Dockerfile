FROM rocker/shiny:latest

RUN R -e “install.packages(c(‘shiny’,‘shinydashboard’,‘DBI’,‘duckdb’,‘plotly’,‘dplyr’,‘DT’,‘googleAnalyticsR’), repos=‘https://cloud.r-project.org’)”

COPY . /srv/shiny-server/

EXPOSE 3838

CMD [”/usr/bin/shiny-server”]