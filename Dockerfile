FROM rocker/shiny:latest

RUN rm -rf /srv/shiny-server/*

WORKDIR /srv/shiny-server/
ADD https://github.com/just-containers/s6-overlay/releases/download/v1.9.1.3/s6-overlay-amd64.tar.gz /tmp/
RUN tar xzf /tmp/s6-overlay-amd64.tar.gz -C /
ENTRYPOINT ["/init"]
RUN install2.r --error --skipinstalled \
    shinythemes \
    shinyWidgets \
    shinydashboard \
    recipes \
    factoextra \
    rstatix \
	radiant \
	purrr

COPY ./appv3.R ./app.R
USER shiny

EXPOSE 3838
ENTRYPOINT ["/init"]
