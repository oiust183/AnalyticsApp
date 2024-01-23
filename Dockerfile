FROM rocker/shiny:latest

RUN rm -rf /srv/shiny-server/*
ADD https://github.com/just-containers/s6-overlay/releases/download/v${S6_OVERLAY_VERSION}/s6-overlay-noarch.tar.xz /tmp
RUN tar -C / -Jxpf /tmp/s6-overlay-noarch.tar.xz
ADD https://github.com/just-containers/s6-overlay/releases/download/v${S6_OVERLAY_VERSION}/s6-overlay-x86_64.tar.xz /tmp
RUN tar -C / -Jxpf /tmp/s6-overlay-x86_64.tar.xz
WORKDIR /srv/shiny-server/

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
