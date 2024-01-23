FROM rocker/shiny:latest
RUN rm -rf /srv/shiny-server/*
RUN install2.r --error --skipinstalled \
   shinythemes \
  shinyWidgets \
  shinydashboard \
  recipes \
  factoextra \
  rstatix \
	radiant \
	purrr

COPY ./appv3.R /srv/shiny-server/index.R

