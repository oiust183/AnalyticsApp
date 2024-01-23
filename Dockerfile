FROM rocker/shiny:latest

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

