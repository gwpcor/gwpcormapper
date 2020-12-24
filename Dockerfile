FROM rocker/geospatial:3.6.3
RUN Rscript -e "install.packages(c('shiny', 'shinyjs', 'shinythemes', 'shinydashboard', 'remotes', 'leaflet', 'corpcor','doParallel', 'here', 'spdplyr', 'GWmodel', 'plotly'), repos='http://cran.rstudio.com/')"
RUN Rscript -e "remotes::install_github('naru-T/MyRMiscFunc')"
RUN Rscript -e "remotes::install_github('naru-T/GWpcor')"

COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod +x /usr/bin/shiny-server.sh

COPY gwpcormapper/app.R /srv/shiny-server/

RUN chown -R rstudio:rstudio /srv/shiny-server/*

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

RUN export ADD=shiny && bash /etc/cont-init.d/add

CMD ["/usr/bin/shiny-server.sh"]