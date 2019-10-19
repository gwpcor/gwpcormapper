FROM rocker/geospatial:3.6.1
RUN Rscript -e "install.packages(c('shiny', 'shinyjs', 'shinythemes','shinydashboard','remotes','leaflet','corpcor','doParallel','here','spdplyr','GWmodel', 'plotly'), repos='http://cran.rstudio.com/')"
RUN Rscript -e "remotes::install_github('naru-T/MyRMiscFunc')"

ARG PASSWORD=password
ARG MAPBOX_TOKEN=token

ENV PASSWORD=${PASSWORD}
ENV MAPBOX_TOKEN=${MAPBOX_TOKEN}

COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod +x /usr/bin/shiny-server.sh

COPY gwpcormapper/app.R /srv/shiny-server/
COPY gwpcormapper/gwpcor_parallel_func.R /srv/shiny-server/

RUN chown -R rstudio:rstudio /srv/shiny-server/*

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

COPY ./pkg/gwpcor_0.1.1.tar.gz /home/shiny/pkg/
RUN chown rstudio:rstudio -R /home/shiny/pkg/gwpcor_0.1.1.tar.gz
RUN chmod -R 775 /home/shiny/pkg/gwpcor_0.1.1.tar.gz

RUN Rscript -e "install.packages('/home/shiny/pkg/gwpcor_0.1.1.tar.gz', repos = NULL, type = 'source')"

RUN export ADD=shiny && bash /etc/cont-init.d/add

CMD ["/usr/bin/shiny-server.sh"]