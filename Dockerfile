FROM rocker/geospatial:3.6.3
RUN export ADD=shiny && bash /etc/cont-init.d/add

RUN Rscript -e "install.packages(c('shinydashboard', 'leaflet', 'plotly', 'geodist', 'corpcor', 'shinyjs'), repos='http://cran.rstudio.com/')"
RUN mkdir /srv/shiny-server/gwpcormapper

COPY shiny-server.sh /usr/bin/shiny-server.sh
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY gwpcormapper/app.R /srv/shiny-server/
COPY gwpcormapper/helpers.R /srv/shiny-server/gwpcormapper/
COPY gwpcormapper/gwpcor.cpp /srv/shiny-server/gwpcormapper/

RUN chmod +x /usr/bin/shiny-server.sh
RUN chown -R 755 /srv/shiny-server/
RUN chown shiny:shiny /var/lib/shiny-server

USER shiny

CMD ["/usr/bin/shiny-server.sh"]