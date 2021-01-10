FROM rocker/geospatial:3.6.3
RUN export ADD=shiny && bash /etc/cont-init.d/add

RUN Rscript -e "install.packages(c('shinydashboard', 'leaflet', 'GWpcor', 'plotly'), repos='http://cran.rstudio.com/')"

COPY shiny-server.sh /usr/bin/shiny-server.sh
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY gwpcormapper/app.R /srv/shiny-server/

RUN chmod +x /usr/bin/shiny-server.sh
RUN chown -R 755 /srv/shiny-server/
RUN chown shiny:shiny /var/lib/shiny-server

USER shiny

CMD ["/usr/bin/shiny-server.sh"]