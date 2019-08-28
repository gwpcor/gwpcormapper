FROM rocker/geospatial:3.5.2
RUN Rscript -e "install.packages(c('shiny', 'shinythemes','shinydashboard','remotes','leaflet','corpcor','doParallel','here','spdplyr','GWmodel', 'plotly'), repos='http://cran.rstudio.com/')"
RUN Rscript -e "remotes::install_github('naru-T/MyRMiscFunc')"

RUN mkdir -p /home/rstudio/ShinyApps
WORKDIR /home/rstudio/ShinyApps/
RUN chown rstudio:rstudio -R /home/rstudio/ShinyApps
RUN chmod -R 775 /home/rstudio/ShinyApps
COPY ./codes/shiny_code.R /home/rstudio/ShinyApps/
RUN chmod -R 775 /home/rstudio/ShinyApps/shiny_code.R
RUN chown rstudio:rstudio -R /home/rstudio/ShinyApps/shiny_code.R


RUN mkdir -p /home/rstudio/pkg
RUN chown rstudio:rstudio -R /home/rstudio/pkg
RUN chmod -R 775 /home/rstudio/pkg
COPY ./pkg/gwpcor_0.1.1.tar.gz /home/rstudio/pkg/
RUN chown rstudio:rstudio -R /home/rstudio/pkg/gwpcor_0.1.1.tar.gz
RUN chmod -R 775 /home/rstudio/pkg/gwpcor_0.1.1.tar.gz

RUN Rscript -e "install.packages('/home/rstudio/pkg/gwpcor_0.1.1.tar.gz', repos = NULL, type = 'source')"
