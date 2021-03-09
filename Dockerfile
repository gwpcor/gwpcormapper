FROM rocker/r-ver:4.0.3
RUN apt-get update && apt-get install -y  gdal-bin git-core libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libicu-dev libpng-dev libssl-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.4.5")'
RUN Rscript -e 'remotes::install_version("Rcpp",upgrade="never", version = "1.0.6")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.3")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.1.1")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.30")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("viridis",upgrade="never", version = "0.5.1")'
RUN Rscript -e 'remotes::install_version("crosstalk",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.0.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.6")'
RUN Rscript -e 'remotes::install_version("corpcor",upgrade="never", version = "1.6.9")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.0.4.1")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.9.3")'
RUN Rscript -e 'remotes::install_version("geodist",upgrade="never", version = "0.0.6")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "0.9-7")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.17")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.2.1")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');gwpcormapper::run_app()"
