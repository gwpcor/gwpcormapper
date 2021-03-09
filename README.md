# gwpcorMapper
__a web-based tool for interactive geographically weighted correlation mapping__

  <!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
  <!-- badges: end -->

This project holds the source code for `gwpcorMapper`: an R package for exploratory spatial data analysis
 through the interactive mapping of geographically weighted correlation and partial correlation coefficients.


## Installation and Usage

Install the `gwpcormapper` package using `remotes` in an R terminal then launch the application by: 

```R
remotes::install_github("gwpcor/gwpcormapper")
gwpcormapper::run_app()
```

or build and launch using docker:

```
docker build -t gwpcormapper .
docker run -p 80:80 gwpcormapper 
```

To run via docker, please ensure that [docker](https://docs.docker.com/install/) is installed.

Please see the getting started vignette for more detailed instructions and examples in using the `gwpcormapper` package.

## Map Styles (Base Maps)

The default basemap used is [CARTO DarkMatter](https://carto.com/blog/getting-to-know-positron-and-dark-matter/) 
 however, this may be changed by declaring either a web-tile source URL or by specifying a [mapbox style](https://docs.mapbox.com/api/maps/styles/)
 as an environmental variable. Please follow the guide for detailed instructions on using custom base maps.

## Local Installation (for development):

To run the application locally, make sure the following dependencies are installed:

* R (>= 3.6.3)
* GDAL (>= 2.4.0)
* GEOS (>= 3.7.1)
* Proj.4 (>= 4.8.0)

Then launch the Shiny app by running the code at `dev/run_dev.R`
