# gwpcorMapper
__a web-based tool for interactive geographically weighted correlation mapping__

This project holds the source code for `gwpcorMapper`: an interactive mapping application
for geographically weighted correlation and partial correlation.

## Usage

You may choose to run gwpcorMapper locally or in a docker container. 

### Docker:

To run gwpcorMapper in Docker, make sure that you have install [docker](https://docs.docker.com/install/).
Then, build and launch the container by executing the following commands in a terminal:

```bash 
docker build -t gwpcormapper .
docker run -p 80:3838 gwpcormapper
```

You can use the application in a web browser by going to [http://localhost:8000](http://localhost:8000).

#### Map Styles

gwpcorMapper uses Mapbox for its basemap, so it is possible to change the
map style by supplying a mapbox token and style definition in the docker run command.
For example:

```bash
 docker run -e MAPBOX_TOKEN=<your mapbox token> -e STYLE=dark -p 80:3838 gwpcormapper
```

The default style used is `carto-darkmatter`, which does not require a mapbox token.

### Local:

To run the application locally, make sure the following dependencies are installed:

* R (>= 3.6.3)
* GDAL (>= 2.4.0)
* GEOS (>= 3.7.1)
* Proj.4 (>= 4.8.0)

Then launch the Shiny app in R Studio or your favorite R IDE.

If you dont use an IDE, you can run the following command from this project's directory:

```bash
R -e "shiny::runApp('gwpcormapper')"
```
