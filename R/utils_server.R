# Title     : gwpcorMapper Server Helper Functions
# Objective : Holds server-side functions to be used by gwpcorMapper.
# Created by: Joseph Percival
# Created on: 2021/02/26


get.viewExtent <- function(data) {
  bounds <- st_bbox(data)
  center.lon <- bounds$xmin + (bounds$xmax - bounds$xmin)/2
  center.lat <- bounds$ymin + (bounds$ymax - bounds$ymin)/2
  zoom.lon <- log(180/abs(center.lon - bounds$xmin))/log(2)
  zoom.lat <- log(90/abs(center.lat - bounds$ymin))/log(2)

  center <- c(center.lon, center.lat)
  zoom <- mean(zoom.lon, zoom.lat)
  return (
    list(center, zoom)
  )
}

dataLoader <- function() {
  modalDialog(
    selectInput(
      inputId = "filetype",
      label = 'Load Data',
      choices = c(
        `Select a file type` = '',
        c(
          "GPKG",
          "GeoJSON",
          "ESRI Shapefile",
          "CSV"
        )
      )
    ),
    conditionalPanel(
      condition = "input.filetype == 'CSV'",
      span('Please specify the longitude and latitude column names to read from.'),
      textInput(
        inputId = "x",
        label = "longitute column name"
      ),
      textInput(
        inputId = "y",
        label = "latidude column name"
      )
    ),
    conditionalPanel(
      condition = "input.filetype == 'ESRI Shapefile'",
      span('You must select and upload ALL shapefile required files (.shp, .shx, .dbf, etc.'),
    ),
    footer = tagList(
      div(style="display:inline",
          fluidRow(
            column(
              width = 4,
              modalButton(label = "Cancel")
            ),
            column(
              width = 8,
              conditionalPanel(
                condition = "input.filetype != '' && input.filetype != 'CSV' ||
                input.filetype == 'CSV' && input.x != '' && input.y != ''",
                uiOutput(outputId = "dynUpload")
              )
            )
          )
      )
    ),
    size = 's'
  )
}

getData <- function(filetype, upload, x, y) {
  # todo: add error handling and validation
  if (filetype == "ESRI Shapefile") {
    esri.files <- upload
    tempdirname <- dirname(esri.files$datapath[1])
    for (i in seq_len(nrow(esri.files))) {
      file.rename(
        esri.files$datapath[i],
        paste0(tempdirname, "/", esri.files$name[i])
      )
    }
    shapefile <- paste(
      tempdirname,
      esri.files$name[grep(pattern = "*.shp$", esri.files$name)],
      sep = "/"
    )
    data <- st_read(shapefile) %>% st_transform(4326)
    name <- esri.files$name[grep(pattern = "*.shp$", esri.files$name)]
  }
  else if (filetype == "CSV") {
    data <- st_read(
      upload$datapath,
      options=c(
        paste0("X_POSSIBLE_NAMES=", x),
        paste0("Y_POSSIBLE_NAMES=", y)
      )
    )
    name <- upload$name
  }
  else {
    data <- sf::st_read(upload$datapath) %>% st_transform(4326)
    name <- upload$name
  }

  num_row <- nrow(data)
  varnames <- data %>% st_drop_geometry() %>% colnames()
  dummy <- 1:num_row
  data <- data %>% cbind(dummy)

  return(list(data = data, name = name, vars = varnames))
}

calc.dMat <- function(data) {
  dp.locat <- sf::st_centroid(data) %>% sf::st_coordinates()
  # todo: allow user-specified distance measures
  dMat <- geodist::geodist(dp.locat, measure = "cheap")
  return(dMat)
}
