# Title     : gwpcorMapper
# Objective : Interactive mapping for geographically weighted correlation and partial correlation with R Shiny.
# Created by: Joseph Percival and Narumasa Tsutsumida
# Created on: 2019/10/10
library(shiny)
library(shinydashboard)
library(sf)
library(tidyverse)
library(geodist)
library(plotly)
library(crosstalk)
library(viridis)
library(leaflet)
library(tools)
library(shinyjs)

source("gwpcormapper/helpers.R")

readRenviron(".env")

style <- Sys.getenv("STYLE")
token <- Sys.getenv("MAPBOX_TOKEN")
source <- Sys.getenv("SOURCE")
layers <- NULL

if (style == '' && source == '') {
  style <- 'carto-darkmatter'
}

if (source != '') {
  # override style if tile layer is given!
  style <- 'white-bg'
  layers <- list(list(
    below = 'traces',
    sourcetype = "raster",
    source = list(source)))
}

if (token == '') {
  Sys.setenv('MAPBOX_TOKEN' = 'token')
}

mapbox.layout <- list(
  style = style,
  layers = layers
)

# define color schemes
rpal <- viridis_pal(option = "D")(11)
rpal_plotly <- list()
rang <- seq_along(rpal)
for (ind in rang) {
  rpal_plotly[[ind]] <- list(
    (ind - min(rang))/(max(rang) - min(rang)),
    rpal[ind]
  )
}
pal1 <- colorBin(rpal, c(-1,1), bins=11, na.color = "#bdbdbd")

supported.types <- c(
  "GPKG",
  "GeoJSON",
  "ESRI Shapefile",
  "CSV"
)

ui <- dashboardPage(
  dashboardHeader(title = "gwpcorMapper"),
  dashboardSidebar(
    width=230,
    sidebarMenu(
    div(style="display:inline-block",
        fluidRow(
          column(4,
                 actionButton("load", "Load data")
          ),
          column(8,
                 disabled(textInput("label", NULL, placeholder = "   No file selected"))
          )
        )
    ),
    conditionalPanel(
      condition = "input.label != ''",
      div(class='progress-bar', 'Upload Complete',
          style="margin-left: 15px; margin-top: -20px; margin-bottom: 20px; padding: 0; width: 202px;"
      )
    ),
    radioButtons(inputId = "radio", label = "Type",
                 choices = list("GW correlation" = "cor",
                                "GW partial correlation" = "pcor"),
                 selected = "cor"),
    radioButtons(inputId = "radio2", label = "Correlation type",
                 choices = list("Pearson" = "pearson",
                                "Spearman" = "spearman"),
                 selected = "pearson"),
    selectInput(inputId = "input_type1",
                label = "Correlation pair 1",
                width = '100%',
                choices = ''),
    selectInput(inputId = "input_type2",
                width = '100%',
                label = "Correlation pair 2",
                choices = ''),
    conditionalPanel(
      condition = "input.radio == 'pcor'",
      selectInput(inputId = "input_type3",
                  label = "Control variables",
                  choices = '',
                  multiple = TRUE)
    ),
    selectInput(inputId = "input_type4",
                label = "Kernel type",
                choices = ,list("Gaussian" = "gaussian",
                                "Exponential"  = "exponential",
                                "Bisquare" = "bisquare",
                                "Tricube"= "tricube",
                                "Box-car" = "boxcar"),
                selected = "bisquare"),
    sliderInput("slider", "Adaptive kernel size:", 0.01, 1, 0.25),
    actionButton("submit", "Map Results", icon("map"),
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 85%"),
    hr(),
    sliderInput("slider2", "Map opacity:", 0, 1, 0.5)
  )),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style("#load {display: inline; margin-top: 12px;}"),
      tags$style("#label {display: inline; margin-left: -28px; padding: 0px; width: 120px;}"),
      tags$style("#map {height: calc(100vh - 60px) !important; padding: 0; margin: 0;}"),
      tags$style("#plot {height: calc(100vh - 60px) !important; padding: 0; margin: 0;}"),
      tags$style("div.col-sm-7 {padding: 0} div.col-sm-5 {padding: 0}
        .content {padding: 0} .content-wrapper {background-color: #191A1A}")
      ),
      column(7, plotlyOutput("map")),
      column(5, plotlyOutput("plot"))
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 200*1024^2)

  # reactiveValues object for storing current data set.
  vals <- reactiveValues(
    data = NULL,
    name = NULL,
    dMat = NULL,
    center = NULL,
    zoom = NULL
  )

  loader <- reactiveValues(
    multiple = FALSE,
    accept = NULL
  )

  output$map <- renderPlotly({
    plot_mapbox() %>%
      layout(
        title=list(text = "Please load data", y = 0.5),
        font = list(color='white'),
        plot_bgcolor = '#191A1A',
        paper_bgcolor = '#191A1A',
        mapbox=mapbox.layout
      )
  })

  output$plot <- renderPlotly({
    plotly_empty() %>%
      layout(
        font = list(color='white'),
        plot_bgcolor = '#191A1A',
        paper_bgcolor = '#191A1A'
      )
  })

  output$dynUpload <- renderUI({
    fileInput(
      "upload",
      NULL,
      buttonLabel = "Upload",
      placeholder = "No file selected",
      multiple = loader$multiple,
      accept = loader$accept
    )
  })

  dataLoader <- function() {
    modalDialog(
      selectInput("filetype", 'Load Data',
                  c(`Select a file type`='', supported.types)),
      conditionalPanel(
        condition = "input.filetype == 'CSV'",
        span('Please specify the longitude and latitude column
                 names to read from.'),
        textInput(inputId = "x",
                    label = "longitute column name"),
        textInput(inputId = "y",
                  label = "latidude column name")
      ),

      conditionalPanel(
        condition = "input.filetype == 'ESRI Shapefile'",
        span('You must select and upload ALL shapefile
                 required files (.shp, .shx, .dbf, etc.'),
      ),

      footer = tagList(
        div(style="display:inline",
            fluidRow(
              column(4,
                     modalButton("Cancel")
              ),
              column(8,
                     conditionalPanel(
                       "input.filetype != '' && input.filetype != 'CSV' ||
                       input.filetype == 'CSV' && input.x != '' && input.y != ''",
                       uiOutput("dynUpload")
                     )
              )
            )
        )
      ),
      size = 's'
    )
  }

  getData <- function() {
    # todo: add error handling and validation

    if (input$filetype == "ESRI Shapefile") {
      esri.files <- input$upload
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
    else if (input$filetype == "CSV") {
      data <- st_read(
        input$upload$datapath,
        options=c(
          paste0("X_POSSIBLE_NAMES=", input$x),
          paste0("Y_POSSIBLE_NAMES=", input$y)
        )
      )
      name <- input$upload$name
    }
    else {
      data <- sf::st_read(input$upload$datapath) %>% st_transform(4326)
      name <- input$upload$name
    }
    updateTextInput(session, "label", value=paste0("   ", name))
    return(data)
  }

  observeEvent(input$load, {
    showModal(dataLoader())
  })

  observeEvent(input$filetype, {
    type <- input$filetype

    if (type == "ESRI Shapefile") {
      loader$multiple <- TRUE
      loader$accept <- c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj')
    }
    else if (type == "CSV") {
      loader$multiple <- FALSE
      loader$accept <- ".csv"
    }
    else if (type == "GPKG") {
      loader$multiple <- FALSE
      loader$accept <- ".gpkg"
    }
    else if (type == "GeoJSON") {
      loader$multiple <- FALSE
      loader$accept <- ".geojson"
    }
  })

  observeEvent(input$upload, {
    data <- getData()
    num_row <- nrow(data)
    num_col <- ncol(data)
    varnames <- colnames(data) # probably should remove "geom"

    if (num_col < 3) {
      # showModal(dataModal(failed = TRUE))
    }
    # todo: disable partial correlation if uploaded data only has 2 variables.
    else {
      dummy <- 1:num_row
      vals$data <- cbind(data, dummy)

      withProgress(
        message = 'Building distance matrix',
        detail = '...',
        value = 1,
        {
          dp.locat <- sf::st_centroid(vals$data) %>% sf::st_coordinates()
          vals$dMat <- geodist(dp.locat, measure = "cheap")
        }
      )

      updateSelectInput(session, "input_type1",
                        label = "Corellation pair 1",
                        choices = varnames[!varnames %in% "dummy"],
                        selected = varnames[1]
      )

      updateSelectInput(session, "input_type2",
                        label = "Corellation pair 2",
                        choices = varnames[!varnames %in% "dummy"],
                        selected = varnames[2]
      )
      updateSelectInput(session, "input_type3",
                        label = "Control variables",
                        choices = varnames[!varnames %in% "dummy"]
      )
    }

    view.extent <- get.viewExtent(vals$data)
    vals$center <- view.extent[[1]]
    vals$zoom <- view.extent[[2]]

    map.features <- plot_mapbox(data = vals$data, opacity = input$slider2) %>%
      add_sf(
        color =  I("violet"),
        showlegend = FALSE
      ) %>%
      layout(
        font = list(color='white'),
        plot_bgcolor = '#191A1A',
        paper_bgcolor = '#191A1A',
        mapbox = c(
          mapbox.layout,
          list(
            zoom = vals$zoom,
            center = list(lat = vals$center[2],
                          lon = vals$center[1])
          )
        )
      )
    output$map <- renderPlotly({map.features})
      removeModal()
    # }
    # else {
    #   showModal(dataModal(failed = TRUE))
    # }
  })

  # todo: add observe event to update variable selections
  observeEvent(input$submit, {
    if (is.null(vals$data)) {
      showModal(modalDialog(
        title = "Missing data!",
        "Please load your dataset and distance matrix file."
      ))
      return()
    }

    withProgress(
      message = 'Calculating GW statistics',
      detail = 'This may take a while...',
      value = 1,
      {
        var1 <- input$input_type1
        var2 <- input$input_type2
        if (is.null(input$input_type3)) {
          var3 <- "dummy"
        }
        else{
          var3 <- input$input_type3
        }
        if (input$radio=="cor") {
          if (input$radio2=="pearson") {
            vn <- paste0("corr_", var1, ".", var2)
            vn2 <- paste0("corr_pval_", var1, ".", var2)
          }
          else {
            vn <- paste0("scorr_", var1, ".", var2)
            vn2 <- paste0("scorr_pval_", var1, ".", var2)
          }
        }
        else {
          if (input$radio2=="pearson") {
            vn <- paste0("pcorr_",var1,".",var2)
            vn2 <- paste0("pcorr_pval_",var1,".",var2)
          }
          else {
            vn <- paste0("spcorr_",var1,".",var2)
            vn2 <- paste0("spcorr_pval_",var1,".",var2)
          }
        }
        selected_vars <- c(var1, var2, var3)

        gwpcor.surface <- gwpcor(
          sdata = vals$data,
          vars = selected_vars,
          method = input$radio2,
          kernel = input$input_type4,
          bw = input$slider,
          adaptive = TRUE,
          dMat = vals$dMat
        )$SDF %>%
          st_transform(.,4326) %>%
          dplyr::rename(val = vn) %>%
          dplyr::rename(val2 = vn2) %>%
          dplyr::mutate(
            var1 = vals$data[[var1]],
            var2 = vals$data[[var2]]
          ) %>%
          dplyr::select(val, val2, var1, var2)

        for (control.variable in var3) {
          gwpcor.surface <- cbind(gwpcor.surface, vals$data[[control.variable]])
        }

        var3.names <- paste0("var",3:(2+length(var3)))
        table.names <- c(c('val', 'val2', 'var1', 'var2'), var3.names, 'geometry')
        colnames(gwpcor.surface) <- table.names
        name.mapping <- c(var1, var2, var3)
        names(name.mapping) <- c(c('var1', 'var2'), var3.names)

        shared_data <- SharedData$new(gwpcor.surface)
      }
    )

    output$map <- renderPlotly({
      plot_mapbox(
        source = "map",
        opacity = as.numeric(input$slider2),
        alpha = as.numeric(input$slider2)
      ) %>%
        add_sf(
          data = shared_data,
          split = ~cut(val, b = c(-1,-.8,-.6, -.4, -.2, 0, .2, .4, .6, .8, 1)),
          color = ~I(pal1(val)),
          colors = rpal,
          showlegend = FALSE,
          text = ~val
        ) %>%
        add_sf(
          data = shared_data,
          split = ~cut(val2, labels = "> 0.01", b = c(.01, 1)),
          color = I('black'),
          showlegend = TRUE,
          visible = 'legendonly',
          text = ~val
        ) %>%
        add_sf(
          data = shared_data,
          split = ~cut(val2, labels = "> 0.05", b = c(.05, 1)),
          color = I('black'),
          showlegend = TRUE,
          visible = 'legendonly',
          text = ~val
        ) %>%
        highlight(
          color = "red",
          selected = attrs_selected(showlegend = FALSE)
        ) %>%
        layout(
          font = list(color='white'),
          plot_bgcolor = '#191A1A',
          paper_bgcolor = '#191A1A',
          showlegend=TRUE,
          legend = list(
            orientation = 'h',
            # title does not support center text ...
            title = list(text = '<b>Masking areas with p-value</b>', side = 'top'),
            xanchor = "center",
            x = 0.5,
            yanchor = "bottom"
          ),
          mapbox = c(
            mapbox.layout,
            list(
              zoom = vals$zoom,
              center = list(lat = vals$center[2],
                            lon = vals$center[1])
            )
          )
        )
      }
    )
    
    # scatter plots
    output$plot <- renderPlotly({
      if (input$radio=="cor") {
        plot_ly(
          shared_data,
          x = ~var2,
          y = ~var1,
          text = ~paste('GW coefficient: ', val)
        ) %>%
          layout(
            font = list(color='white'),
            plot_bgcolor = '#191A1A',
            paper_bgcolor = '#191A1A',
            xaxis = list(title = input$input_type2),
            yaxis = list(title = input$input_type1)
          ) %>%
          add_trace(type = "scatter",
                    mode = "markers",
                    color = ~val,
                    colors = rpal
          ) %>%
          colorbar(title = "Correlation\nCoefficient", limits = c(-1, 1), len = 0.9, nticks = 11) %>%
          highlight("plotly_click", color = "red")
      }
      else {
        variables <- table.names[3:(length(table.names)-1)]
        variable.pairs <- combn(variables, 2, simplify=F)

        btns <- list()
        visibles <- list()

        plt <- plot_ly(
          shared_data
        )

        for (i in seq_along(variable.pairs)) {
          set.visible <- FALSE
          if (i == 1) {
            set.visible <- TRUE
          }
          var.y <- variable.pairs[[i]][1]
          var.x <- variable.pairs[[i]][2]
          plt <- plt %>% add_markers(
            text = ~paste('GW coefficient: ', val),
            x = gwpcor.surface[[var.x]],
            y = gwpcor.surface[[var.y]],
            visible = set.visible,
            marker=list(
              color = ~val,
              colorscale = rpal_plotly,
              cmin = -1,
              cmax = 1,
              colorbar = list(
                title = "Correlation\nCoefficient",
                limits = c(-1, 1),
                len = 0.9,
                nticks = 11
              )
            )
          )
          visibles[[i]] <- rep(FALSE, length(variable.pairs))
          visibles[[i]][i] <- TRUE

          btns[[i]] <- list(
            label = paste0("Y: ", name.mapping[[variable.pairs[[i]][1]]], "\n",
                           "X: ", name.mapping[[variable.pairs[[i]][2]]]),
            method = "update",
            args = list(
              list(visible = visibles[[i]]),
              list(
                xaxis = list(
                  title = name.mapping[[variable.pairs[[i]][2]]]
                ),
                yaxis = list(
                  title = name.mapping[[variable.pairs[[i]][1]]]
                )
              )
            )
          )
        }
        plt %>%
          layout(
            font = list(color='white'),
            showlegend = FALSE,
            plot_bgcolor = '#191A1A',
            paper_bgcolor = '#191A1A',
            xaxis = list(
              title = name.mapping[[variable.pairs[[1]][2]]]
            ),
            yaxis = list(
              title = name.mapping[[variable.pairs[[1]][1]]]
            ),
            updatemenus = list(
              list(
                x = 1.2,
                buttons = btns
              )
            )
          ) %>%
          highlight("plotly_click", color = "red")
      }
    })
  })

  observeEvent(input$slider2, {
    plotlyProxy("map", session) %>%
    plotlyProxyInvoke("restyle", list(opacity=input$slider2))
  })

}

shinyApp(ui, server)
