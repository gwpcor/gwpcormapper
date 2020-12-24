library(shiny)
library(sf)
library(sp)
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(MyRMiscFunc)
library(GWpcor)
library(spdplyr)
library(GWmodel)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(crosstalk)
library(leaflet)

#readRenviron(".env")

style <- Sys.getenv("STYLE")

ui <- dashboardPage(
  dashboardHeader(title = "gwpcorMapper"),
  dashboardSidebar(
    width=230,
    sidebarMenu(
    fileInput("file1", NULL,
              buttonLabel = "Load data",
              placeholder = "No file selected",
              multiple = FALSE,
              accept = c(".geojson", ".gpkg", ".shp")),
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
    sliderInput("slider", "Adaptive kernel size:", 0.1, 1, 0.25),
    actionButton("submit", "Map Results", icon("map"),
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 85%"),
    hr(),
    sliderInput("slider2", "Map opacity:", 0.1, 1, 0.1)
  )),
  dashboardBody(
    tags$head(
      tags$style("#map {height: calc(100vh - 60px) !important; padding: 0; margin: 0;}"),
      tags$style("#plot {height: calc(100vh - 60px) !important; padding: 0; margin: 0;}"),
      tags$style(".shiny-notification {position: fixed; top: 50% ;left: 50%}"),
      tags$style(".shiny-input-container {padding: 0; margin: 0;}"),
      tags$style("#file1_progress {padding: 0; margin: 0;}"),
      tags$style("#file2_progress {padding: 0; margin: 0;}"),
      tags$style(HTML("
        div.col-sm-7 {padding: 0}
        div.col-sm-5 {padding: 0}
        .content {padding: 0}
        .content-wrapper {background-color: #191A1A}
        "))
      ),
      column(7, plotlyOutput("map")),
      column(5, plotlyOutput("plot"))
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 200*1024^2)
  
  data <- NULL
  dMat <- NULL
  varname <- NULL
  num_row <- NULL
  
  makeReactiveBinding("data")
  makeReactiveBinding("dMat")
  makeReactiveBinding("varname")
  makeReactiveBinding("num_row")
  
  # gwpcor wrapper function
  gwpcor_calc <- function(sdata, var1, var2, var3, method, kernel, b, dMat) {
    selected_vars <- c(var1, var2, var3)
    out <- gwpcor(sdata=sdata,
                  vars=selected_vars,
                  method=method,
                  kernel=kernel,
                  bw=b,
                  adaptive=TRUE,
                  dMat=dMat
    )
    result <- out$SDF %>% st_transform(.,4326)
    return(result)
  }

  observeEvent(input$file1, {
    data <- sf::st_read(input$file1$datapath) %>% st_transform(.,4326)
  
    withProgress(
      message = 'Building distance matrix',
      detail = '...',
      value = 1,
     {
       # todo: this might fail if data are points, not polygons
       dp.locat <- sf::st_centroid(data) %>% sf::st_coordinates(.)
       dMat <<- gw.dist(dp.locat = dp.locat, p = 2, theta = 0, longlat = F)
     }
    )

    
    num_row <<- nrow(data)

    dummy <- 1:num_row
    data <- st_sf(data.frame(data, as.data.frame(dummy)))

    data.copy <- data
    st_geometry(data.copy) <- NULL
    
    varname <<- colnames(data.copy)
    data <<- data

    updateSelectInput(session, "input_type1",
                      label = "Corellation pair 1",
                      choices = varname[!varname %in% "dummy"],
                      selected = varname[1]
    )
    
    updateSelectInput(session, "input_type2",
                      label = "Corellation pair 2",
                      choices = varname[!varname %in% "dummy"],
                      selected = varname[2]
    )
    updateSelectInput(session, "input_type3",
                        label = "Control variables",
                        choices = varname
      )
  })
  
  observe({
    if (is.null(input$file1)) {
      output$map <- renderPlotly({
        plot_mapbox() %>%
          layout(
            title=list(text = "Please load data", y = 0.5),
            font = list(color='white'),
            plot_bgcolor = '#191A1A',
            paper_bgcolor = '#191A1A',
            mapbox = list(style = style)
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
    } else{
      bounds <- st_bbox(data)
      
      output$map <- renderPlotly({
        plot_mapbox() %>%
          layout(
            font = list(color='white'),
            plot_bgcolor = '#191A1A',
            paper_bgcolor = '#191A1A',
            mapbox = list(
              style = style,
              center = list(lat = bounds$ymin + (bounds$ymax - bounds$ymin),
                            lon = bounds$xmin + (bounds$xmax - bounds$xmin))
            )
          )
      })

    }
  })
  
  observeEvent(input$submit, {
    if (is.null(input$file1)) {
      showModal(modalDialog(
        title = "Missing data!",
        "Please load your dataset and distance matrix file."
      ))
      return()
    }
    
    bounds <- st_bbox(data)
    
    withProgress(
      message = 'Calculating GW statistics',
      detail = 'This may take a while...',
      value = 1,
      {
        rpal <- brewer.pal(n = 8, name = "RdBu")
        pal1 <- colorBin("RdBu", c(-1,1), bins=11, na.color = "#bdbdbd")
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
            vn <- "corr_" %+% var1 %+% "." %+% var2
            vn2 <- "corr_pval_" %+% var1 %+% "." %+% var2
          }
          else {
            vn <- "scorr_" %+% var1 %+% "." %+% var2
            vn2 <- "scorr_pval_" %+% var1 %+% "." %+% var2
          }
          shapefile <- gwpcor_calc(
            sdata = data,
            var1 = var1,
            var2 = var2,
            var3 = var3,
            method = input$radio2,
            kernel = input$input_type4,
            b = input$slider,
            dMat = dMat
          )
        }
        else {
          if (input$radio2=="pearson") {
            vn <- "pcorr_" %+% var1 %+% "." %+% var2
            vn2 <- "pcorr_pval_" %+% var1 %+% "." %+% var2
          }
          else {
            vn <- "spcorr_" %+% var1 %+% "." %+% var2
            vn2 <- "spcorr_pval_" %+% var1 %+% "." %+% var2
          }
          shapefile <- gwpcor_calc(sdata = data,
          var1 = var1,
          var2 = var2,
          var3 = var3,
          method = input$radio2,
          kernel = input$input_type4,
          b = input$slider,
          dMat = dMat)
        }
        shapefile_selected <- shapefile %>%
          dplyr::rename(val = vn) %>%
          dplyr::rename(val2 = vn2) %>%
          dplyr::mutate(var1 = data[[var1]],
        var2 = data[[var2]]) %>%
          dplyr::select(val, val2, var1, var2)
        for (control.variable in var3) {
          shapefile_selected <- cbind(shapefile_selected, data[[control.variable]])
        }
        var3.names <- "var" %+% 3:(2+length(var3))
        table.names <- c(c('val', 'val2', 'var1', 'var2'), var3.names, 'geometry')
        colnames(shapefile_selected) <- table.names
        name.mapping <- c(var1, var2, var3)
        names(name.mapping) <- c(c('var1', 'var2'), var3.names)
        shared_data <- SharedData$new(shapefile_selected)
      }
    )

    output$map <- renderPlotly({
      plot_mapbox(
        source = "map"
      ) %>%
        add_sf(
          data = shared_data,
          split = ~cut(val, b = c(-1,-.8,-.6, -.4, -.2, 0, .2, .4, .6, .8, 1)),
          color = ~I(pal1(val)),
          colors = rpal,
          opacity = as.integer(input$slider2),
          showlegend = FALSE,
          text = ~val
        ) %>%
        add_sf(
          data = shared_data,
          split = ~cut(val2, labels = "> 0.01", b = c(.01, 1)),
          color = I('black'),
          opacity = as.integer(input$slider2),
          showlegend = TRUE,
          visible = 'legendonly',
          text = ~val
        ) %>%
        add_sf(
          data = shared_data,
          split = ~cut(val2, labels = "> 0.05", b = c(.05, 1)),
          color = I('black'),
          opacity = as.integer(input$slider2),
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
          mapbox = list(
            style = style,
            zoom = 10.55, # replace with auto zoom once available https://github.com/plotly/plotly.js/issues/3434
            center = list(
              lat = bounds$ymin + (bounds$ymax - bounds$ymin),
              lon = bounds$xmin + (bounds$xmax - bounds$xmin)
            )
          )
        )
      }
    )
    
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
          var.y <- variable.pairs[[i]][1]
          var.x <- variable.pairs[[i]][2]
          plt <- plt %>% add_markers(
            text = ~paste('GW coefficient: ', val),
            x = shapefile_selected[[var.x]],
            y = shapefile_selected[[var.y]],
            showlegend = FALSE,
            visible = F,
            marker=list(
              color = ~val,
              colors = rpal,
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
          visibles[[i]] <- rep(F, length(variable.pairs))
          visibles[[i]][i] <- T

          btns[[i]] <- list(
            label =
              "Y: " %+% name.mapping[[variable.pairs[[i]][1]]]
                %+% "\n" %+%
              "X: " %+% name.mapping[[variable.pairs[[i]][2]]],
            visible = T,
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
            plot_bgcolor = '#191A1A',
            paper_bgcolor = '#191A1A',
            updatemenus = list(
              list(
                active = -1,
                x = 1,
                buttons = btns
              )
            )
          ) %>%
          highlight("plotly_click", color = "red")
      }
    })
  })
}

shinyApp(ui, server)
