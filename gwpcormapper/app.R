library(shiny)
library(shinydashboard)
library(sf)
library(tidyverse)
library(geodist)
library(plotly)
library(crosstalk)
library(RColorBrewer)
library(leaflet)

source("gwpcormapper/src/optimized_gwpcor.R")

style <- Sys.getenv("STYLE")
token <- Sys.getenv("MAPBOX_TOKEN")

if (style == '') {
  style <- 'carto-darkmatter'
}

if (token == '') {
  Sys.setenv('MAPBOX_TOKEN' = 'token')
}

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
  
  makeReactiveBinding("data")
  makeReactiveBinding("dMat")
  makeReactiveBinding("varname")
  makeReactiveBinding("num_row")

  observeEvent(input$file1, {
    data <- sf::st_read(input$file1$datapath) %>% st_transform(.,4326)
  
    withProgress(
      message = 'Building distance matrix',
      detail = '...',
      value = 1,
     {
       dp.locat <- sf::st_centroid(data) %>% sf::st_coordinates(.)

       dMat <<- geodist(dp.locat, measure = "cheap")
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
      center.lon <- bounds$xmin + (bounds$xmax - bounds$xmin)/2
      center.lat <- bounds$ymin + (bounds$ymax - bounds$ymin)/2
      zoom.lon <- log(180/abs(center.lon - bounds$xmin))/log(2)
      zoom.lat <- log(90/abs(center.lat - bounds$ymin))/log(2)
      zoom.center <- mean(zoom.lon, zoom.lat)

      output$map <- renderPlotly({
        plot_mapbox() %>%
          layout(
            font = list(color='white'),
            plot_bgcolor = '#191A1A',
            paper_bgcolor = '#191A1A',
            mapbox = list(
              style = style,
              zoom = zoom.center,
              center = list(lat = center.lat,
                            lon = center.lon)
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
    # a very poor approximation, but close enough
    center.lon <- bounds$xmin + (bounds$xmax - bounds$xmin)/2
    center.lat <- bounds$ymin + (bounds$ymax - bounds$ymin)/2
    zoom.lon <- log(180/abs(center.lon - bounds$xmin))/log(2)
    zoom.lat <- log(90/abs(center.lat - bounds$ymin))/log(2)
    zoom.center <- (zoom.lon + zoom.lat)/2

    withProgress(
      message = 'Calculating GW statistics',
      detail = 'This may take a while...',
      value = 1,
      {
        rpal <- brewer.pal(n = 11, name = "RdBu")
        rpal_plotly <- list()
        rang <- seq_along(rpal)
        for (ind in rang) {
          rpal_plotly[[ind]] <- list(
            (ind - min(rang))/(max(rang) - min(rang)),
            rpal[ind]
          )
        }
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
            vn <- paste0("corr_", var1, ".", var2)
            vn2 <- paste0("corr_pval_", var1, ".", var2)
          }
          else {
            vn <- paste0("scorr_", var1, ".", var2)
            vn2 <- paste0("scorr_pval_", var1, ".", var2)
          }
          selected_vars <- c(var1, var2, var3)
          shapefile <- gwpcor(
            sdata = data,
            vars = selected_vars,
            method = input$radio2,
            kernel = input$input_type4,
            bw = input$slider,
            adaptive = TRUE,
            dMat = dMat
          )$SDF %>% st_transform(.,4326)
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
          selected_vars <- c(var1, var2, var3)
          shapefile <- gwpcor(
            sdata = data,
            vars = selected_vars,
            method = input$radio2,
            kernel = input$input_type4,
            bw = input$slider,
            adaptive = TRUE,
            dMat = dMat)$SDF %>% st_transform(.,4326)
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
        var3.names <- paste0("var",3:(2+length(var3)))
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
            zoom = zoom.center,
            center = list(
              lat = center.lat,
              lon = center.lon
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
          set.visible <- FALSE
          if (i == 1) {
            set.visible <- TRUE
          }
          var.y <- variable.pairs[[i]][1]
          var.x <- variable.pairs[[i]][2]
          plt <- plt %>% add_markers(
            text = ~paste('GW coefficient: ', val),
            x = shapefile_selected[[var.x]],
            y = shapefile_selected[[var.y]],
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
}

shinyApp(ui, server)
