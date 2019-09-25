library(shiny)
library(leaflet)
library(sf)
library(sp)
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(MyRMiscFunc)
library(gwpcor)
library(here)
library(spdplyr)
library(GWmodel)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(crosstalk)
library(foreach)
library(doParallel)

# gwpcor function for parallel computing
# core registraion functions were extract outside from gwpcor::gwpcor function due to the suspicion of the slow processing
source(here("/ShinyApps/gwpcor_parallel_func.R"))

ui <- dashboardPage(
  dashboardHeader(title = "gwpcorMapper"),
  dashboardSidebar(sidebarMenu(
    fileInput("file1", NULL,
              buttonLabel = "Load data",
              placeholder = "No file selected",
              multiple = FALSE,
              accept = c(".gpkg")),
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
                choices = ''),
    selectInput(inputId = "input_type2",
                label = "Correlation pair 2",
                choices = ''),
    conditionalPanel(
      condition = "input.radio == 'pcor'",
      selectInput(inputId = "input_type3",
                  label = "Control variable",
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
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 85%")
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
    # fluidRow(
      column(7, plotlyOutput("map")),
      column(5, plotlyOutput("plot"))
    # )
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
  gwpcor_calc <- function(sdata, var1, var2, var3, method,kernel, b, dMat){
    selected_vars <- c(var1, var2, var3)
    out <- gwpcor_parallel(sdata=data,
                           vars = selected_vars,
                           method = method,
                           kernel = kernel,
                           bw = b,
                           adaptive = TRUE,
                           longlat = FALSE,
                           dMat=dMat)
    result <- out$SDF %>% st_transform(.,4326)
    return(result)
  }

  observeEvent(input$file1, {
    data <<- sf::st_read(input$file1$datapath) %>% st_transform(.,4326)
    
    withProgress(message = 'Building distance matrix',
                 detail = '...', value = 1, {
      sp.locat <- sf::st_centroid(data) %>% sf::st_coordinates(.)
      dp.locat <- sf::st_centroid(data) %>% sf::st_coordinates(.)
      
      dMat <<- gw.dist(dp.locat = dp.locat, rp.locat = sp.locat, 
                       p = 2, theta = 0, longlat = F)
                 })
    
    tokyo2005 <- data
    st_geometry(tokyo2005) <- NULL
    num_row <<- nrow(tokyo2005)
    
    varname <<- colnames(tokyo2005)
    
    
    updateSelectInput(session, "input_type1",
                      label = "Corellation pair 1",
                      choices = varname,
                      selected = varname[1]
    )
    
    updateSelectInput(session, "input_type2",
                      label = "Corellation pair 2",
                      choices = varname,
                      selected = varname[2]
    )
    
    updateSelectInput(session, "input_type3",
                      label = "Control variable",
                      choices = varname,
                      selected = varname[3]
    )
  })
  
  observe({
    if(is.null(input$file1)) {
      output$map <- renderPlotly({
        plot_mapbox() %>%
          layout(
            title=list(text = "Please load data", y = 0.5),
            font = list(color='white'),
            plot_bgcolor = '#191A1A',
            paper_bgcolor = '#191A1A',
            mapbox = list(style = 'dark', zoom = 0),
            margin = list(l = 25, r = 25, b = 25, t = 25, pad = 2)
          ) %>%
          hide_colorbar()
      })
      
      output$plot <- renderPlotly({
          plotly_empty() %>%
            layout(
              font = list(color='white'),
              plot_bgcolor = '#191A1A',
              paper_bgcolor = '#191A1A'
            ) 
      })
      
    }
    else{
      bounds <- st_bbox(data)
      
      output$map <- renderPlotly({
        plot_mapbox() %>%
          layout(
            font = list(color='white'),
            plot_bgcolor = '#191A1A',
            paper_bgcolor = '#191A1A',
            mapbox = list(style = 'dark',
                          zoom = 8,
                          center = list(lat = bounds$ymin + (bounds$ymax - bounds$ymin),
                                        lon = bounds$xmin + (bounds$xmax - bounds$xmin))),
            margin = list(l = 25, r = 25, b = 25, t = 25, pad = 2)
          ) %>%
          hide_colorbar()
      })

    }
    

  })
  
  observeEvent(input$submit, {
    if(is.null(input$file1)){
      showModal(modalDialog(
        title = "Missing data!",
        "Please load your dataset and distance matrix file."
      ))
      return()
    }
    
    bounds <- st_bbox(data)
    
    
    withProgress(message = 'Calculating GW statistics',
                 detail = 'This may take a while...', value = 1, {
                   
                   rpal <- brewer.pal(n = 8, name = "RdBu")
                   pal1 <- colorBin("RdBu",
                                    c(-1,1),
                                    bins=11 ,
                                    na.color = "#bdbdbd"
                   )
                   
                   var1 <- input$input_type1
                   var2 <- input$input_type2
                   var3 <- input$input_type3
                   
                   
                   if(input$radio=="cor"){
                     
                     ifelse(input$radio2=="pearson",
                            vn <- "corr_" %+% input$input_type1 %+% "." %+% input$input_type2,
                            vn <- "scorr_" %+% input$input_type1 %+% "." %+% input$input_type2)
                     
                     #cl <- makeCluster(detectCores())
                     #registerDoParallel(cl)
                     
                     shapefile <- gwpcor_calc(sdata = data,
                                              var1 = input$input_type1,
                                              var2 = input$input_type2,
                                              var3 = input$input_type3, # this shouldnt be necessary, but since its calling gwpcor it is.
                                              method = input$radio2,
                                              kernel = input$input_type4,
                                              b = as.integer(input$slider * num_row),
                                              dMat = dMat
                     )
                     #stopCluster(cl)
                   } else{
                     
                     ifelse(input$radio2=="pearson",
                            vn <- "pcorr_" %+% input$input_type1 %+% "." %+% input$input_type2,
                            vn <- "spcorr_" %+% input$input_type1 %+% "." %+% input$input_type2)
                     
                     #cl <- makeCluster(detectCores())
                     #registerDoParallel(cl)
                     shapefile <- gwpcor_calc(sdata = data,
                                              var1 = input$input_type1,
                                              var2 = input$input_type2,
                                              var3 = input$input_type3,
                                              method = input$radio2,
                                              kernel = input$input_type4,
                                              b = as.integer(input$slider * num_row),
                                              dMat = dMat)
                     #stopCluster(cl)
                   }
                   
                   shapefile_selected <- shapefile %>% dplyr::select(vn)
                   
                   names(shapefile_selected)[1] <- "val"
                   
                   shapefile_selected <- shapefile_selected %>% st_as_sf()
                   
                   shapefile_selected <- cbind(shapefile_selected, data[[var1]])
                   shapefile_selected <- cbind(shapefile_selected, data[[var2]])
                   
                   for (control.variable in var3) {
                     shapefile_selected <- cbind(shapefile_selected, data[[control.variable]])
                   }
                   
                   var3.names <- c()
                   for (ind in 1:length(var3)) {
                     var3.names[ind] = paste0("var", ind+2)
                   }
                   
                   table.names <- c(c('val', 'var1', 'var2'), var3.names, c('geometry') )
                   colnames(shapefile_selected) <- table.names
                   
                   name.mapping <- c(c(var1), c(var2), c(var3))
                   
                   names(name.mapping) <- c(c('var1', 'var2'), var3.names)
                   
                   ncsd <- SharedData$new(shapefile_selected)   
  
                 })
    
    
    output$map <- renderPlotly({
      plot_mapbox(
        source = "map",
        ncsd,
        split = ~cut(val, b = c(-1,-.8,-.6, -.4, -.2, 0, .2, .4, .6, .8, 1)),
        stroke = ~I(pal1(val)),
        color = ~val,
        fillcolor = ~I(pal1(val)),
        colors = "RdBu",
        opacity = 0.1,
        text = ~paste("GW coefficient: ", val),
        showlegend = FALSE
      ) %>%
        highlight(color = "red") %>%
        layout(
          font = list(color='white'),
          plot_bgcolor = '#191A1A',
          paper_bgcolor = '#191A1A',
          mapbox = list(style = 'dark',
                        zoom = 8,
                        center = list(lat = bounds$ymin + (bounds$ymax - bounds$ymin),
                                      lon = bounds$xmin + (bounds$xmax - bounds$xmin))
                        ),
          margin = list(l = 25, r = 25, b = 25, t = 25, pad = 2)
        ) %>%
        hide_colorbar()
      
    })
    
    output$plot <- renderPlotly({
      if(input$radio=="cor") {
        plt <- plot_ly(ncsd, x = ~var1, y = ~var2, text = ~paste('GW coefficient: ', val)
        ) %>%
          layout(
            font = list(color='white'),
            plot_bgcolor = '#191A1A',
            paper_bgcolor = '#191A1A',
            xaxis = list(title = input$input_type1),
            yaxis = list(title = input$input_type2)
            # xaxis = list(title = names(which(varname==input$input_type1))),
            # yaxis = list(title = names(which(varname==input$input_type2)))
          ) %>%
          add_trace(type = "scatter",
                    mode = "markers",
                    color = ~val,
                    colors = rpal
          ) %>%
          colorbar(title = "Correlation Strength", limits = c(-1, 1), len = 0.9, nticks = 11) %>%
          hide_colorbar() %>%
          highlight("plotly_click", color = "red")
      }
      else {

        variables = table.names[2:(length(table.names)-1)]
        variable.pairs <- combn(variables, 2, simplify=F)

        plt <- plot_ly(ncsd)

        for (i in 1:length(variable.pairs)) {
          if (i == 1) {
            var.x <- variable.pairs[[i]][1]
            var.y <- variable.pairs[[i]][2]
            plt = add_markers(plt,
                              colors = rpal,
                              color = ~val,
                              text = ~paste('GW coefficient: ', val),
                              x = shapefile_selected[[var.x]],
                              y = shapefile_selected[[var.y]],
                              showlegend = FALSE,
                              visible = T
            )
          }
          else {
            var.x <- variable.pairs[[i]][1]
            var.y <- variable.pairs[[i]][2]
            plt = add_markers(plt,
                              colors = rpal,
                              color = ~val,
                              text = ~paste('GW coefficient: ', val),
                              x = shapefile_selected[[var.x]],
                              y = shapefile_selected[[var.y]],
                              showlegend = FALSE,
                              visible = F
            )
          }

        }

        btns <- list()

        visibles <- list()

        for (i in 1:length(variable.pairs)) {
          visibles[[i]] <- rep(F, length(variable.pairs))
          visibles[[i]][i] <- T
        }
        
        for (i in 1:length(variable.pairs)) {
          if (i == 1) {
            a <- list(list(visible = visibles[[i]]),
                      list(xaxis = list(title = name.mapping[[variable.pairs[[i]][1]]],
                                        range = c(min(shapefile_selected[[2]]), max(shapefile_selected[[2]]) + (0.05 * max(shapefile_selected[[2]])) )
                                        ),
                           yaxis = list(title = name.mapping[[variable.pairs[[i]][2]]],
                                        range = c(min(shapefile_selected[[3]]), max(shapefile_selected[[3]]) + (0.05 * max(shapefile_selected[[3]])) ))
                      )
            )
          }
          
          
          else {
            a <- list(list(visible = visibles[[i]]),
                      list(xaxis = list(title = names(which(varname == substr(name.mapping[[variable.pairs[[i]][1]]], 1, nchar(name.mapping[[variable.pairs[[i]][1]]]) - 5))) ),
                           yaxis = list(title = names(which(varname == substr(name.mapping[[variable.pairs[[i]][2]]], 1, nchar(name.mapping[[variable.pairs[[i]][2]]]) - 5))) )
                      )
            )
          }
          
          btns[[i]] <- list(
            method = "update",
            args = a,
            label = "X: " %+% name.mapping[[variable.pairs[[i]][1]]] %+% "\n" %+%
              "Y: " %+% name.mapping[[variable.pairs[[i]][2]]]
          )
        }

        plt %>%
          layout(
            font = list(color='white'),
            plot_bgcolor = '#191A1A',
            paper_bgcolor = '#191A1A',
            xaxis = list(title = name.mapping[[variable.pairs[[1]][1]]],
                         range = c(min(shapefile_selected[[2]]), max(shapefile_selected[[2]]) + (0.05 * max(shapefile_selected[[2]])) )),
            yaxis = list(title = name.mapping[[variable.pairs[[1]][2]]],
                         range = c(min(shapefile_selected[[3]]), max(shapefile_selected[[3]]) + (0.05 * max(shapefile_selected[[3]])) )),
            updatemenus = list(
              list(
                x = 1,
                buttons = btns
              )
            )
          ) %>%
          colorbar(title = "Correlation Strength", limits = c(-1, 1), len = 0.9, nticks = 11) %>%
          hide_colorbar() %>%
          highlight("plotly_click", color = "red")
      }


    })
    
  })
}

cl <- makeCluster(detectCores())
registerDoParallel(cl -1)

shinyApp(ui, server)

stopCluster(cl)

