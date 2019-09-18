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
source(here("gwpcor_parallel_func.R"))


tokyo2005_sf <- sf::st_read(here("tokyo2005_sf.gpkg")) %>% st_transform(.,4326)
load(here("dMat.rd"))  # load distant matrix
variable.translation <- read.csv(here("eng_code.csv"), stringsAsFactors = FALSE)

tokyo2005 <- tokyo2005_sf
st_geometry(tokyo2005) <- NULL
num_row <- nrow(tokyo2005)





# gwpcor wrapper function
gwpcor_calc <- function(var1, var2, var3, method,kernel, b){
  selected_vars <- c(var1, var2, var3)
  out <- gwpcor_parallel(tokyo2005_sf,
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


varname <- colnames(tokyo2005)

# this is a quick hack:
names(varname) <- variable.translation$English

bounds <- st_bbox(tokyo2005_sf)

ui <- dashboardPage(
  dashboardHeader(title = ""),
  dashboardSidebar(sidebarMenu(
    fileInput("file1", NULL,
              buttonLabel = "Load data",
              placeholder = "No file selected",
              multiple = FALSE,
              accept = c(".gpkg")),
    fileInput("file2", NULL,
              buttonLabel = "Load dMat",
              multiple = FALSE,
              accept = c(".rd")),
    radioButtons(inputId = "radio", label = h4("Type"),
                 choices = list("GW correlation" = "cor",
                                "GW partial correlation" = "pcor"),
                 selected = "cor"),
    radioButtons(inputId = "radio2", label = h4("Correlation type"),
                 choices = list("Pearson" = "pearson",
                                "Spearman" = "spearman"),
                 selected = "pearson"),
    selectInput(inputId = "input_type1",
                label = "Correlation pair 1",
                choices = varname,
                selected=varname[178]),
    selectInput(inputId = "input_type2",
                label = "Correlation pair 2",
                choices = varname,
                selected=varname[171]),
    conditionalPanel(
      condition = "input.radio == 'pcor'",
      selectInput(inputId = "input_type3",
                  label = "Control variable",
                  choices = varname,
                  selected=varname[173],
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
    sliderInput("slider", "Adaptive kernel size:", 0.1, 1, 0.25)
  )),
  dashboardBody(
    tags$head(tags$style("#warning{color: grey;}")),
    h3(uiOutput("warning"), align = "center"),
    h2(uiOutput("title_panel"), align = "center"),
    fluidRow(
      column(7, plotlyOutput("map")),
      column(5, plotlyOutput("plot"))
    )
  )
)

server <- function(input, output, session) {
  old <- Sys.time()
  selected <- reactive({
    if(input$radio=="cor"){

      ifelse(input$radio2=="pearson",
             vn <- "corr_" %+% input$input_type1 %+% "." %+% input$input_type2,
             vn <- "scorr_" %+% input$input_type1 %+% "." %+% input$input_type2)

      old.pcor <- Sys.time() # get start time

      cl <- makeCluster(detectCores())
      registerDoParallel(cl)


      shapefile <- gwpcor_calc(var1 = input$input_type1,
                               var2 = input$input_type2,
                               var3 = input$input_type3, # this shouldnt be necessary, but since its calling gwpcor it is.
                               method = input$radio2,
                               kernel = input$input_type4,
                               b = as.integer(input$slider * num_row))
      stopCluster(cl)
    } else{

      ifelse(input$radio2=="pearson",
             vn <- "pcorr_" %+% input$input_type1 %+% "." %+% input$input_type2,
             vn <- "spcorr_" %+% input$input_type1 %+% "." %+% input$input_type2)
      
      cl <- makeCluster(detectCores())
      registerDoParallel(cl)
      shapefile <- gwpcor_calc(var1 = input$input_type1,
                               var2 = input$input_type2,
                               var3 = input$input_type3,
                               method = input$radio2,
                               kernel = input$input_type4,
                               b = as.integer(input$slider * num_row))
      stopCluster(cl)
    }

    shapefile_selected <- shapefile %>% dplyr::select(vn)

    names(shapefile_selected)[1] <- "val"

    shapefile_selected <- shapefile_selected %>% st_as_sf()
  })

  observe({
    
    if(is.null(input$file1)) {
      output$warning <- renderText({
        "Please upload some data"
      })
    }
    else{
      output$warning <- renderText({
        ""
      })
      rpal <- brewer.pal(n = 8, name = "RdBu")
      pal1 <- colorBin("RdBu",
                       c(-1,1),
                       bins=11 ,
                       na.color = "#bdbdbd"
      )
      shapefile_selected <-  selected()
      var1 <- input$input_type1
      var2 <- input$input_type2
      var3 <- input$input_type3
      
      if(input$radio=="cor"){
        plot_title <- "Geographically Weighted Correlation"
      }
      else {
        plot_title <- "Geographically Weighted Partial Correlation"
      }
      shapefile_selected <- cbind(shapefile_selected, tokyo2005_sf[[var1]])
      shapefile_selected <- cbind(shapefile_selected, tokyo2005_sf[[var2]])
      
      for (control.variable in var3) {
        shapefile_selected <- cbind(shapefile_selected, tokyo2005_sf[[control.variable]])
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
      
      output$title_panel = renderText(plot_title)
      
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
          height = 580,
          text = ~paste("GW coefficient: ", val),
          showlegend = FALSE
        ) %>%
          highlight(color = "red") %>%
          layout(
            font = list(color='white'),
            plot_bgcolor = '#191A1A',
            paper_bgcolor = '#191A1A',
            mapbox = list(style = 'dark',
                          zoom = 11,
                          center = list(lat = 2,
                                        lon = 2)),
            margin = list(l = 25, r = 25, b = 25, t = 25, pad = 2)
          ) %>%
          hide_colorbar()
      })
      
      output$plot <- renderPlotly({
        if(input$radio=="cor") {
          plt <- plot_ly(ncsd, x = ~var1, y = ~var2, text = ~paste('GW coefficient: ', val), height = 580) %>%
            layout(
              font = list(color='white'),
              plot_bgcolor = '#191A1A',
              paper_bgcolor = '#191A1A',
              xaxis = list(title = names(which(varname==input$input_type1))),
              yaxis = list(title = names(which(varname==input$input_type2)))
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
          
          print(variable.pairs)
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
                        list(xaxis = list(title = names(which(varname == substr(name.mapping[[variable.pairs[[i]][1]]], 1, nchar(name.mapping[[variable.pairs[[i]][1]]]) - 5))),
                                          range = c(min(shapefile_selected[[2]]), max(shapefile_selected[[2]]) + (0.05 * max(shapefile_selected[[2]])) )),
                             yaxis = list(title = names(which(varname == substr(name.mapping[[variable.pairs[[i]][2]]], 1, nchar(name.mapping[[variable.pairs[[i]][2]]]) - 5))),
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
              label = "X: " %+% names(which(varname == substr(name.mapping[[variable.pairs[[i]][1]]], 1, nchar(name.mapping[[variable.pairs[[i]][1]]]) - 5))) %+% "\n" %+%
                "Y: " %+% names(which(varname == substr(name.mapping[[variable.pairs[[i]][2]]], 1, nchar(name.mapping[[variable.pairs[[i]][2]]]) - 5)))
            )
          }
          
          plt %>%
            layout(
              height = 580,
              font = list(color='white'),
              plot_bgcolor = '#191A1A',
              paper_bgcolor = '#191A1A',
              xaxis = list(title = names(which(varname == substr(name.mapping[[variable.pairs[[1]][1]]], 1, nchar(name.mapping[[variable.pairs[[1]][1]]]) - 5))),
                           range = c(min(shapefile_selected[[2]]), max(shapefile_selected[[2]]) + (0.05 * max(shapefile_selected[[2]])) )),
              yaxis = list(title = names(which(varname == substr(name.mapping[[variable.pairs[[1]][2]]], 1, nchar(name.mapping[[variable.pairs[[1]][2]]]) - 5))),
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
      
    }
    

  })
}


shinyApp(ui, server)


