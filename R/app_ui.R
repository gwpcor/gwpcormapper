# Title     : gwpcorMapper Server
# Objective : Holds ui function called by gwpcorMapper
# Created by: Joseph Percival and Narumasa Tsutsumida
# Created on: 2021/02/26
#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import plotly
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    dashboardPage(
      dashboardHeader(title = "gwpcorMapper"),
      dashboardSidebar(
        width=230,
        sidebarMenu(
          custom_loader(
            "load",
            "Load data",
            "loadedData",
            NULL
          ),
          radioButtons(
            inputId = "type",
            label = "Type",
            choices = list(
              "GW correlation" = "cor",
              "GW partial correlation" = "pcor"
            ),
            selected = "cor"
          ),
          radioButtons(
            inputId = "corrType",
            label = "Correlation type",
            choices = list(
              "Pearson" = "pearson",
              "Spearman" = "spearman"
            ),
            selected = "pearson"
          ),
          selectInput(
            inputId = "var1",
            label = "Correlation pair 1",
            width = '100%',
            choices = ''
          ),
          selectInput(
            inputId = "var2",
            label = "Correlation pair 2",
            width = '100%',
            choices = ''
          ),
          conditionalPanel(
            condition = "input.type == 'pcor'",
            selectInput(
              inputId = "vars3",
              label = "Control variables",
              width = '100%',
              choices = '',
              multiple = TRUE
            )
          ),
          selectInput(
            inputId = "kernel",
            label = "Kernel type",
            choices = list(
              "Gaussian" = "gaussian",
              "Exponential"  = "exponential",
              "Bisquare" = "bisquare",
              "Tricube"= "tricube",
              "Box-car" = "boxcar"
            ),
            selected = "bisquare"
          ),
          sliderInput(
            inputId = "bandwidth",
            label = "Adaptive kernel size:",
            min = 0.01,
            max = 1,
            value = 0.25
          ),
          actionButton(
            inputId = "calculate",
            label = "Map Results",
            icon = icon("map"),
            style="color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 85%"
          ),
          hr(),
          sliderInput(
            inputId = "opacity",
            label = "Map opacity:",
            min = 0,
            max = 1,
            value = 0.5
          )
        )
      ),
      dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(
          tags$style("#load {display: inline; margin-top: 12px;}"),
          tags$style("#loadedData {display: inline; margin-left: -28px; padding: 0px; width: 120px;}"),
          tags$style("#map {height: calc(100vh - 60px) !important; padding: 0; margin: 0;}"),
          tags$style("#plot {height: calc(100vh - 60px) !important; padding: 0; margin: 0;}"),
          tags$style(
            "div.col-sm-7 {padding: 0} div.col-sm-5 {padding: 0}
            .content {padding: 0} .content-wrapper {background-color: #191A1A}"
          )
        ),
        column(7, plotlyOutput("map")),
        column(5, plotlyOutput("plot"))
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'gwpcormapper'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

