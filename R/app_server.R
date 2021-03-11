# Title     : gwpcorMapper Server
# Objective : Holds server function called by gwpcorMapper
# Created by: Joseph Percival and Narumasa Tsutsumida
# Created on: 2021/02/26
#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import sf
#' @import plotly
#' @noRd

app_server <- function( input, output, session ) {
  options(shiny.maxRequestSize = 200*1024^2)
  
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
  
  # define color schemes
  rpal <- viridis::viridis_pal(option = "D")(11)
  rpal_plotly <- list()
  rang <- seq_along(rpal)
  for (ind in rang) {
    rpal_plotly[[ind]] <- list(
      (ind - min(rang))/(max(rang) - min(rang)),
      rpal[ind]
    )
  }
  pal1 <- leaflet::colorBin(rpal, c(-1,1), bins=11, na.color = "#bdbdbd")

  # reactiveValues object for storing current data set.
  vals <- reactiveValues(
    multiple = FALSE,
    accept = NULL,
    shared_data = NULL,
    name = NULL,
    dMat = NULL,
    center = NULL,
    zoom = NULL,
    update = FALSE
  )

  output$map <- renderPlotly({
    plot_mapbox() %>%
      layout(
        title = list(text = "Please load data", y = 0.5),
        font = list(color='white'),
        plot_bgcolor = '#191A1A',
        paper_bgcolor = '#191A1A',
        mapbox = list(
          style = style,
          layers = layers
        )
      )
  })

  output$plot <- renderPlotly({
    plotly_empty() %>%
      layout(
        title = list(text = "Please load data", y = 0.5),
        font = list(color='white'),
        plot_bgcolor = '#191A1A',
        paper_bgcolor = '#191A1A'
      )
  })

  output$dynUpload <- renderUI({
    fileInput(
      inputId = "upload",
      label = NULL,
      buttonLabel = "Upload",
      placeholder = "No file selected",
      multiple = vals$multiple,
      accept = vals$accept
    )
  })

  observeEvent(input$load, {
    showModal(dataLoader())
  })

  observeEvent(input$filetype, {
    type <- input$filetype

    if (type == "ESRI Shapefile") {
      vals$multiple <- TRUE
      vals$accept <- c('.shp', '.dbf', '.sbn', '.sbx', '.shx', '.prj')
    }
    else if (type == "CSV") {
      vals$multiple <- FALSE
      vals$accept <- ".csv"
    }
    else if (type == "GPKG") {
      vals$multiple <- FALSE
      vals$accept <- ".gpkg"
    }
    else if (type == "GeoJSON") {
      vals$multiple <- FALSE
      vals$accept <- ".geojson"
    }
  })

  observeEvent(input$upload, {
    data.list <- getData(input$filetype, input$upload, input$x, input$y)
    data <- data.list[["data"]]
    name <- data.list[["name"]]
    varnames <- data.list[["vars"]]
    view.extent <- get.viewExtent(data)
    center <- view.extent[[1]]
    zoom <- view.extent[[2]]

    withProgress(
      message = 'Building distance matrix',
      detail = '...',
      value = 1,
      {
        dMat <- calc.dMat(data)
      }
    )

    updateTextInput(
      session,
      inputId = "loadedData",
      value = paste0("   ", name)
    )
    updateSelectInput(
      session,
      inputId = "var1",
      label = "Corellation pair 1",
      choices = varnames[!varnames %in% "dummy"],
      selected = varnames[1]
    )
    updateSelectInput(
      session,
      inputId = "var2",
      label = "Corellation pair 2",
      choices = varnames[!varnames %in% "dummy"],
      selected = varnames[2]
    )
    updateSelectInput(
      session,
      inputId = "vars3",
      label = "Control variables",
      choices = varnames[!varnames %in% "dummy"]
    )

    vals$data <- data
    vals$dMat <- dMat
    vals$center <- center
    vals$zoom <- zoom
    vals$update <- FALSE

    output$plot <- renderPlotly({
      plotly_empty() %>%
        layout(
          font = list(color='white'),
          plot_bgcolor = '#191A1A',
          paper_bgcolor = '#191A1A'
        )
    })

    opacity <- input$opacity

    output$map <- renderPlotly({
      plot_ly(
        source = "map",
        data = data,
        showlegend = FALSE,
        opacity = 0,
        alpha = 0,
        hoverinfo="none",
        type = 'scattermapbox'
      ) %>%
        layout(
          font = list(color='white'),
          plot_bgcolor = '#191A1A',
          paper_bgcolor = '#191A1A',
          mapbox = list(
            style = style,
            layers = layers,
            zoom = vals$zoom,
            center = list(lat = vals$center[2],
                          lon = vals$center[1])
          )
        ) %>%
        config(mapboxAccessToken = token)
    })
    removeModal()
  })

  # todo: add observe event to update variable selections
  observeEvent(input$calculate, {
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
        var1 <- input$var1
        var2 <- input$var2
        val <- paste0("corr_", var1, ".", var2)
        val2 <- paste0("corr_pval_", var1, ".", var2)
        if (input$type=="cor") {
          var3 <- "dummy"
        }
        else {
          var3 <- input$vars3
        }
        selected_vars <- c(var1, var2, var3)

        gwpcor.surface <- gwpcor(
          sdata = vals$data,
          vars = selected_vars,
          method = input$corrType,
          kernel = input$kernel,
          bw = input$bandwidth,
          adaptive = TRUE,
          dMat = vals$dMat
        )$SDF %>%
          st_transform(4326) %>%
          dplyr::rename(val = val) %>%
          dplyr::rename(val2 = val2) %>%
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

        shared_data <- crosstalk::SharedData$new(gwpcor.surface, group = "pcor_coefficients")

        vals$shared_data <- shared_data

        variables <- table.names[3:(length(table.names)-1)]
        variable.pairs <- utils::combn(variables, 2, simplify=F)

        btns <- list()
        visibles <- list()
      }
    )

    if (vals$update == FALSE) {
      vals$update <- TRUE

      opacity <- input$opacity

      output$map <- renderPlotly({
        plot_ly(
          source = "map",
          opacity = opacity,
          type = 'scattermapbox'
        ) %>%
          add_sf(
            data = vals$shared_data,
            split = ~cut(val, b = c(-1,-.8,-.6, -.4, -.2, 0, .2, .4, .6, .8, 1)),
            color = ~I(pal1(val)),
            colors = rpal,
            showlegend = FALSE,
            text = ~val
          ) %>%
          add_sf(
            data = vals$shared_data,
            split = ~cut(val2, labels = "> 0.01", b = c(.01, 1)),
            color = I('black'),
            showlegend = TRUE,
            visible = 'legendonly',
            text = ~val
          ) %>%
          add_sf(
            data = vals$shared_data,
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
            mapbox = list(
              style = style,
              layers = layers,
              zoom = vals$zoom,
              center = list(lat = vals$center[2],
                            lon = vals$center[1])
            )
          ) %>%
          config(mapboxAccessToken = token)
      }
      )
      # scatter plots
      output$plot <- renderPlotly({
        if (input$type=="cor") {
          plot_ly(
            vals$shared_data,
            x = ~var2,
            y = ~var1,
            text = ~paste('GW coefficient: ', val)
          ) %>%
            layout(
              font = list(color='white'),
              plot_bgcolor = '#191A1A',
              paper_bgcolor = '#191A1A',
              xaxis = list(title = input$var2),
              yaxis = list(title = input$var1)
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
          plt <- plot_ly(
            vals$shared_data
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
    }
    else {
      plotlyProxy("map", session) %>%
        plotlyProxyInvoke(
          "restyle",
          list(
            fillcolor = ~I(pal1(val)),
            opacity = input$opacity
          )
        )
      # todo: update scatter plot instead of redraw
      output$plot <- renderPlotly({
        if (input$type=="cor") {
          plot_ly(
            vals$shared_data,
            x = ~var2,
            y = ~var1,
            text = ~paste('GW coefficient: ', val)
          ) %>%
            layout(
              font = list(color='white'),
              plot_bgcolor = '#191A1A',
              paper_bgcolor = '#191A1A',
              xaxis = list(title = input$var2),
              yaxis = list(title = input$var1)
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
          plt <- plot_ly(
            vals$shared_data
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
    }
  })

  observeEvent(input$opacity, {
    plotlyProxy("map", session, FALSE) %>%
      plotlyProxyInvoke(
        "restyle",
        list(
          opacity = input$opacity,
          0
        )
      )
  })

}
