# Title     : TODO
# Objective : TODO
# Created by: iosefa
# Created on: 2021/02/26
library(sf)

# Simple funciton to get view extent.
get.viewExtent <- function(data) {
    bounds <- st_bbox(data)
    center.lon <- bounds$xmin + (bounds$xmax - bounds$xmin)/2
    center.lat <- bounds$ymin + (bounds$ymax - bounds$ymin)/2
    zoom.lon <- log(180/abs(center.lon - bounds$xmin))/log(2)
    zoom.lat <- log(90/abs(center.lat - bounds$ymin))/log(2)

    center <- c(center.lon, center.lat)
    zoom <- mean(zoom.lon, zoom.lat)
    return (list(center, zoom))
}
