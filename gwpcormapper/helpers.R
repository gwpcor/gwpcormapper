# Title     : gwpcorMapper Helper Functions
# Objective : Holds global functions to be used by gwpcorMapper.
# Created by: Joseph Percival
# Created on: 2021/02/26
library(sf)
library(corpcor)
library(Rcpp)
sourceCpp("gwpcormapper/gwpcor.cpp")

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

# main gwpcor function. This is based on the gwpcor function in the GWpcor library
# by Tsutsumida and Percival: https://github.com/naru-T/gwpcor
gwpcor <- function(sdata, vars, method = c("pearson", "spearman"),
                   kernel = "bisquare", adaptive = FALSE, bw, dMat) {

    if (adaptive && ((bw <= 0) || (bw >1))){
        stop("adaptive kernel should be between 0 and 1")
    }

    if (!is(sdata, "sf")) {
        stop("Data must be sf")
    }

    if (!(method %in% c("pearson", "spearman"))) {
        stop("The method option should be 'pearson' or 'spearman'.")
    }

    var_n <- length(vars)

    x <- sf::st_drop_geometry(sdata) %>%
      dplyr::select(vars) %>%
      as.matrix()

    if (method == "spearman") {
        x <- apply(x, 2, rank)
    }

    if (anyNA(x)) stop(" NA values are not allowed")

    name.comb <- NULL
    n <- 1
    for (i in 1:(var_n - 1)) {
        for (j in (i + 1):var_n) {
            name.comb[n] <-paste0( vars[i], ".", vars[j])
            n <- n+1
        }
    }

    nn <- sum(1:(var_n - 1))
    gwpcm <- gwpcorRcpp(dMat, as.double(bw), x, kernel, adaptive, nn)

    m1 <- gwpcm[[1]]
    m2 <- gwpcm[[2]]
    m3 <- gwpcm[[3]]
    m4 <- gwpcm[[4]]

    colnames(m1) <- paste0("corr_", name.comb)
    colnames(m2) <- paste0("corr_pval_", name.comb)
    colnames(m3) <- paste0("pcorr_", name.comb)
    colnames(m4) <- paste0("pcorr_pval_", name.comb)

    SDF <- data.frame(m1, m2, m3, m4, sdata$geom) %>% st_as_sf()

    rownames(SDF) <- rownames(sdata)

    res <- list(SDF = SDF, vars = vars, kernel = kernel, adaptive = adaptive,
                bw = bw, method = method)

    class(res) <- "gwpcor"
    invisible(res)
}
