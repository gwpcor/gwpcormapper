# todo: move this to testthat
# # Title     : TODO
# # Objective : TODO
# # Created by: iosefa
# # Created on: 2021/02/25
#
# library(sf)
# library(geodist)
# library(dplyr)
# library(GWpcor)
# library(Rcpp)
# library(corpcor)
#
# source("gwpcormapper/helpers.R")
#
# # load data in various required formats
#
# # selected variables
# selected_vars <- c("total.population", "Daytime.Population", "Population.Density")
#
# # sf
# sdata <- st_read("data/tokyo2005_sf.gpkg") %>%
#   st_transform(.,4326)
#
# # parameters
#
# # bandwidth
# bw <- 0.2
#
# # bandwidth type
# adaptive <- TRUE
#
# # coordinates
# dp.locat <- st_centroid(sdata) %>% st_coordinates()
#
# # distance matrix
# dMat <- geodist(dp.locat, measure = "cheap")
#
# # correlation type
# method <- "pearson"
#
# # kernel
# kernel <- "bisquare"
#
# app <- gwpcor(
#   sdata=sdata,
#   vars=selected_vars,
#   method=method,
#   kernel=kernel,
#   bw=bw,
#   adaptive=adaptive,
#   dMat=dMat
# )
#
# lib <- GWpcor::gwpcor(
#   sdata=sdata,
#   vars=selected_vars,
#   method=method,
#   kernel=kernel,
#   bw=bw,
#   adaptive=adaptive,
#   dMat=dMat
# )
#
# all.equal(app$SDF$corr_Daytime.Population.Population.Density, lib$SDF$corr_Daytime.Population.Population.Density)
# all.equal(app$SDF$corr_pval_Daytime.Population.Population.Density, lib$SDF$corr_pval_Daytime.Population.Population.Density)
