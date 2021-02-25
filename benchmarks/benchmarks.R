# Title     : Benchmark comparisons for gwpcorMapper
# Objective : Creates benchmark tests for gwpcorMapper and similar functions 
#             that can be used to calculatae geographically weighted correlation
#             coefficients.
# Created by: Joseph Emile Honour Percival
# Created on: 2021/02/20
library(sf)
library(geodist)
library(dplyr)
library(GWmodel)
library(GWpcor)
library(spgwr)
library(lctools)
library(Rcpp)
library(bench)

source("../gwpcormapper/src/optimized_gwpcor.R")

# load data in various required formats

# selected variables
selected_vars <- c("Total.Population", "Daytime.Population", "Population.Density")

# sf
sdata <- st_read("../data/tokyo2005_sf_en.gpkg") %>%
  st_transform(.,4326)

# spdf
spdata <- as_Spatial(sdata)

# df
data <- sf::st_drop_geometry(sdata) %>%
  dplyr::select(selected_vars)

# parameters

# bandwidth
bw <- 0.2
bwgwss <- round(bw*nrow(data))

# bandwidth type
adaptive <- TRUE

# coordinates
dp.locat <- st_centroid(sdata) %>% st_coordinates()

# distance matrix
dMat <- geodist(dp.locat, measure = "cheap")

# correlation type
method <- "pearson"

# kernel
kernel <- "bisquare"


############################################################# 
############## BENCHMARK TESTS ##############################
#############################################################


#####################
### GWPCORMAPPER ####
#####################  

gwpcormapper.mem <- bench_memory(
  gwpcor(
    sdata=sdata,
    vars=selected_vars,
    method=method,
    kernel=kernel,
    bw=bw,
    adaptive=adaptive,
    dMat=dMat
  ))

gwpcormapper.tim <- bench_time(
  gwpcor(
    sdata=sdata,
    vars=selected_vars,
    method=method,
    kernel=kernel,
    bw=bw,
    adaptive=adaptive,
    dMat=dMat
  ))

gwpcormapper.mem$mem_alloc

b1 <- c(gwpcormapper.mem[1, 1], gwpcormapper.tim) %>% as.data.frame()


#####################
###### GWPCOR  ######
##################### 

gwpcor.mem <- bench_memory(
  GWpcor::gwpcor(
    sdata=sdata,
    vars=selected_vars,
    method=method,
    kernel=kernel,
    bw=bw,
    adaptive=adaptive,
    dMat=dMat
  ))

gwpcor.tim <- bench_time(
  GWpcor::gwpcor(
    sdata=sdata,
    vars=selected_vars,
    method=method,
    kernel=kernel,
    bw=bw,
    adaptive=adaptive,
    dMat=dMat
  ))

b2 <- c(gwpcor.mem[1, 1], gwpcor.tim) %>% as.data.frame()


#####################
######## GWSS #######
##################### 


gwss.mem <- bench_memory(
  GWmodel::gwss(
    data=spdata,
    vars=selected_vars,
    kernel=kernel,
    bw=bwgwss,
    adaptive=adaptive,
    dMat=dMat
  )
)

gwss.tim <- bench_time(
  GWmodel::gwss(
    data=spdata,
    vars=selected_vars,
    kernel=kernel,
    bw=bwgwss,
    adaptive=adaptive,
    dMat=dMat
  )
)

b3 <- c(gwss.mem[1, 1], gwss.tim) %>% as.data.frame()


#####################
####### SPGWR #######
##################### 

spgwr.mem <- bench_memory(
  spgwr::gw.cov(
    x = spdata,
    vars = selected_vars,
    adapt = adaptive,
    bw = bw,
    gweight = gwr.bisquare,
    cor = TRUE
  ))

spgwr.tim <- bench_time(
  spgwr::gw.cov(
    x = spdata,
    vars = selected_vars,
    adapt = adaptive,
    bw = bw,
    gweight = gwr.bisquare,
    cor = TRUE
  ))

b4 <- c(spgwr.mem[1, 1], spgwr.tim) %>% as.data.frame()

#####################
###### LCORREL ######
##################### 

lctools.mem <- bench_memory(
  lctools::lcorrel(data, bw, dp.locat)
)

lctools.tim <- bench_time(
  lctools::lcorrel(data, bw, dp.locat)
)

b5 <- c(lctools.mem[1, 1], lctools.tim) %>% as.data.frame()  

#############################
###### BENCHMARK TABLE ######
#############################

benchmark <- rbind(b5, b4, b3, b2, b1)
