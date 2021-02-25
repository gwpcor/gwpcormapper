# Title     : S1 Supplmentary Material for gwpcorMapper: an interactive mapping tool for exploring geographically weighted correlation and partial correlation in high-dimensional geospatial datasets
# Objective : Runs benchmark tests for gwpcorMapper's core algorithmn function.
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

# source helper R function that will in turn call the optimized C++ code.
# this requires that the repository is cloned using git from 
# https://github.com/naru-T/gwpcormapper
# and then set the R working directory to the cloned repository.

source("../gwpcormapper/src/optimized_gwpcor.R")

###############################
######### SAMPLE DATA #########
###############################

# Download the sample data and extract it to the root of the current working 
# directory. The sample data can be downloaded from:
# https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_state_500k.zip
# https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_county_500k.zip
# https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_cousub_500k.zip


# define variables for GW partial correlation
selected_vars <- c("ALAND", "AWATER", "dummy")

# States
states <- st_read("cb_2019_us_state_500k/cb_2019_us_state_500k.shp") %>%
  st_transform(4326)
# a "dummy" variable is required to demostrate partial correlation.
dummy <- 1:nrow(states)
states <- cbind(states, dummy)
# define coordinates
dp.locat.states <- st_centroid(states) %>% st_coordinates()
# distance matrix
dMat.states <- geodist(dp.locat.states, measure = "cheap")

# Counties
counties <- st_read("cb_2019_us_county_500k/cb_2019_us_county_500k.shp") %>%
  st_transform(4326)
# a "dummy" variable is required to demostrate partial correlation.
dummy <- 1:nrow(counties)
counties <- cbind(counties, dummy)
# define coordinates
dp.locat.counties <- st_centroid(counties) %>% st_coordinates()
# distance matrix
dMat.counties <- geodist(dp.locat.counties, measure = "cheap")

# County Subdivisions
counties.sub <- st_read("cb_2019_us_cousub_500k/cb_2019_us_cousub_500k.shp") %>%
  st_transform(4326)
# a "dummy" variable is required to demostrate partial correlation.
dummy <- 1:nrow(counties.sub)
counties.sub <- cbind(counties.sub, dummy)
# define coordinates
dp.locat.counties.sub <- st_centroid(counties.sub) %>% st_coordinates()
# distance matrix
dMat.counties.sub <- geodist(dp.locat.counties.sub, measure = "cheap")


###############################
######### PARAMETERS ##########
###############################

# bandwidth
bw <- 0.25

# bandwidth type
adaptive <- TRUE

# correlation type
method <- "pearson"

# kernel
kernel <- "bisquare"


###############################
####### BENCHMARK TESTS #######
###############################

# states

# memory
states.mem <- bench_memory(
  gwpcor(
    sdata=states,
    vars=selected_vars,
    method=method,
    kernel=kernel,
    bw=bw,
    adaptive=adaptive,
    dMat=dMat.states
  ))
# time
states.tim <- bench_time(
  gwpcor(
    sdata=states,
    vars=selected_vars,
    method=method,
    kernel=kernel,
    bw=bw,
    adaptive=adaptive,
    dMat=dMat.states
  ))

states.bench <- c(states.mem[1, 1], states.tim) %>% as.data.frame()


####

# counties

# memory
counties.mem <- bench_memory(
  gwpcor(
    sdata=counties,
    vars=selected_vars,
    method=method,
    kernel=kernel,
    bw=bw,
    adaptive=adaptive,
    dMat=dMat.counties
  ))
# time
counties.tim <- bench_time(
  gwpcor(
    sdata=counties,
    vars=selected_vars,
    method=method,
    kernel=kernel,
    bw=bw,
    adaptive=adaptive,
    dMat=dMat.counties
  ))

counties.bench <- c(counties.mem[1, 1], counties.tim) %>% as.data.frame()


####

# sub-counties

# memory
subcounties.mem <- bench_memory(
  gwpcor(
    sdata=counties.sub,
    vars=selected_vars,
    method=method,
    kernel=kernel,
    bw=bw,
    adaptive=adaptive,
    dMat=dMat.counties.sub
  ))
#time
subcounties.tim <- bench_time(
  gwpcor(
    sdata=counties.sub,
    vars=selected_vars,
    method=method,
    kernel=kernel,
    bw=bw,
    adaptive=adaptive,
    dMat=dMat.counties.sub
  ))

subcounties.bench <- c(subcounties.mem[1, 1], subcounties.tim) %>% 
  as.data.frame()


####

# final table
meta <- cbind(c("States", "Counties", "County Subdivisions") , 
              c(nrow(states), nrow(counties), nrow(counties.sub)))

benchmark <- rbind(states.bench, counties.bench, subcounties.bench) %>%
  cbind(meta, .)
  

