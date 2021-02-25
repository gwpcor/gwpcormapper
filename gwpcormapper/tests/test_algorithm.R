# Title     : TODO
# Objective : TODO
# Created by: iosefa
# Created on: 2021/02/25

library(sf)
library(geodist)
library(dplyr)
library(GWpcor)
library(Rcpp)
library(corpcor)

source("gwpcormapper/src/optimized_gwpcor.R")

# load data in various required formats

# selected variables
selected_vars <- c("total.population", "Daytime.Population", "Population.Density")

# sf
sdata <- st_read("data/tokyo2005_sf_en.gpkg") %>%
  st_transform(.,4326)

# parameters

# bandwidth
bw <- 0.2

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

source("gwpcormapper/src/optimized_gwpcor.R")
app <- gwpcor(
  sdata=sdata,
  vars=selected_vars,
  method=method,
  kernel=kernel,
  bw=bw,
  adaptive=adaptive,
  dMat=dMat
)

lib <- GWpcor::gwpcor(
  sdata=sdata,
  vars=selected_vars,
  method=method,
  kernel=kernel,
  bw=bw,
  adaptive=adaptive,
  dMat=dMat
)

all.equal(app$SDF$corr_Daytime.Population.Population.Density, lib$SDF$corr_Daytime.Population.Population.Density)
all.equal(app$SDF$corr_pval_Daytime.Population.Population.Density, lib$SDF$corr_pval_Daytime.Population.Population.Density)

x <- sf::st_drop_geometry(sdata) %>%
  dplyr::select(selected_vars) %>%
  as.matrix()


corr.pval <- function (m, n) {
  r <- cov2cor(m)
  t <- r * sqrt((n - 2) / (1 - r ^ 2))
  p.value <- 2 * pt(-abs(t), (n - 2))
  diag(p.value) <- 0
  return(p.value)
}

dist.vi <- dMat[, 100]
sourceCpp("gwpcormapper/src/gwpcor.cpp")
W.i <- calc_weight(type = kernel, adapt = adaptive, dist_vec =dist.vi, bw =bw)
sum.w <- sum(W.i)
Wi <- W.i / sum.w

ans_covwt <-  cov.wt(x, wt = Wi, cor = TRUE)
cov.matrix <- ans_covwt$cov
corr.matrix <- ans_covwt$cor
corr.pval.matrix <- corr.pval(ans_covwt$cov, length(Wi[Wi != 0]))


foo <- corrPval(ans_covwt$cor, length(Wi[Wi != 0]))
corr.pval.matrix
foo


test <- c(0,1,2,3,1,0,0,2,0,2,3,0)
length(test)
length(test[test != 0])
foobar(test)
