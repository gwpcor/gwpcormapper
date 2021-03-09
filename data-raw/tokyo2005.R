library(sf)

tokyo2005 <- st_read("data-raw/tokyo2005_sf.gpkg")

usethis::use_data(tokyo2005, overwrite = TRUE)