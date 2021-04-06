# Title     : gwpcor
# Objective : Main gwpcor function. This is based on the gwpcor function
#             in the GWpcor library by Tsutsumida and Percival:
#             https://github.com/naru-T/gwpcor.
# Created by: Joseph Percival and Narumasa Tsutsumida
# Created on: 2021/02/26


gwpcor <- function(sdata, vars, method = c("pearson", "spearman"),
                   kernel = "bisquare", adaptive = FALSE, bw, dMat) {

  if (adaptive && ((bw <= 0) || (bw >1))){
    stop("adaptive kernel should be between 0 and 1")
  }

  if (!methods::is(sdata, "sf")) {
    stop("Data must be sf")
  }

  if (!(method %in% c("pearson", "spearman"))) {
    stop("The method option should be 'pearson' or 'spearman'.")
  }

  if (nrow(sdata) == 0) {
    stop("data has no observations.")
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
