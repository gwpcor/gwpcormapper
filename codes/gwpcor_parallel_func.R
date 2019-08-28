gwpcor_parallel <- function (sdata, summary.locat, vars, method = c("pearson", "spearman"), 
                             kernel = "bisquare", adaptive = FALSE, bw, p = 2, theta = 0, 
                             longlat = F, dMat) 
{
  requireNamespace("sp")
  requireNamespace("sf")
  requireNamespace("GWmodel")
  requireNamespace("corpcor")
  requireNamespace("foreach")
  requireNamespace("doParallel")
  `%+%` <- function(x, y) {
    paste0(x, y)
  }
  `%!in%` <- function(x, y) {
    !(x %in% y)
  }
  S_Rho <- function(x, y, w) {
    n <- length(w)
    xr <- rank(x)
    yr <- rank(y)
    scorr <- 1 - (6 * w %*% ((xr - yr)^2))/(n * (n^2 - 1))
  }
  corr.pval <- function(m, n) {
    r <- cov2cor(m)
    t <- r * sqrt((n - 2)/(1 - r^2))
    p.value <- 2 * pt(-abs(t), (n - 2))
    diag(p.value) <- 0
    return(p.value)
  }
  pcorr.pval <- function(m, n) {
    gp <- ncol(m) - 2
    pcor <- corpcor::cor2pcor(m)
    s <- pcor * sqrt((n - 2 - gp)/(1 - pcor^2))
    p.value <- 2 * pt(-abs(s), (n - 2 - gp))
    diag(p.value) <- 0
    return(p.value)
  }
  if (missing(vars)) 
    stop("Variables input error")
  if (length(vars) < 3) 
    stop("At least three variables are needed for partial correlation!")
  if (missing(bw) || bw <= 0) 
    stop("Bandwidth is not specified correctly")
  len.var <- length(vars)
  if (length(vars) == 0) 
    stop("Variables input doesn't match with data")
  if (is(sdata, "Spatial")) {
    p4s <- proj4string(sdata)
    dp.locat <- coordinates(sdata)
  }
  else if (is(sdata, "sf")) {
    len <- length(sdata)
    dp.locat <- sf::st_centroid(sdata) %>% sf::st_coordinates(.)
  }
  else if (is(sdata, "data.frame") && (!missing(dMat))) {
    sdata <- sdata
  }
  else stop("Given data must be a Spatial*DataFrame or data.frame object")
  if (missing(summary.locat)) {
    sp.given <- FALSE
    if (is(sdata, "Spatial")) {
      summary.locat <- sdata
      sp.locat <- coordinates(summary.locat)
    }
    else if (is(sdata, "sf")) {
      summary.locat <- sdata
      sp.locat <- sf::st_centroid(sdata) %>% sf::st_coordinates(.)
    }
  }
  else {
    sp.given <- T
    if (is(summary.locat, "Spatial")) {
      sp.locat <- coordinates(summary.locat)
    }
    else if (is(sdata, "sf")) {
      sp.locat <- sf::st_centroid(sdata) %>% sf::st_coordinates(.)
    }
    else {
      warning("Output loactions are not packed in a Spatial object, and it has to be a two-column numeric vector")
      summary.locat <- sp.locat
    }
  }
  if (is(sdata, "Spatial")) {
    data <- as(sdata, "data.frame")
  }
  else if (is(sdata, "sf")) {
    data <- data.frame(sdata)[, 1:(len - 1)]
  }
  dp.n <- nrow(data)
  sp.n <- nrow(sp.locat)
  if (missing(dMat)) {
    DM.given <- F
    DM1.given <- F
    if (sp.given) {
      dMat <- gw.dist(dp.locat = dp.locat, rp.locat = sp.locat, 
                      p = p, theta = theta, longlat = longlat)
    }
    else {
      dMat <- gw.dist(dp.locat = dp.locat, p = p, theta = theta, 
                      longlat = longlat)
    }
    DM.given <- T
  }
  else {
    DM.given <- T
    DM1.given <- T
    dim.dMat <- dim(dMat)
    if (dim.dMat[1] != dp.n || dim.dMat[2] != sp.n) 
      stop("Dimensions of dMat are not correct")
  }
  x <- data %>% dplyr::select(vars) %>% as.matrix()
  var.n <- ncol(x)
  if (data %>% dplyr::select(vars) %>% anyNA()) 
    stop(" NA values are not allowed")
  if (len.var > var.n) 
    warning("Invalid variables have been specified, please check them again!")
  if (method %!in% c("pearson", "spearman")) {
    stop("The method option should be 'pearson' or 'spearman'.")
  }
  name.comb <- expand.grid(vars, vars) %>% subset(., unclass(Var1) < 
                                                    unclass(Var2)) %>% apply(., 1, function(x) {
                                                      x[1] %+% "." %+% x[2]
                                                    }) %>% as.vector()
  
  foreach_out <- foreach(i = 1:sp.n) %dopar% {
    dist.vi <- dMat[, i]
    W.i <- GWmodel::gw.weight(dist.vi, bw, kernel, adaptive)
    sum.w <- sum(W.i)
    Wi <- c(W.i/sum.w)
    if (method == "pearson") {
      ans_covwt <- cov.wt(x, wt = Wi, cor = TRUE)
      cov.matrix <- ans_covwt$cov
      corr.matrix <- ans_covwt$cor
      corr.pval.matrix <- corr.pval(ans_covwt$cov, 
                                    length(Wi[Wi != 0]))
      pcorr.matrix <- corpcor::cor2pcor(ans_covwt$cov)
      pcorr.pval.matrix <- pcorr.pval(ans_covwt$cov, 
                                      length(Wi[Wi != 0]))
      cov.mat_i__ <- cov.matrix[lower.tri(cov.matrix)]
      corr.mat_i__ <- corr.matrix[lower.tri(corr.matrix)]
      corr.pval.mat_i__ <- corr.pval.matrix[lower.tri(corr.matrix)]
      pcorr.mat_i__ <- pcorr.matrix[lower.tri(pcorr.matrix)]
      pcorr.pval.mat_i__ <- pcorr.pval.matrix[lower.tri(pcorr.pval.matrix)]
      out <- list(cov.mat_i__ = cov.mat_i__, corr.mat_i__ = corr.mat_i__, 
                  corr.pval.mat_i__ = corr.pval.mat_i__, pcorr.mat_i__ = pcorr.mat_i__, 
                  pcorr.pval.mat_i__ = pcorr.pval.mat_i__)
    }
    else if (method == "spearman") {
      ans_rank_covwt <- cov.wt(apply(x, 2, rank), wt = Wi, 
                               cor = TRUE)
      s.cov.matrix <- ans_rank_covwt$cov
      s.corr.matrix <- ans_rank_covwt$cor
      s.corr.pval.matrix <- corr.pval(ans_rank_covwt$cor, 
                                      length(Wi[Wi != 0]))
      s.pcorr.matrix <- corpcor::cor2pcor(ans_rank_covwt$cov)
      s.pcorr.pval.matrix <- pcorr.pval(ans_rank_covwt$cor, 
                                        length(Wi[Wi != 0]))
      s.cov.mat_i__ <- s.cov.matrix[lower.tri(s.cov.matrix)]
      s.corr.mat_i__ <- s.corr.matrix[lower.tri(s.corr.matrix)]
      s.corr.pval.mat_i__ <- s.corr.pval.matrix[lower.tri(s.corr.pval.matrix)]
      s.pcorr.mat_i__ <- s.pcorr.matrix[lower.tri(s.pcorr.matrix)]
      s.pcorr.pval.mat_i__ <- s.pcorr.pval.matrix[lower.tri(s.pcorr.matrix)]
      out <- list(s.cov.mat_i__ = s.cov.mat_i__, s.corr.mat_i__ = s.corr.mat_i__, 
                  s.corr.pval.mat_i__ = s.corr.pval.mat_i__, 
                  s.pcorr.mat_i__ = s.pcorr.mat_i__, s.pcorr.pval.mat_i__ = s.pcorr.pval.mat_i__)
    }
    out
  }
  
  
  if (method == "pearson") {
    cov.mat = matrix(t(sapply(foreach_out, "[[", "cov.mat_i__")), 
                     ncol = length(name.comb))
    corr.mat = matrix(t(sapply(foreach_out, "[[", "corr.mat_i__")), 
                      ncol = length(name.comb))
    corr.pval.mat = matrix(t(sapply(foreach_out, "[[", 
                                    "corr.pval.mat_i__")), ncol = length(name.comb))
    pcorr.mat = matrix(t(sapply(foreach_out, "[[", "pcorr.mat_i__")), 
                       ncol = length(name.comb))
    pcorr.pval.mat = matrix(t(sapply(foreach_out, "[[", 
                                     "pcorr.pval.mat_i__")), ncol = length(name.comb))
    colnames(cov.mat) <- "cov_" %+% name.comb
    colnames(corr.mat) <- "corr_" %+% name.comb
    colnames(corr.pval.mat) <- "corr_pval_" %+% name.comb
    colnames(pcorr.mat) <- "pcorr_" %+% name.comb
    colnames(pcorr.pval.mat) <- "pcorr_pval_" %+% name.comb
    res.df <- data.frame(cov.mat, corr.mat, corr.pval.mat, 
                         pcorr.mat, pcorr.pval.mat)
  }
  else if (method == "spearman") {
    s.cov.mat = matrix(t(sapply(foreach_out, "[[", "s.cov.mat_i__")), 
                       ncol = length(name.comb))
    s.corr.mat = matrix(t(sapply(foreach_out, "[[", "s.corr.mat_i__")), 
                        ncol = length(name.comb))
    s.corr.pval.mat = matrix(t(sapply(foreach_out, "[[", 
                                      "s.corr.pval.mat_i__")), ncol = length(name.comb))
    s.pcorr.mat = matrix(t(sapply(foreach_out, "[[", 
                                  "s.pcorr.mat_i__")), ncol = length(name.comb))
    s.pcorr.pval.mat = matrix(t(sapply(foreach_out, "[[", 
                                       "s.pcorr.pval.mat_i__")), ncol = length(name.comb))
    colnames(s.cov.mat) <- "scov_" %+% name.comb
    colnames(s.corr.mat) <- "scorr_" %+% name.comb
    colnames(s.corr.pval.mat) <- "scorr_pval_" %+% name.comb
    colnames(s.pcorr.mat) <- "spcorr_" %+% name.comb
    colnames(s.pcorr.pval.mat) <- "spcorr_pval_" %+% 
      name.comb
    res.df <- data.frame(s.cov.mat, s.corr.mat, s.corr.pval.mat, 
                         s.pcorr.mat, s.pcorr.pval.mat)
  }
  
  
  rownames(res.df) <- rownames(sp.locat)
  griddedObj <- F
  if (is(summary.locat, "Spatial")) {
    if (is(summary.locat, "SpatialPolygonsDataFrame")) {
      polygons <- polygons(summary.locat)
      SDF <- SpatialPolygonsDataFrame(Sr = polygons, data = res.df, 
                                      match.ID = F)
    }
    else {
      griddedObj <- gridded(summary.locat)
      SDF <- SpatialPointsDataFrame(coords = sp.locat, 
                                    data = res.df, proj4string = CRS(p4s), match.ID = F)
      gridded(SDF) <- griddedObj
    }
  }
  else if (is(summary.locat, "sf")) {
    SDF <- st_sf(id = 1:nrow(summary.locat), geometry = st_sfc(st_geometry(summary.locat))) %>% 
      merge(., res.df %>% dplyr::mutate(id = rownames(res.df)))
  }
  else {
    SDF <- SpatialPointsDataFrame(coords = sp.locat, data = res.df, 
                                  proj4string = CRS(p4s), match.ID = F)
  }
  res <- list(SDF = SDF, vars = vars, kernel = kernel, adaptive = adaptive, 
              bw = bw)
  class(res) <- "gwpcor"
  invisible(res)
}
