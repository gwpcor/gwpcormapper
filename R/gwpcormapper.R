# Title     : Source and document
# Objective : Links Rcpp files with package and documents data
# Created by: iosefa
# Created on: 2021/03/04
#' @useDynLib gwpcormapper, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' Tokyo 2005 Census Data.
#'
#' The original source of the data is the Portal Site of Official Statistics of Japan website (https://www.e-stat.go.jp/)
#' Note that the original dataset was modified to provide English translations.
#' @name tokyo2005
#' @docType data
#' @author Joseph Percival and Narumasa Tsutsumida \email{ipercival@gmail.com}
#' @references \url{https://www.e-stat.go.jp/}
#' @keywords data
#' @return An sf object with census statistics of the 23 special wards of Tokyo from the year 2005.
#' @examples
#' \dontrun{
#' data(tokyo2005)
#' summary(tokyo2005)
#' }
"tokyo2005"