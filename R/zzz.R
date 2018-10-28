#' Mr.S
#' 
#' Mr.S identification use GUIDE algorithm
#' 
#' @docType package
#' @author Peigen Zhou <pzhou9@wisc.edu>
#' @import Rcpp 
#' @useDynLib Mr.S
#' @importFrom Rcpp evalCpp
#' @exportPattern "^[[:alpha:]]+"
#' @name Mr.S
NULL  


.onLoad <- function(libname, pkgname) {
    cat(" Create Dummy logger for tree and split;\n")
    initLog(3)
}