#' Check Methods
#' 
#' @description Functions for checking data objects.
#' 
#' @param x Data Object(s).
#' 

#' @export check
check <- function(x, ...) UseMethod("check")

#' @export check.coordinates
check.coordinates <- function(x, ...) UseMethod("check.coordinates")

#' @export check.time
check.time <- function(x, ...) UseMethod("check.time")
