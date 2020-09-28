#' Check Snow Crab Tow/Set Data
#' 
#' @description Functions for validating \code{scsset} data objects for issues.
#' 
#' @param x Object.
#' 
#' @examples 
#' check(read.scsset(2020))

#' @export
check.scsset <- function(x, ...){
   check.key(x)
   check.tow.id(x)
   check.tow.number(x)
}
