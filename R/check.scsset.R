#' Check Snow Crab Tow/Set Data
#' 
#' @description Functions for validating \code{scsset} data objects for issues.
#' 
#' @param x Object.
#' 
#' 
#' @section Functions:
#' \describe{
#'   \item{}
#' }
#' 
#' @examples 
#' check(read.scsset(2020))

#' @export
#' 

check.scsset(x, ...){
   check.key(x)
   check.tow.id(x)
   check.tow.number(x)
}
