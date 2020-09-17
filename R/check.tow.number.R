#' Check Trawl Tow Numbers
#' 
#' @description Checks the tow numbers from snow crab survey set/tow data for errors or inconsistencies.
#' 
#' @param x Data object.
#' 
#' @examples 
#' x <- read.scsset(2020)
#' check.tow.number(x)

#' @export check.tow.number
check.tow.number <- function(x, ...) UseMethod("check.tow.number")

#' @rawNamespace S3method(check.tow.number, scsset)
check.tow.number.scsset <- function(x, ...){
   # Determine which tows are missing on from a list of tow numbers on a given day:
   r <- aggregate(x["tow.number"], by = list(date = date(x)), function(x) setdiff(1:max(x), sort(x)))
   r <- r[unlist(lapply(r$tow.number, length)) > 0, ]
   if (nrow(r) > 0){
      for (i in 1:nrow(r)){
         cat(paste0("Date = '", as.character(r$date[i]), "', missing tow number(s) ", paste(r$tow.number[[i]], collapse = ", "),  ".\n"))
      }
   }
}
