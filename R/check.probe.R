#' Check \strong{probe} Data
#' 
#' @description Check \code{probe} data object for errors or inconsistencies.
#' 
#' 

#' @export check.probe
check.probe <- function(x, ...) UseMethod("check.probe")

#' @rawNamespace S3method(check.probe,default)
check.probe.default <- function(x, ...){
   check.time(x, ...)
}

#' @describeIn check.probe Check 'probe' object times for missing entries or errors.
#' @rawNamespace S3method(check.time,probe)
check.time.probe <- function(x, ...){
   t <- time(x)
   tow.id <- tow.id(x)
   year <- as.numeric(substr(unique(gulf.utils::date(x)), 1, 4))

   # Check that observations are ordered:
   if (any(diff(order(t)) < 0)){
      cat(paste0("Year = ", year, ", tow ID = '", tow.id, "' time observations are not in order.\n"))
   }

   # Check for missing time stamps:
   if (any(is.na(t))){
      cat(paste0("Year = ", year, ", tow ID = '", tow.id, "' has ", sum(is.na(t)), " NA time entries.\n"))
   }

   # Check for regular time measurements:
   dt <- as.numeric(difftime(t[-1], t[-length(t)], units = "secs"))
   tab <- table(dt)
   if (any(dt == 0)){
      cat(paste0("Year = ", year, ", tow ID = '", tow.id, "' ",tab["0"] ," duplicate time entries.\n"))
   }
   if (length(tab) > 1){
      cat(paste0("Year = ", year, ", tow ID = '", tow.id, "' contains a missing data block of ", max(as.numeric(names(tab))), " seconds.\n"))
   }
}
