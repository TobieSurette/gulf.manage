#' Check \strong{eSonar} Data
#' 
#' @description Checks an 'esonar' object for connectivity or data errors.
#' 
#' @param x Data object.
#' 

#' @export check.esonar
check.esonar <- function(x, ...) UseMethod("check.esonar")

#' @describeIn check.esonar Check that \code{scsset} records have corresponding \code{esonar} data.
#' @rawNamespace S3method(check.esonar,scsset)
check.esonar.scsset <- function(x, ...){
   year <- unique(as.numeric(substr(gulf.utils::date(x), 1, 4)))
   files <- locate.esonar(year = year)
   tow.id <- unlist(lapply(strsplit(files, "/"), function(x) x[length(x)]))
   tow.id <- toupper(unlist(lapply(strsplit(tow.id, "[.]"), function(x) x[1])))
   index <- match(x$tow.id, tow.id)  
   tows <- x$tow.id[which(is.na(index) & (x$valid == 1))]
   cat(paste0("Year = ", year, ", Tow ID '", tows, "' is a valid tow with no corresponding eSonar file.\n"))
}

#' @describeIn check.esonar Check that \code{esonar} tow IDs are consistent.
#' @rawNamespace S3method(check.tow.id,esonar)
check.tow.id.esonar <- function(x, ...){
   t <- c(tow.id(x), tow.id(x, method = "time"), tow.id(x, method = "latlon"))
   if (length(unique(t)) > 1){
      cat(paste0("Tow.id mis-match : file = ", t[1], ", time = ", t[2], "lat-lon = ", t[3], ".\n"))
   }   
}

#' @describeIn check.esonar Check 'esonar' object times for missing entries or errors.
#' @export check.time.esonar
check.time.esonar <- function(x, ...){
   t <- time(x)
   year <- as.numeric(substr(unique(gulf.utils::date(x)), 1, 4))
   tow.id <- tow.id(x)
   
   # Check for missing time stamps:
   if (any(is.na(t))) 
      cat(paste0("Year = ", year, ", tow ID = '", tow.id, "' has ", sum(is.na(t)), " NA time entries.\n"))
   
   # Check for regular time measurements:
   dt <- difftime(t[-1], t[-length(t)], units = "secs") 
   if (any(dt > 1)) 
      cat(paste0("Year = ", year, ", tow ID = '", tow.id, "' contains a missing data block of ", max(dt), " seconds.\n"))
}

#' @describeIn check.esonar Check index key for an \code{esonar} object.
#' @rawNamespace S3method(check.key,esonar)
check.key.esonar <- function(x, ...){
   index <- which(any(duplicated(time(x))))
   if (length(index) > 0){
      year <- as.numeric(substr(unique(gulf.utils::date(x)), 1, 4))
      cat(paste0("Year = ", year, ", tow ID = '", tow.id(x), "' has ", length(index), " duplicated time entries.\n"))
   }
}
