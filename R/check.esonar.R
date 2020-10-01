#' Check \strong{eSonar} Data
#' 
#' @description Checks an 'esonar' object for connectivity or data errors.
#' 
#' @param x Data object.
#' 

#' @export check.esonar
check.esonar <- function(x, ...) UseMethod("check.esonar")

check.esonar.default <- function(x, ...){
   check.key(x, ...)
   check.time(x, ...)
}

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

#' @describeIn check.esonar Check index key for an \code{esonar} object.
#' @rawNamespace S3method(check.key,esonar)
check.key.esonar <- function(x, ...){
   index <- which(any(duplicated(time(x))))
   if (length(index) > 0){
      year <- as.numeric(substr(unique(gulf.utils::date(x)), 1, 4))
      cat(paste0("Year = ", year, ", tow ID = '", tow.id(x), "' has ", length(index), " duplicated time entries.\n"))
   }
}
