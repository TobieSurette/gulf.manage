#' Check Trawl Tow Identifiers
#' 
#' @description Checks tow IDs from snow crab survey set/tow data for errors or inconsistencies.
#' 
#' @examples
#' x <- read.scsset(2020)
#' check.tow.number(x)

#' @export check.tow.id
check.tow.id <- function(x, ...) UseMethod("check.tow.id")

#' @describeIn check.tow.id Check that \code{scsset} tow IDs are consistent.
#' @rawNamespace S3method(check.tow.id, scsset)
check.tow.id.scsset <- function(x, ...){
   # Check that tow IDs follows the standard nomenclature:
   regular <- grep("^G[CP][0-9]{3,3}[F][R]*", x$tow.id)
   alternates <- grep("^G[CP][0-9]{3,3}[A][1-3]*", x$tow.id)
   index <- setdiff(1:nrow(x), c(regular, alternates))
   if (length(index) > 0){
      cat("Tows '", paste0(x$tow.id[index], collapse = "', '"), "' do not follow the standard snow crab station nomenclature.\n") 
   }
   
   # Define 'year':
   x$year <- as.numeric(substr(x$date, 1, 4))
   
   # Check for proper tow ID sequencing:
   f <- function(x, suffix = c("F", "FR", "A1", "A2", "A3")){
      index <- match(substr(x, 6, 10), suffix)
      index <- setdiff(1:max(index), index)
      index <- setdiff(index, 2)
      if (length(index) == 0) return(NULL) else return(paste0(unique(substr(x, 1, 5)), suffix[index]))
   }
   r <- aggregate(list(tow.id = x$tow.id),  by = list(year = x$year, station = substr(x$tow.id, 3, 5)), f)
   r <- r[unlist(lapply(r$tow.id, length)) > 0, ]
   if (nrow(r) > 0){
      for (i in 1:nrow(r)){
         cat(paste0("Year = ", r$year[i], ", station/grid = '", as.character(r$station[i]), "', is missing tow ID(s) '", paste(r$tow.id[[i]], collapse = "', '"),  "'.\n"))
      }
   }   
   
   # Check for doubly valid stations:
   t <- table(substr(x$tow.id[x$valid == 1], 3, 5))
   t <- t[t > 1]
   if (length(t) > 0) for (i in 1:length(t)) cat(paste0("Station '", names(t[i]), "' has ", t[i], " valid tows.\n"))
      
   # Check for stations with no valid tows:
   s <- setdiff(1:max(as.numeric(substr(x$tow.id, 3, 5))), as.numeric(substr(x$tow.id[x$valid == 1], 3, 5)))
   if (length(s) > 0) cat(paste0("Station(s) ", paste(s, collapse = ", "), " have no valid tows.\n"))
}

#' @describeIn check.tow.id Check that \code{esonar} tow IDs are consistent.
#' @rawNamespace S3method(check.tow.id,esonar)
check.tow.id.esonar <- function(x, ...){
   t <- c(tow.id(x), tow.id(x, method = "time"), tow.id(x, method = "latlon"))
   if (length(unique(t)) > 1){
      cat(paste0("Tow.id mis-match : file = '", t[1], "', time = '", t[2], "', lat-lon = '", t[3], "'.\n"))
   }   
}

#' @describeIn check.tow.id Check that \code{star.oddi} tow IDs are consistent.
#' @rawNamespace S3method(check.tow.id,esonar)
check.tow.id.star.oddi <- function(x, ...){
   t <- c(tow.id(x), tow.id(x, method = "time"))
   if (length(unique(t)) > 1){
      cat(paste0("Tow.id mis-match : file = '", t[1], "', time = '", t[2], "'.\n"))
   }   
}

