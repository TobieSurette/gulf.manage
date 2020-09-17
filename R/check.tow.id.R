#' Check Trawl Tow Identifiers
#' 
#' @description Checks tow IDs from snow crab survey set/tow data for errors or inconsistencies.
#' 
#' @examples
#' x <- read.scsset(2020)
#' check.tow.number(x)

#' @export check.tow.id
check.tow.id <- function(x, ...) UseMethod("check.tow.id")

#' @rawNamespace S3method(check.tow.id, scsset)
check.tow.id.scsset <- function(x, ...){
   # Check that tow IDs follows the standard nomenclature:
   regular <- grep("^G[CP][0-9][0-9][0-9][F][R]*", x$tow.id)
   alternates <- grep("^G[CP][0-9][0-9][0-9][A][1-3]*", x$tow.id)
   index <- setdiff(1:nrow(x), c(regular, alternates))
   if (length(index) > 0){
      cat("Tows '", paste0(x$tow.id[index], collapse = "', '"), "' do not follow the standard snow crab station nomenclature.\n") 
   }
   
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
}


