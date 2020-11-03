#' Snow Crab Set Data from Biological Data
#' 
#' @description Functions to compile snow crab set data from snow crab biological data.
#' 
#' @param x \code{scsbio} object.
#' 
#' @examples 
#' x <- read.scsbio(1995)
#' as.scsset(x)

#' @describeIn as.scsset Compile snow crab set data from snow crab biological data.
#' @rawNamespace S3method(as.scsset,scsbio)
as.scsset.scsbio <- function(x, collapse = FALSE, ...){
   key <- c("date", "tow.number") # Index key.
   
   # Function to aggregate samplers:
   fun <- function(x) return(paste(sort(unique(unlist(strsplit(unique(x), ", ")))), collapse = ", "))
   
   # Generate summary:
   r <- aggregate(x[c("latitude.start", "longitude.start")], by = x[key], unique)
   r$depth <- aggregate(list(x = x$depth), by = x[key], unique)$x
   r$samplers <- aggregate(list(x = sampler(x)), by = x[key], fun)$x
   
   # Average out multiple values:
   if (collapse){
      if (is.list(r$depth)) r$depth <- unlist(lapply(r$depth, mean, na.rm = TRUE))
      if (is.list(r$latitude.start)) r$latitude.start <- unlist(lapply(r$latitude.start, mean, na.rm = TRUE))
      if (is.list(r$longitude.start)) r$longitude.start <- unlist(lapply(r$longitude.start, mean, na.rm = TRUE))
   }
   
   if (all(unique(gulf.utils::year(r) <= 1998))){
      str <- names(r)
      str <- gsub("latitude.start", "loran.x", str)
      str <- gsub("longitude.start", "loran.y", str)
      names(r) <- str
      
      # Coordonate conversions:
      ix <- unlist(lapply(r$loran.x, function(x) (length(x) == 1))) & unlist(lapply(r$loran.y, function(x) (length(x) == 1)))
      tmp <- gulf.spatial::loran2deg(unlist(r$loran.x[ix]), unlist(r$loran.y[ix]))
      tmp[tmp$lat == 0 | tmp$long == 0, ] <- NA
      r$latitude.start <- NA
      r$latitude.start[ix] <- tmp$lat
      r$longitude.start <- NA
      r$longitude.start[ix] <- tmp$long
   }
   
   return(r)
}



