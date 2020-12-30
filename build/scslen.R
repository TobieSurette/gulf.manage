library(gulf.data)

# Find "new" files:
files <- locate(file = "^scs[0-9]+L")

# Load files:
r <- NULL
for (i in 1:length(files)){
   print(files[i])
   x <- read.csv(files[i], header = TRUE, stringsAsFactors = FALSE)
   
   # Determine survey year:
   year <- unique(x$year)
   
   # Read tow data:
   y <- read.scsset(year, valid = 1)
   y$year <- year(y)
   
   # Import tow IDs and dates:
   if (year < 2019){
      x$tow <- substr(x$tow.id, 3, 5)
      y$tow <- substr(y$tow.id, 3, 5)
      key <- c("year", "tow")
      ix <- match(x[key], y[key])
      x <- cbind(data.frame(date = y$date[ix], stringsAsFactors = FALSE), x)
      x$tow.id <- y$tow.id[ix] # # Replace tow ID.
   }else{
      x$tow.id <- toupper(x$tow.id)
      ix <- match(x[c("year", "tow.id")], y[c("year", "tow.id")])
      x$date <- y$date[ix] # # Replace tow ID.
   }
   
   # Remove redundant data:
   x <- x[-which(names(x) %in% c("year", "tow"))]
   
   # Add missing variables:
   if (i > 1) x[setdiff(names(r), names(x))] <- NA

   # Append data:
   r <- rbind(r, x)
}

# Substitute NA values:
r$comment[is.na(r$comment)] <- ""

# Write to "gulf.data":
path <- gsub("gulf.manage", "gulf.data", getwd())
path <- paste0(path, "/inst/extdata")
if (file.exists(path)){
   years <- sort(unique(year(r)))
   for (i in 1:length(years)){
      file <- paste0(path, "/", "scs.len.", years[i], ".csv") 
      write.csv(r[which(year(r) == years[i]), ], file = file, row.names = FALSE)
   }
}
