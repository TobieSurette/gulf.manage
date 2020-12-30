library(gulf.data)

# Find "new" files:
file <- locate(file = "scs.set.2012")
x <- read.csv(files[i], header = TRUE, stringsAsFactors = FALSE)
x$tow.id[which(x$date == "2012-09-16" & x$tow.id == "GP205A1")] <- "GP225A1"  # Tow id correction.

# Write to "gulf.data":
path <- gsub("gulf.manage", "gulf.data", getwd())
path <- paste0(path, "/inst/extdata")
if (file.exists(path)){
   years <- sort(unique(year(x)))
   for (i in 1:length(years)){
      file <- paste0(path, "/", "scs.set.", years[i], ".csv") 
      write.csv(x[which(year(x) == years[i]), ], file = file, row.names = FALSE)
   }
}
