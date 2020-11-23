library(gulf.data)

# Find "new" files:
files <- locate.nsslen(source = "ascii")

# Load files:
r <- NULL
for (i in 1:length(files)){
   print(files[i])
   x <- read.nsslen(files[i])
   r <- rbind(r, x)
}

# Write to "gulf.data":
path <- gsub("gulf.manage", "gulf.data", getwd())
path <- paste0(path, "/inst/extdata")
if (file.exists(path)){
   years <- sort(unique(year(r)))
   for (i in 1:length(years)){
      file <- paste0(path, "/", "nss.len.", years[i], ".csv") 
      write.csv(r[which(year(r) == years[i]), ], file = file, row.names = FALSE)
   }
}
