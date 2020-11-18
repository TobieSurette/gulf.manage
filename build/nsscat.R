library(gulf.data)

# Find "new" files:
file <- dir(pattern = "*c.new", path = "/Users/crustacean/Desktop/gulf.manage/inst/extdata/raw/", full.names = TRUE)

# Load files:
r <- NULL
for (i in 1:length(file)){
   x <- read.nsscat(file[i])
   r <- rbind(r, x)
}

# Write to "gulf.data":
path <- gsub("gulf.manage", "gulf.data", getwd())
path <- paste0(path, "/inst/extdata")
if (file.exists(path)){
   years <- sort(unique(year(r)))
   for (i in 1:length(years)){
      file <- paste0(path, "/", "nss.cat.", years[i], ".csv") 
      write.csv(r[which(year(r) == years[i]), ], file = file, row.names = FALSE)
   }
}
