# Read raw ascii data:
v <- read.nssset(source = "ascii")

# Corrections here:


# Define local gulf.data repository path:
path <- gsub("gulf.manage", "gulf.data", getwd())
path <- paste0(path, "/inst/extdata")
if (file.exists(path)){
   years <- sort(unique(year(v)))
   for (i in 1:length(years)){
      v[which(year(v) == years[i]), ]
      file <- paste0(path, "/", "nss.set.", years[i], ".csv") 
      write.csv(v[which(year(v) == years[i]), ], file = file, row.names = FALSE)
   }
}
