library(gulf.data)

years <- 1999:2024
for (i in 1:length(years)){
   year <- years[i]
   
   # Read from Oracle:
   v <- read.gulf.set(year = year, password = password, survey = "ns")
   v <- compress(v)
   
   # Remove redundant variables:
   remove <- c("location", "stratum", "unit.area")
   v <- v[setdiff(names(v), remove)]
   
   # Corrections:
   
   
   # Write to file:
   path <- "C:/Users/SuretteTJ/Desktop/github/gulf.data/inst/extdata"
   if (file.exists(path)){
      file <- paste0(path, "/", "nss.set.", year, ".csv") 
      write.csv(v, file = file, row.names = FALSE)
   }
}

