library(gulf.data)

years <- 1999:2024
for (i in 1:length(years)){
   print(years[i])
   year <- years[i]
   
   # Read from Oracle:
   v <- read.gulf.len(year = year, password = password, survey = "ns")
   v <- compress(v)
   
   # Remove redundant variables:
   #remove <- c("location", "stratum", "unit.area")
   #v <- v[setdiff(names(v), remove)]
   
   # Corrections:
   
   # Write to file:
   if (nrow(v) > 0){
      path <- "C:/Users/SuretteTJ/Desktop/github/gulf.data/inst/extdata"
      if (file.exists(path)){
         file <- paste0(path, "/", "nss.len.", year, ".csv") 
         write.csv(v, file = file, row.names = FALSE)
      }
   }
}
