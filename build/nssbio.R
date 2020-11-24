library(gulf.data)

# Find "new" files:
files <- locate.nssbio(source = "ascii")

files <- files[-grep("p140", files)]

# Load files:
r <- NULL
for (i in 1:length(files)){
   print(i)
   tmp <- read.nssbio(files[i])
   if (is.null(r)){
      r <- tmp
   }else{
      r[setdiff(names(tmp), names(r))] <- NA
      tmp[setdiff(names(r), names(tmp))] <- NA
      r <- rbind(r, tmp[names(r)])
   }
   print(names(r))
}
r <- nssbio(as.data.frame(r))

# Write to "gulf.data":
path <- gsub("gulf.manage", "gulf.data", getwd())
path <- paste0(path, "/inst/extdata")
if (file.exists(path)){
   years <- sort(unique(year(r)))
   for (i in 1:length(years)){
      file <- paste0(path, "/", "nss.bio.", years[i], ".csv") 
      write.csv(r[which(year(r) == years[i]), ], file = file, row.names = FALSE)
   }
}
