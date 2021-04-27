library(gulf.data)

year <- 2019

files <- locate(path = "C:/Users/SuretteTJ/Desktop/NSS RawData", file = "*.DAT")
files <- files[grep(year, files)]

# Remove duplicate DAT files:
names <- unlist(lapply(strsplit(files, "/"), function(x) x[length(x)]))

# Remove duplicated files, taking the bigger file:
z <- names[duplicated(names)]
remove <- NULL
for (i in 1:length(z)){
   print(z[i])
   ix <- grep(z[i], toupper(files))
   print(file.info(files[ix])$size)
   #print(which.min(file.info(files[ix])$size))
   remove <- c(remove, ix[file.info(files[ix])$size != max(file.info(files[ix])$size)])
}
files <- files[-remove]

# Remove files with same content:
names <- unlist(lapply(strsplit(files, "/"), function(x) x[length(x)]))
names <- unique(gsub("^0-", "", names))

remove <- NULL
for (i in 1:length(names)){
   ix <- grep(names[i], toupper(files))
     print(file.info(files[ix])$size)
   #print(which.min(file.info(files[ix])$size))
   remove <- c(remove, ix[file.info(files[ix])$size != max(file.info(files[ix])$size)])
   if (length(unique(file.info(files[ix])$size)) == 1) remove <- c(remove, ix[2:length(ix)])
}
files <- files[-remove]

# 
names <- unlist(lapply(strsplit(files, "/"), function(x) x[length(x)]))
names <- gsub("^0-", "", names)

dir.create(paste0("C:/Users/SuretteTJ/Desktop/gulf.trawl.data/inst/extdata/nss.star.oddi.", 2019))
dir.create(paste0("C:/Users/SuretteTJ/Desktop/gulf.trawl.data/inst/extdata/nss.star.oddi.", 2020))

ix <- grep("2019", files)
file.copy(files[ix], paste0("C:/Users/SuretteTJ/Desktop/gulf.trawl.data/inst/extdata/nss.star.oddi.2019/", names[ix]))

ix <- grep("2020", files)
file.copy(files[ix], paste0("C:/Users/SuretteTJ/Desktop/gulf.trawl.data/inst/extdata/nss.star.oddi.2020/", names[ix]))




