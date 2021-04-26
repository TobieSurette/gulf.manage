library(gulf.data)

year <- 2013
probe <- "tilt"
 
files <- locate(path = paste0("W:/Crab/Offshore Crab Common/Fishing Year ", year, "/Trawl Data/South Western Gulf/Star Oddi"))
files <- files[grep("[.][Dd][Aa][Tt]$", files)]
files <- files[-grep("[/][01]-", files)]

# Remove duplicate DAT files:
res <- files[grep(probe, tolower(files))]
names <- unlist(lapply(strsplit(res, "/"), function(x) x[length(x)]))

# Remove duplicated files, taking the bigger file:
z <- names[duplicated(names)]
remove <- NULL
for (i in 1:length(z)){
   ix <- grep(z[i], toupper(res))
   print(which.min(file.info(res[ix])$size))
   if (length(ix) <= 3) remove <- c(remove, ix[file.info(res[ix])$size != max(file.info(res[ix])$size)])
}
res <- res[-remove]


names <- unlist(lapply(strsplit(res, "/"), function(x) x[length(x)]))
ix <- which(names %in% names[which(duplicated(names))])
info <- file.info(res[ix])
if (any(duplicated(names))) res <- res[-which(duplicated(names))]

# Find tow.id:
fun <- function(x){
   ix <- grep("GP[0-9]+", toupper(x))
   if (length(ix) != 1) print(x)
   return(x[ix])
} 

tmp <- unlist(lapply(strsplit(toupper(res), "[/]"), fun))
tmp <- substr(tmp, 6, 120)
sum(duplicated(tmp))

# Remove duplicated files, taking the bigger file:
z <- tmp[duplicated(tmp)]
remove <- NULL
for (i in 1:length(z)){
   ix <- grep(z[i], toupper(res))
   print(which.min(file.info(res[ix])$size))
   
   if (all(file.info(res[ix])$size == max(file.info(res[ix])$size))){
      ix <- ix[1]
   }else{
      ix <- ix[file.info(res[ix])$size != max(file.info(res[ix])$size)]
   }  
   
   remove <- c(remove, ix)
}
res <- res[-remove]

res <- res[grep("GP[0-9]+", res)]

tmp <- unlist(lapply(strsplit(toupper(res), "[/]"), fun))
tmp <- substr(tmp, 6, 120)
sum(duplicated(tmp))

names <- unlist(lapply(strsplit(toupper(res), "[/]"), fun))
names <- substr(names, 6, 120)

dir.create(paste0("C:/Users/SuretteTJ/Desktop/gulf.trawl.data/inst/extdata/scs.star.oddi.", year))
dir.create(paste0("C:/Users/SuretteTJ/Desktop/gulf.trawl.data/inst/extdata/scs.star.oddi.", year, "/tilt"))
dir.create(paste0("C:/Users/SuretteTJ/Desktop/gulf.trawl.data/inst/extdata/scs.star.oddi.", year, "/headline"))
file.copy(res, paste0("C:/Users/SuretteTJ/Desktop/gulf.trawl.data/inst/extdata/scs.star.oddi.", year, "/", probe, "/", paste0(names, ".DAT")))

