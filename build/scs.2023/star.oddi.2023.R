library(gulf.data)

# Search directory for Star Oddi files, assigns a survey tow ID, discards redundant files, 
# and copies the remaining set to 'gulf.probe.data'.

# Define survey year:
year <- 2023

# Read survey set data:
file <- paste0(gsub("gulf.manage", "gulf.data", getwd(), fixed = TRUE), "/inst/extdata/scs.set.", year, ".csv")
x <- scsset(read.csv(file))

# Locate candidate Star Oddi files:
extension <- "ACC"  # "DAT" or ACC"
path <- paste0(options()$gulf.path$snow.crab$survey, "Fishing Year ", year, "/Trawl Survey/star oddi")
files <- locate(path = path, file = paste0("*.", extension))

# Compile file statistics and identify tow ID:
res <- data.frame(file = files)
res$tow.id <- ""
res$difference <- NA
res$records <- NA
res$location <- ""
res$position <- ""
t <- time(x, "start")
t.stop <- time(x, "stop")
t.haul <- time(x, "haul")

# Assign probe location:
res$location[grep("headline", res$file)] <- "headline"
res$location[grep("wing", res$file)] <- "headline"
res$location[grep("foot", res$file)] <- "footrope"
res$location[grep("door", tolower(res$file))] <- "door"

# Assign probe position:
#res$file[grep("port", tolower(res$file))] 
res$position <- "center"
res$position[grep("stbd", tolower(res$file))] <- "starboard"
res$position[grep("starboard", tolower(res$file))] <- "starboard"
res$position[grep("port", tolower(res$file))] <- "port" 

# Get tow ID from file name:
res$tow.id.file <- ""
res$tow.id.start <- ""
res$tow.id.stop <- ""
res$tow.id.haul <- ""

tmp <- unlist(lapply(strsplit(tolower(res$file), "gp"), function(x) x[2]))
tmp <- unlist(lapply(strsplit(tmp, "/"), function(x) x[1]))
ix <- !is.na(tmp) & res$tow.id.file == ""
res$tow.id.file[ix] <- paste0("GP", toupper(tmp[ix]))

for (i in 1:nrow(res)){
   print(i)
   
   z <- read.star.oddi(res$file[i])
   
   zlim <- c(time(z[1,]), time(z[nrow(z),]))
   
   ix <- which((t >= zlim[1]) & (t <= zlim[2]))
   ix.stop <- which((t.stop >= zlim[1]) & (t.stop <= zlim[2]))
   ix.haul <- which((t.haul >= zlim[1]) & (t.haul <= zlim[2]))

   if (length(ix) > 0){
      if (length(ix) > 1) print(x$tow.id[ix])
      res$tow.id.start[i] <- x$tow.id[ix]
   }
   if (length(ix.stop) > 0) res$tow.id.stop[i] <- x$tow.id[ix.stop[1]]
   if (length(ix.haul) > 0) res$tow.id.haul[i] <- x$tow.id[ix.haul[1]]
   
   res$records[i] <- nrow(z)  
}
res$keep <- FALSE

# Identify misaligned tow IDs (due to time errors in set file):
res$tow.id <- res$tow.id.file
ix <- which((res$tow.id.start != res$tow.id.stop) | (res$tow.id.start != res$tow.id.haul) | (res$tow.id.stop != res$tow.id.haul))
r <- res[ix, ]

# Tow IDs for which there are no matching Star Oddi files:
x$tow.id[-which(x$tow.id %in% res$tow.id) ]

# Remove tows with no confirmed tow IDs:
res <- res[res$tow.id.start != "", ]
res$tow.id <- res$tow.id.start

# Remove files with no identified attachment location:
res <- res[res$location != "", ]

# Identify which files to keep:
u <- unique(res[c("tow.id", "location", "position")])
for (i in 1:nrow(u)){
   ix <- which((res$tow.id == u$tow.id[i]) & (res$location == u$location[i]) & (res$position == u$position[i]))
   
   r <- res[ix, ]
   res$keep[ix[which.max(r$records)]] <- TRUE
}

# Remove redundant files:
res <- res[res$keep, ]

# Assign missing position entries:
res$position[which(res$position == "" & res$location == "headline")] <- "center"
res$position[which(res$position == "" & res$location == "footrope")] <- "center"
res$position[which(res$position == "" & res$location == "door")] <- "port" # Arbitrary assignment.

# Create target directories:
target <- paste0(gsub("gulf.manage", "gulf.trawl.data", getwd()), "/inst/extdata/scs.star.oddi.", year)
if (!file.exists(target)) dir.create(target)
if (any(res$location == "headline")){
   if (!file.exists(paste0(target, "/headline"))) dir.create(paste0(target, "/headline"))
   if (!file.exists(paste0(target, "/headline/center"))) dir.create(paste0(target, "/headline/center")) 
}
if (any(res$location == "footrope")){
   if (!file.exists(paste0(target, "/footrope"))) dir.create(paste0(target, "/footrope"))
   if (!file.exists(paste0(target, "/footrope/center"))) dir.create(paste0(target, "/footrope/center")) 
}  
   
# Write files:
for (i in 1:nrow(res)){
   path <- paste0(gsub("gulf.manage", "gulf.trawl.data", getwd()), "/inst/extdata/scs.star.oddi.", year, "/", res$location[i], "/", res$position[i])
   if (file.exists(path)){
      cat(paste0("Copying: '", unlist(lapply(strsplit(res$file[i], "/"), function(x) x[length(x)])), "'\n"))
      file <- paste0(path, "/", res$tow.id[i], ".", extension)
      cat(paste0("     to: '", file, "'\n"))
      file.copy(res$file[i], file)
   }else{
       print(target)
       stop("Path not found.") 
   }
}
