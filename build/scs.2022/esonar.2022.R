# Search directory for Star Oddi files, assigns a survey tow ID, discards redundant files, 
# and copies the remaining set to 'gulf.probe.data'.

# Define survey year:
year <- 2022

# Read survey set data:
file <- paste0(gsub("gulf.manage", "gulf.data", getwd(), fixed = TRUE), "/inst/extdata/scs.set.", 2022, ".csv")
x <- scsset(read.csv(file))

# Locate standard eSonar files:
extension <- "csv"
path <- paste0(options()$gulf.path$snow.crab$survey, "Fishing Year ", year, "/Trawl Survey/esonar")
files <- locate(path = path, file = paste0("*.", extension))
files <- files[-grep("RAW", files)]
files <- files[-grep("test", files)]

# Compile file statistics and identify tow ID:
res <- data.frame(file = files)
res$tow.id <- ""
res$records <- NA
res$location <- ""
res$position <- ""
t <- time(x, "start")
t.stop <- time(x, "stop")
t.haul <- time(x, "haul")

# Get tow ID from file name:
res$tow.id.file <- ""
res$tow.id.start <- ""
res$tow.id.stop <- ""
res$tow.id.haul <- ""
tmp <- unlist(lapply(strsplit(tolower(res$file), "gp"), function(x) x[2]))
tmp <- unlist(lapply(strsplit(tmp, "/"), function(x) x[1]))
tmp <- unlist(lapply(strsplit(tmp, "[.]"), function(x) x[1]))
ix <- !is.na(tmp) & res$tow.id.file == ""
res$tow.id.file[ix] <- paste0("GP", toupper(tmp[ix]))

# Identify tow ID using time:
for (i in 1:nrow(res)){
   print(i)

   z <- read.esonar(res$file[i])
   zlim <- c(time(z[1,]), time(z[nrow(z),]))
   print(zlim)
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

# Identify misaligned tow IDs (due to time errors in set file):
ix <- which((res$tow.id.file != res$tow.id.start) | (res$tow.id.file != res$tow.id.stop) | (res$tow.id.start != res$tow.id.stop))
r <- res[ix, ]

# Tow IDs for which there are no matching eSonar files:
res$tow.id <- res$tow.id.file
x$tow.id[-which(x$tow.id %in% res$tow.id) ]

# Write files:
path <- paste0(gsub("gulf.manage", "gulf.trawl.data", getwd()), "/inst/extdata/scs.esonar.", year)
for (i in 1:nrow(res)){
   cat(paste0("Copying: '", res$tow.id[i], "'\n"))
   cat(paste0("     to: '", file, "'\n"))
   file <- paste0(path, "/", res$tow.id[i], ".csv")
   file.copy(res$file[i], file)
}

# ========================================== SYMMETRY ========================================================================
path <- paste0(options()$gulf.path$snow.crab$survey, "/Fishing Year ", year, "/Trawl Survey/eSonar/RAW")
files <- locate(path = path, keywords = "Symmetry", file = paste0("*.", extension))
tow.ids <- gsub(".csv", "", gsub("DoorSymmetryDoorSlave", "", unlist(lapply(strsplit(files, "/"), function(x) x[length(x)]))))
tow.ids <- toupper(tow.ids)
files <- files[tow.ids != ""]
tow.ids <- tow.ids[tow.ids != ""]

s <- NULL
for (i in 1:length(files)){
   print(i)
   tmp <- read.csv(files[i])
   names(tmp) <- tolower(names(tmp))

   # Date and time variables:
   tmp$time <- gsub("Jul", "07", tmp$time)
   tmp$time <- gsub("Aug", "08", tmp$time)
   tmp$time <- gsub("Sep", "09", tmp$time)
   tmp$date <- unlist(lapply(strsplit(tmp$time, " "), function(x) paste(x[3:1], collapse = "-")))
   tmp$time <- unlist(lapply(strsplit(tmp$time, " "), function(x) x[4]))

   ix <- which((t >= time(tmp[1,])) & (t <= time(tmp[nrow(tmp),])))
   tow.id <- ""
   if (length(ix) == 1) tow.id <- x$tow.id[ix] 
   if (tow.id == ""){
      ix <- which((t.stop >= time(tmp[1,])) & (t.stop <= time(tmp[nrow(tmp),])))
      if (length(ix) == 1) tow.id <- x$tow.id[ix] 
   }
   if (tow.id == ""){
      ix <- which((t.haul >= time(tmp[1,])) & (t.haul <= time(tmp[nrow(tmp),])))
      if (length(ix) == 1) tow.id <- x$tow.id[ix] 
   }
   tmp$tow.id <- tow.id
   if (tow.id == ""){
      tmp$tow.id <- tow.ids[i]
   }
   
   s <- rbind(s, tmp)
}
tows <- unique(s$tow.id)
for (i in 1:length(tows)){
   print(i)
   ss <- s[s$tow.id == tows[i], ]
   ss <- ss[, c("date", "time", "hydrophone", "value", "signalpower.dbu.")]
   names(ss) <- gsub(".dbu.", "", names(ss))
   file <- paste0(gsub("gulf.manage", "gulf.trawl.data", getwd()), "/inst/extdata/scs.esonar.", year, "/symmetry/", tows[i], ".csv")
   write.csv(ss, file = file)
}

# ================================================== DEPTH ============================================================
path <- paste0(options()$gulf.path$snow.crab$survey, "/Fishing Year ", year, "/Trawl Survey/eSonar/RAW")
files <- locate(path = path, keywords = "Depth", file = paste0("*.", extension))
tow.ids <- gsub(".csv", "", gsub("Depth *", "", unlist(lapply(strsplit(files, "/"), function(x) x[length(x)]))))
tow.ids <- toupper(tow.ids)

s <- NULL
for (i in 1:length(files)){
   print(i)
   tmp <- read.csv(files[i])
   names(tmp) <- tolower(names(tmp))

   # Date and time variables:
   tmp$time <- gsub("Jul", "07", tmp$time)
   tmp$time <- gsub("Aug", "08", tmp$time)
   tmp$time <- gsub("Sep", "09", tmp$time)
   tmp$date <- unlist(lapply(strsplit(tmp$time, " "), function(x) paste(x[3:1], collapse = "-")))
   tmp$time <- unlist(lapply(strsplit(tmp$time, " "), function(x) x[4]))

   ix <- which((t >= time(tmp[1,])) & (t <= time(tmp[nrow(tmp),])))
   tow.id <- ""
   if (length(ix) == 1) tow.id <- x$tow.id[ix] 
   if (tow.id == ""){
      ix <- which((t.stop >= time(tmp[1,])) & (t.stop <= time(tmp[nrow(tmp),])))
      if (length(ix) == 1) tow.id <- x$tow.id[ix] 
   }
   if (tow.id == ""){
      ix <- which((t.haul >= time(tmp[1,])) & (t.haul <= time(tmp[nrow(tmp),])))
      if (length(ix) == 1) tow.id <- x$tow.id[ix] 
   }
   tmp$tow.id <- tow.id
   if (tow.id == ""){
      tmp$tow.id <- tow.ids[i]
   }
   
   s <- rbind(s, tmp)
}
tows <- unique(s$tow.id)
for (i in 1:length(tows)){
   ss <- s[s$tow.id == tows[i], ]
   ss <- ss[, c("date", "time", "hydrophone", "value", "signalpower.dbu.")]
   names(ss) <- gsub(".dbu.", "", names(ss))
   file <- paste0(gsub("gulf.manage", "gulf.trawl.data", getwd()), "/inst/extdata/scs.esonar.", year, "/depth/", tows[i], ".csv")
  # file <- paste0("/Users/crustacean/Desktop/gulf.probe.data/inst/extdata/scs.esonar.", year, "/depth/", tows[i], ".csv")
   write.csv(ss, file = file)
}

# ================================================== WINGSPREAD ============================================================
path <- paste0(options()$gulf.path$snow.crab$survey, "/Fishing Year ", year, "/Trawl Survey/eSonar/RAW")
files <- locate(path = path, keywords = "DoorSpread", file = paste0("*.", extension))
tow.ids <- gsub(".csv", "", gsub("DoorSpreadDoorMaster", "", unlist(lapply(strsplit(files, "/"), function(x) x[length(x)]))))
tow.ids <- toupper(tow.ids)

s <- NULL
for (i in 1:length(files)){
   print(i)
   tmp <- read.csv(files[i])
   names(tmp) <- tolower(names(tmp))

   # Date and time variables:
   tmp$time <- gsub("Jul", "07", tmp$time)
   tmp$time <- gsub("Aug", "08", tmp$time)
   tmp$time <- gsub("Sep", "09", tmp$time)
   tmp$date <- unlist(lapply(strsplit(tmp$time, " "), function(x) paste(x[3:1], collapse = "-")))
   tmp$time <- unlist(lapply(strsplit(tmp$time, " "), function(x) x[4]))

   ix <- which((t >= time(tmp[1,])) & (t <= time(tmp[nrow(tmp),])))
   tow.id <- ""
   if (length(ix) == 1) tow.id <- x$tow.id[ix] 
   if (tow.id == ""){
      ix <- which((t.stop >= time(tmp[1,])) & (t.stop <= time(tmp[nrow(tmp),])))
      if (length(ix) == 1) tow.id <- x$tow.id[ix] 
   }
   if (tow.id == ""){
      ix <- which((t.haul >= time(tmp[1,])) & (t.haul <= time(tmp[nrow(tmp),])))
      if (length(ix) == 1) tow.id <- x$tow.id[ix] 
   }
   tmp$tow.id <- tow.id
   if (tow.id == ""){
      tmp$tow.id <- tow.ids[i]
   }
   
   s <- rbind(s, tmp)
}
tows <- unique(s$tow.id)
for (i in 1:length(tows)){
   ss <- s[s$tow.id == tows[i], ]
   ss <- ss[, c("date", "time", "hydrophone", "value", "signalpower.dbu.")]
   names(ss) <- gsub(".dbu.", "", names(ss))
   file <- paste0(gsub("gulf.manage", "gulf.trawl.data", getwd()), "/inst/extdata/scs.esonar.", year, "/wingspread/", tows[i], ".csv")
   write.csv(ss, file = file)
}

# ================================================== HEADLINE ============================================================
files <- locate(path = path, keywords = "Headline", file = paste0("*.", extension))
tow.ids <- gsub("PrimaryHeadline", "", unlist(lapply(strsplit(files, "/"), function(x) x[length(x)])))
type <- rep("primary", length(files))
type[grep("Secondary", files)] <- "secondary"
tow.ids <- gsub("SecondaryHeadline", "", tow.ids)
tow.ids <- gsub("[.]csv", "", tolower(tow.ids))
tow.ids <- toupper(tow.ids)

s <- NULL
for (i in 1:length(files)){
   print(i)
   tmp <- read.csv(files[i])
   names(tmp) <- tolower(names(tmp))

   # Date and time variables:
   tmp$time <- gsub("Jul", "07", tmp$time)
   tmp$time <- gsub("Aug", "08", tmp$time)
   tmp$time <- gsub("Sep", "09", tmp$time)
   tmp$date <- unlist(lapply(strsplit(tmp$time, " "), function(x) paste(x[3:1], collapse = "-")))
   tmp$time <- unlist(lapply(strsplit(tmp$time, " "), function(x) x[4]))

   ix <- which((t >= time(tmp[1,])) & (t <= time(tmp[nrow(tmp),])))
   tow.id <- ""
   if (length(ix) == 1) tow.id <- x$tow.id[ix] 
   if (tow.id == ""){
      ix <- which((t.stop >= time(tmp[1,])) & (t.stop <= time(tmp[nrow(tmp),])))
      if (length(ix) == 1) tow.id <- x$tow.id[ix] 
   }
   if (tow.id == ""){
      ix <- which((t.haul >= time(tmp[1,])) & (t.haul <= time(tmp[nrow(tmp),])))
      if (length(ix) == 1) tow.id <- x$tow.id[ix] 
   }
   tmp$tow.id <- tow.id
   if (tow.id == ""){
      tmp$tow.id <- tow.ids[i]
   }
   tmp$type <- type[i]
   
   s <- rbind(s, tmp)
}
tows <- unique(s[c("tow.id", "type")])
for (i in 1:nrow(tows)){
   ss <- s[s$tow.id == tows$tow.id[i] & s$type == tows$type[i] , ]
   ss <- ss[, c("date", "time", "hydrophone", "value", "signalpower.dbu.")]
   names(ss) <- gsub(".dbu.", "", names(ss))
   file <- paste0(gsub("gulf.manage", "gulf.trawl.data", getwd()), "/inst/extdata/scs.esonar.", year, "/headline/", tows$type[i], "/", tows$tow.id[i], ".csv")
   write.csv(ss, file = file)
}




