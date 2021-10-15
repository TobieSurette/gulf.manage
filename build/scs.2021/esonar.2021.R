# Search directory for Star Oddi files, assigns a survey tow ID, discards redundant files, 
# and copies the remaining set to 'gulf.probe.data'.

# Read survey set data:
x <- scsset(read.csv("/Users/crustacean/Desktop/gulf.data/inst/extdata/scs.set.2021.csv"))

# Locate standard eSonar files:
extension <- "csv"
path <- "/Users/crustacean/Desktop/Backup 2021/ESONAR2021"
files <- locate(path = path, file = paste0("*.", extension))
files <- files[-grep("RAW", files)]

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
tmp <- unlist(lapply(strsplit(tolower(res$file), "test"), function(x) x[2]))
tmp <- unlist(lapply(strsplit(tmp, "/"), function(x) x[1]))
tmp <- unlist(lapply(strsplit(tmp, "[.]"), function(x) x[1]))
res$tow.id.file[!is.na(tmp)] <- paste0("XP0", tmp[!is.na(tmp)])

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

# Identify mislaigned tow IDs (due to time errors in set file):
ix <- which((res$tow.id.file != res$tow.id.start) | (res$tow.id.file != res$tow.id.stop) | (res$tow.id.start != res$tow.id.stop))
r <- res[ix, ]

# Tow IDs for which there are no matching Star Oddi files:
res$tow.id <- res$tow.id.file
x$tow.id[-which(x$tow.id %in% res$tow.id) ]

# ========================================== SYMMETRY ========================================================================
files <- locate(path = path, keywords = "Symmetry", file = paste0("*.", extension))
files <- files[-grep("TESTENMER", files)]
tow.ids <- gsub(".csv", "", gsub("DoorSymmetryDoorSlave", "", unlist(lapply(strsplit(files, "/"), function(x) x[length(x)]))))
tow.ids <- toupper(tow.ids)
tow.ids <- gsub("TEST", "XP0", tow.ids)

s <- NULL
for (i in 1:length(files)){
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
   file <- paste0("/Users/crustacean/Desktop/gulf.probe.data/inst/extdata/scs.esonar.2021/symmetry/", tows[i], ".csv")
   write.csv(ss, file = file)
}

# ================================================== DEPTH ============================================================
files <- locate(path = path, keywords = "Depth", file = paste0("*.", extension))
files <- files[-grep("TESTENMER", files)]
tow.ids <- gsub(".csv", "", gsub("Depth", "", unlist(lapply(strsplit(files, "/"), function(x) x[length(x)]))))
tow.ids <- toupper(tow.ids)
tow.ids <- gsub("TEST", "XP0", tow.ids)

s <- NULL
for (i in 1:length(files)){
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
   file <- paste0("/Users/crustacean/Desktop/gulf.probe.data/inst/extdata/scs.esonar.2021/depth/", tows[i], ".csv")
   write.csv(ss, file = file)
}

# ================================================== WINGSPREAD ============================================================
files <- locate(path = path, keywords = "DoorSpread", file = paste0("*.", extension))
files <- files[-grep("TESTENMER", files)]
tow.ids <- gsub(".csv", "", gsub("DoorSpreadDoorMaster", "", unlist(lapply(strsplit(files, "/"), function(x) x[length(x)]))))
tow.ids <- toupper(tow.ids)
tow.ids <- gsub("TEST", "XP0", tow.ids)

s <- NULL
for (i in 1:length(files)){
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
   file <- paste0("/Users/crustacean/Desktop/gulf.probe.data/inst/extdata/scs.esonar.2021/wingspread/", tows[i], ".csv")
   write.csv(ss, file = file)
}

# ================================================== HEADLINE ============================================================
files <- locate(path = path, keywords = "Headline", file = paste0("*.", extension))
files <- files[-grep("TESTENMER", files)]
tow.ids <- gsub("PrimaryHeadline", "", unlist(lapply(strsplit(files, "/"), function(x) x[length(x)])))
type <- rep("primary", length(files))
type[grep("Secondary", files)] <- "secondary"
tow.ids <- gsub("SecondaryHeadline", "", tow.ids)
tow.ids <- gsub("[.]csv", "", tolower(tow.ids))
tow.ids <- toupper(tow.ids)
tow.ids <- gsub("TEST", "XP0", tow.ids)

s <- NULL
for (i in 1:length(files)){
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
   file <- paste0("/Users/crustacean/Desktop/gulf.probe.data/inst/extdata/scs.esonar.2021/headline/", tows$type[i], "/", tows$tow.id[i], ".csv")
   write.csv(ss, file = file)
}


# Create target directories:
target <- "/Users/crustacean/Desktop/gulf.probe.data/inst/extdata/scs.esonar.symmetry.2021"
if (!file.exists(target)) dir.create(target)

# Write files:
for (i in 1:nrow(res)){
   target <- paste0("/Users/crustacean/Desktop/gulf.probe.data/inst/extdata/scs.star.oddi.2021/", res$location[i], "/", res$position[i])
   if (file.exists(target)){
      cat(paste("Copying: '", res$name[i], "'\n"))
      cat(paste("     to: '", file, "'\n"))
      file <- paste0(target, "/", res$tow.id[i], ".", extension)
      file.copy(res$file[i], file)
   }else{
       print(target)
       stop("Path not found.") 
   }
}



