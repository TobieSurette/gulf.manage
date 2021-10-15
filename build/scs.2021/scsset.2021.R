library(gulf.data)
library(gulf.spatial)

year <- 2021

# Load raw data export:
path <- "/Users/crustacean/Desktop/gulf.manage/inst/extdata/scs.2021/"
x <- read.table(paste0(path, "scs.set.2021.csv"), header = TRUE, sep =",", stringsAsFactors = FALSE)
y <- read.table(paste0(path, "scs.set.2021.experiment.csv"), header = TRUE, sep =",", stringsAsFactors = FALSE)
x <- rbind(y, x)
names(x) <- gsub("_", ".", tolower(names(x)))

# Load file constaining orginal survey station coordinates:
y <- read.csv("/Users/crustacean/Desktop/gulf.data/inst/extdata/scs.stations.2021.csv")
   
# Rename fields:
x$tow.id <- toupper(x$gpnumber)

ix <- grep("TEST", x$tow.id)
x$tow.id[ix] <- paste0("XP0", substr(x$tow.id[ix], 5,6))

# Reformat data:
x$bottom.temperature <- gsub(",", ".", x$bottom.temperature)
x$bottom.temperature <- gsub("O", "0", x$bottom.temperature)
x$bottom.temperature[x$bottom.temperature == ""] <- NA
x$bottom.temperature <- as.numeric(x$bottom.temperature)

# Parse comment fields:
x$comment[is.na(x$comment)] <- ""
x$comment <- gsub("[ ]*$", "", x$comment)
index <- x$comment == ""
x$comment[index] <- x$speed.comment[index]
x$speed.comment <- gsub("[ ]*$", "", x$speed.comment)
x$comment[!index] <- paste0(x$comment[!index], " - ", x$speed.comment[!index])
x$comment <- paste0(substr(x$comment, 1, 1), substr(tolower(x$comment), 2, nchar(x$comment)))

# Parse date fields:
x$day   <- as.numeric(unlist(lapply(strsplit(x$date, ".", fixed = TRUE), function(x) x[1])))
x$month <- as.numeric(unlist(lapply(strsplit(x$date, ".", fixed = TRUE), function(x) x[2])))
x$year  <- as.numeric(unlist(lapply(strsplit(x$date, ".", fixed = TRUE), function(x) x[3])))

# Zone and tow ID fields:
x$zone <- gsub("ZONE", "", toupper(x$zone))
x$zone[x$zone == "E"] <- "12E"
x$zone[x$zone == "F"] <- "12F"
   
# Fix time fields:
x$gpa.time.start <- gsub("?", "", x$gpa.time.start, fixed = TRUE)
x$gpa.time.start[x$gpa.time.start == ""] <- "        "
x$gpa.time.mid <- gsub("?", "", x$gpa.time.mid, fixed = TRUE)
x$gpa.time.mid[x$gpa.time.mid == ""] <- "        "
x$gpa.time.end <- gsub("?", "", x$gpa.time.end, fixed = TRUE)
x$gpa.time.end[x$gpa.time.end == ""] <- "        "

# Observed time fields:
x$start.time.logbook <- x$gpa.time.start
x$mid.time.logbook   <- x$gpa.time.mid
x$stop.time.logbook  <- x$gpa.time.end
x$start.time.logbook[nchar(x$start.time.logbook) > 8] <- "        "
x$mid.time.logbook[nchar(x$mid.time.logbook) > 8]     <- "        "
x$stop.time.logbook[nchar(x$stop.time.logbook) > 8]   <- "        "
x$haul.time <- x$net.end

x$gpa.lon.start[x$gpa.lon.start == "06324.735.927"] <- "06324.735"
x$gpa.lon.start <- as.numeric(x$gpa.lon.start)
x$longitude.start.logbook <- -abs(x$gpa.lon.start)
x$longitude.stop.logbook  <- -abs(x$gpa.lon.end)
x$latitude.start.logbook  <- x$gpa.lat.start
x$latitude.stop.logbook   <- x$gpa.lat.end

# Tow validity:
x$valid <- as.numeric(tolower(x$tow.quality) == "good")

# Add variables to be filled-in later:
x$start.time        <- "        "
x$stop.time          <- "        "
x$swept.area        <- as.numeric(NA)
x$swept.area.method <- as.numeric(NA)
x$groundfish.sample	<- 0
x$water.sample      <- as.numeric(NA)
x$longitude         <- as.numeric(NA)
x$latitude          <- as.numeric(NA)
x$longitude.start   <- as.numeric(NA)
x$longitude.stop    <- as.numeric(NA)
x$latitude.start    <- as.numeric(NA)
x$latitude.stop     <- as.numeric(NA)

x$tow.number <- x$trawl.number
x$warp <- x$cables

x <- x[setdiff(names(x), "date")]
x$date <- as.character(date(year = x$year, month = x$month, day = x$day))

# Remove irrelevant variables:
vars <- c("date", "zone", "tow.number", "tow.id", "valid",
          "start.time.logbook", "stop.time.logbook", "start.time", "stop.time", "haul.time",
          "longitude", "latitude", "longitude.start", "longitude.stop", "latitude.start", "latitude.stop",
          "longitude.start.logbook", "longitude.stop.logbook", "latitude.start.logbook", "latitude.stop.logbook",
          "depth", "bottom.temperature", "warp", "swept.area", "swept.area.method", "groundfish.sample", "water.sample", "comment")
head(x[setdiff(names(x), vars)])

x <- x[vars]

# Corrections:
x <- x[x$date != "2020-07-10", ]
x$valid[x$tow.id == "GP055F"] <- 0
x$valid[x$tow.id == "GP146F"] <- 0

# Time corrections:
x$start.time.logbook[x$tow.id == "GP001F"] <- "06:09:50"
x$start.time.logbook[x$tow.id == "GP015F"] <- "06:09:23"
x$stop.time.logbook[x$tow.id == "GP015F"]  <- "06:14:23"
x$start.time.logbook[x$tow.id == "GP017F"] <- "07:03:35"
x$stop.time.logbook[x$tow.id == "GP017F"]  <- "07:08:35"
x$start.time.logbook[x$tow.id == "GP034F"] <- "13:37:07"
x$start.time.logbook[x$tow.id == "GP034A1"] <- "14:25:32"
x$stop.time.logbook[x$tow.id == "GP034A1"] <- "14:30:32"
x$start.time.logbook[x$tow.id == "GP057F"] <- "13:17:04"
x$start.time.logbook[x$tow.id == "GP059F"] <- "14:45:42"
x$start.time.logbook[x$tow.id == "GP065F"] <- "15:52:53"
x$start.time.logbook[x$tow.id == "GP086F"] <- "17:13:14" 
x$start.time.logbook[x$tow.id == "GP091F"] <- "10:24:00" 
x$start.time.logbook[x$tow.id == "GP092F"] <- "12:34:41" 
x$stop.time.logbook[x$tow.id == "GP092F"] <- "12:39:41" 
x$start.time.logbook[x$tow.id == "GP100F"] <- "10:42:53" 
x$stop.time.logbook[x$tow.id == "GP100F"] <- "10:47:53" 
x$start.time.logbook[x$tow.id == "GP104F"] <- "09:01:54" 
x$stop.time.logbook[x$tow.id == "GP104F"] <- "09:06:54" 
x$start.time.logbook[x$tow.id == "GP108A1"] <- "12:32:55" 
x$start.time.logbook[x$tow.id == "GP110F"] <- "14:11:54"
x$start.time.logbook[x$tow.id == "GP114F"] <- "16:50:02"
x$start.time.logbook[x$tow.id == "GP148A1"] <- "16:39:11"
x$start.time.logbook[x$tow.id == "GP229A1"] <- "13:23:03" 
x$stop.time.logbook[x$tow.id == "GP229A1"] <- "13:28:03" 
x$start.time.logbook[x$tow.id == "GP029F"] <- "12:11:49" 
x$start.time.logbook[x$tow.id == "GP092F"] <- "12:33:41" 
x$start.time.logbook[x$tow.id == "GP034A2"] <- "16:01:35"
x$start.time.logbook[x$tow.id == "GP037F"] <- "06:07:59"
x$start.time.logbook[x$tow.id == "GP129F"] <- "16:53:21"
x$start.time.logbook[x$tow.id == "GP108F"] <- "11:41:21"
x$start.time.logbook[x$tow.id == "GP305F"] <- "16:27:49"                    
x$stop.time.logbook[x$tow.id == "GP150F"]  <- "19:58:19"
x$start.time.logbook[x$tow.id == "GP092F"] <- "12:34:41"

# Standardize time formats:
x$start.time.logbook <- time(x$start.time.logbook)
x$stop.time.logbook <- time(x$stop.time.logbook)
x$haul.time <- time(x$haul.time)

# Load touchdown times:
t <- read.csv(locate(package = "gulf.data", keywords = c("scs", "event", "times", 2021)))
t <- t[t$location == "footrope" & t$position == "center" & t$event == "touchdown", ] 
t$time <- time(t$time)
ix <- match(x$tow.id, t$tow.id)
x$start.time[!is.na(ix)] <- t$time[ix[!is.na(ix)]]
ix <- x$start.time == "        " & x$valid == 1
x$start.time[ix] <- x$start.time.logbook[ix]

# Load liftoff times:
t <- read.csv(locate(package = "gulf.data", keywords = c("scs", "event", "times", 2021)))
t <- t[t$location == "footrope" & t$position == "center" & t$event == "liftoff", ] 
t$time <- time(t$time)
ix <- match(x$tow.id, t$tow.id)
x$liftoff.time[!is.na(ix)] <- t$time[ix[!is.na(ix)]]
ix <- is.na(x$liftoff.time) & x$valid == 1
x$liftoff.time[ix] <- x$stop.time.logbook[ix]

# Update bottom temperatures using headline Star Oddi files:
files <- locate.star.oddi(x[i,], location = "headline", position = "center", year = 2021)
files <- files[grep("center", files)]
for (i in 1:nrow(x)){
   if (x$valid[i] == 1){
      file <- files[grep(x$tow.id[i], files)]
      s <- read.star.oddi(file)
      if (!is.null(s)){
         start.time <- as.POSIXct(paste0(x$date[i], x$start.time[i]))
         end.time   <- as.POSIXct(paste0(x$date[i], x$liftoff.time[i]))
         t <- time2min(time(s), start.time)
         liftoff <- time2min(end.time, start.time)
         ix <- which((t >= 3) & (t <= liftoff))
         tmp <- round(mean(s$temperature[ix], na.rm = TRUE), 2)
         if (!is.na(tmp)) x$bottom.temperature[i] <- tmp
         print(c(i, tmp))
      }
   }
}

# Update touchdown and stop time coordinates using eSonar files:
files <- locate.esonar(2021)
for (i in 1:nrow(x)){
   if (x$valid[i] == 1){
      print(i)
      file <- files[grep(x$tow.id[i], files)]
      if (length(file) > 0){
         e <- read.esonar(files[grep(x$tow.id[i], files)])
         start <- as.POSIXct(paste(x$date[i], x$start.time[i]))
         stop <- as.POSIXct(paste(x$date[i], x$stop.time.logbook[i]))

         # Start time coordinates:
         d <- abs(time2sec(time(e), start))
         if (min(d) <= 5){
            x$longitude.start[i] <- gulf.spatial::deg2dmm(e$longitude[which.min(d)])
            x$latitude.start[i] <- gulf.spatial::deg2dmm(e$latitude[which.min(d)])
         }
     
         # Stop time coordinates:
         d <- abs(time2sec(time(e), stop))
         if (min(d) <= 5){
            x$longitude.stop[i] <- gulf.spatial::deg2dmm(e$longitude[which.min(d)])
            x$latitude.stop[i] <- gulf.spatial::deg2dmm(e$latitude[which.min(d)])
         }
      }
   }
}

# Determine which stations were fixed to 2013 positions:
y <- y[y$station == "fixed 2013", ]
library(gulf.spatial)
x$station.type <- "free"
x <- scsset(x)
for (i in 1:nrow(y)){
   d <- 1000 * distance(y$longitude[i], y$latitude[i], lon(x), lat(x))[1, ]
   d[grep("XP", x$tow.id)] <- NA
   d[x$valid != 1] <- NA
   ix <- which.min(d)
   print(round(d[ix]/1000),2)
   if (d[ix] < 3000) x$station.type[ix] <- "fixed"
}

# Load trawl swept area and swept area method:
if (file.exists("/Users/crustacean/Desktop/gulf.manage/inst/extdata/scs.2021/scs.swept.area.2021.csv")){
   tmp <- read.csv("/Users/crustacean/Desktop/gulf.manage/inst/extdata/scs.2021/scs.swept.area.2021.csv", header = TRUE, stringsAsFactors = FALSE)
   x$swept.area <- tmp$swept.area[match(x$tow.id, tmp$tow.id)]
   x$swept.area.method <- tmp$swept.area.method[match(x$tow.id, tmp$tow.id)]
   x$swept.area.method[is.na(x$swept.area.method)] <- ""
}

# Re-order variables:
tvars <- names(x)[grep("time", names(x))]
vars <- c("date", "zone", "tow.number", "tow.id", "valid", "station.type", tvars, setdiff(vars, c("date", "zone", "tow.number", "tow.id", "valid", "station.type", tvars)))
x <- x[vars]

# Write to stock assessment repository:
#write.csv(x, file = paste0("data/scs.set.", year, ".csv"), row.names = FALSE)

# Write to gulf.data repository:
if (file.exists("/Users/crustacean/Desktop/gulf.data")){
   file <- paste0("/Users/crustacean/Desktop/gulf.data/inst/extdata/scs.set.", year, ".csv")
   write.csv(x, file = file, row.names = FALSE)
}
