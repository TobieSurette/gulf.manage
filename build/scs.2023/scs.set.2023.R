library(gulf.data)
library(gulf.spatial)

# Define survey year:
year <- 2023

# Load raw data export:
path <- paste0("build/scs.", year, "/data/")
x <- read.table(paste0(path, "scs.set.", year, ".csv"), header = TRUE, sep =",", stringsAsFactors = FALSE)
names(x) <- gsub("_", ".", tolower(names(x)))

# Load file containing original survey station coordinates:
file <- paste0(gsub("gulf.manage", "gulf.data", getwd(), fixed = TRUE), "/inst/extdata/scs.stations.", year, ".csv")
y <- read.csv(file)
   
# Rename fields:
x$tow.id <- toupper(x$gpnumber)

# Reformat data:
x$bottom.temperature <- gsub(",", ".", x$bottom.temperature)
x$bottom.temperature <- gsub("O", "0", x$bottom.temperature)
x$bottom.temperature[x$bottom.temperature == ""] <- NA
x$bottom.temperature <- as.numeric(x$bottom.temperature)

# Parse comment fields:
x$comment[is.na(x$comment)] <- ""
x$comment <- gsub("[ ]*$", "", x$comment)
ix <- x$comment == ""
x$comment[ix] <- x$speed.comment[ix]
x$speed.comment <- gsub("[ ]*$", "", x$speed.comment)
x$comment[!ix] <- paste0(x$comment[!ix], " - ", x$speed.comment[!ix])
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

# Stop time fixes:
x$stop.time.logbook[x$tow.id == "GP158F"]  <- "16:54:13"
x$stop.time.logbook[x$tow.id == "GP176F"]  <- "15:34:34"
x$start.time.logbook[x$tow.id == "GP147F"] <- "05:33:18"
x$stop.time.logbook[x$tow.id == "GP147F"]  <- "05:40:08"
x$start.time.logbook[x$tow.id == "GP159F"] <- "06:52:25"
x$stop.time.logbook[x$tow.id == "GP159F"]  <- "06:58:33"
x$start.time.logbook[x$tow.id == "GP055F"] <- "05:40:28"
x$stop.time.logbook[x$tow.id == "GP055F"]  <- "05:46:37"
x$start.time.logbook[x$tow.id == "GP269F"] <- "19:26:29"
x$stop.time.logbook[x$tow.id == "GP269F"]  <- "19:40:59"

# Quick coordinate fixes:
x$gpa.lon.end             <- as.numeric(x$gpa.lon.end)
x$gpa.lon.start           <- as.numeric(x$gpa.lon.start)
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

# Remove empty data rows:
x <- x[x$tow.id != "GP000F", ]

# Standardize time formats:
x$start.time.logbook <- time(x$start.time.logbook)
x$stop.time.logbook  <- time(x$stop.time.logbook)
x$haul.time <- time(x$haul.time)

# Assign fishing captain:
x$captain <- "Ghislain Bourgeois"

# Load touchdown times:
t <- read.csv(paste0(gsub("manage", "data", "C:/Users/SuretteTJ/Desktop/gulf.manage"), "/inst/extdata/scs.event.times.", year, ".csv"))
t <- t[t$location == "footrope" & t$position == "center" & t$event == "touchdown", ] 
t$time <- time(t$time)
ix <- match(x$tow.id, t$tow.id)
x$start.time[!is.na(ix)] <- t$time[ix[!is.na(ix)]]
ix <- x$start.time == "        " & x$valid == 1
x$start.time[ix] <- x$start.time.logbook[ix]

# Load liftoff times:
t <- read.csv(paste0(gsub("manage", "data", "C:/Users/SuretteTJ/Desktop/gulf.manage"), "/inst/extdata/scs.event.times.", year, ".csv"))
t <- t[t$location == "footrope" & t$position == "center" & t$event == "liftoff", ] 
t$time <- time(t$time)
ix <- match(x$tow.id, t$tow.id)
x$liftoff.time[!is.na(ix)] <- t$time[ix[!is.na(ix)]]
ix <- is.na(x$liftoff.time) & x$valid == 1
x$liftoff.time[ix] <- x$stop.time.logbook[ix]
x$liftoff.time[is.na(x$liftoff.time)] <- "        "

# Update bottom temperatures using headline Star Oddi files:
files <- locate.star.oddi(location = "headline", position = "center", year = year)
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

# Point fix:
#x$bottom.temperature[x$tow.id == "GP080F"] <- 0.57  # Original observation.

# Update touchdown and stop time coordinates using eSonar files:
files <- locate.esonar(year)
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
            x$latitude.start[i]  <- gulf.spatial::deg2dmm(e$latitude[which.min(d)])
         }
     
         # Stop time coordinates:
         d <- abs(time2sec(time(e), stop))
         if (min(d) <= 5){
            x$longitude.stop[i] <- gulf.spatial::deg2dmm(e$longitude[which.min(d)])
            x$latitude.stop[i]  <- gulf.spatial::deg2dmm(e$latitude[which.min(d)])
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
   d[x$valid != 1] <- NA
   ix <- which.min(d)
   print(round(d[ix]))
   if (d[ix] < 3000) x$station.type[ix] <- "fixed"
}

# Load trawl swept area and swept area method:
file <- paste0("C:/Users/SuretteTJ/Desktop/gulf.manage/build/scs.", year, "/data/scs.swept.area.", year, ".csv")
if (file.exists(file)){
   tmp <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
   x$swept.area <- tmp$swept.area[match(x$tow.id, tmp$tow.id)]
   x$swept.area.method <- tmp$swept.area.method[match(x$tow.id, tmp$tow.id)]
   x$swept.area.method[is.na(x$swept.area.method)] <- ""
}

# Re-order variables:
tvars <- names(x)[grep("time", names(x))]
vars <- c("date", "zone", "tow.number", "tow.id", "valid", "station.type", "captain", tvars, setdiff(vars, c("date", "zone", "tow.number", "tow.id", "valid", "station.type", "captain", tvars)))
x <- x[vars]

# Write to stock assessment repository:
#write.csv(x, file = paste0("data/scs.set.", year, ".csv"), row.names = FALSE)

# Write to gulf.data repository:
path <- gsub("gulf.manage", "gulf.data", getwd(), fixed = TRUE)
if (file.exists(path)){
   file <- paste0(path, "/inst/extdata/scs.set.", year, ".csv")
   write.csv(x, file = file, row.names = FALSE)
}
