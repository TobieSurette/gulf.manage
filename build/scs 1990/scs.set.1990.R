library(gulf.data)
library(gulf.spatial)

# Flag to publish data: 
publish <- FALSE

# Read original raw set data:
x <- read.csv(locate(file = c("scs.set", "1990", "csv")), header = TRUE)

# Variable name fix:
names(x) <- gsub("[.]$", "", names(x))

# Remove accents:
x$comment <- unaccent(x$comment)

# Add date field:
x <- cbind(data.frame(date = as.character(date(x)), stringsAsFactors = FALSE), x)

# Remove redundant coordinates:
if ("longitude.end.logbook" %in% names(x)){
   x$longitude.end.logbook[which(x$longitude.start.logbook == x$longitude.end.logbook)] <- NA
   x$latitude.end.logbook[which(x$latitude.start.logbook == x$latitude.end.logbook)] <- NA
   x$longitude.start.logbook[which(x$longitude.start.logbook == x$longitude)] <- NA
   x$latitude.start.logbook[which(x$latitude.start.logbook == x$latitude)] <- NA
}
   
# Import original logbook coordinates:
y <- read.csv(locate(file = c("scs", "coordinate", "1990", "csv")), header = TRUE)
y$date <- as.character(date(y))
y <- y[!is.na(y$year), ]
y <- y[setdiff(names(y), c("year", "month", "day"))]
y <- gulf.utils::compress(y)
names(y) <- gsub("[.]logbook", "", tolower(names(y)))
names(y) <- gsub("[.]no", ".number", tolower(names(y)))
y <- y[c("date", setdiff(names(y),  "date"))]

# Restructure data:
r <- aggregate(list(loranx.start = y$loranx),   by = y[c("date", "tow.number")], function(x) x[1])
r$loranx.end   <- aggregate(list(x = y$loranx), by = y[c("date", "tow.number")], function(x) if (length(x) > 1) return(x[length(x)]) else return(NA))$x
r$lorany.start <- aggregate(list(x = y$lorany), by = y[c("date", "tow.number")], function(x) x[1])$x
r$lorany.end   <- aggregate(list(x = y$lorany), by = y[c("date", "tow.number")], function(x) if (length(x) > 1) return(x[length(x)]) else return(NA))$x
r$start.time   <- aggregate(list(x = y$time),   by = y[c("date", "tow.number")], function(x){ t <- x[!is.na(x)]; t <- t[t != ""]; if (length(t) == 0) return("     ") else return(t)})$x

# Import Loran coordinates:
ix <- match(x[c("date", "tow.number")], r[c("date", "tow.number")])
x$loranx.start <- r$loranx.start[ix]
x$loranx.end   <- r$loranx.end[ix]
x$lorany.start <- r$lorany.start[ix]
x$lorany.end   <- r$lorany.end[ix]
x$start.time   <- r$start.time[ix]

# Extract sampler and coordinate data from biological data:
files <- locate(file = "GCR90")
b <- read.scsbio(files, drop = FALSE, verbose = TRUE)
r <- aggregate(b["longitude.start"], by = b[c("date", "tow.number")], unique)
r$latitude.start <- aggregate(list(x = b$latitude.start), by = b[c("date", "tow.number")], unique)$x
r$sampler <- aggregate(list(x = b$samplers), by = b[c("date", "tow.number")], function(x) paste(unique(x), collapse = ","))$x
r <- sort(r, by = c("date", "tow.number"))
r$sampler <- sampler(r$sampler, project = "scs")
ix <- match(x[c("date", "tow.number")], r[c("date", "tow.number")])
x$sampler <- r$sampler[ix]
x$sampler[is.na(x$sampler)] <- ""
x$latitude   <- r$longitude.start[ix]
x$longitude  <- r$latitude.start[ix]

# Move odd Loran Z coordinates tow new colummn:
ix <- which(x$lorany.end > 40000)
x$lorany.start[ix]
x$loranz.start <- NA
x$loranz.start[ix] <- x$lorany.start[ix]
x$loranz.end <- NA
x$loranz.end[ix] <- x$lorany.end[ix]
x$lorany.start[ix] <- x$loranx.start[ix]
x$lorany.end[ix]   <- x$loranx.end[ix]
x$loranx.start[ix] <- NA
x$loranx.end[ix] <- NA

# Test for large differences in coordinates:
ix <- which((x$loranx.start - x$loranx.end) > 4)
x[ix, ]
ix <- which((x$lorany.start - x$lorany.end) > 10)

vars <- c("date", "tow.number", "lorany.start", "lorany.end", "latitude")
x[ix, vars]

# Spot coordinate corrections:
x$loranx.start[which((x$date == "1990-09-12") & (x$tow.number == 3))]  <- 15148.8
x$loranx.end[which((x$date == "1990-08-04") & (x$tow.number == 4))]    <- 15126.6
x$lorany.start[which((x$date == "1990-06-27") & (x$tow.number == 10))] <- 29334.3  
x$lorany.start[which((x$date == "1990-06-29") & (x$tow.number == 8))]  <- 28817.5
x$lorany.start[which((x$date == "1990-10-08") & (x$tow.number == 2))]  <- 30705.9 

# Substitute start and end coordinates:
x$longitude.start.logbook[is.na(x$longitude.start.logbook)] <- x$loranx.start[is.na(x$longitude.start.logbook)]
x$latitude.start.logbook[is.na(x$latitude.start.logbook)]   <- x$lorany.start[is.na(x$latitude.start.logbook)]
x$longitude.end.logbook[is.na(x$longitude.end.logbook)]     <- x$loranx.end[is.na(x$longitude.end.logbook)]
x$latitude.end.logbook[is.na(x$latitude.end.logbook)]       <- x$lorany.end[is.na(x$latitude.end.logbook)] 

# Remove redundant coordinates:
x$longitude[which(!is.na(x$longitude.start.logbook) | !is.na(x$longitude.end.logbook))] <- NA
x$latitude[which(!is.na(x$latitude.start.logbook) | !is.na(x$latitude.end.logbook))] <- NA

# Format start time field:
ix <- which(!is.na(x$start.time.logbook) & (nchar(x$start.time.logbook) == 4))
x$start.time.logbook[ix] <- paste0("0", x$start.time.logbook[ix])
ix <- which(nchar(x$start.time.logbook) == 5)
x$start.time.logbook[ix] <- paste0(x$start.time.logbook[ix], ":00")
x$start.time.logbook[is.na(x$start.time.logbook)] <- "        "

# Format end time field:
x$end.time.logbook <- deblank(x$end.time.logbook)
ix <- which(!is.na(x$end.time.logbook) & (nchar(x$end.time.logbook) == 4))
x$end.time.logbook[ix] <- paste0("0", x$end.time.logbook[ix])
ix <- which(nchar(x$end.time.logbook) == 5)
x$end.time.logbook[ix] <- paste0(x$end.time.logbook[ix], ":00")
x$end.time.logbook[which(is.na(x$end.time.logbook) | x$end.time.logbook == "")] <- "        "

# Fix comments:
x$comment <- deblank(x$comment)
x$comment <- gsub("[Mm]\x88tre[s]*", "m ", x$comment)
x$comment <- gsub("[Mm]\x88[s]*", "meme ", x$comment)
x$comment <- gsub(" +", " ", x$comment)

# Extract tow duration information from comments:
ix <- grep("[0-9]*[:]*[0-9][ ]*minu", tolower(x$comment))
str <- x$comment[ix]
str <- deblank(unlist(lapply(strsplit(str, "minu"), function(x) x[1])))
str <- unlist(lapply(strsplit(str, " "), function(x) x[length(x)]))
str[gsub("[:]", "", str) == str] <- paste0(str[gsub("[:]", "", str) == str], ":00")
x$duration <- ""
x$duration[ix] <- str

# Remove redundant fields:
remove <- c("year", "month", "day", "season", "vessel", "loranx.start", "loranx.end", "lorany.start", "lorany.end")
x <- x[setdiff(names(x), remove)]

# Remove empty variables:
x <- compress(x)

# Rename coordinates:
names(x) <- gsub("longitude", "loran.x", names(x))
names(x) <- gsub("latitude", "loran.y", names(x))

#  Compare coordinates:
y <- read.csv(locate(file = c("scs.set", "1988", "csv")), header = TRUE)
names(y) <- gsub("[.]$", "", names(y))
y <- cbind(data.frame(date = as.character(date(y)), stringsAsFactors = FALSE), y)
y$longitude.end.logbook[which(y$longitude.start.logbook == y$longitude.end.logbook)] <- NA
y$latitude.end.logbook[which(y$latitude.start.logbook == y$latitude.end.logbook)] <- NA
y$longitude.start.logbook[which(y$longitude.start.logbook == y$longitude)] <- NA
y$latitude.start.logbook[which(y$latitude.start.logbook == y$latitude)] <- NA
y <- compress(y)

# Start coordinates:
ix <- which(!is.na(x$loran.x.start.logbook) & !is.na(x$loran.y.start.logbook))
tmp <- loran2deg(x$loran.x.start.logbook[ix], x$loran.y.start.logbook[ix])
x$longitude <- NA
x$latitude <- NA
x$longitude[ix[tmp$lat == 0]] <- deg2dmm(y$longitude[ix[tmp$lat == 0]])
x$latitude[ix[tmp$lat == 0]]  <- deg2dmm(y$latitude[ix[tmp$lat == 0]])

# Re-order fields:
ix <- which(names(x) == "loran.x")
x <- cbind(x[, 1:(ix-1)], x[, "duration", drop = FALSE], x[c("longitude", "latitude")], 
           x[, ix:(ncol(x)-5)], x[, "sampler", drop = FALSE], x[, (ncol(x)-4), drop = FALSE])

# Tow ID format change:
# x$tow.id <- paste0("GP", gsub("S88", "", x$tow.id))
x <- x[setdiff(names(x), "tow.id")]

# Write data to 'gulf.data':
path <- paste0(unlist(strsplit(getwd(), "gulf"))[1], "gulf.data/inst/extdata/")
write.csv(x, file = paste0(path, "scs.set.1989.csv"), row.names = FALSE)

