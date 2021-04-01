library(gulf.data)
library(gulf.spatial)

# Read raw data:
x <- read.csv(locate(file = c("scs.set", "1988", "csv")), header = TRUE)

# Variable name fix:
names(x) <- gsub("[.]$", "", names(x))
     
# Add date field:
x <- cbind(data.frame(date = as.character(date(x)), stringsAsFactors = FALSE), x)

# Remove redundant coordinates:
x$longitude.end.logbook[which(x$longitude.start.logbook == x$longitude.end.logbook)] <- NA
x$latitude.end.logbook[which(x$latitude.start.logbook == x$latitude.end.logbook)] <- NA
x$longitude.start.logbook[which(x$longitude.start.logbook == x$longitude)] <- NA
x$latitude.start.logbook[which(x$latitude.start.logbook == x$latitude)] <- NA

# Import original logbook coordinates:
y <- read.csv(locate(file = c("scs", "coordinate", "1988", "csv")), header = TRUE)
y <- squeeze(y)
names(y) <- gsub("[.]logbook", "", tolower(names(y)))
names(y) <- gsub("[.]no", ".number", tolower(names(y)))
y$date <- as.character(date(y))

# Restructure data:
r <- aggregate(list(loranx.start = y$loranx), by = y[c("date", "tow.number")], function(x) x[1])
r$loranx.end <- aggregate(list(x = y$loranx), by = y[c("date", "tow.number")], function(x) if (length(x) > 1) return(x[length(x)]) else return(NA))$x
r$lorany.start <- aggregate(list(x = y$lorany), by = y[c("date", "tow.number")], function(x) x[1])$x
r$lorany.end <- aggregate(list(x = y$lorany), by = y[c("date", "tow.number")], function(x) if (length(x) > 1) return(x[length(x)]) else return(NA))$x

# Import Loran coordinates:
ix <- match(x[c("date", "tow.number")], r[c("date", "tow.number")])
x$loranx.start <- r$loranx.start[ix]
x$loranx.end   <- r$loranx.end[ix]
x$lorany.start <- r$lorany.start[ix]
x$lorany.end   <- r$lorany.end[ix]

# Substitute start and end coordinates:
x$longitude.start.logbook[is.na(x$longitude.start.logbook)] <- x$loranx.start[is.na(x$longitude.start.logbook)]
x$latitude.start.logbook[is.na(x$latitude.start.logbook)]   <- x$lorany.start[is.na(x$latitude.start.logbook)]
x$longitude.end.logbook[is.na(x$longitude.end.logbook)]     <- x$loranx.end[is.na(x$longitude.end.logbook)]
x$latitude.end.logbook[is.na(x$latitude.end.logbook)]       <- x$lorany.end[is.na(x$latitude.end.logbook)] 

# Fix time fields:
ix <- which(!is.na(x$start.time.logbook) & (nchar(x$start.time.logbook) == 4))
x$start.time.logbook[ix] <- paste0("0", x$start.time.logbook[ix])
ix <- which(nchar(x$start.time.logbook) == 5)
x$start.time.logbook[ix] <- paste0(x$start.time.logbook[ix], ":00")
x$start.time.logbook[is.na(x$start.time.logbook)] <- "        "

x$end.time.logbook <- deblank(x$end.time.logbook)
ix <- which(!is.na(x$end.time.logbook) & (nchar(x$end.time.logbook) == 4))
x$end.time.logbook[ix] <- paste0("0", x$end.time.logbook[ix])
ix <- which(nchar(x$end.time.logbook) == 5)
x$end.time.logbook[ix] <- paste0(x$end.time.logbook[ix], ":00")
x$end.time.logbook[which(is.na(x$end.time.logbook) | x$end.time.logbook == "")] <- "        "

# Fix comments:
x$comment <- deblank(x$comment)

# Remove empty variables:
x <- squeeze(x)

# Remove redundant fields:
remove <- c("year", "month", "day", "season", "vessel", "loranx.start", "loranx.end", "lorany.start", "lorany.end")
x <- x[setdiff(names(x), remove)]


