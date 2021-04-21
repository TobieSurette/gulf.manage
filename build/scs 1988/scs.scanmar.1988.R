library(gulf.data)

# Load data:
x <- read.csv(locate(file = c("1988", "scanmar", "csv")), header = TRUE)
y <- read.csv(locate(file = c("1988", "coordinates", "csv")), header = TRUE)

# Rename variables:
names(y) <- tolower(gsub("tow.no", "tow.number", names(y)))
names(y) <- tolower(gsub("door.spread", "wing.spread", names(y)))
names(y) <- gsub("Time.Logbook", "time", names(y))
names(y) <- gsub("loranx.logbook", "loran.x", names(y))
names(y) <- gsub("lorany.logbook", "loran.y", names(y))
x$measurement <- gsub("doorspread", "door.spread", x$measurement)
x$measurement <- gsub("wingspread", "wing.spread", x$measurement)

# Define date field:
x$date <- as.character(date(x))
y$date <- as.character(date(y))

# Format time variable:
ix <- which(nchar(y$time) == 4)
y$time[ix] <- paste0("0", y$time[ix])
ix <- which(nchar(y$time) == 5)
y$time[ix] <- paste0(y$time[ix], ":00")   
y$time[is.na(y$time)] <- "        "

# Import time and coordinate data:
x$loran.x <- x$loran.y <- NA
x$time <- "        "
iy <- which(!is.na(y$wing.spread))
tows <- sort(unique(y[iy, c("date", "tow.number")]), by = c("date", "tow.number"))
for (i in 1:nrow(tows)){
   # Get indices:
   ix <- which(x$date == tows$date[i] & x$tow.number == tows$tow.number[i] & (x$measurement == "wing.spread"))
   iy <- which(y$date == tows$date[i] & y$tow.number == tows$tow.number[i])
   
   # Import data:
   x$value[ix]   <- y$wing.spread[iy]
   x$time[ix]    <- y$time[iy]
   x$loran.x[ix] <- y$loran.x[iy]
   x$loran.y[ix] <- y$loran.y[iy]
}

# Remove irrelevant data:
vars <- c("date", "time", "tow.id", "tow.number", "loran.x", "loran.y", "measurement", "value")
x <- x[vars]

# Restructure table:
z <- x[x$measurement != "wing.spread", ]
x <- x[which(x$measurement == "wing.spread"), ]
names(x) <- gsub("value", "wing.spread", names(x))
x <- x[, -which(names(x) == "measurement")]
x$door.spread <- NA
x$headline.height <- NA

# Import headline height data:
ix <- z$measurement == "headline.height"
u <- aggregate(z$value[ix], z[ix, c("date", "tow.number")], function(x) list(x))
ix <- match(x[c("date", "tow.number")], u[c("date", "tow.number")])
for (i in 1:nrow(u)){
   ix <- match(x[c("date", "tow.number")], u[i, c("date", "tow.number")])
   x[which(!is.na(ix))[1:length(u$x[i][[1]])], "headline.height"] <- u$x[i][[1]]
}

# Import door spread data:
z <- z[z$measurement == "door.spread", ]
names(z) <- gsub("value", "door.spread", names(z))
z$wing.spread <- NA
z$headline.height <- NA
x <- rbind(z[names(x)], x)

# Write data:
path <- paste0(unlist(strsplit(getwd(), "gulf"))[1], "gulf.trawl.data/inst/extdata/scs.scanmar.1988/")
write.csv(x, file = paste0(path, "scs.scanmar.1988.csv"), row.names = FALSE)

