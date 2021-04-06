library(gulf.data)
library(gulf.spatial)

# Flag to publish data: 
publish <- FALSE

# Read original raw set data:
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

# Extract sampler and coordiante data from biological data:
files <- locate(file = "GCR88")
b <- read.scsbio(files, drop = FALSE)
r <- aggregate(b["longitude.start"], by = b[c("date", "tow.number")], unique)
r$latitude.start <- aggregate(list(x = b$latitude.start), by = b[c("date", "tow.number")], unique)$x
r$sampler <- aggregate(list(x = b$samplers), by = b[c("date", "tow.number")], function(x) paste(unique(x), collapse = ","))$x
r <- sort(r, by = c("date", "tow.number"))
r$sampler <- sampler(r$sampler, project = "scs")
r$sampler[r$date == "1988-11-09" & r$tow.number == 8] <- "HELENE CHIASSON, RENALD HACHE"
r$sampler[r$date == "1988-08-23" & r$tow.number == 3] <- "MARCEL HEBERT, RENALD HACHE"
ix <- match(x[c("date", "tow.number")], r[c("date", "tow.number")])
x$sampler <- r$sampler[ix]
x$sampler[is.na(x$sampler)] <- ""
x$latitude <- r$longitude.start[ix]
x$longitude  <- r$latitude.start[ix]

# Spot coordinate corrections:
x$longitude.end.logbook[which((x$date == "1988-08-09") & (x$tow.number == 6))] <- 14929.6
x$latitude.start.logbook[which((x$date == "1988-08-09") & (x$tow.number == 6))] <- 29917.2
x$latitude.start.logbook[which((x$date == "1988-09-17") & (x$tow.number == 2))] <- 29626.4
x$latitude.start.logbook[which((x$date == "1988-08-07") & (x$tow.number == 6))] <- 30298.5
x$latitude.start.logbook[which((x$date == "1988-11-13") & (x$tow.number == 5))] <- 30720.1  
x$latitude.end.logbook[which((x$date == "1988-08-21") & (x$tow.number == 3))] <- 31029.4 
x$latitude.end.logbook[which((x$date == "1988-09-02") & (x$tow.number == 2))] < 29335.9
x$latitude.start.logbook[which((x$date == "1988-08-09") & (x$tow.number == 9))] <- 29644.0
x$latitude.start.logbook[which((x$date == "1988-08-31") & (x$tow.number == 1))] <- 30151.6
x$longitude.end.logbook[which((x$date == "1988-08-11") & (x$tow.number == 3))] <- 14963.3
x$longitude.end.logbook[which((x$date == "1988-08-22") & (x$tow.number == 3))] <- 15139.0
x$latitude[which((x$date == "1988-08-23") & (x$tow.number == 1))] <- 30239.5 
x$latitude[which((x$date == "1988-08-10") & (x$tow.number == 3))] <- NA   
x$longitude[which((x$date == "1988-08-10") & (x$tow.number == 3))] <- NA 

# Replace redundant coordinates:
x$longitude[which(!is.na(x$longitude.start.logbook) | !is.na(x$longitude.end.logbook))] <- NA
x$latitude[which(!is.na(x$latitude.start.logbook) | !is.na(x$latitude.end.logbook))] <- NA

# Format time fields:
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
x$comment <- gsub("[Mm]\x88tre[s]*", "m ", x$comment)
x$comment <- gsub("[Mm]\x88[s]*", "meme ", x$comment)

# Extract tow duration information from comments:
ix <- grep("[0-9]*[:]*[0-9][ ]*minu", tolower(x$comment))
str <- x$comment[ix]
str <- deblank(unlist(lapply(strsplit(str, "minu"), function(x) x[1])))
str <- unlist(lapply(strsplit(str, " "), function(x) x[length(x)]))
str[gsub("[:]", "", str) == str] <- paste0(str[gsub("[:]", "", str) == str], ":00")
x$duration <- ""
x$duration[ix] <- str

# Extract wing spread information from comments:
ix <- grep("[0-9]+[.;,][0-9]", tolower(x$comment))
x$comment <- gsub("7.5 ms", "7.5m", x$comment)
x$comment <- gsub("6;4[ ms]", "6.4m ", x$comment)
x$comment <- gsub("9;2[ ms]*", "9.2m ", x$comment)
x$comment <- gsub("5;9[ ms]*", "5.9m ", x$comment)
x$comment <- gsub("5;7[ ms]*", "5.7m ", x$comment)
x$comment <- gsub(" 7[ ms]*", " 7.0m ", x$comment)
x$comment <- gsub("6;0[ ms]*", " 6.0m ", x$comment)
x$comment <- gsub("6;4[ ms]*", " 6.4m ", x$comment)
x$comment <- gsub("5;4[ ms]*", " 5.4m ", x$comment)
x$comment <- gsub("5;0[ ms]*", " 5.0m ", x$comment)
x$comment <- gsub("6;3[ ms]*", " 6.3m ", x$comment)
x$comment <- gsub("5;5[ ms]*", " 5.5m ", x$comment)
x$comment <- gsub("5;3[ ms]*", " 5.3m ", x$comment)
x$comment <- gsub("6;1[ ms]*", " 6.1m ", x$comment)
x$comment <- gsub("4;0[ ms]*", " 4.0m ", x$comment)
x$comment <- gsub("8;2[ ms]*", " 8.2m ", x$comment)
x$comment <- gsub("9;0[ ms]*", " 9.0m ", x$comment)
x$comment <- gsub("9;3[ ms]*", " 9.3m ", x$comment)
x$comment <- gsub("3;3[ ms]*", " 3.3m ", x$comment)
x$comment <- gsub("1;8[ ms]*", " 1.8m ", x$comment)
x$comment <- gsub("2;3[ ms]*", " 2.3m ", x$comment)
x$comment <- gsub("8;0[ ms]*", " 8.0m ", x$comment)
x$comment <- gsub("8;3[ ms]*", " 8.3m ", x$comment)
x$comment <- gsub("10;0[ ms]*", "10.0m ", x$comment)

# Remove redundant fields:
remove <- c("year", "month", "day", "season", "vessel", "loranx.start", "loranx.end", "lorany.start", "lorany.end")
x <- x[setdiff(names(x), remove)]

# Remove empty variables:
x <- squeeze(x)

x$duration


