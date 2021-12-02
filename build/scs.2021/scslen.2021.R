library(gulf.data)

year <- 2021

# Regular survey measurements:
x <- read.csv(paste0("inst/extdata/raw/scs.len.", year, ".csv"), header = TRUE, stringsAsFactors = FALSE)

names(x) <- tolower(names(x))
x$tow.id <- toupper(x$gpnum)

# Parse data and time:
x$date <- unlist(lapply(strsplit(x$datetime, " "), function(x) x[1]))
x$time <- unlist(lapply(strsplit(x$datetime, " "), function(x) x[2]))

# Parse measurement field:
x$length.unit <- gsub("[0-9.]", "", x$measurement)
x$length <- (gsub("m", "", x$measurement))
x$length <- as.numeric(gsub(";Male", "", x$length))

# Define length precision:
x$length.precision <- 1

# Remove irrelevant fields:
vars <- c("date", "time", "tow.id", "species", "length", "length.unit", "length.precision")
x <- x[vars]

# Read set of valid tows:
s <- compress(read.scsset(year = year))

# Check that tow IDs exist:
index <- match(x$tow.id, s$tow.id)
x[is.na(index), ]

# Check that dates match:
all(x$month == s$month[index] & x$day == s$day[index])

# Corrections:
x$length.unit[x$length.unit == ""] <- "mm"
x$species[x$tow.id == "GP056FR" & x$length == 285.1] <- 31
x$length.unit <- gsub(";Male", "", x$length.unit)

# Write to file:
write.table(x, file = paste0("data/by-catch/scs.len.", year, ".csv"), sep = ",", row.names = FALSE)

# Write to gulf.data repository:
tmp <- unlist(lapply(strsplit(getwd(), "/"), function(x) x[length(x)]))
path <- paste0(gsub(tmp, "", getwd()), "gulf.data/inst/extdata")
if (file.exists(path)){
   file <- paste0(path, "/", "scs.len.", year, ".csv")
   write.csv(x, file = file, row.names = FALSE)
}
