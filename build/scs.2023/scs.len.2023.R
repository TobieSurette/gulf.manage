library(gulf.data)
library(gulf.spatial)

year <- 2023

# Regular survey measurements:
x <- read.csv(paste0("build/scs.", year, "/data/scs.len.", year, ".csv"), header = TRUE, stringsAsFactors = FALSE)
z <- read.csv(paste0("build/scs.", year, "/data/scs.len.", year, ".lobster.csv"), header = TRUE, stringsAsFactors = FALSE) # Lobster.
x <- rbind(x, z)
names(x) <- gsub("[m]easurements*", "", names(x))
names(x) <- gsub("^[.]+", "", names(x))
names(x) <- tolower(names(x))
x$tow.id <- gulf.utils::deblank(toupper(x$gpnum))
x$tow.id <- gsub("\n", "", x$tow.id)

# Parse data and time:
x$date <- unlist(lapply(strsplit(x$datetime, " "), function(x) x[1]))
x$time <- unlist(lapply(strsplit(x$datetime, " "), function(x) x[2]))

# Parse measurement field:
x$length.unit <- gsub("[0-9.]", "", x$measurement)
x$length <- (gsub("m", "", x$measurement))
x$length <- as.numeric(gsub(";Male", "", x$length))

# Remove empty rows:
x[is.na(x$length), ]
x <- x[!is.na(x$length), ]

# Define length precision:
x$length.precision <- 1

# Comments:
x$comment <- x$measurement.comment
x$comment <- gsub("\n", "", x$comment)

# Extract lobster sexes:
x$sex <- ""
ix <- which(x$species == 2550)
x$sex[ix] <- toupper(substr(x$comment[ix], 1, 1))
tows <- unique(x$tow.id[ix])
for (i in 1:length(tows)){
   sex <- NA
   iy <- which(x$tow.id[ix] == tows[i])
   if (length(iy) > 1){
      sex <- x$sex[ix][iy[1]]
      for (j in 2:length(iy)){
         if (x$sex[ix][iy[j]] == "") x$sex[ix][iy[j]] <- sex else sex <- x$sex[ix][iy[j]]
      }
   }
}

# Remove irrelevant fields:
vars <- c("date", "time", "tow.id", "species", "sex", "length", "length.unit", "length.precision", "comment")
x <- x[vars]

# Spot corrections:
x$tow.id[x$tow.id == "GP049F"] <- "GP049FR1"
x$date[x$tow.id == "GP174F"]   <- "2023-07-27" 
x <- x[x$length != 0.02, ]

# Read set of valid tows:
s <- compress(read.scsset(year = year))

# Check that tow IDs exist:
index <- match(x$tow.id, s$tow.id)
x[is.na(index), ]

# Check that dates match:
all(x$month == s$month[index] & x$day == s$day[index])

# Corrections:
x$length.unit[x$length.unit == ""] <- "mm"
#x$species[x$tow.id == "GP056FR" & x$length == 285.1] <- 31

# Write to file:
#write.table(x, file = paste0("data/by-catch/scs.len.", year, ".csv"), sep = ",", row.names = FALSE)

# Write to gulf.data repository:
tmp <- unlist(lapply(strsplit(getwd(), "/"), function(x) x[length(x)]))
path <- paste0(gsub(tmp, "", getwd()), "gulf.data/inst/extdata")
if (file.exists(path)){
   file <- paste0(path, "/", "scs.len.", year, ".csv")
   write.csv(x, file = file, row.names = FALSE)
}
