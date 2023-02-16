library(gulf.data)
library(gulf.spatial)

# variables:
years <- 2020:2022

# Load tow data:
x <- read.scsset(years, valid = 1, survey = "regular")
x$longitude <- longitude(x)
x$latitude  <- latitude(x)

#format lat lon decimal places
x$longitude <- round(x$longitude,4)
x$latitude  <- round(x$latitude,5)


# Add snow crab counts:
z <- read.scsbio(years)
import(x, fill = 0) <- catch(z, category = c("m", "f")) #creates catch summary per tow
x$snow.crab.male <- x$m
x$snow.crab.female <- x$f

# Load by-catch data from the snow crab survey:
y <- read.scscat(2022, survey = "regular") 
v <- read.csv("W:/Crab/Offshore Crab Common/Fishing Year 2021/Trawl Data/South Western Gulf/scs.cat.2021.csv", header = TRUE, stringsAsFactors = FALSE) #getting 2021 another way
y <- rbind(y, v) #binding 2020 and 2021

# Add species names:
y$species.name <- species.str(y$species)

x$date   <- as.Date(x$date)
x$year <- year(x$date)
x$month <- month(x$date)
x$day <- format(x$date, format = "%d")

#species of interest
species <- c("Atlantic cod", "American plaice", "Yellowtail flounder", "Winter flounder", "Thorny skate", 
             "Smooth skate", "Longhorn sculpin", "Sea potato", "Lesser toad crab")

key <- c("date", "tow.id", "tow.number")
#i = 1

for (i in 1:length(species)){
   tmp <- y[y$species.name == species[i], ]  # Data subset.
   x[, species[i]] <- 0  # Initialize.
   tmp <- tmp[which(!duplicated(tmp[key])), ]
   ix <- match(x[key], tmp[key])
   x[!is.na(ix), species[i]] <- tmp$number.caught[ix[!is.na(ix)]]
} 

vars <- c("year", "month", "day", "tow.id", "longitude", "latitude", "start.time", "comment", "snow.crab.male", "snow.crab.female", "Atlantic cod", 
          "American plaice", "Yellowtail flounder", "Winter flounder", "Thorny skate", "Smooth skate", "Longhorn sculpin", "Sea potato", "Lesser toad crab") 
x <- x[vars]
             
excel(x)

