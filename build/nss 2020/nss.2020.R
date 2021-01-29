source("R/gse.R")

library(gulf.data)
#library(gulf.manage)

# Load raw data:
load(locate(file = "nss.ese.2020"))

x <- NS20
rm(NS20)
x$set <- x$set[x$set$SETNO != 111, ] # Remove blank tow.

# Reformat:
y <- ese2gsd(x, survey = "nss")

y <- lapply(y, squeeze)

# Fix index variables (index variables must all be defined):

# Set card corrections (indexed by 'set.number'):
vars <- c("date", "cruise", "set.number", "experiment", "start.time", "stop.time", "station", "duration")
vars <- vars[vars %in% names(y$set)]
y$set <- y$set[c(vars, setdiff(names(y$set), vars))]
y$set$comment <- gulf.utils::deblank(y$set$comment)

# Set corrections - depth:
y$set$depth.end[y$set$set.number == 51] <- 10
y$set$depth.start[y$set$set.number == 53] <- 16
y$set$depth.end[y$set$set.number == 54] <- 12
y$set$depth.start[y$set$set.number == 55] <- 10
y$set$depth.start[y$set$set.number == 58] <- 32
y$set$depth.end[y$set$set.number == 33] <- 10
y$set$depth.start[y$set$set.number == 43] <- 36
y$set$depth.end[y$set$set.number == 110] <- 19

# Set corrections - time:
y$set$stop.time[y$set$set.number == 23] <- "13:16:00"
y$set$start.time[y$set$set.number == 56] <- "11:37:00"

# Set corrections - comments:
y$set$comment[y$set$set.number == 96] <- "Null - Caught chunk of mud"
y$set$comment[y$set$set.number == 105] <- "The longitude looks fine when mapped and compared to station location. We can double check with GPS"

# Set corrections - Miscellaneous:
y$set$experiment[y$set$set.number == 10] <- 1
y$set$experiment[y$set$set.number == 34] <- 1
y$set$current[y$set$set.number == 18] <- 1
y$set$station[y$set$set.number == 89] <- "B1"
y$set$station[y$set$set.number == 102] <- 331
y$set$wind.force[y$set$set.number == 27] <- 3
y$set$wind.force[y$set$set.number == 35] <- 3

# Set corrections - coordinates:
y$set$longitude.stop[y$set$set.number == 1] <- 6426.14
y$set$latitude.stop[y$set$set.number == 9] <- 4638.53
y$set$latitude.start[y$set$set.number == 10] <- 4644
y$set$longitude.stop[y$set$set.number == 12] <- 6426.3
y$set$latitude.stop[y$set$set.number == 23] <- 4643.3
y$set$longitude.stop[y$set$set.number == 23] <- 6436.96
y$set$latitude.stop[y$set$set.number == 24] <- 4648.52
y$set$longitude.start[y$set$set.number == 31] <- 6355.55
y$set$longitude.stop[y$set$set.number == 31] <- 6354.95
y$set$latitude.start[y$set$set.number == 35] <- 4646.05
y$set$longitude.stop[y$set$set.number == 37] <- 6446.18
y$set$latitude.stop[y$set$set.number == 41] <- 4700.13
y$set$longitude.start[y$set$set.number == 48] <- 6429.3
y$set$longitude.start[y$set$set.number == 66] <- 6428.83
y$set$latitude.start[y$set$set.number == 67] <- 4633.81
y$set$latitude.stop[y$set$set.number == 68] <- 4617.5
y$set$longitude.stop[y$set$set.number == 73] <- 6408.86
y$set$latitude.start[y$set$set.number == 75] <- 4626.3
y$set$latitude.start[y$set$set.number == 88] <- 4611.99
y$set$latitude.stop[y$set$set.number == 91] <- 4614.45
y$set$longitude.stop[y$set$set.number == 104] <- 6400.69
y$set$depth.end[y$set$set.number == 110] <- 19


# y$set[y$set$longitude.stop == 640.69, ] <- 6446.18  # Not sure


# Basket corrections (indexed by 'set.number', 'species', 'size.class' and sometimes 'weight'):
#y$basket$weight[(y$basket$set.number == 13) & (y$basket$species == 2550) & (y$basket$size.class == 1)] <- 0.524
#y$basket$sampled[(y$basket$set.number == 23) & (y$basket$species == 2550) & (y$basket$size.class == 1)] <- "Y"

y$basket$

# Bio card corrections:
#y$bio$egg.condition[(y$bio$set.number == 11) & (y$bio$species == 2550) & (y$bio$specimen == 6742)] <- 0
#y$bio$sex[(y$bio$set.number == 17) & (y$bio$species == 2550) & (y$bio$specimen == 9947)] <- 1
# y$bio$sex[which(y$bio$species == 2550 & is.na(y$bio$sex) & (y$bio$egg.condition %in% c(0, 4)))] <- 2
# y$bio$sex[which(y$bio$species == 2550 & is.na(y$bio$sex) & is.na(y$bio$egg.condition))] <- 1

y$bio$sex[y$bio$set.number == 70] <- 1
y$bio$egg.condition[y$bio$set.number == 32] <- NA


y$bio$comment <- gulf.utils::deblank(y$bio$comment)

# Generate catch table:
key.cat <- c("date", "cruise", "set.number", "species", "size.class")
tmp <- stats::aggregate(list(weight.caught = y$basket$weight), by = y$basket[key.cat], sum)
y$cat$weight.caught <- tmp$weight.caught[match(y$cat[key.cat], tmp[key.cat])]
index <- which(y$basket$sampled == "Y")
tmp <- stats::aggregate(list(sample.weight = y$basket$weight[index]), by = y$basket[index, key.cat], sum)
y$cat$weight.sampled <- tmp$sample.weight[match(y$cat[key.cat], tmp[key.cat])]
tmp <- stats::aggregate(list(n = y$basket$set.number), by = y$basket[key.cat], length)
y$cat$number.basket <- tmp$n[match(y$cat[key.cat], tmp[key.cat])]
y$cat$weight.sampled[is.na(y$cat$weight.sampled)] <- 0
y$cat <- y$cat[c(setdiff(names(y$cat), "comment"), "comment")]
y$cat$comment <- gulf.utils::deblank(y$cat$comment)

# Write data gulf.data:
path <- gsub("gulf.manage", "gulf.data", getwd())
path <- paste0(path, "/inst/extdata")
if (file.exists(path)){
   year <- sort(unique(year(y$set)))
   write.csv(y$set, file = paste0(path, "/", "nss.set.", year, ".csv"), row.names = FALSE)
   write.csv(y$cat, file = paste0(path, "/", "nss.cat.", year, ".csv"), row.names = FALSE)
   write.csv(y$bio, file = paste0(path, "/", "nss.bio.", year, ".csv"), row.names = FALSE)
}

# Re-format for Oracle export:
