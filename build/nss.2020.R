library(gulf.data)
library(gulf.manage)

# Load raw data:
load(locate(file = "nss.ese.2020"))
x <- ns2020
rm(ns2020)
x$set <- x$set[x$set$SETNO != 111, ] # Remove blank tow.

# Reformat:
y <- ese2gsd(x, survey = "nss")

# Fix index variables (index variables must all be defined):

# Set card corrections (indexed by 'set.number'):
vars <- c("date", "cruise", "set.number", "experiment", "start.time", "stop.time", "station", "duration")
vars <- vars[vars %in% names(y$set)]
y$set <- y$set[c(vars, setdiff(names(y$set), vars))]
y$set$comment <- gulf.utils::deblank(y$set$comment)
y$set$latitude.start[y$set$latitude.start == 4433.81] <- 4633.81
y$set$latitude.stop[y$set$latitude.stop == 4517.50] <- 4617.50
y$set$longitude.start[y$set$longitude.start == 6529.30] <- 6429.30    
y$set$longitude.stop[y$set$longitude.stop == 644618.00] <- 6446.18
   
# y$set[y$set$longitude.stop == 640.69, ] <- 6446.18  # Not sure


# Basket corrections (indexed by 'set.number', 'species', 'size.class' and sometimes 'weight'):
#y$basket$weight[(y$basket$set.number == 13) & (y$basket$species == 2550) & (y$basket$size.class == 1)] <- 0.524 
#y$basket$sampled[(y$basket$set.number == 23) & (y$basket$species == 2550) & (y$basket$size.class == 1)] <- "Y" 

# Bio card corrections:
#y$bio$egg.condition[(y$bio$set.number == 11) & (y$bio$species == 2550) & (y$bio$specimen == 6742)] <- 0
#y$bio$sex[(y$bio$set.number == 17) & (y$bio$species == 2550) & (y$bio$specimen == 9947)] <- 1
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




