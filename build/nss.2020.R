# Load raw data:
load(locate(file = "nss.ese.2020"))
x <- ns2020
rm(ns2020)
x$set <- x$set[x$set$SETNO != 111, ] # Remove blank tow.

# Reformat:
y <- ese2gsd(x, survey = "nss")

# Fix index variables (index variables must all be defined):


# Set card corrections (indexed by 'set.number'):
y$set$latitude.stop[y$set$set.number == 68] <- NA
y$set$speed[y$set$set.number == 68] <- NA
y$set$warp[y$set$set.number == 96] <- NA
y$set$experiment[y$set$set.number == 96] <- 1 
y$set$comment[y$set$set.number == 1] <- "Invalid Tow: Belly torn, head back to port to pick up spare trawl"

# Basket corrections (indexed by 'set.number', 'species', 'size.class' and sometimes 'weight'):
y$basket$weight[(y$basket$set.number == 13) & (y$basket$species == 2550) & (y$basket$size.class == 1)] <- 0.524 
y$basket$sampled[(y$basket$set.number == 23) & (y$basket$species == 2550) & (y$basket$size.class == 1)] <- "Y" 

# Bio card corrections:
y$bio$egg.condition[(y$bio$set.number == 11) & (y$bio$species == 2550) & (y$bio$specimen == 6742)] <- 0
y$bio$sex[(y$bio$set.number == 17) & (y$bio$species == 2550) & (y$bio$specimen == 9947)] <- 1

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

# Write tables to gulf.data:

# Re-format for Oracle export:




