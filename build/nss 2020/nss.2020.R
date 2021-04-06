source("R/gse.R")

library(gulf.data)
library(gulf.spatial)

year <- 2020

# Load raw data:
load(locate(file = paste0("nss.ese.", year)))
x <- PER2020150
rm(PER2020150)

x$set <- x$set[x$set$SETNO != 111, ] # Remove blank tow.

#Set corrections
x$set$ELONG[which(x$set$SETNO == 1)] <- 6426.14
index = x$set$SETNO == 9 
x$set[which(index),]$ELAT <- 4638.53
index = x$set$SETNO == 10 
x$set[which(index),]$SLAT <- 4644
index = x$set$SETNO == 10 
x$set[which(index),]$EXPERIMENT_TYPE_CODE <- 1
index = x$set$SETNO == 12 
x$set[which(index),]$ELONG <- 6426.3
index = x$set$SETNO == 18 
x$set[which(index),]$CURNT <- 1
index = x$set$SETNO == 23 
x$set[which(index),]$END_TIME <- 1316
index = x$set$SETNO == 23 
x$set[which(index),]$ELAT <- 4643.3
index = x$set$SETNO == 23 
x$set[which(index),]$ELONG <- 6436.96
index = x$set$SETNO == 24 
x$set[which(index),]$ELAT <- 4648.52
index = x$set$SETNO == 27 
x$set[which(index),]$FORCE <- 3
index = x$set$SETNO == 31 
x$set[which(index),]$SLONG <- 6355.55
index = x$set$SETNO == 31 
x$set[which(index),]$ELONG <- 6354.95
index = x$set$SETNO == 33 
x$set[which(index),]$END_DEPTH <- 10
index = x$set$SETNO == 34 
x$set[which(index),]$EXPERIMENT_TYPE_CODE <- 1
index = x$set$SETNO == 35 
x$set[which(index),]$SLAT <- 4646.05
index = x$set$SETNO == 35 
x$set[which(index),]$FORCE <- 3
index = x$set$SETNO == 37 
x$set[which(index),]$ELONG <- 6446.18
index = x$set$SETNO == 41 
x$set[which(index),]$ELAT <- 4700.13
index = x$set$SETNO == 43 
x$set[which(index),]$START_DEPTH <- 36
index = x$set$SETNO == 48 
x$set[which(index),]$SLONG <- 6429.3
index = x$set$SETNO == 51 
x$set[which(index),]$END_DEPTH <- 10
index = x$set$SETNO == 53 
x$set[which(index),]$START_DEPTH <- 16
index = x$set$SETNO == 54 
x$set[which(index),]$END_DEPTH <- 12
index = x$set$SETNO == 55 
x$set[which(index),]$START_DEPTH <- 10
index = x$set$SETNO == 56 
x$set[which(index),]$START_TIME <- 1137
index = x$set$SETNO == 58 
x$set[which(index),]$START_DEPTH <- 32
index = x$set$SETNO == 66 
x$set[which(index),]$SLONG <- 6428.83
index = x$set$SETNO == 67 
x$set[which(index),]$SLAT <- 4633.81
index = x$set$SETNO == 68 
x$set[which(index),]$ELAT <- 4617.5
index = x$set$SETNO == 73 
x$set[which(index),]$ELONG <- 6408.86
index = x$set$SETNO == 75 
x$set[which(index),]$SLAT <- 4626.3
index = x$set$SETNO == 88 
x$set[which(index),]$SLAT <- 4611.99
index = x$set$SETNO == 89 
x$set[which(index),]$STATION <- "B1"
index = x$set$SETNO == 91 
x$set[which(index),]$ELAT <- 4614.45
index = x$set$SETNO == 96 
x$set[which(index),]$NOTE <- "Null - Caught chunk of mud"
index = x$set$SETNO == 102 
x$set[which(index),]$STATION <- 331
index = x$set$SETNO == 104 
x$set[which(index),]$ELONG <- 6400.69
index = x$set$SETNO == 110
x$set[which(index),]$END_DEPTH <- 19

#Basket corrections
index = x$basket$SPEC == 599  
x$basket[which(index),]$SPEC <- 611
index = x$basket$SETNO == 63 & x$basket$SPEC == 8300
x$basket[which(index),]$SAMPLED <- "N"
index = x$basket$SETNO == 76  & x$basket$SPEC == 6511
x$basket[which(index),]$SAMPLED <- "N"
index = x$basket$SETNO == 57  & x$basket$SPEC == 2550
x$basket[which(index),]$SIZE_CLASS  <- 1
index = x$basket$SETNO == 5  & x$basket$SPEC == 2539
x$basket[which(index),]$SAMPLED <- "Y"

# Check these svp:
x$basket[is.na(x$basket$SPEC), ]
x$basket[is.na(x$basket$SAMPLED), ]

# Detail corrections
index = x$detail$SETNO == 66 & x$detail$SPECIMEN_ID == 21733 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 87 & x$detail$SPECIMEN_ID == 32083 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 83 & x$detail$SPECIMEN_ID == 30070 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 35 & x$detail$SPECIMEN_ID == 14057 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 35 & x$detail$SPECIMEN_ID == 13878 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 70 & x$detail$SPECIMEN_ID == 24129 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 49 & x$detail$SPECIMEN_ID == 18389 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 68 & x$detail$SPECIMEN_ID == 22641 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 70 & x$detail$SPECIMEN_ID == 23868 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 52 & x$detail$SPECIMEN_ID == 18798 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 54 & x$detail$SPECIMEN_ID == 19197 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 73 & x$detail$SPECIMEN_ID == 26240 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 70 & x$detail$SPECIMEN_ID == 23991 
x$detail[which(index),]$SEX <- 1
index = x$detail$SETNO == 32 & x$detail$SPECIMEN_ID == 12016 
x$detail[which(index),]$LOBSTER_EGG_CONDITION <- NA

#x$detail[which(x$detail$SEX),] 


# Check : 
ix <- x$detail$SETNO == 17 & x$detail$SPEC == 60 # Size class 2 change to 1:

# Check herring length units
x$detail$LENGTH[x$detail$SPEC == 60] <- x$detail$LENGTH[x$detail$SPEC == 60] / 10

# Reformat:
y <- ese2gsd(x, survey = "nss")
y <- lapply(y, squeeze)

# Calculate tow distance:
y$set$distance <- (1 / 1.852) * distance(-dmm2deg(y$set$longitude.start), dmm2deg(y$set$latitude.start), -dmm2deg(y$set$longitude.stop), dmm2deg(y$set$latitude.stop), pairwise = FALSE)
y$set$distance <- round(y$set$distance, 3)

# Check tow distance below 0.25nm svp:

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





