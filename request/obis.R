library(gulf.data)
library(gulf.spatial)
library(worrms)

# To do:
# - Finalize format table
# - write update.obis function
# - Write FGP update and export function
# - Remove species that were not consistently observed throughout 2010-2020
# - Expand output to 2010:2020.
# - Add trawl swept areas.
# - Add gear information.
# - Some more metadata?

# Control variables:
years <- 2019
variable <- "number.caught"

# Load tow data:
x <- read.scsset(years, valid = 1, survey = "regular")
x$longitude <- longitude(x)
x$latitude  <- latitude(x)

# Load by-catch data from the snow crab survey:
y <- read.scscat(years, survey = "regular")
y$tow.id <- tow.id(y)
y$aphiaID <- species(y$species, output = "worms")

# Compile classification information from WoRMS database:
u <- sort(unique(y$aphiaID))
classification <- function(x) return(as.data.frame(as.list(wm_classification(id = x))))
tmp <- lapply(u, classification) # Takes a while
names(tmp) <- u
u <- tmp
tab <- data.frame(aphiaID = as.numeric(names(u)))
tab[unique(unlist(lapply(u, function(x) return(x$rank))))] <- ""
for (i in 1:length(u)) tab[i, u[[i]]$rank] <- u[[i]]$scientificname
names(tab) <- tolower(names(tab))

# Re-format for OBIS:
tab$aphiaID <- tab$aphiaid
tab$scientificNameID <- paste0("urn:lsid:marinespecies.org:taxname:", tab$aphiaid)
tab$scientificName   <- tab$species 
for (i in 1:nrow(tab)){
   tab$specificEpithet[i] <- gsub(tab$genus[i], "", tab$species[i])
   tab$specificEpithet[i] <- gsub("[()]", "", tab$specificEpithet[i])
}
tab$specificEpithet <- deblank(tab$specificEpithet)

# Import WoRMS information into catch table:
vars <- c("aphiaID", "scientificNameID", "scientificName", "specificEpithet", "kingdom", "phylum", "class", "order", "family", "genus", "subgenus")                
ix <- match(y$aphiaID, tab$aphiaID)
y[vars] <- tab[ix, vars]
y <- y[!is.na(y$aphiaID), ]

# Import tow data info into catch table:
ix <- match(y[key(x)], x[key(x)])
y[c("longitude", "latitude", "swept.area", "depth", )] <- x[ix, c("longitude", "latitude", "swept.area", "depth")]
y$start.time <- substr(time(x, "start"), 12, 19)[ix]  


y$language        <- "En"
y$license         <- "http://data.gc.ca/eng/open-government-licence-canada & http://www.canadensys.net/norms"
y$rightsHolder    <- "Her Majesty the Queen in right of Canada, as represented by the Minister of Fisheries and Oceans"
y$institutionID   <- "DFO-GFC"
y$datasetID       <- "DFO_Gulf_SnowCrabSurveys" 
y$institutionCode <- "Gulf Fisheries Centre"
y$collectionCode  <- "DFO_Gulf_SnowCrabSurveys"
y$datasetName     <- "Southern Gulf of St. Lawrence Snow crab research trawl survey data (Gulf region, Canada)"
y$basisOfRecord   <- "HumanObservation"

y$dynamicProperties <- paste0("Sample size = ", round(y$swept.area), " meters square; Classification=WoRMS")
y$individualCount   <- y$number.caught

y$minimumDepthInMeters <- ""
y$maximumDepthInMeters <- round(1.8288 * y$depth)
y$decimalLatitude      <- y$latitude
y$decimalLongitude     <- y$longitude

y$modified             <- "2021-03-22T12:00:00Z"
y$occurrenceID <-  paste0(y$institutionCode, "_", y$collectionCode, "_", y$date, "_", y$tow.number, "_", y$aphiaID)

y$catalogNumber        <-    

y$recordedBy           <- ""

samplingProtocol       <- ""
y$eventTime            <- y$start.time 
fieldNumber            <- "" 

# Remove or identify variables for export:

# Remove fields with zero or missing 'individualCount'


