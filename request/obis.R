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
years <- 2020
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
lapply(u, function(x) return(x$rank))
tab <- data.frame(aphiaID = as.numeric(names(u)))
tab[unique(unlist(lapply(u, function(x) return(x$rank))))] <- ""
for (i in 1:length(u)) tab[i, u[[i]]$rank] <- u[[i]]$scientificname
names(tab) <- tolower(names(tab))

tab$aphiaID <- tab$aphiaid
tab$scientificNameID <- paste0("urn:lsid:marinespecies.org:taxname:", tab$aphiaid)
tab$scientificName   <- tab$species 
for (i in 1:nrow(tab)){
   tab$specificEpithet[i] <- gsub(tab$genus[i], "", tab$species[i])
   tab$specificEpithet[i] <- gsub("[()]", "", tab$specificEpithet[i])
}
tab$specificEpithet <- deblank(tab$specificEpithet)

# Remove irrelevant fields:
vars <- c("aphiaID", "scientificNameID", "scientificName", "specificEpithet", "kingdom", "phylum", "class", "order", "family", "genus", "subgenus")                
tab <- tab[vars]

ix<- match(y$aphiaID, tab$aphiaID)

id
modified
language                En
license                 http://data.gc.ca/eng/open-government-licence-canada & http://www.canadensys.net/norms
rightsHolder            Her Majesty the Queen in right of Canada, as represented by the Minister of Fisheries and Oceans
institutionID           DFO-GFC
datasetID               DFO_Gulf_SnowCrabSurveys 
institutionCode         Gulf Fisheries Centre
collectionCode          DFO_Gulf_SnowCrabSurveys
datasetName             Snow crab research trawl survey database (Southern Gulf of St. Lawrence, Gulf region, Canada) from 1988 to 2010
basisOfRecord           HumanObservation
dynamicProperties       Sample size = 3098 meters square; Classification=WoRMS
occurrenceID            DFO_Gulf_SnowCrabSurveys_Zone19-10/18/2006-3-Aspidophoroides monopterygius
catalogNumber           
recordedBy
individualCount        number.caught
sex
lifeStage
samplingProtocol
eventTime
year
month
day
fieldNumber
minimumDepthInMeters
maximumDepthInMeters             x$depth
decimalLatitude                  x$latitude
decimalLongitude                 x$longitude


y <- aggregate(y[variable], by = y[key(y)], sum)
import(x, var = variable, group = "species", fill = 0) <- y

vars <- c("date", "longitude", "latitude", "swept.area", "species", "name.en", "name.latin", variable)
x[vars]
excel(x)

scientificNameID
scientificName
kingdom
phylum
class
order
family
genus
subgenus
specificEpithet
infraspecificEpithet
scientificNameAuthorship


