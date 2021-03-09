library(gulf)   # Best library ever.

# Snow crab data:

years <- 2012:2017
years2 <- 2012:2018
strata <- c(415:417, 424)   # Ça vient du September survey, c'est comme des blocks dans le NS survey.

# Read tow data:
s <- read.scset(year = years2, valid = 1)

# Determine stratum from coordinates:
s$stratum <- stratum(longitude(s), latitude(s))

# Eliminate irrelevant tows:
s <- s[which(s$stratum %in% strata), ]   # Le 'which' elimine les NA qui sont agacants.

# Load snow crab survey by-catch data for 2012:2017
c <- read.sccat(year = years)

# Add comment field to <= 2017 to match 2018 fields
c$comment=""

# Load snow crab survey by-catch data for 2018
c2 <- read.sccat(year = 2018)

# Joining data from <=2017 and 2018 together
c <- rbind(c, c2)

# Add species names:
c$species.name <- species.str(c$species)

# Get rid of irrelevant data:
index <- match(c[key], s[key])
c <- c[!is.na(index), ]

# List of species to merge:
species <- unique(c$species.name)
species <- species[!is.na(species)]

for (i in 1:length(species)){
  cc <- c[c$species.name == species[i], ]  # Data subset.
    s[, species[i]] <- 0  # Initialize.
  
  index <- match(cc[key], s[key])
  
  s[index, species[i]] <- cc$number.caught

} 

#checks for data
# table(c$year)
# table(c$species.name)
#var <- c("COD(ATLANTIC)", "AMERICAN PLAICE", "ARCTIC STAGHORN SCULPIN", "4-LINE SNAKE BLENNY", "WHELK EGGS (NS)", "SHRIMP", "TOAD CRAB,UNIDENT.", "PAGUROIDEA S.F.", "WHELKS", "ASTEROIDEA S.C.", "SEA URCHINS", "SPONGES", "LAVAL'S EELPOUT", "CAPELIN", "TWOHORN SCULPIN", "ALLIGATORFISH", "ATLANTIC SEA POACHER", "OCEAN QUAHAUG", "BASKET STARS", "SAND DOLLARS", "SEA ANEMONE", "SHORTHORN SCULPIN", "LUMPFISH", "SEA POTATO", "JELLYFISHES", "WITCH FLOUNDER", "SCALLOPS", "SEAWEED,(ALGAE),KELP", "REDFISH UNSEPARATED", "THORNY SKATE", "COMMON WOLF EEL", "SQUID (NS)", "YELLOWTAIL FLOUNDER", "NUCULANA SP.", "HAKE", "SPATULATE SCULPIN", "LONGHORN SCULPIN", "SMOOTH SKATE", "TURBOT,GREENLAND HALIBUT", "HADDOCK", "SILVER HAKE", "LONGFIN HAKE", "DOGFISHES (NS)", "MARLIN-SPIKE GRENADIER", "WRYMOUTH", "SKATE UNID. EGGS", "NORTHERN STONE CRAB", "OCTOPUS", "HALIBUT(ATLANTIC)", "SNAKE BLENNY", "NORTHERN HAGFISH", "HOOKEAR SCULPIN,ATL.", "DAUBED SHANNY", "SEASNAIL,GELATINOUS", "SEA CUCUMBER (UNIDENTIFIED)", "GREENLAND COD", "HERRING(ATLANTIC)", "BROWN SEAWEEDS", "MAILED SCULPIN", "SEA RAVEN", "ATLANTIC SPINY LUMPSUCKER", "HYAS COARCTATUS", "TOAD CRAB", "FOURBEARD ROCKLING", "WINTER SKATE", "GRUBBY OR LITTLE SCULPIN", "SEASNAIL,DUSKY", "CANCER SP.", "BRITTLE STAR", "EELPOUTS (NS)", "STRIPED ATLANTIC WOLFFISH", "SEA TADPOLE", "WINTER FLOUNDER", "RAINBOW SMELT", "WOLF EELPOUT", "STOUT EELBLENNY", "EELPOUT,NEWFOUNDLAND", "WHITE HAKE", "SHRIMPS", "ASTROTECTEN DUPLICATUS", "HEART URCHIN", "SEA PEN", "CLAMS (NS)", "SEMIROSSIA TENERA", "ARCTIC COD", "OCEAN POUT(COMMON)", "SPOTTED WOLFFISH")
#aggregate(s[var], by = s["year"], sum)


# Export to Excel:
excel(s)