library(gulf.data)     
library(gulf.spatial) 

# Snow crab data:
years <- 1989:2022
strata <- c(415:417, 424) # Ca vient du September survey, c'est comme des blocks dans le NS survey.

# Read tow data:
s <- read.scsset(year = years, survey = "regular", valid = 1)
s$longitude <- lon(s)
s$latitude  <- lat(s)
s$stratum <- stratum(s$longitude, s$latitude) # Determine stratum from coordinates.
tmp <- attributes(s)
s <- s[which(s$stratum %in% strata), ]   # # Eliminate irrelevant tows: Le 'which' elimine les NA qui sont agacants.
s <- s[, c("date", "tow.id", "tow.number", "swept.area", "longitude", "latitude", "stratum")]
attributes(s) <- c(attributes(s),  tmp[setdiff(names(tmp), names(attributes(s)))])
by.catch <- s[year(s) %in% 1989:2020, ]

# Prepare snow crab data:
b <- read.scsbio(year = years, survey = "regular")
b$tow.id <- tow.id(b)  
ix <- match(b[key(s)], s[key(s)])
b <- b[!is.na(ix), ]

vars <- c("M", "F", "FI", "MI", "MM", "FM", "COM")

# Import crab counts:
import(s, fill = 0) <- summary(b, category = vars)
str <- names(s)
str[str %in% vars] <- category(vars)
names(s) <- str

# Import crab weights:
import(s, fill = 0) <- summary(b, category = vars, weight = TRUE)
s[vars] <- round(s[vars]) / 1000
str <- names(s)
str[str %in% vars] <- paste0(category(vars), ".kg")
names(s) <- str

# Load snow crab survey by-catch data:
c <- read.scscat(year = 2018:2020, survey = "regular")
c$tow.id <- tow.id(c)                # Ensure 'tow.id' is defined.
c$species.name <- species(c$species) # Add species names.
c <- c[!is.na(c$species.name), ]

# Loop over different species:
species <- unique(c$species.name)
for (i in 1:length(species)){
  tmp <- c[c$species.name == species[i], ]  # Data subset.
  by.catch[, species[i]] <- 0  # Initialize.
  tmp <- tmp[which(!duplicated(tmp[key(by.catch)])), ]
  ix <- match(by.catch[key(by.catch)], tmp[key(by.catch)])
  by.catch[!is.na(ix), species[i]] <- tmp$weight.caught[ix[!is.na(ix)]]
} 

# Length-frequency exports for crab:
lf.mature.males <- s[, c("date", "tow.id", "tow.number", "swept.area", "longitude", "latitude", "stratum")] 
import(lf.mature.males, fill = 0) <- freq(b[which((b$sex == 1) & is.mature(b)), ], by = key(s))
lf.immature.males <- s[, c("date", "tow.id", "tow.number", "swept.area", "longitude", "latitude", "stratum")] 
import(lf.immature.males, fill = 0) <- freq(b[which((b$sex == 1) & !is.mature(b)), ], by = key(s))
lf.mature.females <- s[, c("date", "tow.id", "tow.number", "swept.area", "longitude", "latitude", "stratum")] 
import(lf.mature.females, fill = 0) <- freq(b[which((b$sex == 2) & is.mature(b)), ], by = key(s))
lf.immature.females <- s[, c("date", "tow.id", "tow.number", "swept.area", "longitude", "latitude", "stratum")] 
import(lf.immature.females, fill = 0) <- freq(b[which((b$sex == 2) & !is.mature(b)), ], by = key(s))

# Length-frequency exports for target by-catch species:
z <- read.scslen(2010:2020, species = c(10, 30, 31))
z$length.unit[which(year(z) %in% c(2011:2012))] <- "cm"
z$length[z$length.unit == "mm"] <- z$length[z$length.unit == "mm"] / 10
z$length.unit <- "cm"
z$length <- round(z$length)

lf.cod <- s[year(s) >= 2010, c("date", "tow.id", "tow.number", "swept.area", "longitude", "latitude", "stratum")] 
import(lf.cod, fill = 0) <- freq(z[z$species == 10, ], by = key(s))
lf.halibut <- s[year(s) >= 2010, c("date", "tow.id", "tow.number", "swept.area", "longitude", "latitude", "stratum")] 
import(lf.halibut, fill = 0) <- freq(z[z$species == 30, ], by = key(s))
lf.turbot <- s[year(s) >= 2010, c("date", "tow.id", "tow.number", "swept.area", "longitude", "latitude", "stratum")] 
import(lf.turbot, fill = 0) <- freq(z[z$species == 31, ], by = key(s))

# Export data to excel:
excel(s)
excel(by.catch)
excel(lf.mature.males)
excel(lf.immature.males)
excel(lf.mature.females)
excel(lf.immature.females)
excel(lf.cod)
excel(lf.halibut)
excel(lf.turbot)