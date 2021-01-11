library(gulf.data)

species <- 10
years <- 2020
variable <- "number.caught"
x <- read.scsset(years, valid = 1, survey = "regular")
y <- read.scscat(years, survey = "regular", species = species)
y$tow.id <- tow.id(y)
y <- aggregate(y[variable], by = y[key(x)], sum)
import(x, var = variable, fill = 0) <- y
x$swept.area
x$longitude <- lon(x)
x$latitude  <- lat(x)
x$species <- species[1]
x$name.en <- species(species[1])
x$name.latin <- species(species[1], lang = "latin")

vars <- c("date", "longitude", "latitude", "swept.area", "species", "name.en", "name.latin", variable)
x[vars]


