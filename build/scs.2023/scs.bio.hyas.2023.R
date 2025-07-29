# Program to format and build hyas sp. data measured on snow crab survey:

#Define survey year:
year <- 2023
source(paste0(gsub("[.]manage", ".data", getwd()), "/R/maturity.R"))

# Read hyas data:
x <- read.csv(paste0("build/scs.", year, "/data/scs.bio.hyas.", year, ".csv"))
names(x) <- tolower(names(x))

# Remove irrelevant variables:
mvars <- c(paste0("l", 1:5), paste0("r", 1:5)) # Missing leg variables.
x$missing.legs <- apply(x[mvars], 1, paste, collapse = "")
x$abdomen.width <- NA

vars <- c('gpnumber', 'date', 'timestamp', 'trawl.number', 'araneus.vs.coarctatus', 'samplers', 'crab.number', 'carapace.width', 'chela.height', 'abdomen.width',
          'sex', 'shell.condition', 'missing.legs', 'comment')
x <- x[vars]

# Rename data fields:
names(x) <- gsub('araneus.vs.coarctatus', 'species', names(x))
names(x) <- gsub('gpnumber', 'tow.id', names(x))
names(x) <- gsub('trawl.number', 'tow.number', names(x))
names(x) <- gsub('samplers', 'sampler', names(x))
names(x) <- gsub('timestamp', 'time', names(x))

# Fix data fields:
x <- x[x$species != "*", ]
x$species <- as.numeric(x$species)
x$time <- substr(x$time, 12, 19)

# Extract secondary sexual characters:
ix <- which(x$comment != "")
x$abdomen.width[ix] <- as.numeric(gsub("[ a-zA-Z\n]", "", x$comment[ix]))
x$sex <- as.numeric(x$sex)
x$maturity <- ""
x$maturity[grep("mm", x$comment)] <- "immature"
x$maturity[grep("mat", x$comment)] <- "mature"

# Spot corrections:
x$tow.id[which(x$tow.id == "GP044FR180.74")] <- "GP044FR1"
x$chela.height[which(x$carapace.width >= 35 & x$carapace.width <= 45 & x$chela.height >= 25)] <- NA
x$carapace.width[which(x$tow.id == "GP173F" & x$crab.number == 1)] <- 82.26

# Fix mix-ups for snow crab and hyas biological data:
x$tow.id[x$tow.id == "GP154F"] <- "GP170F"
x$tow.id[x$tow.id == "GP170FR2"] <- "GP154FR2"
x$tow.id[x$tow.id == "GP287F"] <- "GPXXX"
x$tow.id[x$tow.id == "GP290F"] <- "GP287F"
x$tow.id[x$tow.id == "GPXXX"]  <- "GP290F"
x$tow.id[x$tow.id == "GP006F"] <- "GPXXX"
x$tow.id[x$tow.id == "GP004F"] <- "GP006F"
x$tow.id[x$tow.id == "GPXXX"]  <- "GP004F"

# Output to gulf data package:
file <- paste0(gsub("gulf.manage", "gulf.data", getwd(), fixed = TRUE), "/inst/extdata/scs.bio.hyas.", year, ".csv")
write.csv(x, file = file,  row.names = FALSE)

