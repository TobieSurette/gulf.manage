# Program to format and build hyas sp. data measured on snow crab survey:

#Define survey year:
year <- 2022

# Read hyas data:
x <- read.csv(paste0("build/scs.", year, "/data/scs.len.hyas.", year, ".csv"))
names(x) <- tolower(names(x))

# Remove irrelevant variables:
mvars <- c(paste0("l", 1:5), paste0("r", 1:5)) # Missing leg variables.
x$missing.legs <- apply(x[mvars], 1, paste, collapse = "")

vars <- c('gpnumber', 'date', 'timestamp', 'trawl.number', 'araneus.vs.coarctatus', 'samplers', 'crab.number', 'carapace.width', 'chela.height', 
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

# Output to gulf data package:
file <- paste0(gsub("gulf.manage", "gulf.data", getwd(), fixed = TRUE), "/inst/extdata/scs.len.hyas.", year, ".csv")
write.csv(x, file = file,  row.names = FALSE)

