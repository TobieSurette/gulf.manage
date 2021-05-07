library(gulf.data)

year <- 1987  # Survey year.

# Extract sampler and coordinate data from biological data:
files <- locate(file = paste0("GCR", substr(year, 3,4)))
b <- read.scsbio(files, drop = FALSE, verbose = TRUE)

# Collapse to individual tows:
keep <- c("date", "zone", "tow.number", "latitude.start", "longitude.start", "depth", "samplers")
b <- b[, keep]
b <- unique(b)

# Field names:
names(b) <- gsub("latitude", "loran.x", names(b))
names(b) <- gsub("longitude", "loran.y", names(b))
names(b) <- gsub("samplers", "sampler", names(b))

# Fix sampler field:
b$sampler <- gsub("WADE", "WADE LANDSBURG", b$sampler)
b$sampler <- gsub("DEY", "PIERRE", b$sampler) 
b$sampler <- gsub("PIERRE/PIERRE", "PIERRE MALLET/PIERRE DEGRACE", b$sampler) 
b$sampler <- sampler(b$sampler, project = "scs")
b$sampler <- gsub("^, ", "", b$sampler)

# Fix duplicate entry:
ix <- which(b$date == "1987-08-10" & b$tow.number == 2 & b$sampler == "")
b <- b[-ix, ]

# Write data to 'gulf.data':
path <- paste0(unlist(strsplit(getwd(), "gulf"))[1], "gulf.data/inst/extdata/")
write.csv(b, file = paste0(path, paste0("scs.set.", year, ".csv")), row.names = FALSE)
