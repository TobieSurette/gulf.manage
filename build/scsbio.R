library(gulf.data)

year <- 1988  # Survey year.

# Extract sampler and coordinate data from biological data:
files <- locate(file = paste0("GCR", substr(year, 3,4)))
b <- read.scsbio(files, drop = FALSE, verbose = TRUE)

# Process numerical codes:
b$eggs.remaining <- gsub("[*]", "", b$eggs.remaining)
b$comments <- gsub("[*]", "", b$comments)
b$comments <- gsub(" +", " ", b$comments)
names(b) <- gsub("comments", "comment", names(b))
b$durometer <- gsub("[*]", "", b$durometer)
b$durometer <- as.numeric(b$durometer)
b <- b[, -grep("tude", names(b))]
b <- b[, -grep("position", names(b))]
b <- b[, -grep("samplers", names(b))]
b <- b[, -grep("maturity", names(b))]
b <- b[, -grep("depth", names(b))]
b <- b[, -grep("zone", names(b))]
b <- compress(b)

# Write data to 'gulf.data':
path <- paste0(unlist(strsplit(getwd(), "gulf"))[1], "gulf.data/inst/extdata/")
write.csv(b, file = paste0(path, paste0("scs.bio.", year, ".csv")), row.names = FALSE)
