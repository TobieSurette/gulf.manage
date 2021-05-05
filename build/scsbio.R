library(gulf.data)

year <- 1990  # Survey year.

# Extract sampler and coordinate data from biological data:
files <- locate(file = paste0("GCR", substr(year, 3,4)))
b <- read.scsbio(files, drop = FALSE)

b <- b[, -grep("tude", names(b))]
b <- b[, -grep("position", names(b))]
b <- b[, -grep("samplers", names(b))]
b <- b[, -grep("maturity", names(b))]
b <- b[, -grep("depth", names(b))]
b <- b[, -grep("durometer", names(b))]
b <- compress(b)

# Write data to 'gulf.data':
path <- paste0(unlist(strsplit(getwd(), "gulf"))[1], "gulf.data/inst/extdata/")
write.csv(b, file = paste0(path, paste0("scs.bio.", year, ".csv")), row.names = FALSE)
