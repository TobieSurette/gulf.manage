# Load raw data:
load(locate(file = "nss.gse"))
x <- ns2020
rm(ns2020)

# Perform corrections:


# Write to gulf.data:

# Re-format for Oracle input:
y <- gsd2card(gse2gsd(x))


