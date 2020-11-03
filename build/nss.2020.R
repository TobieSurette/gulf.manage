# Load raw data:
load(locate("nss.ese.2020"))
x <- ns2020
rm(ns2020)
x$set <- x$set[x$set$SETNO != 111, ] # Remove blank tow.

# Perform corrections:


# Write tables to gulf.data:

# Re-format for Oracle export:
y <- ese2gsd(x, survey = "nss")


