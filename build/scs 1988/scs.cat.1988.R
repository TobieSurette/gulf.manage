library(gulf.data)

# Find "new" files:
files <- locate(file = "^scs[0-9]+C")


files <- locate(file = c("1988", "cat", "txt"))

read.table(files, header = TRUE, sep = "\t")

x <- readLines(files)

vars <- unlist(strsplit(x[[1]], "\t"))

x <- strsplit(x[-1], "\t")
for (i in 1:length(vars)){
   
}
