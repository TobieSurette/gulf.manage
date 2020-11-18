library(gulf.utils)

# Load raw data:
x <- readLines(locate(file = "species.foreign"), encoding = "latin1")
x <- x[-1]
x <- strsplit(x, ",")

v <- data.frame(code = unlist(lapply(x, function(x) x[3])),
                english = unlist(lapply(x, function(x) x[1])),
                french = unlist(lapply(x, function(x) x[2])),
                stacac = unlist(lapply(x, function(x) x[4])),
                nafo = unlist(lapply(x, function(x) x[5])),
                stringsAsFactors = FALSE)
v$code <- as.numeric(v$code)
v$stacac <- as.numeric(v$stacac)
v$nafo <- as.numeric(v$nafo)

v$french <- tolower(v$french)
v$french = gsub("_ufs", "oeufs", v$french)  
v$french = gsub("t_te", "tête", v$french)  
v$french = gsub("gro[?]nland", "groenland", v$french)   
v$french = gsub("sp[?]cifi[?]", "spécifié", v$french)   
v$french = gsub("p?toncle", "pétoncle", v$french) 
v$french = gsub("p?lerin", "pélerin", v$french) 
v$french = gsub("chim_re", "chimère", v$french) 
v$french = gsub("rivi_re", "rivière", v$french) 
v$french = gsub("mara?che", "maraîche", v$french) 
v$french = gsub("crustac?s", "crustacés", v$french) 
v$french = gsub("app_t", "appât", v$french) 
v$french = gsub("ÿ", "é", v$french) 
v$french = gsub(" <90> ", " à ", v$french) 
v$french = gsub("<90>", "É", v$french) 

v$english <- tolower(v$english)

# Capitalization:
v$english <- paste0(toupper(substr(v$english,1,1)), substr(v$english,2,nchar(v$english)))
v$french <- paste0(toupper(substr(v$french,1,1)), tolower(substr(v$french,2,nchar(v$french))))

# Correct blanks:
v$english <- deblank(v$english)
v$french <- deblank(v$french)

# Write:
path <- gsub("gulf.manage", "gulf.data", getwd())
path <- paste0(path, "/inst/extdata")
if (file.exists(path)){
   file <- paste0(path, "/", "species.foreign", ".tab") 
   write.table(v, file = file, row.names = FALSE, sep = "\t", fileEncoding = "utf-8")
}
