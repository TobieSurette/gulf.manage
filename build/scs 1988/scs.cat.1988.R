library(gulf.data)

# Function to find multiple keywords at a time:
rep <- function(x, y, and = TRUE){
   if (and){
      ix <- 1:length(y) 
      fun <- intersect
   }else{
      ix <- NULL
      fun = union
   }
   for (i in 1:length(x)) ix <- fun(ix, grep(x[i], y))
   return(ix)
}

# Find "new" files:
files <- locate(file = "^scs[0-9]+C")

# Load data:
files <- locate(file = c("1988", "cat", "csv"))
x <- read.csv(files, header = TRUE)

# French characters:
x$species.name <- gsub("\x82", "e", x$species.name)
x$species.name <- gsub("<82>", "e", x$species.name)
x$species.name <- gsub("\x8a", "e", x$species.name) 
x$species.name <- gsub("_ufs", "oeufs", x$species.name) 

# Parentheses:
x$species.name <- gsub("[(]", " (", x$species.name)

# Remove pluralizations:
x$species.name <- gsub("s$", "", x$species.name)
x$species.name <- gsub("s ", " ", x$species.name)

# Spelling mistakes:
x$species.name <- gsub("hya", "hyas", x$species.name) 
x$species.name <- gsub("yellow tail", "yellowtail", x$species.name) 
x$species.name <- gsub("tettard", "tetard", x$species.name) 
x$species.name <- gsub("artica", "arctica", x$species.name) 
x$species.name <- gsub("holoturie", "holothurie", x$species.name) 

# Keyword substitutions:
x$species.name[intersect(rep("etoile", x$species.name), rep(c("branche", "brain", "patte", "non", "speci", "tentacule"), x$species.name, and = FALSE))] <- "etoile"
x$species.name[intersect(rep("mollusque", x$species.name), rep(c("toute", "sorte"), x$species.name, and = FALSE))] <- "mollusque"
x$species.name[intersect(grep("moule", x$species.name), grep("coque", x$species.name))] <- "bivalve"
x$species.name[intersect(rep("poisson", x$species.name), rep(c("non", "sorte", "petit"), x$species.name, and = FALSE))] <- "poisson"
x$species.name[setdiff(grep("bigorneau", x$species.name), grep("oeuf", x$species.name))] <- "buccin"
x$species.name[intersect(grep("morue", x$species.name), grep("merluche", x$species.name))] <- "Gadiformes"
x$species.name[x$species.name == "clam (1 pleine)"] <- "clam"

# Move to comments:
ix <- intersect(grep("blackbook", x$species.name), which(x$comment == ""))
x$comment[ix] <- x$species.name[ix]
x$species.name[ix] <- ""
ix <- intersect(grep("blackbook", x$species.name), which(x$comment != ""))
x$comment[ix] <- paste0(x$comment[ix], x$species.name[ix])

# Fix spacing issues:
x$species.name <- deblank(x$species.name)

# Remove things in parentheses:
x$species.name <- gsub("\\([a-z]+\\)", "", x$species.name)

x[grep("vide", x$species.name), ]

x$species.name[grep("etoile", x$species.name)] 
x$species.name[grep("eponge", x$species.name)] 

# Remove redundant species names:
x <- x[setdiff(names(x), c("english", "latin", "french"))]
names(x) <- tolower(gsub("species[.]1", "species.name", tolower(names(x))))

x$species.name <- tolower(x$species.name)

x <- read.csv(file = files, sep = ",", header = TRUE, fileEncoding = "utf-8", stringsAsFactors = FALSE)

"Illisible"
"laminaire??"
"_ufs"
"??"
"artica"
"sands"

grep("black", x$species.name)

grep("\x82toile de mer non sp\x82cifi\x82", x$species.name)


"holoturies" "comcombres"