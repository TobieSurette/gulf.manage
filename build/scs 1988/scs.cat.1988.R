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

# Load data:
files <- locate(file = c("1988", "cat", "csv"))
x <- read.csv(files, header = TRUE)

# French characters:
x$species.name <- gsub("\x82", "e", x$species.name)
x$species.name <- gsub("<82>", "e", x$species.name)
x$species.name <- gsub("\x8a", "e", x$species.name) 
x$species.name <- gsub("_ufs", "oeufs", x$species.name) 

# Parentheses:
x$species.name <- gsub("\\([a-z 0-9?]+\\)", "", x$species.name)

# Move to comments:
ix <- intersect(grep("blackbook", x$species.name), which(x$comment == ""))
x$comment[ix] <- x$species.name[ix]
x$species.name[ix] <- ""
ix <- intersect(grep("blackbook", x$species.name), which(x$comment != ""))
x$comment[ix] <- paste0(x$comment[ix], x$species.name[ix])

# Remove pluralizations:
x$species.name <- gsub("s$", "", x$species.name)
x$species.name <- gsub("s ", " ", x$species.name)

# Spelling mistakes:
x$species.name <- gsub("hya", "hyas", x$species.name) 
x$species.name <- gsub("yellow tail", "yellowtail", x$species.name) 
x$species.name <- gsub("tettard", "tetard", x$species.name) 
x$species.name <- gsub("artica", "arctica", x$species.name) 
x$species.name <- gsub("holoturie", "holothurie", x$species.name) 
x$species.name[grep("laminaire", x$species.name) ] <- "laminaria"

# Keyword substitutions:
x$species.name[intersect(rep("etoile", x$species.name), rep(c("grosse", "mer", "branche", "filament", "fine", "carr", "brain", "patte", "non", "speci", "tentacule"), x$species.name, and = FALSE))] <- "etoile"
x$species.name[intersect(rep("eponge", x$species.name), rep(c("grosse", "branche", "filament", "fine", "carr", "brain", "patte", "non", "speci", "tentacule"), x$species.name, and = FALSE))] <- "eponge"
x$species.name[intersect(rep("mollusque", x$species.name), rep(c("toute", "sorte"), x$species.name, and = FALSE))] <- "mollusque"
x$species.name[intersect(grep("moule", x$species.name), grep("coque", x$species.name))] <- "bivalve"
x$species.name[intersect(rep("poisson", x$species.name), rep(c("non", "sorte", "petit"), x$species.name, and = FALSE))] <- "poisson"
x$species.name[setdiff(grep("bigorneau", x$species.name), grep("oeuf", x$species.name))] <- "buccin"
x$species.name[intersect(grep("morue", x$species.name), grep("merluche", x$species.name))] <- "gadiformes"

# Remove unknown species:
x$species.name[grep("salastere", x$species.name)] <- ""
x$species.name[grep("llisible", x$species.name)] <- ""               

# Standardize species names:
x$species.name[grep("arctica", x$species.name)] <- "arctica islandica"
x$species.name[rep(c("dollar", "sable"), x$species.name)] <- "sand dollar"
x$species.name[x$species.name == "crapaud"] <- "crapaud de mer"
x$species.name[rep(c("concombre", "mer"), x$species.name)] <- "concombre"
x$species.name[grep("corne", x$species.name)] <- "basketstar"
x$species.name[rep(c("morue", "pilote"), x$species.name)] <- "morue de roche"
x$species.name[grep("tetard", x$species.name)] <- "seasnail"
x$species.name[grep("terassier", x$species.name)] <- "cunner"

# Fix spacing issues:
x$species.name <- tolower(deblank(x$species.name))

# Empty shells:
x$species.name[intersect(rep("vide", x$species.name), rep(c("moule", "clam"), x$species.name, and = FALSE))] <- "empty shell"

# Add date field:
x <- cbind(data.frame(date = as.character(date(x))), x[setdiff(names(x), remove)])





