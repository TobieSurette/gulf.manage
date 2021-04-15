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
x$species.logbook <- gsub("\x82", "e", x$species.logbook)
x$species.logbook <- gsub("<82>", "e", x$species.logbook)
x$species.logbook <- gsub("\x8a", "e", x$species.logbook) 
x$species.logbook <- gsub("_ufs", "oeufs", x$species.logbook) 

# Parentheses:
x$species.logbook <- gsub("\\([a-z 0-9?]+\\)", "", x$species.logbook)

# Move to comments:
ix <- intersect(grep("blackbook", x$species.logbook), which(x$comment == ""))
x$comment[ix] <- x$species.logbook[ix]
x$species.logbook[ix] <- ""
ix <- intersect(grep("blackbook", x$species.logbook), which(x$comment != ""))
x$comment[ix] <- paste0(x$comment[ix], x$species.logbook[ix])

# Remove pluralizations:
x$species.logbook <- gsub("s$", "", x$species.logbook)
x$species.logbook <- gsub("s ", " ", x$species.logbook)

# Spelling mistakes:
x$species.logbook <- gsub("hya", "hyas", x$species.logbook) 
x$species.logbook <- gsub("yellow tail", "yellowtail", x$species.logbook) 
x$species.logbook <- gsub("tettard", "tetard", x$species.logbook) 
x$species.logbook <- gsub("artica", "arctica", x$species.logbook) 
x$species.logbook <- gsub("holoturie", "holothurie", x$species.logbook) 
x$species.logbook[grep("laminaire", x$species.logbook) ] <- "laminaria"

# Keyword substitutions:
x$species.logbook[intersect(rep("etoile", x$species.logbook), rep(c("grosse", "mer", "branche", "filament", "fine", "carr", "brain", "patte", "non", "speci", "tentacule"), x$species.logbook, and = FALSE))] <- "etoile"
x$species.logbook[intersect(rep("eponge", x$species.logbook), rep(c("grosse", "branche", "filament", "fine", "carr", "brain", "patte", "non", "speci", "tentacule"), x$species.logbook, and = FALSE))] <- "eponge"
x$species.logbook[intersect(rep("mollusque", x$species.logbook), rep(c("toute", "sorte"), x$species.logbook, and = FALSE))] <- "mollusque"
x$species.logbook[intersect(grep("moule", x$species.logbook), grep("coque", x$species.logbook))] <- "bivalve"
x$species.logbook[intersect(rep("poisson", x$species.logbook), rep(c("non", "sorte", "petit"), x$species.logbook, and = FALSE))] <- "poisson"
x$species.logbook[setdiff(grep("bigorneau", x$species.logbook), grep("oeuf", x$species.logbook))] <- "buccin"
x$species.logbook[intersect(grep("morue", x$species.logbook), grep("merluche", x$species.logbook))] <- "gadiformes"

# Remove unknown species:
x$species.logbook[grep("salastere", x$species.logbook)] <- ""
x$species.logbook[grep("llisible", x$species.logbook)] <- ""               

# Standardize species names:
x$species.logbook[grep("arctica", x$species.logbook)] <- "arctica islandica"
x$species.logbook[rep(c("dollar", "sable"), x$species.logbook)] <- "sand dollar"
x$species.logbook[x$species.logbook == "crapaud"] <- "crapaud de mer"
x$species.logbook[rep(c("concombre", "mer"), x$species.logbook)] <- "concombre"
x$species.logbook[grep("corne", x$species.logbook)] <- "basketstar"
x$species.logbook[rep(c("morue", "pilote"), x$species.logbook)] <- "morue de roche"
x$species.logbook[grep("tetard", x$species.logbook)] <- "seasnail"
x$species.logbook[grep("terassier", x$species.logbook)] <- "cunner"

# Fix spacing issues:
x$species.logbook <- tolower(deblank(x$species.logbook))

# Empty shells:
x$species.logbook[intersect(rep("vide", x$species.logbook), rep(c("moule", "clam"), x$species.logbook, and = FALSE))] <- "empty shell"

# Add date field:
x <- cbind(data.frame(date = as.character(date(x))), x[setdiff(names(x), remove)])

