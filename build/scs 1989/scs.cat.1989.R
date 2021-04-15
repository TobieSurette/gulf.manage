library(gulf.data)

# Load data:
x <- read.csv(locate(file = c("1989", "cat", "csv")), header = TRUE)

# Move to comments:
ix <- grep("blackbook", x$species.logbook)
ix <- union(ix, grep("logbook", x$species.logbook))
ix <- union(ix, grep("llisible", x$species.logbook))
x$comment[ix] <- paste0(x$comment[ix], "; ", x$species.logbook[ix])
x$comment <- gsub("^; ", "", x$comment)
x$species.logbook[ix] <- ""

x$species.logbook <- spelling(x$species.logbook)


sort(lexicon(x$species.logbook))

# French character substitutions:
x$species.logbook <- gsub("\x82", "e", x$species.logbook)
x$species.logbook <- gsub("<82>", "e", x$species.logbook)
x$species.logbook <- gsub("\x8a", "e", x$species.logbook) 
x$species.logbook <- gsub("_ufs", "oeufs", x$species.logbook)

# Parentheses:
x$species.logbook <- gsub("\\([a-z 0-9?]+\\)", "", x$species.logbook)

# Remove pluralizations:
x$species.name <- gsub("s$", "", x$species.name)
x$species.name <- gsub("s ", " ", x$species.name)

# Spelling mistakes:
x$species.logbook <- spelling(x$species.logbook)
x$species.name <- gsub("hya", "hyas", x$species.name) 

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

