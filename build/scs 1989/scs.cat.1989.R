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
x <- readLines(locate(file = c("1989", "cat", "txt")))
fields <- unlist(strsplit(x[1], "\t")[[1]])
x <- x[-1]

y <- strsplit(x, "\t")
ix <- which(is.na(unlist(lapply(y, function(x) x[1]))))
y <- y[-ix]

# Parse data columns:
for (i in 1:length(fields)){
   tmp <- data.frame(unlist(lapply(y, function(x) x[i])), stringsAsFactors = FALSE)
   names(tmp) <- fields[i]
   if (i == 1) r <- tmp else r <- cbind(r, tmp)
}
x <- r

# French character substitutions:
x$species.logbook <- gsub("\x82", "e", x$species.logbook)
x$species.logbook <- gsub("<82>", "e", x$species.logbook)
x$species.logbook <- gsub("\x8a", "e", x$species.logbook) 
x$species.logbook <- gsub("_ufs", "oeufs", x$species.logbook)

# Remove parentheses:
x$species.logbook <- gsub("\\([a-z 0-9?]+\\)", "", x$species.logbook)

# Move to comments:
x$comment[is.na(x$comment)] <- ""
words <- c("blackbook", "llisible")
ix <- rep(words, x$species.logbook, and = FALSE)
x$species.logbook[ix] <- paste0(x$comment[ix], "; ", x$species.logbook[ix])
x$species.logbook <- gsub("^; ", "", x$species.logbook)
x$species.logbook[ix] <- ""

# Correct spelling:
x$species.logbook <- spelling(x$species.logbook)

# Remove pluralizations:
x$species.logbook <- gsub("s$", "", x$species.logbook)
x$species.logbook <- gsub("s ", " ", x$species.logbook)

# Spelling mistakes:
x$species.logbook <- spelling(x$species.logbook)
x$species.logbook <- gsub("hya", "hyas", x$species.logbook) 

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
x$species.logbook[intersect(rep("vide", x$species.logbook), rep(c("coquille", "shell", "moule", "clam"), x$species.logbook, and = FALSE))] <- "empty shells"
x$species.logbook[grep(c("coquille"), x$species.logbook)] <- "empty shells"

# Add date field:
remove <- c("year", "month", "day")
x <- cbind(data.frame(date = as.character(date(x))), x[setdiff(names(x), remove)])

