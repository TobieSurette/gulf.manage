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
files <- locate(file = c("1989", "cat", "csv"))
x <- read.csv(files, header = TRUE)

# French characters:
x$species.logbook <- gsub("\x82", "e", x$species.logbook)
x$species.logbook <- gsub("<82>", "e", x$species.logbook)
x$species.logbook <- gsub("\x8a", "e", x$species.logbook) 
x$species.logbook <- gsub("\xe9", "e", x$species.logbook)  
x$species.logbook <- gsub("\xc9", "e", x$species.logbook)  
x$species.logbook <- gsub("\x9c", "oe", x$species.logbook)  
x$species.logbook <- gsub("\xe8", "e", x$species.logbook)   
x$species.logbook <- gsub("[_œ]ufs", "oeufs", x$species.logbook) 
x$species.logbook <- gsub("[éè]", "e", x$species.logbook)

# Parentheses:
x$species.logbook <- gsub("\\([a-z 0-9?]+\\)", "", x$species.logbook)

# Move to comments:
ix <- intersect(grep("blackbook", x$species.logbook), which(x$comment == ""))
x$comment[ix] <- x$species.logbook[ix]
x$species.logbook[ix] <- ""
ix <- intersect(grep("blackbook", x$species.logbook), which(x$comment != ""))
x$comment[ix] <- paste0(x$comment[ix], x$species.logbook[ix])
ix <- intersect(grep("logbook", x$species.logbook), which(x$comment == ""))
x$comment[ix] <- x$species.logbook[ix]
x$species.logbook[ix] <- ""
ix <- intersect(grep("logbook", x$species.logbook), which(x$comment != ""))
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
x$species.logbook <- gsub("l[a]mp[o]nie", "lompenie", x$species.logbook) 
x$species.logbook <- gsub("pa[l]+ourde", "palourde", x$species.logbook)  
x$species.logbook <- gsub("ar[e]*a[n]*e[n]*u[s]*", "araneus", x$species.logbook)   
x$species.logbook <- gsub("areanu[s]*", "araneus", x$species.logbook)   
x$species.logbook <- gsub("quohag", "quahog", x$species.logbook)   
x$species.logbook <- gsub("aligatore", "alligator", x$species.logbook)   
x$species.logbook <- gsub("dollar[ds]*", "dollar", x$species.logbook) 
x$species.logbook[grep("co[m]co[nm]bre", x$species.logbook) ] <- "concombre"

# Save species logbook names:
x$species.name.logbook <- x$species.logbook

# Keyword substitutions:
x$species.logbook[intersect(rep("[eé]toile", x$species.logbook), rep(c("grosse", "mer", "branche", "filament", "fine", "carr", "brain", "patte", "non", "speci", "tentacule"), x$species.logbook, and = FALSE))] <- "etoile"
x$species.logbook[intersect(rep("[eé]ponge", x$species.logbook), rep(c("grosse", "branche", "filament", "fine", "carr", "brain", "patte", "non", "speci", "tentacule"), x$species.logbook, and = FALSE))] <- "eponge"
x$species.logbook[intersect(rep("mollusque", x$species.logbook), rep(c("toute", "sorte"), x$species.logbook, and = FALSE))] <- "mollusque"
x$species.logbook[intersect(grep("moule", x$species.logbook), grep("coque", x$species.logbook))] <- "bivalve"
x$species.logbook[intersect(rep("poisson", x$species.logbook), rep(c("non", "sorte", "petit"), x$species.logbook, and = FALSE))] <- "poisson"
x$species.logbook[setdiff(grep("bigorneau", x$species.logbook), grep("oeuf", x$species.logbook))] <- "whelk eggs"
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
x$species.logbook[grep("coquille", x$species.logbook)] <- "empty shell"

# Fish species:
x$species.logbook[x$species.logbook == "morue"] <- "cod"
x$species.logbook[x$species.logbook == "hareng"] <- "herring"
x$species.logbook[x$species.logbook == "poisson rouge"] <- "redfish"
x$species.logbook[x$species.logbook == "poisson"] <- "fish"
x$species.logbook[x$species.logbook == "plie canadienne"] <- "American plaice"
x$species.logbook[x$species.logbook == "plie rouge"] <- "winter flounder"
x$species.logbook[x$species.logbook == "eperlan"] <- "rainbow smelt"
x$species.logbook[x$species.logbook == "barbue"] <- "white hake"
x$species.logbook[x$species.logbook == "crapaud de mer"] <- "sculpin"
x$species.logbook[x$species.logbook == "chaboisseau"] <- "sculpin" 
x$species.logbook[x$species.logbook == "poule d'eau"] <- "Atlantic lumpfish"
x$species.logbook[x$species.logbook == "poule de mer"] <- "Atlantic lumpfish"
x$species.logbook[x$species.logbook == "loup atlantique"] <- "Atlantic wolffish"
x$species.logbook[x$species.logbook == "morue de roche"] <- "Greenland cod"
x$species.logbook[x$species.logbook == "loche"] <- "monkfish"
x$species.logbook[x$species.logbook == "plie grise"] <- "witch flounder"
x$species.logbook[x$species.logbook == "plie"] <- "plaice"
x$species.logbook[x$species.logbook == "capelan"] <- "capelin"
x$species.logbook[x$species.logbook == "raie"] <- "skate" 
x$species.logbook[x$species.logbook == "poisson alligator"] <- "alligatorfish" 
x$species.logbook[x$species.logbook == "chaboisseau bronze"] <- "shorthorn sculpin"
x$species.logbook[x$species.logbook == "chien de mer"] <- "dogfish"
x$species.logbook[x$species.logbook == "lompenie"] <- "eelblenny"

# Crustaceans:
x$species.logbook[x$species.logbook == "bernard l'hermite"] <- "hermit crab"
x$species.logbook[grep("homard", x$species.logbook)] <- "American lobster"
x$species.logbook[x$species.logbook == "crevette"] <- "shrimp"

# Mollusks:
x$species.logbook[x$species.logbook == "limace"] <- "sea slug"
x$species.logbook[x$species.logbook == "mollusque"] <- "mollusk"
x$species.logbook[x$species.logbook == "mollusque vide"] <- "empty shells" 
x$species.logbook[x$species.logbook == "petoncle"] <- "scallop"
x$species.logbook[x$species.logbook == "petoncle geante"] <- "giant scallop"
x$species.logbook[x$species.logbook == "petoncle d'islande"] <- "Iceland scallop"
x$species.logbook[x$species.logbook == "buccin"] <- "whelk"
x$species.logbook[x$species.logbook == "oeuf de buccin"] <- "whelk eggs" 
x$species.logbook[x$species.logbook == "pieuvre"] <- "octopus"
x$species.logbook[x$species.logbook == "calmar"] <- "squid"
"palourde"

# Miscellaneous invertebrates:
"algue"
"anemone" 
x$species.logbook[x$species.logbook == "etoile"] <- "starfish"
x$species.logbook[x$species.logbook == "holothurie"] <- "sea cucumber"
x$species.logbook[x$species.logbook == "concombre"] <- "sea cucumber"
x$species.logbook[x$species.logbook == "oursin"] <- "sea urchin"
x$species.logbook[x$species.logbook == "ophiure"] <- "brittle star"
x$species.logbook[x$species.logbook == "eponge"] <- "sea sponge"
x$species.logbook[x$species.logbook == "ophiure et eponge"] <- "eponge et ophiure"
x$species.logbook[x$species.logbook == "eponge et ophiure"] <- "sponges and brittlestars"

# Initialize species coding:
x$species <- NA

# Fish species coding:
x$species[x$species.logbook == "cod"]                 <- 10 
x$species[x$species.logbook == "gadiformes"]          <- 18 # "Gadiformes"
x$species[x$species.logbook == "greenland cod"]       <- 118
x$species[grep("redfish", x$species.logbook)]         <- species("redfish")[1]
x$species[grep("American plaice", x$species.logbook)] <- species("American plaice")[1]
x$species[grep("winter flounder", x$species.logbook)] <- species("winter flounder")[1]
x$species[grep("witch flounder", x$species.logbook)]  <- species("witch flounder")[1]
x$species[grep("yellowtail", x$species.logbook)]      <- species("yellowtail")[1] 
x$species[grep("white hake", x$species.logbook)]      <- species("white hake")[1] 
x$species[grep("rainbow smelt", x$species.logbook)]   <- species("rainbow smelt")[1] 
x$species[grep("lumpfish", x$species.logbook)]        <- species("lumpfish")[1]  
x$species[grep("cunner", x$species.logbook)]          <- species("cunner")[1]  
x$species[grep("herring", x$species.logbook)]         <- species("herring")[1]  
x$species[grep("monkfish", x$species.logbook)]        <- species("monkfish")[1]  
x$species[x$species.logbook == "skate"]               <- 211 # Skates unsp.
x$species[x$species.logbook == "plaice"]              <- 346 # Flatfish unsp. 
x$species[x$species.logbook == "fish"]                <- 90  # Unsp. fish"
x$species[x$species.logbook == "sculpin"]             <- 311 # Sculpin unsp."
x$species[x$species.logbook == "plaice"]              <- 346 # Flatfish unsp. 
x$species[x$species.logbook == "capelin"]             <- 64

x$species[x$species.logbook == "American lobster"]       <- 2550
x$species[x$species.logbook == "rock crab"]              <- 2513
x$species[grep("hermit crab", x$species.logbook)]        <- 2560
x$species[x$species.logbook == "hyas"]                   <- 2520 
x$species[x$species.logbook == "shrimp"]                 <- 2100 

x$species[x$species.logbook == "sea slug"]               <- 4400
x$species[x$species.logbook == "seasnail"]               <- 4200 # "Snails and slugs" 
x$species[x$species.logbook == "mollusk"]                <- 4000 # "Mollusca p."
x$species[x$species.logbook == "whelk"]                  <- 4210 
x$species[x$species.logbook == "whelk eggs"]             <- 1511 

x$species[x$species.logbook == "sea urchin"]             <- 6400 
x$species[x$species.logbook == "sand dollar"]            <- 6500 
x$species[x$species.logbook == "sea cucumber"]           <- 6600 

x$species[x$species.logbook == "bivalve"]                <- 4300 # "Bivalvia c." 
x$species[x$species.logbook == "empty shells"]           <- 4348
x$species[x$species.logbook == "arctica islandica"]      <- 4304 # Ocean quahog
x$species[x$species.logbook == "clam"]                   <- 4310 # "Clams unsp."
x$species[x$species.logbook == "scallop"]                <- 4320 # scallop"
x$species[x$species.logbook == "giant scallop"]          <- 4321 # Giant scallop"
x$species[x$species.logbook == "Iceland scallop"]        <- 4322 # Iceland scallop"

x$species[x$species.logbook == "brittle star"]             <- 6200 
x$species[x$species.logbook == "basketstar"]               <- 6300 
x$species[x$species.logbook == "starfish"]                 <- 6100 # "Asteroidea s.c."
x$species[x$species.logbook == "sponges and brittlestars"] <- 1701 # "Marine invertebrates unsp."
x$species[x$species.logbook == "sea sponge"]               <- 8600
x$species[x$species.logbook == "laminaria"]                <- 9321

x$species.logbook[is.na(x$species)]

# Re-order fields:
x$species.name <- x$species.logbook
remove <- c("year", "month", "day", "species.logbook")
x$date <- as.character(date(x))
x <- x[setdiff(names(x), remove)]
vars <- c("date", "tow.id", "tow.number", "species", "species.name", "species.name.logbook")
x <- x[c(vars, setdiff(names(x), c(vars, "comment")), "comment")]

# Delete lines with missing IDs:
ix <- which(is.na(x$species) & x$species.name.logbook == "")
x <- x[-ix, ]

# Fix tow.id format:
x$tow.id <- paste0("GP", gsub("S88", "", x$tow.id))

# Write data:
path <- paste0(unlist(strsplit(getwd(), "gulf"))[1], "gulf.data/inst/extdata/")
write.csv(x, file = paste0(path, "scs.cat.1988.csv"), row.names = FALSE)

