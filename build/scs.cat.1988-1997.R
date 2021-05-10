library(gulf.data)

year <- 1997  # Survey year

# Function to find multiple keywords at a ime:
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

# Load raw data:
files <- locate(file = c(year, "cat", "csv"))
x <- read.csv(files, header = TRUE)
names(x) <- tolower(names(x))
x <- x[setdiff(names(x), c("english", "latin", "french"))]
names(x) <- gsub("townumber", "tow.number", names(x))
names(x) <- gsub("towid", "tow.id", names(x))
names(x) <- gsub("numbercaught", "number.caught", names(x))
names(x) <- gsub("weightcaught", "weight.caught", names(x))
names(x) <- gsub("species[.]1", "species.logbook", names(x))

# French characters:
x$species.logbook <- gsub("\x82", "e", x$species.logbook)
x$species.logbook <- gsub("<82>", "e", x$species.logbook)
x$species.logbook <- gsub("\x8a", "e", x$species.logbook) 
x$species.logbook <- gsub("\x8e", "e", x$species.logbook)  
x$species.logbook <- gsub("\x8f", "e", x$species.logbook)  
x$species.logbook <- gsub("\xe9", "e", x$species.logbook)  
x$species.logbook <- gsub("\xc9", "e", x$species.logbook)  
x$species.logbook <- gsub("\x9c", "oe", x$species.logbook)  
x$species.logbook <- gsub("\xe8", "e", x$species.logbook)   
x$species.logbook <- gsub("[_œ]ufs", "oeufs", x$species.logbook) 
x$species.logbook <- gsub("[éè]", "e", x$species.logbook)

# Lowercase:
x$species.logbook <- tolower(x$species.logbook)
x$species.logbook <- gsub("american", "American", x$species.logbook)

# Create date field:
x$date <- as.character(date(x))

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
x$species.logbook <- gsub("a[l]+igator[e]*", "alligator", x$species.logbook)  
x$species.logbook <- gsub("a[n]+elid[e]*", "annelid", x$species.logbook) 
x$species.logbook <- gsub("artica", "arctica", x$species.logbook) 
x$species.logbook <- gsub("ar[e]*a[n]*e[n]*u[s]*", "araneus", x$species.logbook)   
x$species.logbook <- gsub("areanu[s]*", "araneus", x$species.logbook)   

x$species.logbook <- gsub("bar[l]*b[o]*u", "barbu", x$species.logbook) 
x$species.logbook <- gsub("blenni[e]*", "blenny", x$species.logbook) 
x$species.logbook <- gsub("bigorneaux", "bigorneau", x$species.logbook)  
x$species.logbook <- gsub("ca[n]+adie[n]+e", "canadienne", x$species.logbook) 
x$species.logbook <- gsub("caod", "cod", x$species.logbook) 
x$species.logbook <- gsub("cock[l]*e[r]*", "cockle", x$species.logbook) 
x$species.logbook[grep("co[m]co[nm]bre", x$species.logbook) ] <- "concombre"
x$species.logbook <- gsub("coarctatu$", "coarctatus", x$species.logbook) 

x$species.logbook <- gsub("dollar[ds]*", "dollar", x$species.logbook)
x$species.logbook <- gsub("etoi[l]+e", "etoile", x$species.logbook)  
x$species.logbook <- gsub("flound$", "flounder", x$species.logbook) 
x$species.logbook[x$species.logbook == "hahe"] <- "hake"
x$species.logbook <- gsub("hya[s]*", "hyas", x$species.logbook) 
x$species.logbook <- gsub("hy$", "hyas", x$species.logbook) 
x$species.logbook <- gsub("h[ae][r]+[ie]ng", "herring", x$species.logbook) 
x$species.logbook <- gsub("haddeck", "haddock", x$species.logbook) 
x$species.logbook <- gsub("h[eo]lot[h]*urie", "holothurie", x$species.logbook) 
x$species.logbook <- gsub("hermit[e]*[ ]+crab", "hermit crab", x$species.logbook)  

x$species.logbook <- gsub("gelatinou ", "gelatinous ", x$species.logbook)  
x$species.logbook[grep("laminaire", x$species.logbook) ] <- "laminaria"
x$species.logbook <- gsub("l[a]mp[eo]nie", "lompenie", x$species.logbook) 
x$species.logbook <- gsub("pa[l]+ourde", "palourde", x$species.logbook)  
x$species.logbook <- gsub("potato.", "potato", x$species.logbook) 
x$species.logbook <- gsub("pandalu$", "pandalus", x$species.logbook)  
x$species.logbook <- gsub("p[ao]lype", "polyp", x$species.logbook)
x$species.logbook <- gsub("^blie", "plie", x$species.logbook)

x$species.logbook[x$species.logbook == "pep"]   <- "pepin"
x$species.logbook[x$species.logbook == "ophiu"] <- "ophiure"  

x$species.logbook <- gsub("je[l]+y[ ]+fish", "jellyfish", x$species.logbook)
x$species.logbook <- gsub("quohag", "quahog", x$species.logbook)   
x$species.logbook <- gsub("sea[ ]rav[iae]*n", "sea raven", x$species.logbook) 
 
x$species.logbook <- gsub("s[ao]l[ao]ster[e]*", "solaster", x$species.logbook) 
x$species.logbook <- gsub(" tigle", " trigle", x$species.logbook) 
x$species.logbook <- gsub("te[t]+ard", "tetard", x$species.logbook)
x$species.logbook <- gsub("turbo.*", "turbot", x$species.logbook) 
x$species.logbook <- gsub("yellow tail", "yellowtail", x$species.logbook) 
x$species.logbook[x$species.logbook == "irish mos"] <- "irish moss" 

# Save species logbook names:
x$species.name.logbook <- x$species.logbook

# Keyword substitutions:
x$species.logbook[x$species.logbook == "eponges(video)"] <- "eponge"
x$species.logbook[intersect(grep("etoile", x$species.logbook), grep("violet", x$species.logbook))] <- "solaster"
x$species.logbook[intersect(grep("chaboisse", x$species.logbook), grep("18", x$species.logbook))] <- "longhorn sculpin"
x$species.logbook[intersect(grep("tetard", x$species.logbook), grep("sculpin", x$species.logbook))] <- "sculpin"
x$species.logbook[intersect(grep("skate", x$species.logbook), grep("thorny", x$species.logbook))] <- "thorny skate"
x$species.logbook[intersect(grep("basket", x$species.logbook), grep("star", x$species.logbook))]  <- "basketstar"
x$species.logbook[intersect(rep("[eé]toile", x$species.logbook), rep(c("grosse", "mer", "branche", "filament", "fine", "carr", "brain", "patte", "non", "speci", "tentacule"), x$species.logbook, and = FALSE))] <- "etoile"
x$species.logbook[intersect(rep("[eé]ponge", x$species.logbook), rep(c("feuille", "puant", "grosse", "branche", "pue", "filament", "fine", "carr", "brain", "patte", "non", "speci", "tentacule"), x$species.logbook, and = FALSE))] <- "eponge"
x$species.logbook[intersect(rep("mollusque", x$species.logbook), rep(c("toute", "sorte"), x$species.logbook, and = FALSE))] <- "mollusque"
x$species.logbook[intersect(grep("moule", x$species.logbook), grep("coque", x$species.logbook))] <- "bivalve"

x$species.logbook[intersect(grep("coque", x$species.logbook), grep("vide", x$species.logbook))]   <- "empty shells"
x$species.logbook[intersect(grep("arctica", x$species.logbook), grep("vide", x$species.logbook))] <- "empty shells"
x$species.logbook[intersect(grep("clam", x$species.logbook), grep("shell", x$species.logbook))]   <- "empty shells"

x$species.logbook[intersect(rep("poisson", x$species.logbook), rep(c("non", "sorte", "petit"), x$species.logbook, and = FALSE))] <- "poisson"
x$species.logbook[intersect(grep("bigorneau", x$species.logbook), grep("plusieur", x$species.logbook))] <- "whelk"
x$species.logbook[intersect(grep("bigorneau", x$species.logbook), grep("oeuf", x$species.logbook))] <- "whelk eggs"
x$species.logbook[intersect(grep("bigorneau", x$species.logbook), grep("oeuf", x$species.logbook))] <- "whelk eggs"
x$species.logbook[intersect(grep("large", x$species.logbook), grep("star", x$species.logbook))]  <- "starfish"
x$species.logbook[intersect(grep("long", x$species.logbook), grep("sponge", x$species.logbook))] <- "sponge"
x$species.logbook[x$species.logbook == "lumpsucker"]   <- "spiny lumpsucker"
x$species.logbook[intersect(grep("poule", x$species.logbook), grep("petite", x$species.logbook))]   <- "spiny lumpsucker"
x$species.logbook[intersect(grep("merluche", x$species.logbook), grep("morue", x$species.logbook))] <- "gadiformes"


x$species.logbook[grep("barbu", x$species.logbook)] <- "white hake"
x$species.logbook[grep("plaice", x$species.logbook)] <- "American plaice"

x$species.logbook[setdiff(union(grep("winkle", x$species.logbook), grep("whelk", x$species.logbook)), grep("egg", x$species.logbook))] <- "whelk"
x$species.logbook[rep("pieuvre", x$species.logbook)] <- "octopus"
x$species.logbook[rep(c("stimpson", "clam"), x$species.logbook)] <- "Stimpson's surf clam"

# Special cases:
x$species.logbook[x$species.logbook == "flounder(plie blanche)"] <- "American plaice"
x$species.logbook[x$species.logbook == "greysole(plie grise)"]   <- "witch flounder" 
x$species.logbook[grep("grey[ ]*sole", x$species.logbook)]       <- "witch flounder" 
x$species.logbook[rep(c("lompenie", "serpent"), x$species.logbook)] <- "slender eelblenny" 

# Remove terms in parentheses:
x$species.logbook <- gsub("\\([a-z '0-9?]+\\)", "", x$species.logbook)
x$species.logbook <- gsub("[?]", "", x$species.logbook)

# Remove unknown species:
x$species.logbook[grep("llisible", x$species.logbook)] <- ""               

# Standardize species names:
x$species.logbook[setdiff(grep("arctica", x$species.logbook), grep("coquille", x$species.logbook))] <- "arctica islandica"
x$species.logbook[rep(c("dollar", "sable"), x$species.logbook)] <- "sand dollar"
x$species.logbook[x$species.logbook == "crapaud"] <- "crapaud de mer"
x$species.logbook[rep(c("concombre", "mer"), x$species.logbook)] <- "concombre de mer"
x$species.logbook[x$species.logbook == "concombre"] <- "concombre de mer"
x$species.logbook[grep("corne", x$species.logbook)] <- "basketstar"
x$species.logbook[rep(c("morue", "pilote"), x$species.logbook)] <- "morue de roche"
x$species.logbook[grep("tetard", x$species.logbook)] <- "seasnail"
x$species.logbook[grep("terassier", x$species.logbook)] <- "cunner"
x$species.logbook <- gsub("crapeau", "crapaud", x$species.logbook)
x$species.logbook <- gsub("pelotte de mer", "sea mouse", x$species.logbook) 
x$species.logbook[rep(c("eponge", "main"), x$species.logbook)] <- "dead man's fingers"
x$species.logbook[rep(c("gosse", "mer"), x$species.logbook)] <- "sea potato"

# Fix spacing issues:
x$species.logbook <- deblank(x$species.logbook)

# Empty shells:
x$species.logbook[intersect(rep("vide", x$species.logbook), rep(c("moule", "clam"), x$species.logbook, and = FALSE))] <- "empty shell"
x$species.logbook[grep("coquille", x$species.logbook)] <- "empty shells"

# Fish species:
x$species.logbook[x$species.logbook == "morue"]                 <- "cod"
x$species.logbook[x$species.logbook %in% c("pepin", "small green cod", "rock cod", "morue de roche", "black cod")] <- "Greenland cod"

x$species.logbook[x$species.logbook == "hake"]                  <- "white hake"

x$species.logbook[x$species.logbook %in% c("plie blanche", "plie canadienne", "americain plaice", "plie americaine")] <- "American plaice"
x$species.logbook[x$species.logbook %in% c("yellowtail", "plie a queue jaune", "limande a queue jaune")] <- "yellowtail flounder"

x$species.logbook[x$species.logbook == "plie rouge"]            <- "winter flounder"
x$species.logbook[x$species.logbook == "plie grise"]            <- "witch flounder"
x$species.logbook[x$species.logbook == "plie"]                  <- "plaice"
x$species.logbook[x$species.logbook == "plie grise et blanche"] <- "flatfish"
x$species.logbook[x$species.logbook == "plie blanche et grise"] <- "flatfish"
x$species.logbook[x$species.logbook == "plie grise canadienne"] <- "flatfish"
x$species.logbook[x$species.logbook == "fletan du groenland"]   <- "turbot"
x$species.logbook[x$species.logbook == "small flounder"]        <- "flounder"

x$species.logbook[x$species.logbook %in% c("poisson rouge", "red fish", "sebaste")] <- "redfish"
x$species.logbook[which(x$date == "1994-08-24" & x$species.logbook == "red")]       <- "redfish"
x$species.logbook[x$species.logbook %in% c("poisson alligator", "alligator fish")]  <- "alligatorfish"

x$species.logbook[x$species.logbook == "crapet de mer"]         <- "crapaud de mer"
x$species.logbook[x$species.logbook == "goberge"]               <- "pollock"
x$species.logbook[x$species.logbook == "poisson"]               <- "fish"
x$species.logbook[x$species.logbook == "quelque poisson"]       <- "fish"

x$species.logbook[x$species.logbook == "capelan"]               <- "capelin"
x$species.logbook[x$species.logbook == "caplan"]                <- "capelin"
x$species.logbook[x$species.logbook == "truite de mer"]         <- "salmon" 
x$species.logbook[x$species.logbook == "maquereau"]             <- "mackerel"
x$species.logbook[x$species.logbook == "eperlan"]               <- "rainbow smelt"
x$species.logbook[x$species.logbook == "faux trigle"]           <- "mailed sculpin"
x$species.logbook[x$species.logbook == "agone"]                 <- "sea poacher"

x$species.logbook[x$species.logbook == "poule d'eau"]           <- "Atlantic lumpfish"
x$species.logbook[x$species.logbook == "poule de mer"]          <- "Atlantic lumpfish"
x$species.logbook[x$species.logbook == "lump fish"]             <- "Atlantic lumpfish"
x$species.logbook[x$species.logbook == "loup atlantique"]       <- "Atlantic wolffish"
x$species.logbook[x$species.logbook %in% c("wolf", "loup de mer")] <- "wolffish"

x$species.logbook[x$species.logbook == "diable de mer"]         <- "monkfish"
x$species.logbook[x$species.logbook == "loche"]                 <- "ocean pout"
x$species.logbook[x$species.logbook == "small & slim eelpout"]  <- "eelpout"
x$species.logbook[x$species.logbook == "lompenie"]              <- "eelblenny"

x$species.logbook[x$species.logbook == "snakeblenny"]           <- "snake blenny" 
x$species.logbook[x$species.logbook %in% c("sea snail")]        <- "seasnail"
x$species.logbook[x$species.logbook == "chaboisseau bronze"]     <- "shorthorn sculpin"
x$species.logbook[x$species.logbook == "chaboisseau a 18 epine"] <- "sculpin"
x$species.logbook[x$species.logbook == "chien de mer"]           <- "dogfish"
x$species.logbook[x$species.logbook == "dog fish"]               <- "dogfish"
x$species.logbook[x$species.logbook %in% c("hemitriptere", "crapaud de mer")] <- "sea raven"
x$species.logbook[x$species.logbook == "chaboisseau"]            <- "sculpin" 

x$species.logbook[rep(c("petite", "poule"), x$species.logbook)]      <- "spiny lumpsucker"
x$species.logbook[x$species.logbook == "spring lumpsucker"]          <- "spiny lumpsucker"
x$species.logbook[x$species.logbook == "spine belly lumpsucker"]     <- "spiny lumpsucker"
x$species.logbook[rep(c("chat de mer"), x$species.logbook)]          <- "fourline snakeblenny"
x$species.logbook[rep(c("catfish"), x$species.logbook)]              <- "fourline snakeblenny"
x$species.logbook[rep(c("round nose grenadier"), x$species.logbook)] <- "roundnose grenadier"

x$species.logbook[x$species.logbook %in% c("raie", "raie blanche", "raie grise")]   <- "skate" 
x$species.logbook[x$species.logbook %in% c("raie epineuse")]                        <- "thorny skate" 

# Crustaceans:
x$species.logbook[grep("homard", x$species.logbook)]         <- "American lobster"
x$species.logbook[x$species.logbook == "bernard l'hermite"]  <- "hermit crab"
x$species.logbook[x$species.logbook == "crabe epineux"]      <- "northern stone crab"
x$species.logbook[x$species.logbook == "crevette"]           <- "shrimp"
x$species.logbook[x$species.logbook %in% c("pandalus", "crevette pandalus")] <- "Pandalus borealis"
x$species.logbook[x$species.logbook == "casque de police"]   <- "toad crab"
x$species.logbook[x$species.logbook == "balane"]             <- "barnacle"
x$species.logbook[x$species.logbook == "hyas aneanu"]        <- "hyas araneus"
x$species.logbook[x$species.logbook == "araignee de mer"]    <- "sea spider"
x$species.logbook[x$species.logbook == "ecrevisse"]          <- "crangon"

# Mollusks:
x$species.logbook[x$species.logbook == "huitre"]             <- "oyster" 
x$species.logbook[x$species.logbook == "limace"]             <- "sea slug"
x$species.logbook[x$species.logbook == "limace arc en ciel"] <- "sea slug"
x$species.logbook[x$species.logbook == "mollusque"]          <- "mollusk"
x$species.logbook[x$species.logbook == "mollusque vide"]     <- "empty shells" 
x$species.logbook[x$species.logbook == "petoncle"]           <- "scallop"
x$species.logbook[x$species.logbook == "petoncle geante"]    <- "giant scallop"
x$species.logbook[x$species.logbook %in% c("iceland scallop", "petoncle d'islande")] <- "Iceland scallop"
x$species.logbook[x$species.logbook %in% c("cockle d'islande", "coque d'islande", "iceland clam")]   <- "Iceland cockle"
x$species.logbook[x$species.logbook == "buccin"]             <- "whelk"
x$species.logbook[x$species.logbook == "bigorneau"]          <- "whelk"
x$species.logbook[x$species.logbook == "oeuf de buccin"]     <- "whelk eggs" 
x$species.logbook[x$species.logbook == "oeuf de bigorneau"]  <- "whelk eggs"
x$species.logbook[x$species.logbook == "octopu"]             <- "octopus"
x$species.logbook[x$species.logbook == "pieuvre"]            <- "octopus"
x$species.logbook[x$species.logbook == "calmar"]             <- "squid"
x$species.logbook[x$species.logbook == "sea clam"]           <- "clam"
x$species.logbook[x$species.logbook == "palourde"]           <- "clam"
x$species.logbook[x$species.logbook == "espece de palourde"] <- "clam"
x$species.logbook[x$species.logbook == "coque"]              <- "clam" 
x$species.logbook[x$species.logbook == "oeuf de palourde"]   <- "clam eggs" 
x$species.logbook[x$species.logbook == "moule noir"]         <- "mussel"
x$species.logbook[x$species.logbook == "moule"]              <- "mussel"
x$species.logbook[rep(c("ecaille", "palou"), x$species.logbook)] <- "empty shells"
x$species.logbook[rep(c("ecaille", "peton"), x$species.logbook)] <- "empty shells"
x$species.logbook[x$species.logbook %in% c("snail")]             <- "snail"

# Miscellaneous invertebrates:
x$species.logbook[x$species.logbook %in% c("pou de mer", "puce de mer", "puceron")] <- "amphipod"

x$species.logbook[x$species.logbook %in% c("soleil de mer")] <- "jellyfish"  
x$species.logbook[x$species.logbook == "sponge"]             <- "sea sponge" 
x$species.logbook[rep("sea star", x$species.logbook)]        <- "starfish"
x$species.logbook[x$species.logbook %in% c("sun star", "solaster", "etoile solaster")]  <- "sunstar" 
x$species.logbook[x$species.logbook == "star fish"]          <- "starfish"
x$species.logbook[x$species.logbook == "etoile de"]          <- "starfish"
x$species.logbook[x$species.logbook == "anemone"]            <- "sea anemone"  
x$species.logbook[x$species.logbook %in% c("dollar", "dollar de mer")] <- "sand dollar"
x$species.logbook[x$species.logbook == "etoile"]             <- "starfish"
x$species.logbook[x$species.logbook == "holothurie"]         <- "sea cucumber"
x$species.logbook[x$species.logbook == "concombre de mer"]   <- "sea cucumber"
x$species.logbook[x$species.logbook == "oursin"]             <- "sea urchin"
x$species.logbook[x$species.logbook == "oursin de mer"]      <- "sea urchin"
x$species.logbook[x$species.logbook == "ophiure"]            <- "brittle star"
x$species.logbook[x$species.logbook == "ophiure de mer"]     <- "brittle star"
x$species.logbook[x$species.logbook == "eponge"]             <- "sea sponge"
x$species.logbook[x$species.logbook == "ophiure et eponge"]  <- "eponge et ophiure"
x$species.logbook[x$species.logbook == "eponge et ophiure"]  <- "sponges and brittlestars"
x$species.logbook[x$species.logbook == "etoile eponge"]      <- "sponges and starfish"
x$species.logbook[x$species.logbook == "mousse"]             <- "bryozoan" 
x$species.logbook[grep("balane", x$species.logbook)]         <- "barnacle"  
x$species.logbook[grep("corail", x$species.logbook)]         <- "coral"  
x$species.logbook[grep("patate de mer", x$species.logbook)]  <- "sea potato"   
x$species.logbook[grep("cocon", x$species.logbook)]          <- "whelk eggs"  
x$species.logbook[grep("sea mice", x$species.logbook)]       <- "sea mouse"  
x$species.logbook[x$species.logbook %in% c("ver de mer", "verre de mer")] <- "annelid"  

# Algae:
x$species.logbook[x$species.logbook %in% c("algue marine", "algue verte", "algue", "kelp", "seaweed")]  <- "algae" 
x$species.logbook[x$species.logbook %in% c("algue laminaria")]           <- "laminaria" 
x$species.logbook[grep("laminaire", x$species.logbook)]                  <- "laminaria" 
x$species.logbook[grep("laminaria", x$species.logbook)]                  <- "laminaria"

# Delete niaisage:
ix <- which(x$species.logbook %in% c("cracha d'admiral"))
if (length(ix) > 0) x <- x[-ix, ]

# Initialize species coding:
x$species <- NA

# Fish species coding:
x$species[x$species.logbook == "cod"]                 <- 10 
x$species[x$species.logbook == "haddock"]             <- 11  
x$species[x$species.logbook == "pollock"]             <- 16 
x$species[x$species.logbook == "gadiformes"]          <- 18 # "Gadiformes"
x$species[x$species.logbook == "Greenland cod"]       <- 118
x$species[x$species.logbook == "mackerel"]            <- 70
x$species[x$species.logbook == "flounder"]            <- 49

x$species[x$species.logbook == "flatfish"]            <- 346 
x$species[x$species.logbook == "turbot"]              <- 31
x$species[x$species.logbook == "salmon"]              <- 980 # "Salmon, trouts, etc."
x$species[x$species.logbook == "grenadier"]           <- 416 
x$species[x$species.logbook == "roundnose grenadier"] <- 414 
x$species[x$species.logbook == "sea raven"]           <- 320 
x$species[x$species.logbook == "thorny skate"]        <- 201
x$species[grep("gasp[ea]reau", x$species.logbook)]    <- 62

x$species[grep("redfish", x$species.logbook)]         <- species("redfish")[1]
x$species[grep("American plaice", x$species.logbook)] <- species("American plaice")[1]
x$species[grep("winter flounder", x$species.logbook)] <- species("winter flounder")[1]
x$species[grep("witch flounder", x$species.logbook)]  <- species("witch flounder")[1]
x$species[grep("yellowtail", x$species.logbook)]      <- species("yellowtail")[1] 
x$species[grep("white hake", x$species.logbook)]      <- species("white hake")[1] 
x$species[grep("red hake", x$species.logbook)]        <- species("red hake")[1] 
x$species[grep("rainbow smelt", x$species.logbook)]   <- species("rainbow smelt")[1] 
x$species[grep("lumpfish", x$species.logbook)]        <- species("lumpfish")[1]  
x$species[grep("cunner", x$species.logbook)]          <- species("cunner")[1]  
x$species[grep("herring", x$species.logbook)]         <- species("herring")[1]  
x$species[grep("monkfish", x$species.logbook)]        <- species("monkfish")[1]  
x$species[x$species.logbook == "skate"]               <- 211 # Skates unsp.
x$species[x$species.logbook == "plaice"]              <- 346 # Flatfish unsp. 
x$species[x$species.logbook == "fish"]                <- 90  # Unsp. fish"
x$species[x$species.logbook == "sculpin"]             <- 311 # Sculpin unsp."
x$species[x$species.logbook == "mailed sculpin"]      <- 304 
x$species[x$species.logbook == "sea poacher"]         <- 350
x$species[x$species.logbook == "plaice"]              <- 346 # Flatfish unsp. 
x$species[x$species.logbook == "capelin"]             <- 64
x$species[x$species.logbook == "dogfish"]             <- 274 
x$species[x$species.logbook == "alligatorfish"]       <- 340 
x$species[x$species.logbook == "Atlantic wolffish"]   <- 50
x$species[x$species.logbook == "wolffish"]            <- 59  # "Wolffish, unsp."
x$species[x$species.logbook == "shorthorn sculpin"]   <- 301
x$species[x$species.logbook == "spatulate sculpin"]   <- 314    
x$species[x$species.logbook == "longhorn sculpin"]    <- 300    
x$species[x$species.logbook == "ocean pout"]          <- 640                 
x$species[x$species.logbook == "slender eelblenny"]   <- 631    

x$species[x$species.logbook == "American eel"]        <- 600
x$species[x$species.logbook %in% c("anguille", "eel")]<- 634 # "Unsp. eels"
x$species[x$species.logbook == "fish doctor"]         <- 616  
x$species[x$species.logbook == "eelpout"]             <- 598 # "Eelpouts unsp."
x$species[x$species.logbook == "spiny lumpsucker"]    <- 502
x$species[x$species.logbook == "snake blenny"]        <- 622 
x$species[x$species.logbook == "fourline snakeblenny"]<- 626
x$species[x$species.logbook == "dusky seasnail"]      <- 512
x$species[x$species.logbook == "seasnail"]            <- 500 # "Seasnail unsp." 
x$species[x$species.logbook == "gelatinous seasnail"] <- 505 # "Seasnail, gelatinous" 


# Crustacean species coding:
x$species[x$species.logbook == "American lobster"]       <- 2550
x$species[x$species.logbook == "rock crab"]              <- 2513
x$species[grep("hermit crab", x$species.logbook)]        <- 2560
x$species[x$species.logbook == "hyas"]                   <- 2520
x$species[x$species.logbook %in% c("tiger shrimp", "shrimp")] <- 2100 
x$species[x$species.logbook == "hyas araneus"]           <- 2527 
x$species[x$species.logbook == "hyas coarctatus"]        <- 2521
x$species[x$species.logbook == "toad crab"]              <- 2520
x$species[x$species.logbook == "northern stone crab"]    <- 2523
x$species[x$species.logbook == "Pandalus borealis"]      <- 2210
x$species[x$species.logbook == "crangon"]                <- 2400 # Crangonidae f.

# Molluscan species coding:
x$species[x$species.logbook == "annelid"]                <- 3000
x$species[x$species.logbook == "sea slug"]               <- 4400
x$species[x$species.logbook == "snail"]                  <- 4200 # "Snails and slugs" 
x$species[x$species.logbook == "mollusk"]                <- 4000 # "Mollusca p."
x$species[x$species.logbook == "whelk"]                  <- 4210 
x$species[x$species.logbook == "whelk eggs"]             <- 1510
x$species[x$species.logbook == "bivalve"]                <- 4300 # "Bivalvia c." 
x$species[x$species.logbook == "empty shells"]           <- 4348
x$species[x$species.logbook == "arctica islandica"]      <- 4304 # Ocean quahog
x$species[x$species.logbook == "clam"]                   <- 4310 # "Clams unsp."
x$species[x$species.logbook == "scallop"]                <- 4320 # scallop"
x$species[x$species.logbook == "giant scallop"]          <- 4321 # Giant scallop"
x$species[x$species.logbook == "Iceland scallop"]        <- 4322 # Iceland scallop"
x$species[x$species.logbook == "squid"]                  <- 4514 # "Squid unsp."
x$species[x$species.logbook == "octopus"]                <- 4520
x$species[x$species.logbook == "cockle"]                 <- 4340
x$species[x$species.logbook == "mussel"]                 <- 4330
x$species[x$species.logbook == "Iceland cockle"]         <- 4342
x$species[x$species.logbook == "Stimpson's surf clam"]   <- 4355
x$species[x$species.logbook == "oyster"]                 <- 4326 # "American cupped oyster"

# Miscellaneous species coding:
x$species[x$species.logbook == "sea potato"]               <- 1823
x$species[x$species.logbook == "bryozoan"]                 <- 1900
x$species[x$species.logbook == "sunstar"]                  <- 6121  # Solaster
x$species[x$species.logbook == "sea mouse"]                <- 3200
x$species[x$species.logbook == "amphipod"]                 <- 2801
x$species[x$species.logbook == "barnacle"]                 <- 2990
x$species[x$species.logbook == "sea urchin"]               <- 6400 
x$species[x$species.logbook == "sand dollar"]              <- 6500 
x$species[x$species.logbook == "sea cucumber"]             <- 6600 
x$species[x$species.logbook == "brittle star"]             <- 6200 
x$species[x$species.logbook == "basketstar"]               <- 6300 
x$species[x$species.logbook == "starfish"]                 <- 6100 # "Asteroidea s.c."
x$species[x$species.logbook == "sponges and brittlestars"] <- 1701 # "Marine invertebrates unsp."
x$species[x$species.logbook == "sea sponge"]               <- 8600
x$species[x$species.logbook == "laminaria"]                <- 9321
x$species[x$species.logbook == "algae"]                    <- 9300
x$species[x$species.logbook == "sea anemone"]              <- 8300
x$species[x$species.logbook %in% c("polyp", "polyps")]     <- 8200
x$species[x$species.logbook == "coral"]                    <- 8530 # "Sea corals unsp."
x$species[x$species.logbook == "dead man's fingers"]       <- 8336
x$species[x$species.logbook == "jellyfish"]                <- 8500 
x$species[x$species.logbook == "sea spider"]               <- 5100
x$species[x$species.logbook == "sponges and starfish"]     <- 1701
x$species[x$species.logbook == "clam eggs"]                <- 1500  # "Mollusc eggs unsp."
x$species[grep("irish moss", x$species.logbook)]           <- 9332 

unique(x[which(is.na(x$species)), "species.logbook"]) 
x[is.na(x$species), ]

# Re-order fields:
remove <- c("year", "month", "day", "species.logbook")
x <- x[setdiff(names(x), remove)]
vars <- c("date", "tow.id", "tow.number", "species", "species.name.logbook")
x <- x[c(vars, setdiff(names(x), c(vars, "comment")), "comment")]

# Delete lines with missing IDs:
# Doublecheck these in the logbook:
ix <- which(is.na(x$species) & (is.na(x$number.caught) | x$number.caught == 0) & (x$species.name == ""))
if (length(ix) > 0) x <- x[-ix, ]

ix <- which(is.na(x$species) & x$species.name.logbook == "")
#if (length(ix) > 0) x <- x[-ix, ]

# Remove tow.id:
x <- x[setdiff(names(x), "tow.id")]
#x$tow.id <- paste0("GP", gsub(" ", "0", formatC(as.numeric(gsub("S89", "", x$tow.id)), width = 3)))

# Fix tow.id format:
#x$tow.id <- paste0("GP", gsub(" ", "0", formatC(as.numeric(gsub("S89", "", x$tow.id)), width = 3)))

# Fix index key:
ux <- unique(x[c("date", "tow.number", "species")])
remove <- NULL
for (i in 1:nrow(ux)){
   ix <- which((x$date == ux$date[i]) & (x$tow.number == ux$tow.number[i]) & (x$species == ux$species[i]))
   if (length(ix) > 1){
      print(x[ix, ])
      if (all(is.na(as.numeric(x$number.caught[ix])))){
         s <- x$number.caught[ix]
         s[is.na(s)] <- ""
         s <- paste(s, collapse = " + ")
         s <- gsub(" [+] $", "", s)
         s <- gsub(" [+] $", "", s)
         if (length(s) == 0) s <- ""
      }else{
         s <- sum(as.numeric(x$number.caught[ix]), na.rm = TRUE)
      }  
      print(s)
      x$number.caught[ix[1]] <- s
      x$species.name.logbook[ix[1]] <- paste(unique(x$species.name.logbook[ix]), collapse = " & ")
      x$comment[ix[1]] <- paste(unique(x$comment[ix]), collapse = "; ")
      remove <- c(remove, ix[2:length(ix)])
   }
}
if (length(remove) > 0) x <- x[-remove, ]


# Comment clean-up:
x$comment <- gsub("^; ", "", x$comment)
x$comment <- gsub(";[ ]*$", "", x$comment)

# Write data:
path <- paste0(unlist(strsplit(getwd(), "gulf"))[1], "gulf.data/inst/extdata/")
write.csv(x, file = paste0(path, paste0("scs.cat.", year, ".csv")), row.names = FALSE)

print(unique(x[which(is.na(x$species)), "species.logbook"]) )
print(x[is.na(x$species), ])

print(aggregate( x$species.name.logbook, by = list(species(x$species)), function(x) paste(unique(x), collapse = "; ")))


