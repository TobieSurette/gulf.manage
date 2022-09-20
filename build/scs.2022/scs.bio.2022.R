library(gulf.data)

# Define survey year:
year <- 2022

# Read raw data export:
x <- read.table(paste0("build/scs.2022/data/scs.bio.", year, ".csv"), header = TRUE, sep =",", stringsAsFactors = FALSE)

# format data fields:
names(x)          <- gsub("_", ".", tolower(names(x)))
x$tow.id          <- deblank(toupper(x$gpnumber))
x$tow.number      <- x$trawl.number
x$sex             <- as.numeric(gsub("[*]", "", x$sex))
x$egg.colour      <- as.numeric(gsub("[*]", "", x$egg.color))
x$eggs.remaining  <- as.numeric(gsub("[*]", "", x$percent.eggs))
x$gonad.colour    <- as.numeric(gsub("[*]", "", x$gonade))
x$shell.condition <- deblank(gsub("[*]", "", x$shell.condition))
x$missing.legs    <- apply(x[, c(paste0("l", 1:5), paste0("r", 1:5))], 1, paste0, collapse = "")

# Remove blank entries:
x <- x[x$tow.id != "GP000F", ]

# Define date field:
x$date  <- unlist(lapply(strsplit(x$date.tow, "-"), function(x) x[1]))
x$year  <- as.numeric(unlist(lapply(strsplit(x$date, "/"), function(x) x[3])))
x$month <- as.numeric(unlist(lapply(strsplit(x$date, "/"), function(x) x[1])))
x$day   <- as.numeric(unlist(lapply(strsplit(x$date, "/"), function(x) x[2])))
x$date <- as.character(date(year = x$year, month = x$month, day = x$day))
   
# Remove columns:
remove <- c("gpnumandcrabnum", "date.tow", "gpnumber.clean", "lencw", 
            "carapace.width.number", "chela.height.number",
            "year", "month", "day",
            names(x)[grep("^[lr][1-5]", names(x))],
            names(x)[grep("total", names(x))],
            names(x)[grep("test", names(x))], names(x)[grep("new", names(x))],
            names(x)[grep("check", names(x))], names(x)[grep("maturity", names(x))],
            names(x)[grep("trimmed", names(x))])
x <- x[, !(names(x) %in% remove)]

# Extract time stamp:
x$time <- substr(time(unlist(lapply(strsplit(x$timestamp, " "), function(x) x[2]))), 1, 8)

x$shell.condition.mossy <- ""
x$shell.condition.mossy[substr(toupper(x$shell.condition), 2, 2) == "M"] <- "M"
x$shell.condition <- as.numeric(substr(x$shell.condition, 1, 1))
if (!("tag.number" %in% names(x)))    x$tag.number <- NA
if (!("weight" %in% names(x)))        x$weight <- NA
if (!("durometer" %in% names(x)))     x$durometer <- NA
if (!("trap.code" %in% names(x)))     x$trap.code <- NA
if (!("abdomen.width" %in% names(x))) x$abdomen.width <- NA

# Remove irrelevant data:
#x <- x[order(paste(time(x), x$crab.number), ]
ix <- is.na(x$carapace.width) & is.na(x$shell.condition) & is.na(x$gonad.colour) & is.na(x$egg.colour) & is.na(x$eggs.remaining)
x <- x[which(!ix), ]
x <- x[x$sex %in% c(1:2), ]

# Fix comments:
x$comment <- paste0(toupper(substr(x$comment, 1, 1)), tolower(substr(x$comment, 2, nchar(x$comment))))

# Check missing legs:
table(unlist((strsplit(x$missing.legs, ""))))

# Check for inconsistencies is sex measurements:
which(x$sex == 2 & !is.na(x$chela.height))
index <- which((x$sex == 1) & ((!is.na(x$gonad.colour)) | (!is.na(x$egg.colour)) | (!is.na(x$eggs.remaining))))

# Missing eggs remaining:
x$eggs.remaining[which((x$egg.colour == 4) & is.na(x$eggs.remaining))] <- 0

# Carapace width corrections:
x$carapace.width <- abs(x$carapace.width)

# Female size corrections:
x$carapace.width[which(x$sex == 2 & is.mature.scsbio(x) & x$carapace.width < 30)] <- NA

# Delete irrelevant data and re-order variables:
vars <- c("date", "time", "tow.id", "tow.number", "crab.number", "sex",
          "carapace.width", "chela.height", "abdomen.width", "durometer", "weight",
          "shell.condition", "missing.legs", "shell.condition.mossy",
          "gonad.colour", "egg.colour", "eggs.remaining", "samplers", "tag.number", "trap.code", "comment")
x <- x[vars]

# Update gulf.data repo:
file <- paste0(gsub("gulf.manage", "gulf.data", getwd(), fixed = TRUE), "/inst/extdata/scs.bio.", year, ".csv")
write.csv(x, file = file, row.names = FALSE)

