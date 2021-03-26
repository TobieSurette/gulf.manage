library(gulf.data)

# Find "new" files:
files <- locate(file = "^scs[0-9]+C")

# Load files:
r <- NULL
for (i in 1:length(files)){
   print(files[i])
   x <- read.csv(files[i], header = TRUE, stringsAsFactors = FALSE)
   
   # Determine survey year:
   year <- unique(x$year)
   
   if (year %in% c(2006:2014, 2019)) x <- cbind(data.frame(date = as.character(date(year = x$year, month = x$month, day = x$day)), stringsAsFactors = FALSE), x)
   
   # Read tow data:
   y <- read.scsset(year, valid = 1)
   y$year <- year(y)

   if (year %in% 2006:2014){
      x$tow <- substr(x$tow.id, 3, 5)
      y$tow <- substr(y$tow.id, 3, 5)
      key <- c("date", "tow")
      ix <- match(x[key], y[key])   
      x$tow.id[!is.na(ix)] <- y$tow.id[ix[!is.na(ix)]]
   }
   
   # Import tow IDs and dates:
   if (year %in% 2015:2018){
      x$tow <- substr(x$tow.id, 3, 5)
      y$tow <- substr(y$tow.id, 3, 5)
      key <- c("year", "tow")
      ix <- match(x[key], y[key])
      x <- cbind(data.frame(date = y$date[ix], stringsAsFactors = FALSE), x)
      x$tow.id <- y$tow.id[ix]         # Replace tow ID.
      x$tow.number <- y$tow.number[ix] # Replace tow number.
   }
   
   if (year > 2018){
      x$tow.id <- toupper(x$tow.id)
      ix <- match(x[c("year", "tow.id")], y[c("year", "tow.id")])
      x$date <- y$date[ix] # Replace tow ID.
      x$tow.number <- y$tow.number[ix] # Replace tow number.
   }
   
   # Remove redundant data:
   x <- x[-which(names(x) %in% c("year", "month", "day", "tow"))]

   if (!("comment" %in% names(x))) x$comment <- ""
   # Add missing variables:
   if (i > 1){
      x[setdiff(names(r), names(x))] <- NA
      x[setdiff(names(x), names(r))] <- NA
      x <- x[names(r)]
   }

   # Append data:
   r <- rbind(r, x)
}

# Spot corrections:
r$species[which((r$date == "2019-08-26") & (r$tow.id == "GP135F") & (r$species == 2526))] <- 2560

# Substitute NA values:
r$comment[is.na(r$comment)] <- ""

# Write to "gulf.data":
path <- gsub("gulf.manage", "gulf.data", getwd())
path <- paste0(path, "/inst/extdata")
if (file.exists(path)){
   years <- sort(unique(year(r)))
   for (i in 1:length(years)){
      file <- paste0(path, "/", "scs.cat.", years[i], ".csv") 
      write.csv(r[which(year(r) == years[i]), ], file = file, row.names = FALSE)
   }
}
