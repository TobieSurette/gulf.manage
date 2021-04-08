# Load data:
x <- read.csv(locate(file = c("1988", "scanmar", "csv")), header = TRUE)
y <- read.csv(locate(file = c("1988", "coordinates", "csv")), header = TRUE)

names(y) <- tolower(gsub("tow.no", "tow.number", names(y)))

# Define date:
x$date <- as.character(date(x))
y$date <- as.character(date(y))

# Merge data sets:
vars <- c("date", "tow.number")

tows <- sort(unique(rbind(y[vars], x[vars])), by = vars)
v <- NULL
for (i in 1:nrow(tows)){
   ix <- which(x$date == tows$date[i] & x$tow.number == tows$tow.number[i])
   iy <- which(y$date == tows$date[i] & y$tow.number == tows$tow.number[i])
   
   xx <- NULL
   if (length(ix) > 0){
      xx <- x[ix, ]
      xx$door.spread <- NA
      xx$wing.spread <- NA
      for (j in 1:nrow(xx)){
         tmp <- as.numeric(gsub(",", ".", strsplit(xx$value[j], "-")[[1]]))
         wing.spread <- TRUE
         if (xx$measurement[j] == "doorspread") wing.spread <- FALSE
         xx <- rbind(xx[-j, ], xx[rep(j, length(tmp)), ])
         xx$value <- tmp
         if (wing.spread) xx$wing.spread <- tmp else xx$door.spread <- tmp
      }

      xx[vars]
   }
   
   yy <- NULL
   if (length(iy) > 0){
      yy <- y[iy, vars]
      yy$time <- y$time.logbook[iy]
      yy$longitude <- y$loranx.logbook[iy] 
      yy$latitude  <- y$lorany.logbook[iy]  
      yy$wing.spread <- y$door.spread[iy]
      yy$door.spread <- NA
   }
   
   v <- rbind(v, rbind(xx, yy))
   
}

