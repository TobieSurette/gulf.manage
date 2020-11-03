#' GSE Data Functions
#'
#' @description Convert from Groundfish Survey Entry (GSE) format to Groundfish Survey Data (GSD) format.
#'
#' This function reformats the raw data from a GSE database into a standard
#' format.  It renames fields, parses variables, converts coordinates to a
#' decimal-degree format, adds auxiliary fields and homogenizes the index keys
#' between each table.
#'
#' The Gulf Survey Data format separates the data into three tables.
#' Information pertaining to individual sets are stored in the \code{set}
#' table. Data on individual catches separated by species are stored in the
#' \code{cat} table.  Data on individual specimens are stored in the \code{bio}
#' table. Note that there is no length-frequency summary table, though it may
#' be derived the above tables.
#'
#' @param x List with fields \code{set}, \code{basket}, \code{catch} and \code{detail}, which are data tables 
#'          imported from a GSE Oracle database.
#' @param survey Research survey, see \code{\link[gulf.metadata]{project}} for options.
#'             
#' @return List with \code{set}, \code{cat} and \code{bio} fields, which are
#' data frames of set, catch and biological data.
#' 
#' @seealso \code{\link[gulf]{read.gse}}, \code{\link[gulf]{gsd2card}}
#' @examples
#' # Convert imported data to 'gsd' format:
#' y <- gse2gsd(x)

#' @describeIn gse Convert GSE tables to a Gulf survey data object.
#' @export
gse2gsd <- function(x, survey, sort = TRUE){
   survey <- gulf.metadata::project(survey)

   # Create set data frame:
   y <- list(set = NULL, cat = list(), bio = list())

   # Define index keys for each table:
   key.set <- c("date", "cruise", "set.number")
   key.cat <- c("date", "cruise", "set.number", "species", "size.class")
   key.bio <- c("date", "cruise", "set.number", "species", "fish.number")

   # Copy data into appropriate fields:
   y$set <- data.frame(cruise = paste0(substr(x$set[ ,"MISSION"],1,1), substr(x$set[ ,"MISSION"],8,10)), stringsAsFactors = FALSE)
   y$set$stratum <- x$set[ ,"STRAT"]
   y$set$set.number <- x$set[ ,"SETNO"]
   y$set$date <- paste0(substr(x$set$START_DATE, nchar(x$set$START_DATE)-3, nchar(x$set$START_DATE)), "-", 
                       substr(x$set$START_DATE, nchar(x$set$START_DATE)-5, nchar(x$set$START_DATE)-4), "-",
                       substr(x$set$START_DATE, 1, nchar(x$set$START_DATE)-6))
   y$set$date[y$set$date != ""] <- as.character(gulf.utils::date(y$set$date[y$set$date != ""]))
   y$set$experiment <- x$set[,"EXPERIMENT_TYPE_CODE"]
   y$set$start.time <- paste0(gsub(" ", "0", substr(format(x$set$START_TIME, width = 4), 1, 2)), ":",
                              gsub(" ", "0", substr(format(x$set$START_TIME, width = 4), 3, 4)), ":00")
   y$set$end.time <- paste0(gsub(" ", "0", substr(format(x$set$END_TIME, width = 4), 1, 2)), ":",
                            gsub(" ", "0", substr(format(x$set$END_TIME, width = 4), 3, 4)), ":00")  
   
   y$set$duration <- as.numeric(difftime(as.POSIXct(paste(y$set$date, y$set$end.time)), as.POSIXct(paste(y$set$date, y$set$start.time)), units = "mins"))
   y$set$gear <- x$set$GEAR
   y$set$auxiliary <- x$set[, "AUX"]
   y$set$speed <- round(x$set[, "SPEED"]*10)/10
   y$set$speed.method <- x$set[, "HOWS"]
   y$set$latitude.start <- x$set[, "SLAT"]
   y$set$longitude.start <- x$set[, "SLONG"]
   y$set$latitude.end <- x$set[, "ELAT"]
   y$setlongitude.end <- x$set[, "ELONG"]
   y$set$depth.start <- x$set[, "START_DEPTH"]
   y$set$depth.end <- x$set[, "END_DEPTH"]
   y$set$distance <- round(x$set[, "DIST"] * 100)/100
   y$set$distance.method <- x$set[, "HOWD"]
   y$set$wind.direction <- x$set[, "WIND"]
   y$set$wind.force <- x$set[, "FORCE"]
   y$set$tide <- x$set[, "CURNT"]
   y$set$warp.port <- x$set[, "WARPOUT"]
   y$set$warp.starboard <- x$set[, "WARPOUT"]
   y$set$station.number <- x$set$STATION
   y$set$block.number <- NA
   y$set$comment <- x$set[, "NOTE"]
   y$set$comment[is.na(y$set$comment)] <- ""
   y$set$comment <- gsub(",", ";", y$set$comment)
   y$set <- sort(y$set, by = key.set)

   # Change BASKETS GSE table field names:
   temp <- names(x$basket)
   temp[temp == "SETNO"] <- "set.number"
   temp[temp == "SPEC"] <- "species"
   temp[temp == "SIZE_CLASS"] <- "size.class"
   names(x$basket) <- temp
   x$basket$cruise <- paste0(substr(x$basket[ ,"MISSION"],1,1), substr(x$basket[ ,"MISSION"],8,10))
   index <- match(x$basket$set.number, y$set$set.number)
   x$basket$date <- y$set$date[index]
   
   # Create catch data frame:
   x$basket <- x$basket[!is.na(x$basket[, "BASKET_WEIGHT"]), ] 
   # x$basket[is.na(x$basket[,"SAMPLED"]), ]
   temp <- x$basket[which(x$basket[,"SAMPLED"] == "Y"), ] # Isolate the sampled baskets.
   temp <- stats::aggregate(list(weight.sampled = temp[, "BASKET_WEIGHT"]), by = temp[key.cat], sum)
   y$cat <- stats::aggregate(list(weight.caught = x$basket[, "BASKET_WEIGHT"]), by = x$basket[, key.cat], sum)
   y$cat$weight.sampled <- temp$weight.sampled[match(y$cat[key.cat], temp[key.cat])]
   temp <- stats::aggregate(list(number.basket = rep(1,dim(x$basket)[1])), by = x$basket[key.cat], sum)
   y$cat$number.basket <- temp$number.basket[match(y$cat[key.cat], temp[key.cat])]
   y$cat$weight.sampled[is.na(y$cat$weight.sampled)] <- 0
   y$cat$weight.unit <- "kg"
   
   # Merge catch table comments:
   temp <- names(x$catch)
   temp[temp == "SPEC"] <- "species"
   temp[temp == "SETNO"] <- "set.number"
   temp[temp == "SPEC_COMMENT"] <- "comment"
   temp[temp == "SIZE_CLASS"] <- "size.class"
   names(x$catch) <- temp
   index <- match(y$cat[c("set.number", "species", "size.class")], x$catch[c("set.number", "species", "size.class")])
   y$cat$comment <- x$catch$NOTE[index]
   y$cat$comment[is.na(y$cat$comment)] <- ""
   y$cat$number.caught <- y$cat$comment <- x$catch$NUMBER_CAUGHT[index]
   y$cat <- sort(y$cat, by = key.cat)

   # Create biological data frame:
   y$bio <- x$detail[!is.na(x$detail[, "LENGTH"]), ] # Remove fish with no recorded lengths.

   # Change DETAIL GSE table field names:
   temp <- names(x$detail)
   temp[temp == "SETNO"] <- "set.number"
   temp[temp == "SPEC"] <- "species"
   temp[temp == "SIZE_CLASS"] <- "size.class"
   temp[temp == "LENGTH"] <- "length"
   temp[temp == "SEX"] <- "sex"
   temp[temp == "WEIGHT"] <- "weight"
   temp[temp == "MATURITY"] <- "maturity"
   temp[temp == "FISHNO"] <- "fish.number"
   temp[temp == "AGE_MAT"] <- "age.material"
   temp[temp == "DET_COMMENT"] <- "comment"
   names(x$detail) <- temp

   # Add index variables which are not present ('year', 'vessel.code' and/or 'cruise.number'):
   x$detail <- merge(x$detail, y$set[, key.set], by = intersect(key.set, names(x$detail)), all.x = TRUE)
   y$bio <- x$detail

   y$bio <- y$bio[!is.na(y$bio$length), ]
   # Add auxiliary variables:
   y$bio[, "width"] <- NA
   y$bio[, "shell.condition"] <- NA
   y$bio[, "egg.condition"] <- NA
   y$bio[, "length.unit"] <- "cm"
   y$bio <- set(y$bio, list(species = c(2513, 2520, 2521, 2523, 2526, 2527, 2550, 4321, 4322, 4349, 2539, 6411, 4211)), length.unit = "mm")
   y$bio[, "length.interval"] <- 1
   y$bio[, "measurement.type"] <- 2
   if (survey %in% c("rv" , "ns")){
      y$bio <- set(y$bio, list(species = 60), length.interval = 0.5)
      y$bio <- set(y$bio, list(species = 60), measurement.type = 1)
   }
   y$bio[, "weight.unit"] <- "g"

   y$bio <- sort(y$bio, by = key.bio)

   return(y)
}

#' @describeIn gse Convert from to GSD to ASCII Data Card Format
#' @export
gsd2card <- function(x, survey = "rv"){
   # GSD2CARD - Convert a Gulf survey data object to survey card format.

   # Define index keys:
   key.set <- c("year", "vessel.code", "cruise.number", "set.number")
   key.cat <- c("year", "vessel.code", "cruise.number", "set.number", "species", "size.class")
   key.bio <- c("year", "vessel.code", "cruise.number", "set.number", "species", "size.class", "fish.number")
   key.len <- c("year", "vessel.code", "cruise.number", "set.number", "species", "size.class", "sex", "start.length")
   y <- x

   # Build catch card information from bio card:
   catch <- x$cat

   # Calculate number measured for length:
   temp <- stats::agregate(list(number.length = rep(1,dim(x$bio)[1])), x$bio[, key.cat], sum)
   catch <- merge(catch, temp, by = key.cat, all.x = TRUE)

   # Calculate number measured for weight:
   temp <- x$bio[!is.na(x$bio[, "weight"]), ]

   if(dim(temp)[1] > 0){
      temp <- stats::agregate(list(number.weight = rep(1,dim(temp)[1])), temp[, key.cat], sum)
      catch <- merge(catch, temp, by = key.cat, all.x = TRUE)
   }else{
      catch$number.weight = NA;
   }

   # Calculate number measured for sex:
   temp <- x$bio[!is.na(x$bio[, "sex"]), ]
   if(dim(temp)[1]){
      temp <- stats::agregate(list(number.sex = rep(1,dim(temp)[1])), temp[, key.cat], sum)
      catch <- merge(catch, temp, by = key.cat, all.x = TRUE)
   }else{
      catch$number.sex = NA;
   }
   # Calculate number measured for maturity:
   temp <- x$bio[!is.na(x$bio[, "maturity"]), ]
   if(dim(temp)[1]){
      temp <- stats::agregate(list(number.maturity = rep(1,dim(temp)[1])), temp[, key.cat], sum)
      catch <- merge(catch, temp, by = key.cat, all.x = TRUE)
   }else{
      catch$number.maturity = NA;
   }
   # Calculate number measured for otolith:
   temp <- x$bio[!is.na(x$bio[, "age.material"]), ]
   if(dim(temp)[1]){
      temp <- stats::agregate(list(number.otolith = rep(1,dim(temp)[1])), temp[, key.cat], sum)
      catch <- merge(catch, temp, by = key.cat, all.x = TRUE)
   }else{
      catch$number.otolith = NA;
   }
   # Replace NA values by zeroes:
   fields <- c("number.length", "number.weight", "number.sex", "number.maturity", "number.otolith")
   catch$number.weight = 0;
   temp <- catch[, fields]
   temp[is.na(temp)] <- 0
   catch[, fields] <- temp

   catch[, "weight.sampled"] <- convert.vector(catch[, "weight.sampled"], "F10.0", "F4.0")
   catch[, "weight.caught"] <- convert.vector(catch[, "weight.caught"], "F10.0", "F4.0")
   catch[, "ratio"] <- catch[, "weight.sampled"] / catch[, "weight.caught"]
   number.caught <- (1/catch[, "ratio"]) * catch[, "number.length"]
   index <- is.na(catch[, "number.caught"])
   catch[index, "number.caught"] <- number.caught[index]
   catch[, "ratio"] <- round(catch[, "ratio"]*1000000000)/1000000000
   catch[, "number.stomach"] <- 0
   catch[, "weight.calculated"] <- NA

   # Final catch card is an aggregated version of 'catch':
   fields <- c("number.caught", "weight.caught", "weight.sampled", "number.basket", "number.length", "number.sex", "number.maturity", "number.weight", "number.otolith", "number.stomach", "weight.calculated")
   y$cat <- stats::agregate(catch[, fields], by = catch[, setdiff(key.cat, "size.class")], sum)

   # Add 'comment' field:
   fun <- function(x) paste(x, collapse = " ")
   temp <- stats::agregate(catch[, "comment", drop = FALSE], by = catch[, setdiff(key.cat, "size.class")], fun)
   temp$comment <- gsub("NA", "", temp$comment)
   temp$comment <- convert.vector(temp$comment, to = paste("A", max(nchar(temp$comment)), sep = ""))
   y$cat <- merge(y$cat, temp, by = setdiff(key.cat, "size.class"), all.x = TRUE)
   # Save data frame with cleaned up comments
   tc = y$cat
   y$cat <- stats::agregate(catch[, fields], by = catch[, setdiff(key.cat, "size.class")], sum)
   #need to merge cleaned up comments back into catch card
   y$cat = merge(y$cat, tc, by = setdiff(key.cat, "size.class"), names="comment")
   # Add card.type:
   y$cat <- cbind(list(card.type = rep(6, dim(y$cat)[1])), y$cat)



   # Build set card:
   set <- stats::agregate(list(species.fish.number = is.fish(y$cat[, "species"]),
                         species.invertebrate.number = is.invertebrate(y$cat[, "species"])),
                    y$cat[, key.set], sum)

   y$set <- merge(x$set[, setdiff(names(x$set), c("species.fish.number", "species.invertebrate.number"))],
                  set, by = key.set, all.x = TRUE)
   y$set <- merge(y$set, stats::agregate(list(catch.total.weight = x$cat[,"weight.caught"]), x$cat[, key.set], sum),
                  by = key.set, all.x = TRUE, overwrite = TRUE)
   y$set[, "catch.total.weight"] <- round(y$set[, "catch.total.weight"])


   # Add start second:
   if(survey == "rv"){
      y$set[, "start.second"] = NA
      y$cat[, "start.second"] = NA
   }

   # Set card is complete:
   fields <- c("stratum", "month", "day", "unit.area", "experiment", "start.hour",
               "start.minute", "start.second", "duration", "bottom.temperature",
               "bottom.salinity", "light", "bottom.type")
   y$cat <- merge(y$cat, y$set[, c(key.set, fields)], by = key.set, overwrite = TRUE)
   y$set[, "depth"] <- (y$set[, "depth.start"] + y$set[, "depth.end"])/2
   y$cat <- merge(y$cat, y$set[, c(key.set, "depth")], by = key.set, overwrite = TRUE)
   # Catch card is complete:

   # Build length card:
   temp.bio <- x$bio
   temp.bio[is.na(temp.bio[, "sex"]), "sex"] <- 9
   temp.bio[temp.bio[, "species"] %in% c(10, 11, 60, 70), "sex"] <- 4

   # Transform 'bio' lengths into frequency matrix format:
   temp.bio$start.length <- floor(temp.bio$length / (10 * temp.bio[, "length.interval"])) * (10 * temp.bio[, "length.interval"])
   temp <- repvec(temp.bio$start.length, ncol = 10) + (repvec(0:9, nrow = dim(x$bio)[1]) * repvec(temp.bio[, "length.interval"], ncol = 10))

   temp.bio$length = round(temp.bio$length/temp.bio$length.interval) * temp.bio$length.interval

   freq <- as.data.frame((repvec(temp.bio$length, ncol = 10) == temp)+1-1)
   names(freq) <- paste("freq", 0:9, sep = "")
   temp.bio <- cbind(temp.bio, freq)
   y$len <- stats::agregate(temp.bio[paste("freq", 0:9, sep = "")], by = temp.bio[key.len], sum)
   temp.bio$length.unit <- ifelse(temp.bio$length.unit == "mm", 2, 1)
   temp <- stats::agregate(temp.bio["length.unit"], by = temp.bio[key.len], unique)
   y$len <- merge(y$len, temp, by = key.len, all.x = TRUE)
   y$len[, "card.type"] <- 7
   y$len[, "group"] <- 1
   y$len[, "record.number"] <- 1
   y$len <- merge(y$len, catch[c(key.cat, "ratio")], by <- key.cat, all.x = TRUE)
   y$len <- merge(y$len, stats::agregate(list(number.length = rep(1,dim(temp.bio)[1])), by = temp.bio[c(key.cat, "sex")], sum),
                  by = c(key.cat, "sex"), all.x = TRUE)
   fields <- c("stratum", "month", "day", "unit.area", "experiment")
   y$len <- merge(y$len, y$set[c(key.set, fields)], by = key.set, all.x = TRUE, overwrite = TRUE)

   # Length card is complete.

   # Build bio card:
   # Weights are not registered in NS surveys but all fish are measured.
   # those fish will appear in the bio card
   if(survey == "ns"){
      index <- !(is.na(x$bio[, "length"]))
   }else{
      index =  !is.na(x$bio$weight) | (!is.na(x$bio$maturity) | !is.na(x$bio$chela) |  x$bio$missing.legs !=  "          "  | !is.na(x$bio$disc.width) | !is.na(x$bio$bobtail))
   }

   y$bio <- x$bio[index, ]
   # Import set data information:
   fields <- c("stratum", "month", "day", "unit.area", "experiment")
   y$bio <- merge(y$bio, y$set[c(key.set, fields)], by = key.set, all.x = TRUE, overwrite = TRUE)
   y$bio[, "card.type"] = 8
   y$bio[, "record.number"] <- 1


   # Sort data:
   y$set <- gulf::sort.data.frame(y$set, key.set)
   y$cat <- gulf::sort.data.frame(y$cat, setdiff(key.cat, "size.class"))
   y$bio <- gulf::sort.data.frame(y$bio, setdiff(key.bio, "size.class"))
   y$len <- gulf::sort.data.frame(y$len, setdiff(key.len, "size.class"))

   # Create 'ascii' data objects:
   if (survey == "rv"){
      y$set <- merge(convert(rvset()), y$set)
      y$cat <- merge(convert(rvcat()), y$cat)
      y$bio <- merge(convert(rvbio()), y$bio)
      y$len <- merge(convert(rvlen()), y$len)
   }else{
      y$set <- merge(convert(nsset()), y$set)
      y$cat <- merge(convert(nscat()), y$cat)
      y$bio <- merge(convert(nsbio()), y$bio)
      y$len <- merge(convert(nslen()), y$len)
   }

   # Need to do this after merge
   index = y$len$species == 60
   y$len$length.interval[index] = 0.5
   y$len$length.interval[!index] = 1
   # Merge is converting raios to NA because of a format issue (F9.9)
   index = is.na(y$len$ratio)
   y$len = set(y$len, index, ratio = 1)

   return(y)
}
