#'  Data Functions
#'
#' @description Functions to convert and reformat data from Groundfish Survey Entry (GSE) and 
#'              Ecosystrem Survey Entry (GSE) to standardized formats. The Gulf Survey Data format separates the 
#'              data into three tables. Information pertaining to individual sets are stored in the \code{set}
#'              table. Data on individual catches separated by species are stored in the \code{cat} table.  Data on 
#'              individual specimens are stored in the \code{bio} table. Note that there is no length-frequency summary 
#'              table, though it may be derived the above tables.
#'
#' @param x List with fields \code{set}, \code{basket}, \code{catch} and \code{detail}, which are data tables 
#'          imported from a GSE or ESE Oracle database.
#' @param survey Research survey, see \code{\link[gulf.metadata]{project}} for options.
#'             
#' @return List with \code{set}, \code{cat} and \code{bio} fields, which are data frames of set, catch and biological data.
#' 
#' @examples
#' # Convert imported data to 'gsd' format:
#' y <- gse2gsd(x)
#' z <- gsd2oracle(y)

#' @describeIn gse Convert GSE (Gulf Survey Entry) or ESE (Ecological Survey Entry) data to a Gulf survey data object.
#' @export
ese2gsd <- function(x, survey, sort = TRUE){
   # Define survey:
   survey <- gulf.metadata::project(survey)
   
   # Define index keys for each table:
   key.set <- c("date", "cruise", "set.number")
   key.cat <- c("date", "cruise", "set.number", "species", "size.class")
   key.bio <- c("date", "cruise", "set.number", "species", "size.class", "specimen")
   
   # Set variable names to lowercase:
   x <- lapply(x, function(x){ names(x) <- gsub("_", ".", tolower(names(x))); return(x) })
   
   # Eliminate variables with no data:
   x <- lapply(x, squeeze)
   
   # Generate 'cruise' variable:
   for (i in 1:length(x)){
      if ("mission" %in% names(x[[i]])) 
         x[[i]]$cruise <- paste0(substr(x[[i]]$mission,1,1), substr(x[[i]]$mission,nchar(x[[i]]$mission)-2,nchar(x[[i]]$mission)))
   }
   
   # Rename variables:
   for (i in 1:length(x)){
      str <- names(x[[i]])
      str[str %in% c("setno")] <- "set.number"
      str[str %in% c("spec")] <- "species"
      str[str %in% c("specimen.id")] <- "specimen"
      str[str %in% c("note", "comments")] <- "comment"
      str[str %in% c("slat")] <- "latitude.start"
      str[str %in% c("slong")] <- "longitude.start"
      str[str %in% c("elat")] <- "latitude.stop"
      str[str %in% c("elong")] <- "longitude.stop"
      str[str %in% c("curnt")] <- "current"
      str[str %in% c("start.depth")] <- "depth.start"
      str[str %in% c("end.depth")] <- "depth.end"
      str[str %in% c("experiment.type.code")] <- "experiment"
      str[str %in% c("aux")] <- "auxiliary"
      str[str %in% c("warpout")] <- "warp"
      str[str %in% c("strat")] <- "stratum"
      str[str %in% c("start.date")] <- "date"
      str[str %in% c("end.time")] <- "stop.time"
      str[str %in% c("force")] <- "wind.force"
      str[str %in% c("basket.weight")] <- "weight"  
      str[str %in% c("lobster.carapace.condition")] <- "carapace.condition"
      str[str %in% c("lobster.egg.condition")] <- "egg.condition"

      names(x[[i]]) <- str
   }
   
   # Rename tables:
   names(x) <- gsub("catch", "cat", names(x))
   names(x) <- gsub("detail", "bio", names(x))
   
   # Copy data into appropriate fields:
   x$set$date <- paste0(substr(x$set$date, nchar(x$set$date)-3, nchar(x$set$date)), "-", 
                        substr(x$set$date, nchar(x$set$date)-5, nchar(x$set$date)-4), "-",
                        substr(x$set$date, 1, nchar(x$set$date)-6))
   x$set$date[grep("NA", x$set$date)] <- "" 
   x$set$date[x$set$date != ""] <- as.character(gulf.utils::date(x$set$date[x$set$date != ""]))
   x$set$start.time <- paste0(gsub(" ", "0", substr(format(x$set$start.time, width = 4), 1, 2)), ":",
                              gsub(" ", "0", substr(format(x$set$start.time, width = 4), 3, 4)), ":00")
   x$set$stop.time <- paste0(gsub(" ", "0", substr(format(x$set$stop.time, width = 4), 1, 2)), ":",
                             gsub(" ", "0", substr(format(x$set$stop.time, width = 4), 3, 4)), ":00")  
   
   x$set$duration <- as.numeric(difftime(as.POSIXct(paste(x$set$date, x$set$stop.time)), as.POSIXct(paste(x$set$date, x$set$start.time)), units = "mins"))
   x$set$comment[is.na(x$set$comment)] <- ""
   x$set$comment <- gsub(",", ";", gulf.utils::deblank(x$set$comment))
   x$set <- x$set[, !(names(x$set) %in% c("end.date"))]
   x$set <- sort(x$set, by = key.set)

   # Change BASKETS GSE table field names:
   x$basket$date <- x$set$date[match(x$basket$set.number, x$set$set.number)]
   x$basket <- x$basket[!is.na(x$basket$weight), ] 
   
   # Create catch data frame:
   x$cat$date <- x$set$date[match(x$cat$set.number, x$set$set.number)]
   x$cat$comment[is.na(x$cat$comment)] <- ""
   x$cat$comment <- gsub(",", ";", gulf.utils::deblank(x$cat$comment))
   x$cat <- sort(x$cat, by = key.cat)
   
   # Create biological data frame:
   x$bio$date <- x$set$date[match(x$bio$set.number, x$set$set.number)]
   x$bio$not.intact[is.na(x$bio$not.intact)] <- 0
   x$bio$length.unit <- "cm"
   x$bio$length.unit[x$bio$species %in% c(2513, 2518, 2520, 2521, 2523, 2526, 2527, 2550, 4321, 4322, 4349, 2539, 6411, 4211)] <- "mm"
   x$bio$length.interval <- 1
   x$bio$measurement.type <- 2
   if (survey %in% c("rvs" , "nss")){
      x$bio$length.interval[x$bio$species == 60] <- 0.5
      x$bio$measurement.type[x$bio$species == 60] <- 1
   }
   x$bio$weight.unit <- "g"
   x$bio$comment[is.na(x$bio$comment)] <- ""
   x$bio$comment <- gsub(",", ";", gulf.utils::deblank(x$bio$comment))
   x$bio <- sort(x$bio, by = key.bio)

   # Remove irrelevant or redundant variables:
   remove <- "mission"
   for (i in 1:length(x)) x[[i]] <- x[[i]][, setdiff(names(x[[i]]), remove)]
   
   # Re-order variables:
   x$set <- x$set[c(key.set, setdiff(names(x$set), key.set))]
   x$cat <- x$cat[c(key.cat, setdiff(names(x$cat), key.cat))]
   x$bio <- x$bio[c(key.bio, setdiff(names(x$bio), key.bio))]
   
   return(x)
}

#' @describeIn gse Convert GSE tables to a Gulf survey data object.
#' @export
gse2gsd <- ese2gsd

#' @describeIn gse Convert from to GSD to Oracle import format.
#' @export
gsd2oracle <- function(x, survey = "rv"){
   # Define index keys:
   key.set <- c("date", "cruise", "set.number")
   key.cat <- c("date", "cruise", "set.number", "species", "size.class")
   key.bio <- c("date", "cruise", "set.number", "species", "size.class", "specimen")
   key.len <- c("date", "cruise", "set.number", "species", "size.class", "sex", "start.length")
   y <- x

   # Build catch card information from bio card:
   catch <- x$cat

   # Calculate number measured for length:
   temp <- stats::agregate(list(number.length = rep(1,dim(x$bio)[1])), x$bio[, key.cat], sum)
   catch <- merge(catch, temp, by = key.cat, all.x = TRUE)

   # Calculate number measured for weight:
   temp <- x$bio[!is.na(x$bio[, "weight"]), ]

   if(dim(temp)[1] > 0){
      temp <- stats::aggregate(list(number.weight = rep(1,dim(temp)[1])), temp[, key.cat], sum)
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
