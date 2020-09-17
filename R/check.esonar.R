#' Check \strong{eSonar} Data
#' 
#' @description Checks an 'esonar' object for errors.

#' @export
check.esonar <- function(x, key = TRUE, coordinates = TRUE, time = TRUE, match = TRUE, ...){
   msg <- NULL

   if (key)             msg <- c(msg, verify.key(x, key = c("year", "month", "day", "hour", "minute", "second"), ...))
   if (coordinates)     msg <- c(msg, verify.coordinates(x, ...))
   if (time)            msg <- c(msg, verify.time(x, ...))
   if (match)           msg <- c(msg, verify.match(x, ...))

   return(msg)
}

#' @export check.coordinates.esonar 
check.coordinates.esonar <- function(x, ...){
   # CHECK.COORDINATES - Check coordinates for a 'esonar' object.
   msg <- NULL

   if (!is.null(header(x))){
      # Check for missing values:
      vars <- c("year", "month", "day")
      res <- aggregate(x[c("longitude", "latitude")], by = x[vars], function(x) sum(is.na(x)))
      index <- which((res$longitude > 0) | (res$latitude > 0))
      temp <- paste("There are missing coordinates in '", info(x)$tow.id, "' .", sep = "")
      msg <- c(msg, msg.str(res[index, ], msg = temp, key = vars,  ...))

      # Check coordinates:
      n <- dim(x)[1]
      d <- distance(x$longitude[1:(n-1)], x$latitude[1:(n-1)], x$longitude[2:n], x$latitude[2:n], pairwise = FALSE)*1000
      m <- max(d[2:(length(d)-1)], na.rm = TRUE)

      if (m > 50){
         msg <- c(msg, paste("There was a coordinate skip in ", info(x)$tow.id, " whose distance was ", round(m, 1), " meters.", sep = ""))
      }
   }else{
      # Check for missing values:
      vars <- c("year", "month", "day", "tow.id", "tow.number")
      res <- aggregate(x[c("longitude", "latitude")], by = x[vars], function(x) sum(is.na(x)))
      index <- which((res$longitude > 0) | (res$latitude > 0))
      temp <- paste("There are missing coordinates.", sep = "")
      msg <- c(msg, msg.str(res[index, ], msg = temp, key = vars,  ...))

      # Check coordinates:
      n <- dim(x)[1]
      d <- c(NA, distance(x$longitude[1:(n-1)], x$latitude[1:(n-1)], x$longitude[2:n], x$latitude[2:n], pairwise = FALSE))*1000
      fun <- function(x){
         return(max(x[2:(length(x)-1)], na.rm = TRUE))
      }
      temp <- aggregate(d, by = x[c("tow.id", "tow.number")], fun)

      index <- (temp[, 3] > 50)
      str <- paste("There was a coordinate skip whose distance was ", round(temp[index, 3], 1), " meters.", sep = "")
      msg <- c(msg, msg.str(temp[index, ], msg = str, key = c("tow.id", "tow.number")),  ...)
   }

   return(msg)
}

# VERIFY.TIME - Checks a 'esonar' object time errors.
#' @export check.time.esonar
check.time.esonar <- function(x, ...){
   msg <- NULL

   # Check time values:
   msg <- c(msg, verify.time.default(x))

   index <- which(diff(time(x)) != 1)
   fun <- function(x){ return(max(diff(x))) }
   temp <- aggregate(time(x), by = x["file.name"], fun)
   index <- which(temp[, 2] > 1)
   if (length(index) > 0)
      msg <- paste("E-sonar file '", temp[index, 1], "' contains a missing data block of ", temp[index, 2], " seconds.", sep = "")

   return(msg)
}

# .MATCH.ESONAR - Checks that a 'esonar' object's entries have matches in a target object.
#' @export check.key.esonar
check.key.esonar <- function(x, set.card, ...){
   msg <- NULL

   # Define set card if missing:
   if (missing(set.card)) set.card <- oracle.read.scset(year = unique(x$year))

   # Match indices using various methods:
   res <- as.data.frame(matrix(NA, nrow = dim(x)[1], ncol = 3))
   names(res) <- c("latlon", "file.name", "time")
   res$latlon    <- match(x, set.card, method = "latlon")
   res$file.name <- match(x, set.card, method = "file.name")
   res$time      <- match(x, set.card, method = "time")

   # Check coordinate versus file match:
   if (!is.null(header(x))) vars <- c("year", "month", "day") else vars <- c("year", "month", "day", "tow.number", "file.name")

   index <- which(is.na(res[, 1]) & !is.na(res[, 2]))
   temp <- unique(x[index, vars])
   str <- paste("Coordinate match did not work.", sep = "")
   msg <- c(msg, msg.str(temp, msg = str, key = vars, ...))

   # Check for matches with no file matches:
   index <- which((!is.na(res[, 1]) | !is.na(res[, 3])) & is.na(res[, 2]))
   temp <- unique(x[index, vars])
   str <- paste("File does not exist in set card but there are coordinate or time matches.", sep = "")
   msg <- c(msg, msg.str(temp, msg = str, key = vars, ...))

   # Check for differing matches:
   index <- which(!is.na(res[, 1]) & !is.na(res[, 2]) & (res[, 1] !=  res[, 2]))
   temp <- unique(x[index, vars])
   str <- paste("Coordinate and file macthes are different.", sep = "")
   msg <- c(msg, msg.str(temp, msg = str, key = vars, ...))

   # Check for differing matches:
   index <- which(!is.na(res[, 1]) & !is.na(res[, 3]) & (res[, 1] !=  res[, 3]))
   temp <- unique(x[index, vars])
   str <- paste("Coordinate and time macthes are different.", sep = "")
   msg <- c(msg, msg.str(temp, msg = str, key = vars, ...))

   # Check for differing matches:
   index <- which(!is.na(res[, 2]) & !is.na(res[, 3]) & (res[, 2] !=  res[, 3]))
   temp <- unique(x[index, vars])
   str <- paste("File and time macthes are different.", sep = "")
   msg <- c(msg, msg.str(temp, msg = str, key = vars, ...))

   # Check that all set card entries have a matching Netmind:
   index <- setdiff(1:dim(set.card)[1], unique(res$file.name, na.rm = TRUE))
   str <- paste("Set file has no corresponding E-Sonar file.", sep = "")
   msg <- c(msg, msg.str(set.card[index, ], msg = str, var = "file.name",  ...))

   return(msg)
}
