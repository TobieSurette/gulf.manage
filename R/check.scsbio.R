#' Check Snow Crab Biological Data
#' 
#' @description Functions for validating \code{scsbio} data objects for issues.
#' 
#' @param x Object.
#' 
#' @export check
#' 
#' @section Functions:
#' \describe{
#'   \item{}
#' }
#' 
#' @examples 
#' check(read.scsbio(2020))

#' @rdname check
#' @export
check.scsbio <- function(x, ...){
   # Males with female information:
   index <- which(x$sex == 1 & (!is.na(x$abdomen.width) | !is.na(x$gonad.colour) | !is.na(x$egg.colour)| !is.na(x$eggs.remaining)))
   if (length(index) > 0) cat(paste0("   Males with female observations : ", length(index), "\n"))

   # Females with male information:
   index <- which(x$sex == 2 & !is.na(x$chela.height))
   if (length(index) > 0) cat(paste0("   Females with male observations : ", length(index), "\n"))

   # Gonad measurement with mature measures:
   index <- which((!is.na(x$egg.colour) | !is.na(x$eggs.remaining)) & !is.na(x$gonad.colour))
   if (length(index) > 0) cat(paste0("   Females with mixtures of gonad and egg measurements : ", length(index), "\n"))

   # Missing carapace width measurement:
   index <- which(is.na(x$carapace.width) | x$carapace.width == 0)
   if (length(index) > 0) cat(paste0("   Crab with no carapace width measurement : ", length(index), "\n"))

   # Missing zero chela beight measurement:
   index <- which(x$chela.height == 0)
   if (length(index) > 0) cat(paste0("   Crab with 0 chela height measurement : ", length(index), "\n"))
   
   # Missing shell condition measurement:
   index <- which(is.na(x$shell.condition))
   if (length(index) > 0) cat(paste0("   Crab with no shell condition : ", length(index), "\n"))

   # Number of NA rows::
   index <- which(is.na(rownames(x)))
   if (length(index) > 0) cat(paste0("   Number of NA rows : ", length(index), "\n"))

   # Missing chela measurement when there should be one:
   index <- which(is.na(x$chela.height) & (x$sex == 1) & (x$carapace.width >= 40))
   if (length(index) > 0) cat(paste0("   Males with missing chela mesurements : ", length(index), "\n"))

   # Missing chela measurement when there should for legal sizes:
   index <- which(is.na(x$chela.height) & (x$sex == 1) & (x$carapace.width >= 95))
   if (length(index) > 0) cat(paste0("   Legal-sized males with no chela measurements : ", length(index), "\n"))

   # Check for females with no gonad or egg information:
   index <- which(x$sex == 2 & apply(is.na(x[,c("gonad.colour", "egg.colour", "eggs.remaining")]), 1, sum) == 3)
   if (length(index) > 0) cat(paste0("   Females with no gonad or egg observations : ", length(index), "\n"))

   # Check for females with no eggs remaining :
   index <- which(x$eggs.remaining == 0)
   if (length(index) > 0) cat(paste0("   Females with no eggs remaining : ", length(index), "\n"))

   # Check date field:
   index <- which(is.na(x$date)| gulf.utils::deblank(x$date) == "")
   if (length(index) > 0) cat(paste0("   Records with missing date information : ", length(index), "\n"))

   # Check tow number:
   index <- which(is.na(x$tow.number))
   if (length(index) > 0) cat(paste0("   Records with missing tow number : ", length(index), "\n"))

   # Check tow ID:
   index <- which(is.na(x$tow.id) | gsub(" ", "", x$tow.id) == "")
   if (length(index) > 0) cat(paste0("   Records with emtpy tow ID : ", length(index), "\n"))

   # Check tow ID from tow data against that found in the biological data:
   if ("year" %in% names(x)) x$date <- as.character(date(x))
   s <- read.scsset(year = as.numeric(unique(substr(x$date,1,4))))
   vars <- c("date", "tow.number")
   tows <- unique(x[vars])
   index <- match(tows, s[vars])
   if (any(is.na(index))){
      index <- which(is.na(index))
      str <- NULL
      for (i in 1:length(index)){
         str <- c(str, paste0("   Tow on date = ", tows$date[index[i]], ", tow number = ", tows$tow.number[index[i]], " in the biological data was not found in the tow database.\n"))
      }
      cat(str)
   }

   # Check that tow IDs match between biological and tow data:
   vars <- c("date", "tow.number", "tow.id")
   tows <- unique(x[vars])
   index <- match(tows, s[vars])
   index <- which(is.na(index))
   if (length(index) > 0){
      for (i in 1:length(index)){
         cat(paste0("   Tow ID : '", tows$tow.id[index[i]], "' in the biological data was not found in the tow database.\n"))
      }
   }

   # Check that there is no biological data associated with non-valid tows:
   vars <- c("date", "tow.number")
   tows <- unique(x[vars])
   index <- match(tows, s[vars])
   index <- which(s$valid[index] != 1)
   if (length(which(s$valid[index] != 1)) > 0)
     cat(paste0("   Tow number : '", tows$tow.number[index], "' on ", tows$date[which(s$valid[index] != 1)], " corresponds to an invalid tow.\n"))
}
