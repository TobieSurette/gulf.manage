#' Check Index Key
#' 
#' @description Checks whether a data frame's index key is unique or contains any missing values. 
#'              An index key may be a single or combination of variables which uniquely identify data 
#'              rows in a table. Index keys are important for merging data tables.
#'              
#' @param x Object.
#' @param key Character string specifying an index key.
#' @param echo Logical value specifying whether to print error messages to the R console.
#'  
#' @examples 
#' # Vector example:
#' x <- rpois(20, 5)
#' check.key(x) # Checks for repeat entries.
#' 
#' x <- data.frame(year = sample(2010:2014, 20, replace = TRUE),
#'                 site = sample(LETTERS[1:5], 20, replace = 20),
#'                 count = rpois(20, 5),
#'                 value = rnorm(20))
#' x$year[sample(1:nrow(x), 3)] <- NA
#' check.key(x, key = c("year", "site"))
#' 
#' @return A character vector of error messages (invisible). 

# #' @describeIn check.key Generic \code{check.key} method.
#' @export check.key
check.key <- function(x, ...) UseMethod("check.key")

# #' @describeIn check.key Default \code{check.key} method. Checks that entries are unique.
#' @rawNamespace S3method(check.key,default)
check.key.default <- function(x, echo = TRUE, ...){
   v <- NULL
   if (is.null(dim(x))){
      t <- table(x)
      t <- t[t > 1]
      if (length(t) > 0) v <- msg(names(t), paste0(" has ", as.numeric(t), " repeat entries."), echo = echo, ...)
   }
   invisible(v)
}

# #' @describeIn check.key Checks that data frame variable entries are unique.
#' @rawNamespace S3method(check.key,data.frame)
check.key.data.frame <- function(x, key, ...){
   v <- NULL

   # Check 'key' argument:
   if (missing(key)) if (!is.null(attr(x, "key"))) key <- attr(x, "key") 
   if (missing(key)) stop("'key' argument must be specified.")
   if (any(is.na(key))) stop("'key' cannot contain NA values.")
   
   if (!all(key %in% names(x))) stop("Some index key variables are not variables in 'x'.")

   # Check for missing values in the variables of the index key:
   index <- apply(is.na(x[key]), 1, sum) > 0 
   m <- msg(x[index, ], "Index key contains a missing value.", key = key, ...)
   v <- c(v, m)  
   
   # Remove lines with NA values in its index key:
   x <- x[!index, ]

   # Check if key of 'x' is unique:
   index <- which(duplicated(x[key]))
   index <- !is.na(match(x[key], unique(x[index, key])))
   
   m <- msg(x[index, ], "Duplicate index key found.", key = key, ...)
   v <- c(v, m)
 
   return(v)
}
