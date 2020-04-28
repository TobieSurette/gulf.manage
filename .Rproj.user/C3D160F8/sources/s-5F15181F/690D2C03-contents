#' Return Message Strings
#' 
#' @description Functions to build and report message strings.
#' 
#' @param x Object.
#' @param msg Character string specifying the message to be returned.
#' @param key Character string specifying an index key.
#' @param names Character string specifying variable names to be included in the output string.
#' @param echo Logical value specifying whether to print strings to the R console.
#' 
#' @examples 
#' x <- data.frame(year = 2010:2014, site = LETTERS[1:5])
#' key(x) <- "year"  # Define index key.
#' 
#' # Console output:
#' msg(x, "Sampling was performed.", echo = TRUE)
#' msg(x, paste("Record number", 1:nrow(x), "is ok."), echo = TRUE)
#' 
#' # String output:
#' m <- msg(x, "Sampling was performed.")
#' 
#' @export msg
#' @export msg.default
#' @export msg.data.frame
#' 
#' @return Character string of concatenated data fields and messages (invisible).
#' 
msg <- function(x, ...) UseMethod("msg")

#' @describeIn msg Default msg method.
msg.default <- function(x, msg = "", echo = FALSE, ...){
   if (is.null(dim(x))){
      v <- paste0("'", x, "'")
      if (any(msg != "")) v <- paste0(v, " = '", msg, "'")
   }else{
      v <- msg(as.data.frame(x), msg, ...)
   }
   if (!echo){
      return(v)
   }else{
      cat(paste0(v, "\n"))
      invisible(v)
   }
}

#' @describeIn msg Generate message from data frame entries.
msg.data.frame <- function(x, msg = "", key, names, echo = FALSE, ...){
   # Convenience functions:
   q <- function(x) return(paste0("'", x, "'"))  # Add quotes.
   f <- function(x) if (is.factor(x) | is.character(x)) return(q(x)) else return(x) # Format.
   
   # Check 'x' argument:
   if (any(dim(x)== 0)) return(NULL)

   # Initialize variables:
   vars <- NULL
       
   # Check that 'key' is properly defined:
   if (missing(key)) if (!is.null(attr(x, "key"))) key <- attr(x, "key")
   if (!missing(key)){
      if (!is.character(key)) stop("Index key must be a character string.")
      if (!all(key %in% colnames(x))) stop("Some index key variables are not in 'x'.")
   }
    
   # Check that 'names' is properly defined:
   if (missing(names) & missing(key)) names <- colnames(x)
   if (missing(names) & !missing(key)) names <- setdiff(colnames(x), key)
   if (!missing(names)){
      if (!is.character(names)) stop("Variable 'names' must be a character string.")
      if (!all(names %in% names(x))) stop("Some variable 'names' are not in 'x'.")
   }   

   # Convert named fields to strings:
   names.str <- NULL
   if (!missing(names)){
      for (i in 1:length(names)) names.str <- cbind(names.str, paste0(names[i], " = ", f(x[, names[i]])))
      colnames(names.str) <- names
      names.str <- apply(names.str, 1, paste0, collapse = ", ")
   }
   
   # Convert key index fields to strings:
   key.str <- NULL
   if (!missing(key)){
      for (i in 1:length(key)) key.str <- cbind(key.str, paste0(key[i], " = ", f(x[, key[i]])))
      colnames(key.str) <- key
      key.str <- apply(key.str, 1, paste0, collapse = ", ")
      key.str <- paste0("[", key.str, "]")       
   }
  
   # Concatenate:
   v <- paste0(names.str, " : ", msg) 
   if (!missing(key)) v <- paste0(key.str, ", ", v)
      
   # Print to R console:
   if (echo) cat(paste0(v, "\n"))

   invisible(v)
}
