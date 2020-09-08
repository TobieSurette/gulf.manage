library(gulf.data)

x <- read.scsset(2019)

x$tow.id

check.tow.id.scsset <- function(x){
   regular <- grep("^G[CP][0-9][0-9][0-9][F][R]*", x$tow.id)
   alternates <- grep("^G[CP][0-9][0-9][0-9][A][1-3]*", x$tow.id)

   # Check that tow IDs follows the standard nomenclature:
   index <- setdiff(1:nrow(x), c(regular, alternates))
   if (length(index) > 0){
      cat("Tows '", paste0(x$tow.id[index], collapse = "', '"), "' do not follow the standard snow crab station nomenclature.\n") 
   }
   
   # Determine which tows are missing on from a list of tow numbers on a given day:
   r <- aggregate(x["tow.number"], by = list(date = date(x)), function(x) setdiff(1:max(x), sort(x)))
   r <- r[unlist(lapply(r$tow.number, length)) > 0, ]
   if (nrow(r) > 0){
      for (i in 1:nrow(r)){
         cat(paste0("Date = '", as.character(r$date[i]), "', missing tow number(s) ", paste(r$tow.number[[i]], collapse = ", "),  "\n"))
      }
   }
   
   # Check that tow ID of the last valid tow at station is uniquely valid:
   
   c("F", "FR", "A1", "A2", "A3")  # Sequence of tow ID extensions.
   
}
