createCards <- function(x, survey){
   
   Y = ese2gsd (x, survey=survey)
   Z = gsd2card(Y, survey=survey)
   
   return(Z)
}


