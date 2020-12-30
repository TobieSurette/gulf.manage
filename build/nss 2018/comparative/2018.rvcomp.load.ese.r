load.ese <- function(survey){
   
  # read eSE Oracle data

    x = read.ESE(database = 'dtran',  user = 'GLF_ESE',  password= 'shotgun',  schema = 'glf_ese', mission='PER2018029')  
    x$set = x$set[-dim(x$set)[1],]
    index = x$set$SETNO == 5
    x$set[index,]$SLAT = 4551.48
    
    #unit area were not correct, willsimply set to NA
    x$set$AREA = NA
    x$set$GEAR = 13
    return(x)
}

# 

