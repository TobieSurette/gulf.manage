load.ese <- function(survey){
   
  # read eSE Oracle data

    x = read.ESE(database = 'dtran',  user = 'GLF_ESE',  password= 'shotgun',  schema = 'glf_ese', mission='PER2019153')  
    return(x)
}

# 

