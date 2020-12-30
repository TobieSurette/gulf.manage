# 2019 NS survey

load.ese <- function(survey){

  survey="ns"

  x = read.ESE(database = 'dtran',  user = 'GLF_ESE',  password= 'shotgun',  schema = 'glf_ese', mission='PER2019140')
  #hack - off network
#    load(file = "E:/work/HD2_repositorywc/northumberland/corrections/ns2019.rda")
#    x =ns2019
  #hack - off network
  temp = read.csv(file="E:/work/HD2_repositorywc/northumberland/corrections/2019/NSSurvey_Corrections_2019.Apr20.csv", stringsAsFactors = FALSE)
  temp = temp[,c(1,3,4,5,6,7,9)]
  names(temp) = c("year", "set.number","card.type", "species", "fish.number", "x","val")

#temp = spread(temp, x , val)
set.temp = temp[temp$card.type == 5, ]

for(i in 1:dim(set.temp)[1]){
  index = set.temp[i,]$set.number == x$set$SETNO

  if(set.temp[i,]$x == "latitude.end")
  {
    print(paste0("changing ELAT to : ",set.temp[i,]$val, " for set #", set.temp[i,]$set.number))
    x$set[which(index),]$ELAT = deg2dmm(set.temp[i,]$val)
  }
  if(set.temp[i,]$x == "latitude.start")
  {
    print(paste0("changing SLAT to : ",set.temp[i,]$val, " for set #", set.temp[i,]$set.number))
    x$set[which(index),]$SLAT = deg2dmm(set.temp[i,]$val)
  }
  if(set.temp[i,]$x == "longitude.end"){
    print(paste0("changing ELONG to : ",set.temp[i,]$val, " for set #", set.temp[i,]$set.number))
    x$set[which(index),]$ELONG = deg2dmm(set.temp[i,]$val)
  }
  if(set.temp[i,]$x == "longitude.start"){
    print(paste0("changing SLONG to : ",set.temp[i,]$val, " for set #", set.temp[i,]$set.number))
    x$set[which(index),]$SLONG = deg2dmm(set.temp[i,]$val)
  }
  if(set.temp[i,]$x == "speed"){
    print(paste0("changing SPEED to : ",set.temp[i,]$val, " for set #", set.temp[i,]$set.number))
    x$set[which(index),]$SPEED = set.temp[i,]$val
  }
  if(set.temp[i,]$x == "experiment"){
    print(paste0("changing experiment to : ",set.temp[i,]$val, " for set #", set.temp[i,]$set.number))
    x$set[which(index),]$EXPERIMENT_TYPE_CODE = set.temp[i,]$val
  }
  if(set.temp[i,]$x == "gear"){
    print(paste0("changing gear to : ",set.temp[i,]$val, " for set #", set.temp[i,]$set.number))
    x$set[which(index),]$GEAR = set.temp[i,]$val
  }
  #  if(set.temp[i,]$x == "warp.out.port")
  #  if(set.temp[i,]$x == "warp.out.starboard")
}

  catch.temp  =temp[temp$card.type == 6, ]
  for(i in 1:dim(catch.temp)[1]){
    index = catch.temp[i,]$set.number == x$basket$SETNO  & catch.temp[i,]$species == x$basket$SPEC
    if(catch.temp[i,]$x == "weight.sampled"){
      print(paste0("changing BASKET_WEIGHT to : ",catch.temp[i,]$val, " for set #", catch.temp[i,]$set.number))
      x$basket[which(index),]$BASKET_WEIGHT = catch.temp[i,]$val
    }

  }

  bio.temp  =temp[temp$card.type == 8, ]
  for(i in 1:dim(bio.temp)[1]){
    index = bio.temp[i,]$set.number == x$detail$SETNO  & bio.temp[i,]$species == x$detail$SPEC & bio.temp[i,]$fish.number == x$detail$SPECIMEN_ID
    if(bio.temp[i,]$x == "length"){
      print(paste0("changing LENGTH to : ",bio.temp[i,]$val, " for set #", bio.temp[i,]$set.number))
      x$detail[which(index),]$LENGTH = bio.temp[i,]$val
    }
    if(bio.temp[i,]$x == "sex"){
      print(paste0("changing SEX to : ",bio.temp[i,]$val, " for set #", bio.temp[i,]$set.number))
      x$detail[which(index),]$SEX = bio.temp[i,]$val
    }
    if(bio.temp[i,]$x == "egg.condition"){
      print(paste0("changing LOBSTER_EGG_CONDITION to : ",bio.temp[i,]$val, " for set #", bio.temp[i,]$set.number))
      x$detail[which(index),]$LOBSTER_EGG_CONDITION = bio.temp[i,]$val
    }
  }

  #CORRECTION TO RAW ESE DATA TO GO HERE
  index = x$set$AREA == 999
  x$set[which(index),]$AREA = NA

  return(x)
}
