# 2018 NS survey

load.ese <- function(survey){
  
  survey="ns"

  x = read.GSE(database = 'dtran',  user = 'vergarap',  password= 'shotgun',  schema = 'glf_gse_archive', mission='PER2018041')
   
  index = x$set$SETNO ==1
  x$set = x$set[-which(index),]
  
  index = x$set$SETNO ==104
  x$set = x$set[-which(index),]
  
  
  
  
  # ******** START ******* Natalie Asselin corrections 
  index = x$set$SETNO == 3 
  x$set[index,]$SLAT = deg2dmm(46.43466667)
  
  index = x$set$SETNO == 15
  x$set[index,]$SLAT = deg2dmm(46.9055)
  
  index = x$set$SETNO == 40
  x$set[index,]$SLAT = deg2dmm(46.35952374)
  
  index = x$set$SETNO == 46
  x$set[index,]$ELONG = deg2dmm(63.625)
  
  index = x$set$SETNO == 61
  x$set[index,]$SLAT = deg2dmm(45.9005)
  index = x$set$SETNO == 61
  x$set[index,]$ELAT = deg2dmm(45.911)
  
  index = x$set$SETNO == 70
  x$set[index,]$SLONG = deg2dmm(63.10483416)
  
  
  index = x$basket$SETNO == 45 & x$basket$SPEC == 9991
  x$basket[index,]$SPEC = 110
  index = x$catch$SETNO == 45 & x$catch$SPEC == 9991
  x$catch[index,]$SPEC = 110
  index = x$detail$SETNO == 45 & x$detail$SPEC == 9991
  x$detail[index,]$SPEC = 110
  
  index = x$basket$SETNO == 62 & x$basket$SPEC == 9991
  x$basket[index,]$SPEC = 2541
  index = x$catch$SETNO == 62 & x$catch$SPEC == 9991
  x$catch[index,]$SPEC = 2541

  index = x$catch$SETNO == 32 & x$catch$SPEC == 43
  x$catch[index,]$SPEC_COMMENT = "weight ok , no length measurements due to mixing of data (error)    "  
  index = x$basket$SETNO == 32 & x$basket$SPEC == 43
  x$basket[index,]$SAMPLED = "N"  
  
  index = x$basket$SETNO == 46 & x$basket$SPEC == 2212
  x$basket[index,]$SAMPLED = "N"  
  
  index = x$basket$SETNO == 56 & x$basket$SPEC == 9300
  x$basket[index,]$SAMPLED = "N"  
  
  index = x$basket$SETNO == 70 & x$basket$SPEC == 6110
  x$basket[index,]$SAMPLED = "N"  
  
  index = x$basket$SETNO == 70 & x$basket$SPEC == 8511
  x$basket[index,]$SAMPLED = "N"  
    
  index = x$basket$SETNO == 83 & x$basket$SPEC == 42
  x$basket[index,]$BASKET_WEIGHT = 3.202
    
  index = x$basket$SETNO == 84 & x$basket$SPEC == 6121
  x$basket[index,]$BASKET_WEIGHT = 0.198
  
  
  

  
 index = x$detail$SETNO == 3 & x$detail$FISHNO == 537 & x$detail$SPEC == 2550
 x$detail[index,]$SEX =2
 
 index = x$detail$SETNO == 3 & x$detail$FISHNO == 557 & x$detail$SPEC == 2550
 x$detail[index,]$SEX =2
 
 index = x$detail$SETNO ==7 & x$detail$FISHNO == 1128 & x$detail$SPEC == 2550
 x$detail[index,]$DET_COMMENT = ""
 
 index = x$detail$SETNO ==7 & x$detail$FISHNO == 1131 & x$detail$SPEC == 2550
 x$detail[index,]$DET_COMMENT = ""
 
 index = x$detail$SETNO ==7 & x$detail$FISHNO == 1132 & x$detail$SPEC == 2550
 x$detail[index,]$DET_COMMENT = ""
 
 index = x$detail$SETNO ==21 & x$detail$FISHNO == 1582 & x$detail$SPEC == 2550
 x$detail[index,]$DET_COMMENT = "0"
 
 index = x$detail$SETNO == 21 & x$detail$FISHNO == 1726 & x$detail$SPEC == 2550
 x$detail[index,]$SEX = 2
 
 index = x$detail$SETNO ==23 & x$detail$FISHNO == 2498 & x$detail$SPEC == 2550
 x$detail[index,]$SEX = 2
 
 index = x$detail$SETNO ==23 & x$detail$FISHNO == 2249 & x$detail$SPEC == 2550
 x$detail[index,]$DET_COMMENT = ""
 
 index = x$detail$SETNO ==30 & x$detail$FISHNO == 3831 & x$detail$SPEC == 2550
 x$detail[index,]$AGE_MAT =1
 
  
 index = x$detail$SETNO ==41 & x$detail$FISHNO == 68 & x$detail$SPEC == 2539   
 x$detail[index,]$DET_COMMENT = ""
 
 index = x$detail$SETNO ==80 & x$detail$FISHNO == 7793  & x$detail$SPEC == 2550   
 x$detail[index,]$DET_COMMENT = "0"
 index = x$detail$SETNO ==82 & x$detail$FISHNO == 8093 & x$detail$SPEC == 2550   
 x$detail[index,]$DET_COMMENT = ""
 index = x$detail$SETNO ==92 & x$detail$FISHNO == 8150 & x$detail$SPEC == 2550   
 x$detail[index,]$DET_COMMENT = ""
 
 index = x$detail$SETNO == 32 & x$detail$SPEC == 43
 x$detail = x$detail[!index,]
 
 #All fish from that set for species 4511 to be removed. Keeping one record to change lenght top 13 after delete
 index = x$detail$SETNO == 75 & x$detail$SPEC == 4511 & x$detail$LENGTH < 25
 x$detail = x$detail[!index,]
  index = x$detail$SETNO == 75 & x$detail$SPEC == 4511
 x$detail[which(index),]$LENGTH = 13
 
 index = x$detail$SETNO == 75 & x$detail$SPEC == 3500
 x$detail = x$detail[!index,]
 # ******** END ******* Natalie Asselin corrections 
 # ******** START ******* Natalie/Patty corrections 
 
 #no jelly fish were sampled
 index =x$detail$SPEC == 8500
 x$detail = x$detail[!index,]
 index =x$basket$SPEC == 8500
 x$basket[index,]$SAMPLED = "N"
 
 index = x$detail$SETNO == 30 & x$detail$SPEC ==2550 & x$detail$FISHNO == 3759
 x$detail = x$detail[!index,]
 index = x$detail$SETNO == 30 & x$detail$SPEC ==2550 & x$detail$FISHNO == 3850
 x$detail = x$detail[!index,]
 index = x$detail$SETNO == 82 & x$detail$SPEC ==2550 & x$detail$FISHNO == 8094
 x$detail = x$detail[!index,]
 
 
 #
 # ******** END ******* Natalie/Patty corrections 
 
 
  index = x$set$SETNO == 65
  x$set[index,]$START_DEPTH = 13
  x$set[index,]$END_DEPTH =12
  
  index = x$detail$SPEC %in% c(40,42,43) & x$detail$LENGTH < 13 & x$detail$SEX == 0

  x$detail = set(x$detail, index,  SEX = 9)


  
  
  
  
  
  
  
  
   y = gse2gsd(x, survey=survey)
   
   # they do not take distance on board, must be calculated


   z = gsd2card(y, survey=survey)
   
   #comment for spec 204 to be put in as disc width
   index = z$bio$species == 204
   z$bio[index, ]$disc.width = as.numeric(z$bio[index,]$comment)
   
   
   z$set$distance = distance(z$set, method="latlon")
   
   #for egg condition and shell condition
   species.list=  c(2513,2518, 2521, 2526,2527, 2539,2550)
   
   index = z$bio$species %in%  species.list
   
   z$bio[index,"shell.condition"] = z$bio[index, "maturity"]
   z$bio[index, "maturity"] = NA
   
   
   #for this subset, replace NA or empty comment field with 0 for  egg.condition (for female for the spieces list set above)
   trim <- function (x) gsub("^\\s+|\\s+$", "", x)              
   index = z$bio$species %in% species.list & z$bio$sex ==2 & nchar(trim(z$bio$comment)) == 0
   z$bio[which(index),"egg.condition"] = 0
   
   index = z$bio$species %in% species.list & z$bio$sex ==2 & !is.na(as.integer(z$bio$comment)) 
   z$bio[which(index),"egg.condition"] = as.integer(z$bio[which(index), "comment"])
   
   
   
   
   
   #   index = (!is.na(z$bio$species) & (z$bio$species %in% c(40,42,43)) & z$bio$length < 13 & z$bio$sex == 0)
   #  z$bio[index ,"sex"] = 9
   
   
   # Stratum needs to be calculated
   z$set <- set(z$set, NULL, stratum = stratum(z$set,method = "latlong"))
   # Block numbers need to be calculated
   z$set$block.number <- block.number(longitude(z$set), latitude(z$set))
   

   # Remove all letters and symbols from comments 
   # The first three numbers are always the station numbers, any other number in that striong can be ignored 
    z$set$station.number = substr(gsub("[^0-9]", "", z$set$comment),1,3)
   
   #special case
   index = z$set$station.number =="1"
    z$set = set(z$set, index, station.number = "B1")
  
  # need to reset number caught as per NAtalie Asselin request
    index = z$cat$species == 2550 & z$cat$set.number == 30
    z$cat[index,]$number.caught = z$cat[index,]$number.caught +2
  
    index = z$cat$species == 2550 & z$cat$set.number == 82
    z$cat[index,]$number.caught = z$cat[index,]$number.caught +1
    
  # end 
    
  index = z$bio$shell.condition == 0
  z$bio = set(z$bio, index, shell.condition  = NA)
  # merge set back into other cards
  z$cat = merge(z$cat, z$set, by=key(z$set), all.x=TRUE,overwrite = TRUE, names= setdiff(names(z$cat)[which(names(z$cat) %in% names(z$set))], c("card.type","comment")))
  z$bio = merge(z$bio, z$set, by=key(z$set), all.x=TRUE,overwrite = TRUE, names= setdiff(names(z$bio)[which(names(z$bio) %in% names(z$set))], c("card.type","comment")))
  z$len = merge(z$len, z$set, by=key(z$set), all.x=TRUE,overwrite = TRUE, names= setdiff(names(z$len)[which(names(z$len) %in% names(z$set))], c("card.type","comment")))

  setwd("E:/work/HD2_repositorywc/northumberland")

  write(z$set, file = "rvp041s.new")
  write(z$cat, file = "rvp041c.new")
  write(z$bio, file = "rvp041b.new")
  write(z$len, file = "rvp041l.new")
  
  
   
   return(z)
   
}