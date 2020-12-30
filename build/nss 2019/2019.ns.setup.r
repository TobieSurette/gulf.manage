  year = 2019
  cruise.num = "140"
  survey="ns"
  file.survey.prefix = "rv"
  vessel = "p"
  setwd("E:/work/HD2_repositorywc/northumberland")
  
  source(paste0('O:/research/northumberland/corrections/',year,'/',year,'.ns.load.ese.r'), echo=TRUE)
  source(paste0('O:/research/northumberland/corrections/',year,'/',year,'.ns.create.gulf.cards.r'), echo=TRUE)
  source('O:/research/groundfish/corrections/misc scripts/verify.functions.rv.r', echo=TRUE)
  library(stringr)
  
  correction.file = paste0(year,"corrections.log")
  correction.path = paste(.gulf.path$rv, paste0("corrections/",year,"/logs/"), sep="")
  
  # run this line to reset log file
  gulf.fprint(text = paste("Reset log file", sep=""), file = correction.file, path = correction.path, overwrite = TRUE)
  
  x = load.ese(survey)
  Z = createCards(x, survey)
  
  # Z$bio = special.bio.data(Z$bio)
  
  Z$set$cruise.number = cruise.num
  Z$len$cruise.number = cruise.num
  Z$bio$cruise.number = cruise.num
  Z$cat$cruise.number = cruise.num
  
  # Load when I get them
  #Z = load.temperature(Z)
  #Z = load.ctd(Z)
  
  #load csv with corrections again as we need to pull the warpout corrections that can't go in with ESE
  temp = read.csv(file="E:/work/HD2_repositorywc/northumberland/corrections/2019/NSSurvey_Corrections_2019.Apr20.csv", stringsAsFactors = FALSE)
  temp = temp[,c(1,3,4,5,6,7,9)]
  names(temp) = c("year", "set.number","card.type", "species", "fish.number", "x","val")
  
  index =  temp$x == "warp.out.starboard" | temp$x == "warp.out.port"
  temp = temp[which(index),]
  
  for(i in 1:dim(temp)[1]){
    index = temp[i,]$set.number == Z$set$set.number
    if(temp[i,]$x == "warp.out.port")
    {
      print(paste0("changing warp.out.port to : ",temp[i,]$val, " for set #", temp[i,]$set.number))
      Z$set[which(index),]$warp.out.port = temp[i,]$val
    }
    if(temp[i,]$x == "warp.out.starboard"){
      print(paste0("changing warp.out.starboard  to : ",temp[i,]$val, " for set #", temp[i,]$set.number))
      Z$set[which(index),]$warp.out.starboard = temp[i,]$val
    }
  }
  
  Z$set$distance = distance(Z$set, method="latlon")
  
  # Stratum needs to be calculated
  Z$set <- set(Z$set, NULL, stratum = stratum(Z$set,method = "latlong"))
  # Block numbers need to be calculated
 Z$set$block.number <- block.number(longitude(Z$set), latitude(Z$set))
  
    # Remove all letters and symbols from comments 
  # The first three numbers are always the station numbers, any other number in that striong can be ignored 
  Z$set$station.number = substr(gsub("[^0-9]", "", Z$set$comment),1,3)
  

  Z$len = merge(Z$len, Z$set, by=key(Z$set), all.x=TRUE,overwrite = TRUE, names= setdiff(names(Z$len)[which(names(Z$len) %in% names(Z$set))], c("card.type","comment")))
  Z$bio = merge(Z$bio, Z$set, by=key(Z$set), all.x=TRUE,overwrite = TRUE, names= setdiff(names(Z$bio)[which(names(Z$bio) %in% names(Z$set))], c("card.type","comment")))
  Z$cat = merge(Z$cat, Z$set, by=key(Z$set), all.x=TRUE,overwrite = TRUE, names= setdiff(names(Z$cat)[which(names(Z$cat) %in% names(Z$set))], c("card.type","comment")))
  
  
  #Z$bio = addwhihakeages(Z$bio)
  #Z$bio = addcodages(Z$bio)
  #Z$bio = addplaiceages(Z$bio)
  #Z$bio = addyellowtailages(Z$bio)
  
  #Z = corrections.r(Z)

  # Copy files locally before putting on network

paste("files being generated on: ", getwd())
paste("writting file", paste0(file.survey.prefix, vessel,cruise.num,"s.new"))
  write(Z$set, file = paste0(file.survey.prefix, vessel,cruise.num,"s.new"))
paste("writting file", paste0(file.survey.prefix, vessel,cruise.num,"c.new"))
  write(Z$cat, file = paste0(file.survey.prefix, vessel,cruise.num,"c.new"))
paste("writting file", paste0(file.survey.prefix, vessel,cruise.num,"b.new"))
  write(Z$bio, file = paste0(file.survey.prefix, vessel,cruise.num,"b.new"))
paste("writting file", paste0(file.survey.prefix, vessel,cruise.num,"l.new"))
  write(Z$len, file = paste0(file.survey.prefix, vessel,cruise.num,"l.new"))










