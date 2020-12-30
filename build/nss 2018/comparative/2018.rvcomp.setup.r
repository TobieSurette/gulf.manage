  rm(list=ls())  
  year = 2018
  cruise.num = "829"
  survey="ns"
  vessel = "p"
  setwd("E:/work/HD2_repositorywc/northumberland/")
  
  source(paste0('O:/research/northumberland/corrections/',year,'/comparative/',year,'.rvcomp.load.ese.r'), echo=TRUE)
  source(paste0('O:/research/northumberland/corrections/',year,'/comparative/',year,'.rvcomp.create.gulf.cards.r'), echo=TRUE)
  library(stringr)
  
  correction.file = paste0(year,"corrections.log")
  correction.path = paste(.gulf.path$rv, paste0("corrections/",year,"/logs/"), sep="")
  
  # run this line to reset log file
  gulf.fprint(text = paste("Reset log file", sep=""), file = correction.file, path = correction.path, overwrite = TRUE)
  
  x = load.ese(survey)
  Z = createCards(x, survey)
  
  # Z$bio = special.bio.data(Z$bio)
  Z$set$distance = distance(Z$set, method="latlon")
  
  Z$set$cruise.number = cruise.num
  Z$len$cruise.number = cruise.num
  Z$bio$cruise.number = cruise.num
  Z$cat$cruise.number = cruise.num
  
  Z$len = merge(Z$len, Z$set, by=key(Z$set), all.x=TRUE,overwrite = TRUE, names= setdiff(names(Z$len)[which(names(Z$len) %in% names(Z$set))], c("card.type","comment")))
  Z$bio = merge(Z$bio, Z$set, by=key(Z$set), all.x=TRUE,overwrite = TRUE, names= setdiff(names(Z$bio)[which(names(Z$bio) %in% names(Z$set))], c("card.type","comment")))
  Z$cat = merge(Z$cat, Z$set, by=key(Z$set), all.x=TRUE,overwrite = TRUE, names= setdiff(names(Z$cat)[which(names(Z$cat) %in% names(Z$set))], c("card.type","comment")))
  
  #Z = corrections.r(Z)
  survey="rv"
  # Copy files locally before putting on network
paste("files being generated on: ", getwd())
paste("writting file", paste0(survey, vessel,cruise.num,"s.new"))
  write(Z$set, file = paste0(survey, vessel,cruise.num,"s.new"))
paste("writting file", paste0(survey, vessel,cruise.num,"c.new"))
  write(Z$cat, file = paste0(survey, vessel,cruise.num,"c.new"))
paste("writting file", paste0(survey, vessel,cruise.num,"b.new"))
  write(Z$bio, file = paste0(survey, vessel,cruise.num,"b.new"))
paste("writting file", paste0(survey, vessel,cruise.num,"l.new"))
  write(Z$len, file = paste0(survey, vessel,cruise.num,"l.new"))










