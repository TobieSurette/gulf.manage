v <- list()
v$set <- subset(Z$set, experiment =1)
v$cat <- Z$cat
v$len <- Z$len
v$bio <- Z$bio

# Quick check - check keys  and formats first
verify.key(v$set)     #pass
verify.key(v$cat)     #pass
verify.key(v$len)     #pass
verify.format(v$set)  #pass
verify.format(v$cat)  #pass
verify.format(v$len)  #pass
verify.format(v$bio)  #pass
verify.card.type(v$set)    #pass
verify.card.type(v$cat)    #pass
verify.card.type(v$len)    #pass
verify.card.type(v$bio)    #pass


# ********************** VERIFY SET CARD

#run all verifications at once or break it down
#verify.gulf.set(v$set,v$cat,temperature = FALSE)
# verify.gulf.set function broken down 
verify.coordinates(v$set)                               #pass
verify.distance(v$set,print=FALSE, tolerance = .10)     #pass
verify.duration(v$set)                                  # set 54 and 74 are in fact ok
verify.depth(v$set)         #need to verify these with scanmar
# set 12, hydrolog shows a depth of 340. No seaStar available for that tow.  
# set 86, seaStar data confirms the dephs

verify.speed(v$set)        #pass
verify.stratum(v$set)       #pass
verify.temperature(v$set)  #waiting for temperature data
verify.time(v$set)         #pass
verify.unit.area(v$set)   # area 441 is fine, strata 437 and 439 are in part inside the 441 (4VN) area
verify.species.rvcat(v$cat,v$bio,v$len)   #pass
verify.species.number(v$set, v$cat) #pass

#SET PLOTS

#Plot ALL survey stations
xx = v$set
windows()
gulf.map(region = "gulf")
map.strata(region = "gulf", survey = "rv", label = TRUE) # Draw blocks.
points(xx$longitude.start, xx$latitude.start, pch=19)
points(xx$longitude.end, xx$latitude.end, cex=0.5 ,pch=19, col="red")
lines(xx$longitude.start, xx$latitude.start)
text(xx$longitude.end, xx$latitude.end, paste("set=", xx$set.number), pos = 3, cex = 0.7)

# PLOT distances
dist.obs= distance(v$set)
dist.latlon= distance(v$set, method="latlon")
dist.speed.duration= distance(v$set, method="speed-duration")
plot(dist.obs ,dist.latlon)
plot(dist.latlon, dist.speed.duration)
plot(dist.speed.duration, dist.obs)
text(dist.speed.duration, dist.obs, paste("set=", v$set$set.number), pos = 3, cex = 0.7)

#PLOT depth
plot(abs(depth(v$set, method="latlon")), depth(v$set))
text(abs(depth(v$set, method="latlon")), depth(v$set),paste("", v$set$set.number), pos = 3, cex = 0.7)
abline(0,1)
#verified set#12 and depth is actually in the 300 range


#PLOT speed
windows()
plot(distance(v$set), distance(v$set, method = "speed"))
plot(distance(v$set), distance(v$set, method = "latlon"))
plot(distance(v$set, method="speed"), distance(v$set, method = "latlon"))
text(distance(v$set, method="speed"), distance(v$set, method = "latlon"), paste("set=", v$set$set.number), pos = 3, cex = 0.7)



# # coordinate verifcation.  use scanmar data to double check on our data
# files <- scanmar.file.str(year = 2017)
# xx=subset(v$set, set.number= 1:5)
# xx = v$set
# scan <- read.scanmar(files[148:151])
# scan <- read.scanmar(files)
# 
# match.scanmar()
# windows()
# start.color = "blue"
# end.color ="red"
# plot(scan$longitude, scan$latitude, col="grey")
# points(xx$longitude.start, xx$latitude.start, cex=1.0 ,pch=19, col = start.color,)
# points(xx$longitude.end, xx$latitude.end, cex=1.0 ,pch=19, col=end.color)
# text(xx$longitude.start, xx$latitude.start, paste("START - set =", xx$set.number), pos = 3, cex = 0.7, col=start.color)
# text(xx$longitude.end, xx$latitude.end, paste("End - set =", xx$set.number), pos = 3, cex = 0.7,col=end.color)
# # 
# #   windows()
# #   plot(time(scan), scan$depth, ylim=c(60,80))
# #   grid()



#VERIFY CATCH CARD
verify.gulf.cat(v$cat, v$set, v$bio, v$len , format = FALSE,number.length = FALSE,number.caught = FALSE,number.sex    = FALSE, weight.caught = FALSE , tolerance = 10)  #pass
verify.number.caught(v$cat,v$len, tolerance = 30)  #pass 
verify.number.sex.gulf.cat(v$cat, v$len, v$bio)    #pass





#here we set our data set to only the main species
v$cat = subset(v$cat, species = species.we.like()[1:11])
v$len = subset(v$len, species = species.we.like()[1:11])
v$bio = subset(v$bio, species = species.we.like()[1:11])

verify.species(v$cat,v$bio, v$len) #pass

#this checks to see if any species that have been set to be sampled exist in either the len or bio card.
spec= unique(subset(v$cat, weight.sampled >0)$species)
temp = spec[!(spec %in% unique(v$bio$species) | spec %in% unique(v$len$species))]

#VERIFY LEN CARD
# VERIFY that RATIO in len card it not -0
# ratio == 0 need to be set to 1 at this point.. but if set in my correction file it generates some strange bugs....

verify.ratio(v$len) #pass

verify.number.caught(v$cat,v$len, tolerance = 100)  #pass.  7 entries are from species that ARE NOT sampled, they entered number caught by hand.)
verify.number.sex.gulf.cat(v$cat, v$len, v$bio) #pass .. Discrepancies come from fish entered as 9 for sex




# Quick basic check of common vars for odd data

table(v$bio$sex) #pass
table(v$bio$record.number) #pass
which(diff(Z$bio$fish.number) > 1 ) # should return nothing or positive numbers #pass
table(v$set$card.type)
table(v$set$vessel.code)
table(v$set$cruise.number)
table(v$set$stratum)
table(v$set$set.number)
diff(v$set$set.number)
table(v$set$year)
table(v$set$day)
table(v$set$unit.area)
table(v$set$experiment)
table(v$set$gear)
table(v$set$auxiliary)
table(v$set$speed)
table(v$set$speed.method)
table(v$set$latitude.start)
table(v$set$longitude.start)
table(v$set$latitude.end)
table(v$set$longitude.end)
table(v$set$depth.start)
table(v$set$depth.end)
table(v$set$distance)
table(v$set$distance.method)
table(v$set$wind.direction)
table(v$set$wind.force)
table(v$set$tide)
table(v$set$surface.temperature)
table(v$set$bottom.temperature)
table(v$set$bottom.salinity)
table(v$set$light)
table(v$set$btslide)
table(v$set$hydrostation)
table(v$set$bottom.type)
table(v$set$species.fish.number)
table(v$set$species.invertebrate.number)
table(v$set$catch.total.weight)
table(v$set$warp.out.port)
table(v$set$warp.out.starboard)
table(v$set$comment)
#check speed for NA or outliers
which(is.na(v$set$speed))  #should return NULL   #pass
table(v$set$speed)
#check duration for NA or outliers
which(is.na(v$set$duration))  #should return NULL  #pass
table(v$set$duration)  

#these should not return NA's.  if they do it means there's an invalid code
unique(speed.method.str(v$set$speed.method))  #pass
unique(gear.str(v$set$gear))  #pass
unique(auxiliary.str(v$set$auxiliary))  #pass
unique(distance.method.str(v$set$distance.method))  #pass
unique(wind.direction.str(v$set$wind.direction))  #pass
unique(wind.force.str(v$set$wind.force)) #pass
unique(tide.str(v$set$tide))   #pass
unique(bottom.type.str(v$set$bottom.type)) #pass
unique(Z$bio$card.type)
unique(Z$bio$vessel.code)
unique(Z$bio$cruise.number)
unique(Z$bio$stratum)
unique(Z$bio$set.number)
unique(Z$bio$year   )          
unique(Z$bio$month)
unique(Z$bio$day)
unique(Z$bio$unit.area)
unique(Z$bio$experiment)
unique(Z$bio$bottom.type)
unique(Z$bio$species      )    
unique(Z$bio$record.number)
unique(Z$bio$fish.number)
unique(Z$bio$length)
unique(Z$bio$sex)
unique(Z$bio$maturity)
unique(Z$bio$weight       )    
unique(Z$bio$stomach.type)
unique(Z$bio$stomach.weight)
unique(Z$bio$stomach.full)
unique(Z$bio$stomach.part)
unique(Z$bio$weight.gonad)
unique(Z$bio$age.material     )
unique(Z$bio$annuli)
unique(Z$bio$edge.type)
unique(Z$bio$age.check)
unique(Z$bio$age)
unique(Z$bio$year.class)
unique(Z$bio$ager     )        
unique(Z$bio$parasite)
unique(Z$bio$field.definition)
unique(Z$bio$expedition.number)
unique(Z$bio$comment)
unique(Z$bio$shell.condition)
unique(Z$bio$egg.condition    )
unique(Z$bio$chela)
unique(Z$bio$abdomen)
unique(Z$bio$block.number)
unique(Z$bio$station.number)
unique(Z$bio$missing.legs)
unique(Z$bio$disc.width       )
unique(Z$bio$bobtail)
unique(Z$bio$specimen.number)








































