#survey="ns"

#x = read.ESE(database = 'dtran',  user = 'GLF_ESE',  password= 'shotgun',  schema = 'glf_ese', mission='PER2020150')

load("C:/Users/Gagnondj/Documents/GitHub/gulf.manage/inst/extdata/raw/PER2020150.rda")
x = PER2020150

set=read.card(year=2020, survey="ns", card="set")


index = x$detail$SPEC %in% c(40,42,43) & x$detail$LENGTH < 13 & x$detail$SEX == 0

#  x$detail = set(x$detail, index,  SEX = 9)
if(length(which(index)) > 0)
  x$detail[which(index),]$SEX = 9


index =x$set$SETNO == 111
x$set = x$set[!index,]


# add corrections here.  here are some examples 

# y$set$depth.end[y$set$set.number == 51] <- 10
# index = x$set$SETNO == 51
# x$set[which(index),]$END_DEPTH  =1
# 
# y$set$stop.time[y$set$set.number == 23] <- "13:16:00"
# index = x$set$SETNO == 23
# x$set[which(index),]$END_TIME  =1316
# 
# 
# y$set$comment[y$set$set.number == 96] <- "Null - Caught chunk of mud"
# index = x$set$SETNO == 96
# x$set[which(index),]$NOTE = "Null - Caught chunk of mud"
# 
# y$set$longitude.stop[y$set$set.number == 1] <- 6426.14
# index = x$set$SETNO == 1
# x$set[which(index),]$ELONG = 6426.14
# 
# y$set$latitude.stop[y$set$set.number == 9] = 4638.53
# index = x$set$SETNO == 9
# x$set[which(index),]$ELAT = 4638.53
# 
# 
# y$bio$sex[y$bio$set.number == 70] <- 1
# index = x$detail$SETNO == 70 & x$detail$SPECIMEN_ID == 23991
# x$detail[which(index),]$SEX = 1

#Set corrections
index = x$set$SETNO == 1 
x$set[which(index),]$ELONG <- 6426.14
index = x$set$SETNO == 9 
x$set[which(index),]$ELAT <- 4638.53
index = x$set$SETNO == 10 
x$set[which(index),]$SLAT <- 4644
index = x$set$SETNO == 10 
x$set[which(index),]$EXPERIMENT_TYPE_CODE <- 1
index = x$set$SETNO == 12 
x$set[which(index),]$ELONG <- 6426.3
index = x$set$SETNO == 18 
x$set[which(index),]$CURNT <- 1
index = x$set$SETNO == 23 
x$set[which(index),]$END_TIME <- "1:16:00 PM"
index = x$set$SETNO == 23 
x$set[which(index),]$ELAT <- 4643.3
index = x$set$SETNO == 23 
x$set[which(index),]$ELONG <- 6436.96
index = x$set$SETNO == 24 
x$set[which(index),]$ELAT <- 4648.52
index = x$set$SETNO == 27 
x$set[which(index),]$FORCE <- 3
index = x$set$SETNO == 31 
x$set[which(index),]$SLONG <- 6355.55
index = x$set$SETNO == 31 
x$set[which(index),]$ELONG <- 6354.95
index = x$set$SETNO == 33 
x$set[which(index),]$END_DEPTH <- 10
index = x$set$SETNO == 34 
x$set[which(index),]$EXPERIMENT_TYPE_CODE <- 1
index = x$set$SETNO == 35 
x$set[which(index),]$SLAT <- 4646.05
index = x$set$SETNO == 35 
x$set[which(index),]$FORCE <- 3
index = x$set$SETNO == 37 
x$set[which(index),]$ELONG <- 6446.18
index = x$set$SETNO == 41 
x$set[which(index),]$ELAT <- 4700.13
index = x$set$SETNO == 43 
x$set[which(index),]$START_DEPTH <- 36
index = x$set$SETNO == 48 
x$set[which(index),]$SLONG <- 6429.3
index = x$set$SETNO == 51 
x$set[which(index),]$END_DEPTH <- 10
index = x$set$SETNO == 53 
x$set[which(index),]$START_DEPTH <- 16
index = x$set$SETNO == 54 
x$set[which(index),]$END_DEPTH <- 12
index = x$set$SETNO == 55 
x$set[which(index),]$START_DEPTH <- 10
index = x$set$SETNO == 56 
x$set[which(index),]$START_TIME <- "11:37:00 AM"
index = x$set$SETNO == 58 
x$set[which(index),]$START_DEPTH <- 32
index = x$set$SETNO == 66 
x$set[which(index),]$SLONG <- 6428.83
index = x$set$SETNO == 67 
x$set[which(index),]$SLAT <- 4633.81
index = x$set$SETNO == 68 
x$set[which(index),]$ELAT <- 4617.5
index = x$set$SETNO == 73 
x$set[which(index),]$ELONG <- 6408.86
index = x$set$SETNO == 75 
x$set[which(index),]$SLAT <- 4626.3
index = x$set$SETNO == 88 
x$set[which(index),]$SLAT <- 4611.99
index = x$set$SETNO == 89 
x$set[which(index),]$STATION <- "B1"
index = x$set$SETNO == 91 
x$set[which(index),]$ELAT <- 4614.45
index = x$set$SETNO == 96 
x$set[which(index),]$NOTE <- "Null - Caught chunk of mud"
index = x$set$SETNO == 102 
x$set[which(index),]$STATION <- 331
index = x$set$SETNO == 104 
x$set[which(index),]$ELONG <- 6400.69
index = x$set$SETNO == 110
x$set[which(index),]$END_DEPTH <- 19


#Basket corrections
index = x$basket$SPEC == 599  
x$basket[which(index),]$SPEC <- 611
index = x$basket$SETNO == 63 & x$basket$SPEC == 8300
x$basket[which(index),]$SAMPLED <- "N"
index = x$basket$SETNO == 76  & x$basket$SPEC == 6511
x$basket[which(index),]$SAMPLED <- "N"
index = x$basket$SETNO == 57  & x$basket$SPEC == 2550
x$basket[which(index),]$SIZE_CLASS  <- 1
index = x$basket$SETNO == 5  & x$basket$SPEC == 2539
x$basket[which(index),]$SAMPLED <- "Y"

#Detail corrections
index = x$detail$SETNO == 66 & x$detail$SPECIMEN_ID == 21733 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 87 & x$detail$SPECIMEN_ID == 32083 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 83 & x$detail$SPECIMEN_ID == 30070 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 35 & x$detail$SPECIMEN_ID == 14057 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 35 & x$detail$SPECIMEN_ID == 13878 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 70 & x$detail$SPECIMEN_ID == 24129 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 49 & x$detail$SPECIMEN_ID == 18389 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 68 & x$detail$SPECIMEN_ID == 22641 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 70 & x$detail$SPECIMEN_ID == 23868 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 52 & x$detail$SPECIMEN_ID == 18798 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 54 & x$detail$SPECIMEN_ID == 19197 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 73 & x$detail$SPECIMEN_ID == 26240 
x$detail[which(index),]$LENGTH <- NA
index = x$detail$SETNO == 70 & x$detail$SPECIMEN_ID == 23991 
x$detail[which(index),]$SEX <- 1
index = x$detail$SETNO == 32 & x$detail$SPECIMEN_ID == 12016 
x$detail[which(index),]$LOBSTER_EGG_CONDITION <- NA

