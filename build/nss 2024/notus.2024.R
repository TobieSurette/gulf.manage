library(gulf.data)

year <- 2024
setwd("C:/Users/SuretteTJ/Desktop/github/gulf.manage/inst/extdata/raw")
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = 'PER-2024-402-ecosystem-survey-version-1.0-2025_05_08_19_27.sqlite3')

#DBI::dbListTables(con)

# Read biological specimens table:
s <- DBI::dbGetQuery(con, "SELECT * FROM sensor_data")
s$date <- substr(s$datetime, 1, 10)
s$time <- substr(s$datetime, 12, 19)

types <- unique(s$type)

r <- sort(unique(s[c("date", "time")]))
for (i in 1:length(types)){
   ix <- which(s$type == types[i]) 
   iy <- match(s[ix, c("date", "time")], r[c("date", "time")])
   r[types[i]] <- NA
   r[iy, types[i]] <- s[ix, "value"]
}
names(r) <- gsub("true_track", "heading", names(r))
names(r) <- gsub("spd_over_grnd_kts", "speed", names(r))
names(r) <- gsub("true_depth", "depth.true", names(r))
names(r) <- gsub("sounder_depth", "depth.sounder", names(r))
names(r) <- gsub("StarOddi-temp", "temperature", names(r))
names(r) <- gsub("StarOddi-depth", "depth.star.oddi", names(r))
names(r) <- gsub("starboard_door_dist", "door.distance.starboard", names(r))
names(r) <- gsub("wing_spread", "wingspread", names(r))
names(r) <- gsub("port_door_dist", "door.distance.port", names(r)) 

# Vessel data:
vars <- c("heading", "speed", "depth.true", "depth.sounder", "wingspread", "door.distance.starboard", "door.distance.port")
ix <- rep(FALSE, nrow(r))
for (i in 1:length(vars)) ix <- ix | !is.na(r[,vars[i]])
vessel <- r[which(ix), c("date", "time", vars)]

# Write vessel data:
write.csv(vessel, row.names = FALSE, 
          file = "W:/Lobster/Northumberland Strait/NS_Survey/2024/vessel/vessel.2024.csv")



