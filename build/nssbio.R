library(gulf.data)

years <- 2024
for (i in 1:length(years)){
   print(years[i])
   year <- years[i]
   
   # Read from Oracle:
   v <- read.gulf.bio(year = year, password = password, survey = "ns")
   v <- compress(v)
   
   # Change variable names:
   names(v) <- gsub("specimen.number", "specimen", names(v))
   if (("fish.number" %in% names(v)) & ("specimen" %in% names(v))) print(describe(v))
   #if (("fish.number" %in% names(v)) & ("specimen" %in% names(v))) remove <- "fish.number"
   remove <- NULL
   if (("fish.number" %in% names(v)) & !("specimen" %in% names(v))){
      v$specimen <- v$fish.number
      remove <- "fish.number"
   } 

   # Remove redundant variables:
   v <- v[setdiff(names(v), remove)]
   
   if (year[i] %in% 2023:2024){
      setwd("C:/Users/SuretteTJ/Desktop/github/gulf.manage/inst/extdata/raw")
      
      if (year[i] == 2023){
         bio <- read.csv("PER2023303_specimen_export.csv")
         bio$andes_id <- bio$id
         
         obs <- read.csv("PER2023303_observation_export.csv")    
         obs$ix <- match(obs$specimen_id, bio$id) # Add indexing variable.
         
         obs$observation_type_name <- gsub(" ", "_", tolower(obs$observation_type))
         
         obs$raw_value <- obs$observation_value_raw 
      }
  
      if (year[i] == 2024){
         con <- DBI::dbConnect(RSQLite::SQLite(), dbname = 'PER-2024-402-ecosystem-survey-version-1.0-2025_05_08_19_27.sqlite3')
         
         #DBI::dbListTables(con)
         
         # Read biological specimens table:
         bio <- DBI::dbGetQuery(con, "SELECT * FROM specimens")
       
         z <- DBI::dbGetQuery(con, "SELECT * FROM images")
         
         # Read biological observations table:
         obs <- DBI::dbGetQuery(con, "SELECT * FROM specimen_observations")
         obs$ix <- match(obs$specimen_uuid, bio$uuid) # Add indexing variable.
         
         DBI::dbDisconnect(con)
      }
      
      vars <- unique(obs$observation_type_name)
      keep <- c("collect_fin_sample", "lobster_carapace_condition", "lobster_egg_condition",     
                "lobster_shell_disease", "crab_carapace_condition", "crab_egg_condition",        
                "collect_stomach", "lobster_claw_infection") 
      vars <- intersect(vars, keep)
      for (j in 1:length(vars)){
         xx <- obs[which(obs$observation_type_name == vars[j]), ]
         bio[, vars[j]] <- NA
         bio[xx$ix, vars[j]] <- xx$raw_value
      }
      
      # Combine lobster and crab carapace condition:
      bio$carapace_condition <- NA
      ix <- !is.na(bio$lobster_carapace_condition)
      bio$carapace_condition[ix] <- bio$lobster_carapace_condition[ix]
      ix <- !is.na(bio$crab_carapace_condition)
      bio$carapace_condition[ix] <- bio$crab_carapace_condition[ix]
      
      # Combine lobster and crab egg condition:
      bio$egg_condition <- NA
      ix <- !is.na(bio$lobster_egg_condition)
      bio$egg_condition[ix] <- bio$lobster_egg_condition[ix]
      ix <- !is.na(bio$crab_egg_condition)
      bio$egg_condition[ix] <- bio$crab_egg_condition[ix]
        
      # Specimen ID:
      bio$specimen <- as.numeric(bio$andes_id)
      
      # Variable name changes:
      names(bio) <- gsub("carapace_condition", "shell.condition", names(bio))
      names(bio) <- gsub("egg_condition", "egg.condition", names(bio))
      names(bio) <- gsub("collect_stomach", "stomach.collected", names(bio))
      names(bio) <- gsub("collect_fin_sample", "fin.collected", names(bio))
      names(bio) <- gsub("lobster_shell_disease", "shell.disease", names(bio))
      names(bio) <- gsub("lobster_claw_infection", "claw.infection", names(bio)) 

      # Set variables to be imported:
      vars <- c("shell.condition", "egg.condition", "stomach.collected", "fin.collected", "shell.disease", "claw.infection")
      vars <- intersect(vars, names(bio))
         
      # Import shell conditions:   
      iy <- match(v$specimen, bio$specimen)
      ix <- which(is.na(v$shell.condition))
      v$shell.condition[ix] <- as.numeric(bio$shell.condition[iy])[ix]
      
      # Import egg conditions:   
      ix <- which(is.na(v$egg.condition))
      v$egg.condition[ix] <- as.numeric(bio$egg.condition[iy])[ix]
      
      # Sample collections:
      vars <- setdiff(vars, c("shell.condition", "egg.condition"))
      for (j in 1:length(vars)) v[, vars[j]] <- as.numeric(bio[iy, vars[j]])
      
      # Clean-up:
      remove <- c("fish.number")
      v <- v[setdiff(names(v), remove)]
   }

   # Corrections:

   # Write to file:
   path <- "C:/Users/SuretteTJ/Desktop/github/gulf.data/inst/extdata"
   if (nrow(v) > 0){
      #print(names(v))
      if (file.exists(path)){
         file <- paste0(path, "/", "nss.bio.", year, ".csv") 
         write.csv(v, file = file, row.names = FALSE)
      }
   }
}




