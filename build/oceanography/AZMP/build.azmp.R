# Load the tidyr package
library(tidyr)
library(reshape2)

# Set the directory path where your raw data files are located
data_dir <- "C:/Users/Gagnondj/Documents/Covid/Crab/requests/azmp temperatures"

# Get a list of all CSV files in the directory
data_files <- list.files(path = data_dir, pattern = "*.csv", full.names = TRUE)

# Loop through each data file
for (data_file in data_files) {
  # Load the data from the current file
  df <- read.csv(data_file)

  # Melt the data to long format
  df_long <- melt(df, id.vars = "depth.m.", variable.name = "date", value.name = "temperature")
  
  # Remove rows with missing values (NaN)
  df_long <- df_long[!is.na(df_long$temperature), ]
  
  # Extract and convert the date column
  df_long$date <- as.Date(sub("^X", "", df_long$date), format = "%Y.%m.%d")
  
  # Extract the original file name without the file extension
  file_name <- gsub(".csv", "", basename(data_file))
  
  # Create a new file name with the "azmp." prefix
  new_file_name <- paste("azmp.", file_name, ".csv", sep = "")
  
  # Set the path for the output file
  output_file <- file.path(data_dir, new_file_name)
  
  # Write the transformed data to the output file
  write.csv(df_long, file = output_file, row.names = FALSE)
  
  cat("Processed:", data_file, "\n")
}

cat("All files processed.\n")
  
