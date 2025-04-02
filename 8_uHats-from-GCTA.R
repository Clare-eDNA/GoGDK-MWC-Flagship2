## 1 April 2025
## double checking u-hat values from gBLUP / gREML in R

# set working directory
setwd("/projects/sciences/maths_stats/wilcox_group/GoGDK-GRMs-2025/")

# Set the directory where your results are stored
results_dir <- "/projects/sciences/maths_stats/wilcox_group/GoGDK-GRMs-2025/EastPolys_results"

############ Make histogram to confirm yes


# List all files ending in ".indi.blp"
blp_files <- list.files(results_dir, pattern = "*.indi.blp", full.names = TRUE)

# Create an output directory for histograms
output_dir <- file.path(results_dir, "histograms")
dir.create(output_dir, showWarnings = FALSE)

# Loop through each file and generate a histogram
for (file in blp_files) {
  
  # Read the file, assuming tab or space delimiter
  df <- read.table(file, header = FALSE, stringsAsFactors = FALSE)
  
  # Extract the phenotype name from the filename
  phenotype <- gsub("EuroPoly_|_GBLUP.indi.blp", "", basename(file))
  
  # Check if the file has at least 4 columns
  if (ncol(df) >= 4) {
    
    # Extract the 4th column
    blup_values <- df[, 4]
    
    # Generate the histogram
    png(file.path(output_dir, paste0(phenotype, "_histogram.png")), width = 800, height = 600)
    hist(blup_values, main = paste("Histogram of", phenotype, "BLUP values"),
         xlab = "BLUP Values", col = "skyblue", border = "black", breaks = 30)
    dev.off()
  } else {
    message(paste("Skipping", file, "because it has less than 4 columns."))
  }
}

print("Histograms saved in the 'histograms' folder.")



# Loop through each file and generate a histogram in RStudio
for (file in blp_files) {
  
  # Read the file, assuming tab or space delimiter
  df <- read.table(file, header = FALSE, stringsAsFactors = FALSE)
  
  # Extract the phenotype name from the filename
  phenotype <- gsub("EuroPoly_|_GBLUP.indi.blp", "", basename(file))
  
  # Check if the file has at least 4 columns
  if (ncol(df) >= 4) {
    
    # Extract the 4th column
    blup_values <- df[, 4]
    
    # Plot the histogram in RStudio's plot viewer
    hist(blup_values, main = paste("Histogram of", phenotype, "BLUP values"),
         xlab = "BLUP Values", col = "skyblue", border = "black", breaks = 30)
    
    # Pause for user input before showing the next histogram
    readline(prompt = "Press [Enter] to view the next histogram...")
  } else {
    message(paste("Skipping", file, "because it has less than 4 columns."))
  }
}


par(mfrow=c(3, 3))  # 3 rows, 3 columns of plots
for (file in blp_files[1:9]) {  # Show the first 9 histograms
  df <- read.table(file, header = FALSE, stringsAsFactors = FALSE)
  phenotype <- gsub("EuroPoly_|_GBLUP.indi.blp", "", basename(file))
  if (ncol(df) >= 4) {
    hist(df[, 4], main = phenotype, xlab = "BLUP Values", col = "skyblue", border = "black", breaks = 30)
  }
}

par(mar = c(5, 5, 4, 2))  

########################### munge data to get uHat correlations ###########################
#######
# Load required libraries
library(dplyr)

# Define the folder containing the .indi.blp files
folder_path <- "/projects/sciences/maths_stats/wilcox_group/GoGDK-GRMs-2025/EastPolys_results"  # Update as needed

# List all .indi.blp files
blp_files <- list.files(folder_path, pattern = "*.indi.blp", full.names = TRUE)

# Check if files were found
if (length(blp_files) == 0) {
  stop("No .indi.blp files found in the specified directory!")
} else {
  cat("Found", length(blp_files), "files.\n")
}

# Function to extract u-hat values from each .indi.blp file
extract_u_hat <- function(file_path) {
  cat("Processing:", file_path, "\n")  # Progress tracking
  
  # Read the file
  df <- read.table(file_path, header = FALSE, stringsAsFactors = FALSE)
  
  # Check if the file has at least 4 columns
  if (ncol(df) < 4) {
    warning("Skipping file", file_path, "- it does not have at least 4 columns.")
    return(NULL)
  }
  
  # Extract phenotype name from filename
  phenotype_name <- gsub(".*/EastPolys_(.*)_GBLUP.indi.blp", "\\1", file_path)
  
  # Create a clean dataframe
  df_clean <- data.frame(
    IID = df[, 2],  # Second column is IID
    u_hat = df[, 4] # Fourth column is the u-hat values
  )
  
  # Rename the u_hat column to the phenotype name
  colnames(df_clean)[2] <- phenotype_name
  
  # Print a small sample for sanity check
  cat("Sample from", phenotype_name, ":\n")
  print(head(df_clean, 3))  # Show first 3 rows
  
  return(df_clean)
}

# Apply function to all files
u_hat_data <- lapply(blp_files, extract_u_hat)

# Remove any NULLs (files that were skipped due to errors)
u_hat_data <- Filter(Negate(is.null), u_hat_data)

# Check how many datasets were successfully loaded
cat("Successfully loaded", length(u_hat_data), "datasets.\n")

# Merge all u_hat dataframes by "IID"
if (length(u_hat_data) > 1) {
  u_hat_merged <- Reduce(function(x, y) merge(x, y, by = "IID", all = TRUE), u_hat_data)
} else if (length(u_hat_data) == 1) {
  u_hat_merged <- u_hat_data[[1]]
} else {
  stop("No valid data to merge!")
}

# Print summary of the final merged dataframe
cat("Final merged dataset summary:\n")
print(head(u_hat_merged, 5))  # First 5 rows
cat("Dimensions of merged dataset:", dim(u_hat_merged), "\n")
