setwd("/projects/sciences/maths_stats/wilcox_group/GoGDK")

# Load necessary libraries
library(data.table)  

# Define file paths
euro_file <- "EthnicityGroup/europeans.tsv"
keep_file <- "keep_samples.txt"
output_file <- "EthnicityGroup/europeans_keep.txt"

# Read the phenotype data
euro_data <- fread(euro_file, header = TRUE, sep = "\t")

# Read the keep_samples.txt file (Assuming the IID is in the first column)
keep_samples <- fread(keep_file, header = FALSE)[[2]]  # Extract first column as vector

# Ensure matching is done correctly by checking column names
col_to_match <- 501  # Change this if the column number is different
colnames(euro_data)[col_to_match]  # Print to confirm correct column

# Filter rows where IID (column 501) is in the keep_samples list
euro_keep <- euro_data[euro_data[[col_to_match]] %in% keep_samples, ]

# Write filtered data to file
fwrite(euro_keep, output_file, sep = "\t", quote = FALSE)

# Print success message
cat("Filtered dataset saved to", output_file, "with", nrow(euro_keep), "individuals.\n")





# Load necessary library
library(data.table)

# Define the folder path
ethnicity_folder <- "EthnicityGroup"

# List all .tsv files in the folder
ethnicity_files <- list.files(ethnicity_folder, pattern = "\\.tsv$", full.names = TRUE)

# Function to count individuals in a file
count_individuals <- function(file) {
  data <- fread(file, header = TRUE, sep = "\t")
  num_individuals <- nrow(data) - 1  # Subtract 1 if there's a header row
  return(num_individuals)
}

# Create a named list of counts
individual_counts <- sapply(ethnicity_files, count_individuals)

# Convert to a data frame and print
counts_df <- data.frame(File = basename(ethnicity_files), Count = individual_counts)
print(counts_df)

# Optionally, save to a file
write.csv(counts_df, file = "EthnicityGroup/individual_counts.csv", row.names = FALSE)




## an R function to do what we think it should do

library(data.table)

# Set working directory
setwd("/projects/sciences/maths_stats/wilcox_group/GoGDK")

# Define file paths
keep_file <- "keep_samples.txt"
input_dir <- "EthnicityGroup"
output_dir <- "EthnicityGroup"

# Read the keep_samples.txt file (Assuming the first column is the key and second is the IID)
keep_samples <- fread(keep_file, header = FALSE, sep = " ", fill = TRUE)

# Read as data.table
head(keep_samples)
setnames(keep_samples, c("V1", "V2"))  # Rename for clarity

# List of ethnicity groups to process
ethnic_groups <- c("allPolys", "EastPolys", "europeans", "Oceanians", "EuroPoly_data", "eastwests", "WestPolys")

# Function to filter and extract two columns
filter_ethnicity <- function(group_name) {
  input_file <- file.path(input_dir, paste0(group_name, ".tsv"))
  output_file <- file.path(output_dir, paste0(group_name, "_keep.txt"))
  
  if (!file.exists(input_file)) {
    cat("Warning: File", input_file, "does not exist. Skipping...\n")
    return(invisible(NULL))  
  }
  
  # Read ethnicity file
  data <- fread(input_file, header = TRUE, sep = "\t")
  col_to_match <- 501  # Adjust if needed
  
  # Subset rows where column 501 matches keep_samples' second column
  filtered_data <- data[data[[col_to_match]] %in% keep_samples$V2, ]
  
  # Merge with keep_samples to get first column
  result <- merge(keep_samples, filtered_data[, .SD, .SDcols = col_to_match], 
                  by.x = "V2", by.y = col_to_match)[, .(V1, V2)]
  
  # Save result with only two columns
  # Write filtered data as space-separated (better for PLINK/GCTA)
  fwrite(final_output, output_file, sep = " ", quote = FALSE, col.names = FALSE)
  
  cat("Filtered dataset saved to", output_file, "with", nrow(result), "individuals.\n")
  
  invisible(NULL)  
}

# Apply function to all groups
invisible(lapply(ethnic_groups, filter_ethnicity))




### 

# Load necessary library
library(data.table)

# Define function to process ethnicity groups
filter_and_save <- function(ethnicity_file) {
  # Define file paths
  input_file <- paste0("EthnicityGroup/", ethnicity_file, ".tsv")
  output_file <- paste0("EthnicityGroup/", ethnicity_file, "_keep.txt")
  
  # Read ethnicity data
  ethnicity_data <- fread(input_file, header = TRUE, sep = "\t")
  
  # Read keep_samples file (assuming space-separated)
  keep_samples <- fread("keep_samples.txt", header = FALSE, sep = " ")
  
  # Ensure correct column names for merging
  setnames(keep_samples, c("V1", "V2"))  # Assuming two columns: (FID, IID)
  
  # Define column to match in ethnicity data
  col_to_match <- 501  # Adjust if column index is different
  colnames(ethnicity_data)[col_to_match]  # Debugging check
  
  # Filter based on IID
  filtered_data <- ethnicity_data[get(col_to_match) %in% keep_samples$V2, .(keep_samples$V1, get(col_to_match))]
  
  # Save output in space-separated format for PLINK/GCTA
  fwrite(filtered_data, output_file, sep = " ", quote = FALSE, col.names = FALSE)
  
  # Print success message
  cat("Filtered dataset saved to", output_file, "with", nrow(filtered_data), "individuals.\n")
}

# List of ethnicity groups to process
ethnic_groups <- c("allPolys", "EastPolys", "europeans", "Oceanians", "EuroPoly_data", "eastwests", "WestPolys")

# Apply function to each ethnicity group
lapply(ethnic_groups, filter_and_save)














# Load required library
library(data.table)

# Define file paths for EastPolys
ethnicity_file <- "EthnicityGroup/EastPolys.tsv"
keep_file <- "keep_samples.txt"
output_file <- "EthnicityGroup/EastPolys_keep.txt"

# Read the ethnicity data
ethnicity_data <- fread(ethnicity_file, header = TRUE, sep = "\t")

# Read the keep_samples file (space-separated)
keep_samples <- fread(keep_file, header = FALSE, sep = " ")

# Check structure of keep_samples (should have two columns: FID, IID)
print(head(keep_samples))

# Ensure correct column names
setnames(keep_samples, c("FID", "IID"))  

# Verify column 501 exists in ethnicity data
col_to_match <- 501  
if (col_to_match > ncol(ethnicity_data)) stop("Column 501 does not exist!")

# Check the actual column name for 501
print(colnames(ethnicity_data)[col_to_match])





# Verify that Geno.SampleID column exists
if (!"Geno.SampleID" %in% colnames(ethnicity_data)) stop("Column 'Geno.SampleID' not found!")

# Save the filtered data as space-separated for PLINK/GCTA with IID first, then FID
fwrite(filtered_data[, .(IID, FID)], output_file, sep = " ", quote = FALSE, col.names = FALSE)


# Save as space-separated file for PLINK/GCTA
fwrite(filtered_data, output_file, sep = " ", quote = FALSE, col.names = FALSE)




# Filter individuals where IID (column 501) is in keep_samples$IID
filtered_data <- ethnicity_data[get(col_to_match) %in% keep_samples$IID, .(keep_samples$FID, get(col_to_match))]

# Save as space-separated file for PLINK/GCTA
fwrite(filtered_data, output_file, sep = " ", quote = FALSE, col.names = FALSE)

# Print success message
cat("Filtered dataset saved to", output_file, "with", nrow(filtered_data), "individuals.\n")










####


library(data.table)

# Define function to filter and reorder columns
filter_ethnicity <- function(group_name) {
  # Construct file paths
  input_file <- paste0("EthnicityGroup/", group_name, ".tsv")
  output_file <- paste0("EthnicityGroup/", group_name, "_keep.txt")
  
  # Read in ethnicity data
  ethnicity_data <- fread(input_file, header = TRUE, sep = "\t")
  
  # Read in keep_samples (assumes space-separated file with FID and IID)
  keep_samples <- fread("keep_samples.txt", header = FALSE, sep = " ")
  setnames(keep_samples, c("FID", "IID"))  # Ensure proper column names
  
  # Check if the required column exists
  if (!("Geno.SampleID" %in% colnames(ethnicity_data))) {
    stop(paste("Column 'Geno.SampleID' not found in", group_name))
  }
  
  # Filter individuals based on IID
  filtered_data <- ethnicity_data[Geno.SampleID %in% keep_samples$IID]
  
  # Merge with keep_samples to retain FID and reorder columns (IID first, then FID)
  filtered_data <- merge(keep_samples, filtered_data, by.x = "IID", by.y = "Geno.SampleID")
  
  # Save output as space-separated file for PLINK/GCTA
  fwrite(filtered_data[, .(IID, FID)], output_file, sep = " ", quote = FALSE, col.names = FALSE)
  
  # Print success message
  cat("Filtered dataset saved to", output_file, "with", nrow(filtered_data), "individuals.\n")
}

# Define the list of ethnicity groups
ethnic_groups <- c("allPolys", "EastPolys", "europeans", "Oceanians", 
                   "EuroPoly_data", "eastwests", "WestPolys")

# Apply function to all groups
lapply(ethnic_groups, filter_ethnicity)








##########

setwd("/projects/sciences/maths_stats/wilcox_group/GoGDK/EthnicityGroup")

# Read in SNP lists
master_snp_list <- read.table("master_snp_list.txt", header = FALSE, stringsAsFactors = FALSE)[[1]]
EastPolys_snp_list <- read.table("EastPolys_snp_list.txt", header = FALSE, stringsAsFactors = FALSE)[[1]]

# Find SNPs in master list that are missing from europeans list
missing_snps <- setdiff(master_snp_list, EastPolys_snp_list)

# Save the missing SNPs to a file
write.table(missing_snps, "EastPolys_missing_snps.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

# Print the number of missing SNPs
cat("Number of SNPs missing from europeans:", length(missing_snps), "\n")










# Read master SNP list
master_snp_list <- read.table("master_mafgeno_snp_list.txt", header = FALSE, stringsAsFactors = FALSE)[[1]]

# List of population SNP files
populations <- c("allPolys", "EastPolys", "eastwests", "europeans", "EuroPoly", "WestPolys")

# Create empty data frame to store presence/absence
presence_matrix <- data.frame(SNP = master_snp_list)

# Loop over each population
for (pop in populations) {
  pop_snp_list <- read.table(paste0(pop, "_filtered_snps.txt"), header = FALSE, stringsAsFactors = FALSE)[[1]]
  
  # Mark presence (1) or absence (0)
  presence_matrix[[pop]] <- ifelse(master_snp_list %in% pop_snp_list, 1, 0)
}

# Write the SNP presence/absence matrix to file
write.table(presence_matrix, "snp_presence_matrix.txt", row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")

presence_matrix

# Print some summary stats
missing_counts <- rowSums(presence_matrix[,-1] == 0)  # Count missing populations per SNP
cat("Number of SNPs missing in at least one population:", sum(missing_counts > 0), "\n")
cat("Number of SNPs missing in all but one population:", sum(missing_counts == (length(populations) - 1)), "\n")
cat("Number of SNPs missing in all populations:", sum(missing_counts == length(populations)), "\n")



# Load required package
library(data.table)

# Define the list of ethnicity groups
ethnic_groups <- c("allPolys", "EastPolys", "eastwests", "europeans", 
                   "EuroPoly", "Oceanians", "WestPolys")

# Loop through each ethnic group and process its frequency file
for (group in ethnic_groups) {
  
  # Define file name
  freq_file <- paste0(group, "_freq.afreq")
  
  # Read the file into R
  df <- fread(freq_file, header = TRUE)
  
  # Ensure column names are correct
  colnames(df) <- c("CHROM", "ID", "REF", "ALT", "PROVISIONAL_REF", "ALT_FREQS", "OBS_CT")
  
  # Filter rows where OBS_CT == 0 (SNPs missing in this population)
  missing_snps <- df[OBS_CT == 0, .(ID)]
  
  # Save the missing SNP list to a text file
  output_file <- paste0(group, "_missing_snps.txt")
  fwrite(missing_snps, output_file, col.names = FALSE, quote = FALSE)
  
  # Print confirmation message
  cat("Processed", group, "- Missing SNPs saved to", output_file, "\n")
}



















