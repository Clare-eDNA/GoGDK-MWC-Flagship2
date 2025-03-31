## 31 March 2025
# Trying to create phenotype files for gBLUP
# trying to create covariate (XB) files for gBLUP

## Trying to read xlsx on laptop given that .tsv files do not work and also the data contains commas and spaces

setwd("~/OneDrive - University of Otago/Quant_Gen_Postdoc/GoGDK-Phenotypes")

library(readxl)
df <- read_excel("GoGDKPheno_19-03-2025.xlsx", sheet = 1) # reads pretty much everything in as characters

str(df)

library(openxlsx)
df <- read.xlsx("GoGDKPheno_19-03-2025.xlsx", sheet = 1, detectDates = TRUE) # reads some things in as numbers 

str(df)
summary(df)

# went with openxlsx for now, see what happens

# Make a basic covariate file
# columns for age sex (and PCAs)

library(tidyverse)

# Prevent scientific notation globally
options(scipen = 999)


library(dplyr)

# Create the first dataframe: SUBJECT, AGECOL, Geno.GeneticSex
df_basicCoVar <- df %>% 
  select(SUBJECT, AGECOL, Geno.GeneticSex) %>%
  mutate(
    Geno.GeneticSex = case_when(
      Geno.GeneticSex == "Male" ~ 1,
      Geno.GeneticSex == "Female" ~ 2,
      TRUE ~ -9  # Missing values as -9
    )
  ) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), -9, .)))  # Convert NA to -9

# Create the second dataframe: SUBJECT, AGECOL, Genetic Sex, and PCs
df_CovarPCAs <- df %>% 
  select(SUBJECT, AGECOL, Geno.GeneticSex, Geno.PCVector1, 
         Geno.PCVector2, Geno.PCVector3, Geno.PCVector4, Geno.PCVector5) %>%
  mutate(
    Geno.GeneticSex = case_when(
      Geno.GeneticSex == "Male" ~ 1,
      Geno.GeneticSex == "Female" ~ 2,
      TRUE ~ -9  # Missing values as -9
    )
  ) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), -9, .)))  # Convert NA to -9

# Ensure all PC columns are numeric (avoids character issues)
df_CovarPCAs <- df_CovarPCAs %>%
  mutate(across(starts_with("Geno.PCVector"), as.numeric))  



# Save both as space-separated files for GCTA
write.table(df_basicCoVar, "CovariateBasic.txt", row.names = FALSE, col.names = TRUE, quote = FALSE, sep = " ")
write.table(df_CovarPCAs, "CovariatePCAs.txt", row.names = FALSE, col.names = TRUE, quote = FALSE, sep = " ")

########################################################
#########.   Making Phenotype File.   ###########
########################################################

# Prevent scientific notation globally
options(scipen = 999)


df_phenotype <- df %>%
  select(SUBJECT, SEX, 
         DIABETES, GOUT, HIBP, HEART, STROKE, RHEUMATICHD, KIDNEY, 
         WEIGHT, HEIGHT, WAIST, BMI, 
         MRURATE, MRCREAT, TOPHUS, CHOLES, TRIGLY, HDL, LDL, 
         SCREAT, SURICACID, EGFR_SCL, FEUA) %>%
  
  # Convert SEX to numeric (Male = 1, Female = 2, Missing = -9)
  mutate(SEX = case_when(
    SEX == "Male" ~ 1,
    SEX == "Female" ~ 2,
    TRUE ~ -9
  )) %>%
  
  # Recode DIABETES: Change 3 to NA (-9 for GCTA compatibility)
  mutate(DIABETES = case_when(
    DIABETES == 3 ~ -9,
    is.na(DIABETES) ~ -9,
    TRUE ~ as.numeric(DIABETES)
  )) %>%
  
  # Replace all NA values with -9 (for all numeric columns), but exclude SUBJECT
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), -9, .)))

# Ensure SUBJECT stays as character (if needed)
df_phenotype$SUBJECT <- as.character(df_phenotype$SUBJECT)

