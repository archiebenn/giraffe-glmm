# Your task is to determine what factors are the key drivers of body mass in your data, 
# whilst accounting for any factors which may systematically bias your results.

# reset environment
rm(list=ls())

# load libraries
library(vroom)
library(here)
library(tidyverse)
library(Hmisc)
library(ggpubr)
library(rstatix)
library(fitdistrplus)
library(MASS)
library(performance)
library(see)
library(TMB)
library(glmmTMB)
library(DHARMa)
library(data.table)



#####
# 1. IMPORT DATA
##### 

# read in the full dataset (as a tibble)
df <- read_tsv("mass_sj19031.tsv")

# check class (= data.frame/tibble)
class(df)

# describe the data for an overview
describe(df)
### can see some missing data/NAs, as well as typos.
### 3 typos in lion_presence - asbent, preesent, preseet
### big distribution of mass, and assuming 



#####
# 2. TIDY DATA
#####

# to ensure consistent data structure across all, remove N/A variables
df_clean <- df %>%
  drop_na(landscape, vegetation, lion_presence, env_value)

# fix typos 
### (https://stackoverflow.com/questions/35017731/how-to-change-rename-specific-attributes-within-a-data-frame-column)
df_clean$lion_presence[df_clean$lion_presence == "Preesnt"] <- "Present" 
df_clean$lion_presence[df_clean$lion_presence == "Preseet"] <- "Present"
df_clean$lion_presence[df_clean$lion_presence == "Asbent"] <- "Absent"

# describe to check changes
describe(df_clean)
### happy with this as cleaned data




### VISUALISE


### COMMUNICATE







