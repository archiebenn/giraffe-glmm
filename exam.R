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

# read in the full dataset
data <- read_tsv("mass_sj19031.tsv")














