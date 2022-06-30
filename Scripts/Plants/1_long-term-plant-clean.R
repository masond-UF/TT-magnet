## --------------- HEADER ------------------------------------------------------
## Script name: 1_Long-term-plant-clean.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-6-28
## Date Last Modified: 2022-6-28
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script prepares the longterm plant data for analysis
## and figures.

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Clear the deck
rm(list = ls())

# Bring in the data
plants <- read.csv("Raw-data/Plants/Long-term-veg-survey.csv")

## --------------- FIX SPECIES AND DROP NAs ------------------------------------

list <- as.data.frame(unique(plants$SPECIES))

for (i in 1:nrow(plants)){
	if (plants$SPECIES[i] == "Quercus"){
		plants$SPECIES[i] <- "Quercus sp."
	}
}

list <- as.data.frame(unique(plants$SPECIES))

plants <- plants %>% 
	drop_na()

## --------------- SUMMARIZE THE DATA ------------------------------------------

# Is this technically a truncated poisson? Need to check on this.

focal <- plants %>% 
	group_by(TRANSECT, TREATMENT, FOCAL) %>% 
	summarise(COUNT = n())

names(plants)[names(plants) == "ANIMAL.DISPERSED"] <- "DISPERSAL.MODE"

write.csv(focal, "Clean-data/Plants/Long-term-veg-survey.csv")

dispersal <- plants %>% 
	group_by(TRANSECT, TREATMENT, DISPERSAL.MODE) %>% 
	summarise(COUNT = n())

write.csv(dispersal, "Clean-data/Plants/Long-term-veg-survey.csv")
