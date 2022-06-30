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

names(plants)[names(plants) == "ANIMAL.DISPERSED"] <- "DISPERSAL.MODE"

## --------------- SUMMARIZE THE DATA ------------------------------------------

# Are these technically truncated distributions? In other words, can you use
# transects for a count when they are meant to represent cover? 
# Need to check on this.

# Create dataframe for focal species
focal <- plants %>% 
	group_by(TRANSECT, TREATMENT, FOCAL) %>% 
	summarise(COUNT = n())

# Convert into binary format 
focal <- focal %>% 
	pivot_wider(names_from = FOCAL, values_from = COUNT)

# Replace NAs with zeroes to reflect no detections 
focal[is.na(focal)] <- 0

# Reorganize
focal <- focal %>% dplyr::select(TREATMENT, TRANSECT, Y, N)

write.csv(focal, "Clean-data/Plants/Long-term-veg-focal.csv",
					row.names = FALSE)

# Create dataframe for dispersal mode 

dispersal <- plants %>% 
	group_by(TRANSECT, TREATMENT, DISPERSAL.MODE) %>% 
	summarise(COUNT = n())

# An NA was incorrectly recorded as N
dispersal <- dispersal %>% 
	filter(DISPERSAL.MODE != "N")

# Need to introduce zeroes

# Pivot wider to generate NAs in columns with no detections of dispersal mode
dispersal.wd <- dispersal %>% 
	pivot_wider(names_from = DISPERSAL.MODE, values_from = COUNT)

# Replace NAs with zeroes to reflect no detections 
dispersal.wd[is.na(dispersal.wd)] <- 0

# Return dataframe to long format 
dispersal <- dispersal.wd %>% 
	pivot_longer(cols = 3:9, names_to = "DISPERSAL.MODE", values_to = "COUNT")

write.csv(dispersal, "Clean-data/Plants/Long-term-veg-dispersal.csv",
					row.names = FALSE)
