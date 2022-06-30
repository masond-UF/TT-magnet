## --------------- HEADER ------------------------------------------------------
## Script name: 1_Class-survey-clean.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-6-29
## Date Last Modified: 2022-6-29
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script for cleaning the WHM class avian survey data

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Clear the deck
rm(list = ls())

brd.surv <- read.csv("Raw-data/Avian/Class-survey.csv")

## --------------- CLEAN DATA --------------------------------------------------

# Fix the date
brd.surv$DATE <- mdy(brd.surv$DATE)

# Drop pair 2 on the first day
brd.surv <- brd.surv %>% 
	filter(DATE != "2021-05-18" | PAIR != "2")

# Drop NAs
brd.surv <- brd.surv %>%  na.omit()

# Standardize factor levels
for (i in 1:nrow(brd.surv)){
	if (brd.surv$FUNCTIONAL[i] == "Others"){
		brd.surv$FUNCTIONAL[i] <- "Other"
	}
}

## --------------- SUMMARIZE DATA ----------------------------------------------

# We have bird species tallied by species and sex, but we just care about 
# species. 

brd.surv <- brd.surv %>% 
	group_by(DATE, PAIR , GROUP, TREATMENT, FUNCTIONAL) %>% 
	summarize(COUNT = sum(COUNT))

# Pivot wider
brd.surv <- brd.surv %>% 
	pivot_wider(names_from = FUNCTIONAL, values_from = COUNT)

brd.surv[is.na(brd.surv)] <- 0

# Pivot longer
brd.surv <- brd.surv %>% 
	pivot_longer(cols = 5:7, names_to = "FUNCTIONAL", values_to = "COUNT")

write.csv(brd.surv, "Clean-data/Avian/Avian-class-survey-func-other.csv",
					row.names = FALSE)

## --------------- CALCULATE ES ------------------------------------------------

# Need to add 1 to each value *****

# Pivot wider
brd.surv <- brd.surv %>% 
	pivot_wider(names_from = FUNCTIONAL, values_from = COUNT)

burn <- brd.surv %>% filter(TREATMENT == "BURN")
control <- brd.surv %>% filter(TREATMENT == "CONTROL")


frugivore.df <- NULL
other.df <- NULL

for (i in 1:nrow(burn)){
	frugivore.df[i] <- log(burn[i,5]/control[i,5])
	other.df[i] <- log(burn[i,6]/control[i,6])
}

frugivore.df <- as.data.frame(unlist(frugivore.df))
colnames(frugivore.df)[1] <- "Frugivores"

other.df <- as.data.frame(unlist(other.df))
colnames(other.df)[1] <- "Other"

pred <- as.data.frame(burn[,1:2])

merge <- cbind(pred, frugivore.df, other.df)

write.csv(merge, "Clean-data/Avian/Avian-class-survey-func-other-ES.csv",
					row.names = FALSE)

