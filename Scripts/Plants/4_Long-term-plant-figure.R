## --------------- HEADER ------------------------------------------------------
## Script name: 4_Long-term-plant-figure.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-6-29
## Date Last Modified: 2022-6-29
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script for visualizing a binomial 
## model of detection for focal species

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Clear the deck
rm(list = ls())

means <- read.csv("Analysis/Plants/Long-term-veg-binomial-means.csv")

# Change labels
levels(means$TREATMENT)[levels(means$TREATMENT) == "FIRE"] <- "2-YEAR RETURN INTERVAL"
levels(means$TREATMENT)[levels(means$TREATMENT) == "CONTROL"] <- "FIRE EXCLUSION"

ggplot(d = means, aes(y = TREATMENT, x = prob, fill = TREATMENT))+
	geom_errorbar(aes(xmin = prob-SE, xmax = prob+SE),
								width = 0, size = 3)+
	geom_point(size = 20, shape = 21)+
	scale_fill_manual(values = c('#1F9E89FF' ,'orange'))+
	ylab("") +
	xlab("Probability of detecting focal species")+
	theme_bw() +
	theme(
				legend.position = 'NONE',
				axis.title.x = element_text(face = "bold"),
				text = element_text(size = 20))

ggsave(filename = "Figures/Plants/Long-term-plant-binomial.png")
