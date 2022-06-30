## --------------- HEADER ------------------------------------------------------
## Script name: 2_Long-term-plant-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-6-28
## Date Last Modified: 2022-6-28
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script explores the longterm plant data 

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Clear the deck
rm(list = ls())

focal <- read.csv("Clean-data/Plants/Long-term-veg-focal.csv")
dispersal <- read.csv("Clean-data/Plants/Long-term-veg-dispersal.csv")

## --------------- DATA EXPLORATION --------------------------------------------
library(DataExplorer)

plot_str(focal)
plot_missing(focal) # nothing missing
plot_histogram(focal)

plot_str(dispersal)
plot_missing(dispersal) # nothing missing
plot_histogram(dispersal)

## --------------- DISTRIBUTIONS -----------------------------------------------
library(fitdistrplus)

descdist(dispersal$COUNT, discrete = TRUE) # Nearly poisson

## --------------- VISUALIZATIONS ----------------------------------------------

ggplot(d = focal, aes(x = Y))+
	geom_histogram()+
	facet_wrap(~TREATMENT)

ggplot(d = dispersal, aes(x = DISPERSAL.MODE, y = COUNT, color = DISPERSAL.MODE))+
		geom_violin()+
		facet_wrap(~TREATMENT)
