## --------------- HEADER ------------------------------------------------------
## Script name: 2_Class-survey-EDA.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-6-29
## Date Last Modified: 2022-6-29
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script for exploring the WHM class avian survey data

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Clear the deck
rm(list = ls())

brd.surv <- read.csv("Clean-data/Avian/Avian-class-survey-func-other.csv")

## --------------- DATA EXPLORATION --------------------------------------------
library(DataExplorer)

plot_str(brd.surv)
plot_missing(brd.surv) # nothing missing
plot_histogram(brd.surv)

## --------------- DISTRIBUTIONS -----------------------------------------------
library(fitdistrplus)

descdist(brd.surv$COUNT, discrete = TRUE) # Nearly poisson

## --------------- VISUALIZATIONS ----------------------------------------------

ggplot(d = brd.surv, aes(x = FUNCTIONAL, y = COUNT))+
	geom_violin()+
	geom_point()+
	facet_wrap(~TREATMENT)
