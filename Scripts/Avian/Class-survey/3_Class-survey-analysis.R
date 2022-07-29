## --------------- HEADER ------------------------------------------------------
## Script name: 3_Class-survey-analysis.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-6-29
## Date Last Modified: 2022-6-29
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script for analyzing the WHM class avian survey data

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)
library(lme4)
library(DHARMa)
library(emmeans)

# Clear the deck
rm(list = ls())

brd.surv <- read.csv("Clean-data/Avian/Avian-class-survey-func-other.csv")

## --------------- RUN THE MODELS ----------------------------------------------

m1 <- glmer(COUNT ~ TREATMENT * FUNCTIONAL + (1|PAIR) + (1|GROUP),
					 data = brd.surv, family = 'poisson')
summary(m1)

m2 <- glmer.nb(COUNT ~ TREATMENT * FUNCTIONAL + (1|PAIR) + (1|GROUP),
						data = brd.surv)
summary(m2)

sim.m2 <- simulateResiduals(m2)
plot(sim.m2)

m3 <- glmer.nb(COUNT ~ TREATMENT * FUNCTIONAL * DATE + (1|PAIR) + (1|GROUP),
							 data = brd.surv)
summary(m3)
sim.m3 <- simulateResiduals(m3)
plot(sim.m3)

library(car)
Anova(m2)

## --------------- EXTRACT THE MEANS -------------------------------------------

means <- as.data.frame(emmeans(m2, ~TREATMENT*FUNCTIONAL, type = 'response'))
write.csv(means, "Analysis/Avian/Class-survey-func-other-means.csv",
					row.names = FALSE)

means <- as.data.frame(emmeans(m3, ~TREATMENT*FUNCTIONAL*DATE, type = 'response'))
write.csv(means, "Analysis/Avian/Class-survey-func-other-means-date.csv",
					row.names = FALSE)
