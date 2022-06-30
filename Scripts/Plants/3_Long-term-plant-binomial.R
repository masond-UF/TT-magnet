## --------------- HEADER ------------------------------------------------------
## Script name: 3_Long-term-plant-binomial.R
## Author: David S. Mason, UF D.E.E.R. Lab
## Department: Wildlife Ecology and Conservation
## Affiliaton: University of Florida
## Date Created: 2022-6-29
## Date Last Modified: 2022-6-29
## Copyright (c) David S. Mason, 2022
## Contact: masond@ufl.edu, @EcoGraffito
## Purpose of script: This script for conducting a binomial 
## model of detection for focal species

## --------------- SET—UP WORKSPACE --------------------------------------------
library(tidyverse)
library(tidylog)
library(lubridate)
library(styler)

# Clear the deck
rm(list = ls())

focal <- read.csv("Clean-data/Plants/Long-term-veg-focal.csv")

successes <- focal$Y
failures <- focal$N # - successes??

## --------------- CONDUCT MODEL -----------------------------------------------

mod <- glm(cbind(successes, failures) ~ TREATMENT, 
					 data=focal, family=binomial(link = "logit") )

hist(mod$residuals)
summary(mod)

# Explore simulated residuals
library(DHARMa)
sim_mod <- simulateResiduals(fittedModel = mod, n = 250)
plot(sim_mod) # VIOLATED

# Extract means
mod_means <- as.data.frame(emmeans(mod, pairwise ~ TREATMENT, type="response"))      ## type= does contrasts before back-transforming (more appropriate!)
mod_means <- mod_means[-3, -2]

# Save model output
write.csv(mod_means, "Analysis/Plants/Long-term-veg-binomial-means.csv",
					row.names = FALSE)

