# 08 October 2021—David S. Mason
# Creating an EDA figure for seed removal data

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(plyr)
library(car)

# Load in data and select useful columns
d <- read.csv("Clean-data/Seed-trap/Seed-trap-removal-cleaned.csv")
d <- d %>% dplyr::select(TRIAL, INTERVAL, SITE, TREATMENT, TRAP, TRIAL.LENGTH,
									SPECIES, START.SEEDS, END.SEEDS)
d <- d %>% 
	mutate(PROP = END.SEEDS / START.SEEDS)
	
m1 <- glmer(PROP ~ TREATMENT + (1|TRIAL/SITE) + (1|SPECIES),
			data = d, family = binomial) 

Anova(m1)

pairs(emmeans(m1, ~TREATMENT))

d1 <- d %>% 
	filter(TRIAL == "1")

m2 <- glmer(PROP ~ TREATMENT + (1|SITE) + (1|SPECIES),
						data = d1, family = binomial) 
Anova(m2)

pairs(emmeans(m2, ~TREATMENT))

d2 <- d %>% 
	filter(TRIAL == "2")

m3 <- glmer(PROP ~ TREATMENT + (1|SITE) + (1|SPECIES),
						data = d2, family = binomial) 
Anova(m3)

pairs(emmeans(m3, ~TREATMENT))

d3 <- d %>% 
	filter(TRIAL == "3")

m4 <- glmer(PROP ~ TREATMENT + (1|SITE) + (1|SPECIES),
						data = d3, family = binomial) 
Anova(m4)

pairs(emmeans(m4, ~TREATMENT))
