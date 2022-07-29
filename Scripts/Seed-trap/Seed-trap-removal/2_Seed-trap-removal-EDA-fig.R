# 08 October 2021—David S. Mason
# Creating an EDA figure for seed removal data

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(plyr)

# Load in data and select useful columns
d <- read.csv("Clean-data/Seed-trap/Seed-trap-removal-cleaned.csv")
d <- d %>% select(TRIAL, INTERVAL, SITE, TREATMENT, TRAP, TRIAL.LENGTH,
											 SPECIES, SEEDS.RM.RATE, SEEDS.RM.PROP)

# Expand abbreviations in the text
d$TREATMENT <- revalue(d$TREATMENT, c("B"="BURN", "C"="CONTROL"))
d$SPECIES <- revalue(d$SPECIES, c("SIAL"="MUSTARD", 
																		"HEAN"="SUNFLOWER",
																		"CAAN"="PEPPER"))

library(fitdistrplus)
d$SEEDS.RM.RATE <- as.numeric(d$SEEDS.RM.RATE)
descdist(d$SEEDS.RM.RATE)

library(betareg)
glmer(SEEDS.RM.RATE ~ TREATMENT*TRIAL + (1|SITE), family = binomial,
							data = d)


# Create functons to calculate confidence intervals
lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

# Calculate mean seed removal
means.seed.rm <- d %>%
 	dplyr::group_by(TRIAL, TREATMENT, SPECIES) %>%
  dplyr::summarise(smean = mean(SEEDS.RM.RATE, na.rm = TRUE),
            ssd = sd(SEEDS.RM.RATE, na.rm = TRUE),
  					count = n()) %>%
  dplyr::mutate(se = ssd / sqrt(count),
         lower_ci = lower_ci(smean, se, count),
         upper_ci = upper_ci(smean, se, count))

# Make a figure for mean seed removal
ggplot(means.seed.rm, aes(x = TRIAL, y = smean, col = TREATMENT))+
	scale_color_manual(values = c('orange', '#1F9E89FF'))+
	facet_wrap(~SPECIES, dir = "v")+
	scale_x_continuous(name = "Weeks since burn",
										 breaks = c(1, 2, 3, 4, 5, 6),
									   labels = c("1", "3", "5", "7", "13","17"))+
	ylab("Seeds removed per day")+
	geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
								 position=position_dodge(width=0.75))+
	theme(text = element_text(size = 20))+
	theme_bw()

ggsave("Figures/Seed-trap-removal-errorbars-rate.svg", device = "svg", dpi = 300)

# Calculate mean seed proportion removed 
means.seed.prop <- d %>%
 	dplyr::group_by(TRIAL, TREATMENT, SPECIES) %>%
  dplyr::summarise(smean = mean(SEEDS.RM.PROP, na.rm = TRUE),
            ssd = sd(SEEDS.RM.RATE, na.rm = TRUE),
  					count = n()) %>%
  dplyr::mutate(se = ssd / sqrt(count),
         lower_ci = lower_ci(smean, se, count),
         upper_ci = upper_ci(smean, se, count))

# Make a figure for mean seed removal
ggplot(means.seed.prop, aes(x = TRIAL, y = smean, col = TREATMENT))+
	scale_color_manual(values = c('orange', '#1F9E89FF'))+
	facet_wrap(~SPECIES, dir = "v")+
	scale_x_continuous(name = "Weeks since burn",
										 breaks = c(1, 2, 3, 4, 5, 6),
									   labels = c("1", "3", "5", "7", "13","17"))+
	ylab("Seeds proportion removed")+
	geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
								 position=position_dodge(width=0.75))+
	theme(text = element_text(size = 20))+
	theme_bw()

ggsave("Figures/Seed-trap-removal-errorbars-proportion.svg", device = "svg", dpi = 300)
