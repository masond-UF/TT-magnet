# 09 October 2021—David S. Mason—Exploring treatment and trap richness
# Set up #####

# Load packages
library(tidyverse)

# Load data
d <- read.csv("Raw-data/Seed-trap/2021-10-08_Seed-trap-seed-rain.csv")

# Check out the data
dim(d)
str(d)

# Convert dates
d$DATE <- mdy(d$DATE)
d$TREATMENT <- revalue(d$TREATMENT, c("B"="BURN", "C"="1-YEAR ROUGH"))

# Create a summarised site x species matrix ####

# Convert the data to long format 
d.long <- d %>% pivot_longer(cols = 5:88, 
														 names_to = "SPECIES", values_to = "SEEDS")

# Calculate the total seeds for each species at each trap
d.long.sum <- d.long %>% group_by(SITE, TREATMENT, TRAP, SPECIES) %>% 
	dplyr::summarise(SEEDS.TOT = sum(SEEDS))

# Convert the data to wide format
d.wide.sum <- d.long.sum %>% pivot_wider(names_from = SPECIES, 
																				 values_from = SEEDS.TOT)

# Calculate seed detection richness at each trap ####

# Count the number of zero observations (species not detected)
d.wide.sum$RICH <- 85-rowSums(d.wide.sum=="0")

# Remove everything else and make a seed detection richness dataframe
trap.rich <- d.wide.sum %>% select(SITE, TREATMENT, TRAP, RICH)

# Plot seed detection richness at each trap ####

# Rename the sites for better labels
trap.rich$SITE <- as.factor(trap.rich$SITE)
trap.rich$SITE <- plyr::revalue(trap.rich$SITE, 
																					c("1"="SITE 1", "4"="SITE 4",
																						"6"="SITE 6", "7"="SITE 7",
																						"8"="SITE 8", "16"="SITE 16",
																						"17"= "SITE 17"))

ggplot(trap.rich, aes(x = TREATMENT, y = RICH,
																	fill = TREATMENT))+
	geom_jitter(size = 4, alpha = 0.5, width = 0.15, height = 0.15,
							pch=21, color = "black")+
	scale_fill_manual(values = c('orange', '#1F9E89FF', '#1F9E89FF'))+
	ylab("Seed richness")+
	xlab("")+
	facet_wrap(~SITE)+
	scale_y_continuous(limits = c(0,20))+
	theme_bw()+
	theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))+
	scale_x_discrete(labels = NULL, breaks = NULL)

ggsave("Figures/Seed-trap-seed-rain-per-trap-richness-scatter.svg", 
				device = "svg", dpi = 300)
