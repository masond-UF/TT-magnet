# 8 October 2021—David S. Mason
# Cleaning the raw seed removal data

library(lubridate)
library(ggplot2)
library(magrittr)

# Bring in the data
d <- read.csv("Raw-data/Seed-trap/Seed-trap-removal-raw-excel-munged.csv")

# Make sure the dates are dates
d$START.DATE <- mdy(d$START.DATE)
d$END.DATE <- mdy(d$END.DATE)
str(d)

# Calculate seeds removed
d <- d %>%
	dplyr::mutate(SEEDS.RM = abs(END.SEEDS-START.SEEDS))

# Calculate length of trial
d <- d %>%
	dplyr::mutate(INTERVAL = START.DATE %--% END.DATE) %>% 
	dplyr::mutate(TRIAL.LENGTH = as.numeric(as.duration(INTERVAL), "days"))

# Calculate removal rate
d <- d %>%
	dplyr::mutate(SEEDS.RM.RATE = SEEDS.RM/TRIAL.LENGTH) 

# Calculate proportion removed
d <- d %>%
	dplyr::mutate(SEEDS.RM.PROP = SEEDS.RM/START.SEEDS) 


write.csv(d,"Clean-data/Seed-trap/Seed-trap-removal-cleaned.csv")
