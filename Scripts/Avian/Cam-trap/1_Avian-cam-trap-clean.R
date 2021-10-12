#12 October 2021—David S. Mason—Cleaning bird data
###################### SET—UP WORKSPACE ###############################
library(tidyverse)
library(lubridate)

brd <- read.csv("Raw-data/Avian/2021-10-12_Avian-cam-raw.csv")
###################### MUNGE DATA ##################################

# Convert the date into a format the tidyverse trucks with
brd$Date <- mdy(brd$Date)

# Round the date by week to improve figure EDA interpretation
brd$Week <- round_date(brd$Date, unit = 'week')

# Create vector of animals observed which are not birds
not.brds <- c("Bat", "Southern flying squirrel", "Unknown animal",
							"Unknown bat", "Unknown mammal", "Unknown mouse",
							"White-tailed deer", "Southern flying squirrel ")

# Filter out non-birds from the data set
brd.filt <- brd %>% filter(!Species %in% not.brds)

# Drop unknowns we can't assign a functional group to
brd.filt <- brd.filt %>% filter(!Species == "Unknown bird")

# Drop vulture because there is only one scavenger observation
brd.filt <- brd.filt %>% filter(!Species == "Unknown vulture")

# Check to make sure we only have useful values left
spec.list <- as.data.frame(unique(brd.filt$Species))

# Need to make Towhee into Eastern towhee for consistency
levels(brd.filt$Species)[levels(brd.filt$Species) == "Towhee"] <- "Eastern towhee"

# Need to make Eastern Kingbird into Eastern kingbird for consistency
levels(brd.filt$Species)[levels(brd.filt$Species) == "Eastern Kingbird"] <- "Eastern kingbird"

write.csv(brd.filt,"Clean-data/Avian/Avian-cam-clean.csv")
