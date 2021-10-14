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

# Make vectors for each functional group
Frugivores <- c("American crow", "Blue jay", "Eastern towhee",
							 "Northern cardinal", "Pileated woodpecker",
							 "Unknown woodpecker", "Eastern bluebird", "Eastern kingbird", 
								"Eastern phoebe",	"Northern mockingbird", "Red-eyed vireo", 
								"Yellow-breasted chat", "Yellow-throated vireo")
Others <- c("Mourning dove", "Blue grosbeak", "Brown-headed cowbird", 
						"Field sparrow", "Carolina wren", "Chuck-will's-widow", "Common yellowthroat",
						"Great crested flycatcher", "Loggerhead shrike")

# Create and fill a column for functional groups
brd.filt.func <- brd.filt %>% 
	mutate(Functional = ifelse(Species %in% Frugivores, "Frugivore",
											ifelse(Species %in% Others, "Other","None")))

# Check to make sure this worked
unique(brd.filt.func$Functional)

# We have one trap that was started earlier than others &
# there may be a true zero at control. Either way it looks messy.

brd.filt.func <- brd.filt.func %>% 
	filter(!Week=='2020-04-12')

write.csv(brd.filt.func,"Clean-data/Avian/Avian-cam-clean.csv")
