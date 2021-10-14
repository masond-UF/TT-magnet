# 2021 October 12—David S. Mason—Exploring avian camera trap data
###################### DATA AND PACKAGES ####################################### 
brd.filt.func  <- read.csv("Clean-data/Avian/Avian-cam-clean.csv")
library(tidyverse)
library(lubridate)
###################### EXPLORATORY DATA ANALYSIS ###############################  

# Looking at bird observations by treatment
brd.filt.func %>% group_by(Treatment,Site) %>% summarize(Count = n()) %>% 
	ggplot(aes(x = Treatment, y = Count)) + geom_boxplot()

# Looking by site (looks like an important RV)
brd.filt.func %>% group_by(Site, Treatment) %>% summarize(Count = n()) %>% 
	ggplot(aes(x = Treatment, y = Count)) + geom_col() + facet_wrap(~Site)

# Looking by functional group (looks like a possible covariate)
brd.filt.func %>% group_by(Treatment, Functional) %>% 
	summarize(Count = n()) %>% 
	ggplot(aes(x = Treatment, y = Count, color = Functional)) + 
	geom_col(position = "Dodge") 

# Lets explore the magnet effect with Date/Week (WOAH!)
brd.filt.func %>% 
	group_by(Treatment,Week,Functional) %>% 
	summarize(Count = n()) %>% 
	ggplot(aes(x = Week, y = Count, color = Functional))+
	geom_point()+
	geom_line()+
	facet_wrap(~Treatment)



