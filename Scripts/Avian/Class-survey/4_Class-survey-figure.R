## --------------- HEADER ------------------------------------------------------
## Script name: 4_Class-survey-figure.R
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

rm(list=ls())
means <- read.csv("Analysis/Avian/Class-survey-func-other-means.csv")
means.date <- read.csv("Analysis/Avian/Class-survey-func-other-means-date.csv")

means.date$DATE <- as.Date(means.date$DATE)
is.Date(means.date$DATE)

levels(means$TREATMENT)[levels(means$TREATMENT)=="BURN"] <- "Recent burn"
levels(means$TREATMENT)[levels(means$TREATMENT)=="CONTROL"] <- "One-year rough"


ggplot(means, aes(x = FUNCTIONAL, y = response, fill = FUNCTIONAL))+
	geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
								width = 0)+
	geom_point(size = 10, pch = 21, color = "black")+
	scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
	scale_y_continuous(limits = c(0,30))+
	ylab("Daily detections")+
	xlab("")+
	theme_bw()+
	theme(text = element_text(size = 22),
				legend.title = element_blank(),
				legend.position = "none",
				plot.title = element_text(hjust = 0.5),
				strip.background = element_blank(),
				strip.text.y = element_blank(),
				axis.title.x = element_text(face='bold', vjust=-2.5),
				axis.title.y = element_text(face='bold', vjust=3),
				strip.text.x = element_text(size = 18,face ='bold'),
				legend.spacing.x = unit(0.5, 'cm'),
				plot.margin = unit(c(1,1.2,0.9,1.2),"cm"),
				legend.box.spacing = unit(1.2,'cm'),
				axis.ticks.x = element_blank(),
				axis.ticks.y = element_line(size=1.2))+
	facet_wrap(~TREATMENT, ncol=2)
 
ggsave("Figures/Avian/Class-survey.png")
