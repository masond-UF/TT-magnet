# 2021 October 14—David S. Mason—Figures for avian camera trap data
###################### LOAD THE DATA ###########################################

means <- read.csv("Clean-data/Avian/Avian-cam-means.csv")
brd.filt.func <- read.csv("Clean-data/Avian/Avian-cam-clean.csv")
###################### MAKE TOTAL FIGURE #######################################
library("ggpubr")
levels(means$Treatment)[levels(means$Treatment)=="B"] <- "Recent burn"
levels(means$Treatment)[levels(means$Treatment)=="C"] <- "One-year rough"

levels(means$Functional)[levels(means$Functional)=="Frugivore"] <- "Disperser"

p1 <- ggplot(means, aes(x = Functional, y = rate))+
	geom_linerange(aes(ymin=lower.CL,ymax=upper.CL, col=Functional), 
								 size=1.5)+
	geom_point(aes(col = Functional),size = 8)+
	geom_point(shape = 1,size = 8,colour = "black")+
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
	scale_color_manual(values = c("#00AFBB", "#E7B800"))+
	scale_y_continuous(breaks=c(0,2,4,6,8,10), limits = c(0,11))+
	ylab("Mean total observations")+
	xlab("Feeding guild")+
	facet_wrap(~Treatment)
	
###################### CREATE THE TIME-SERIES FIGURE ########################### 
levels(brd.filt.func$Treatment)[levels(brd.filt.func$Treatment)=="B"] <- "Recent burn"
levels(brd.filt.func$Treatment)[levels(brd.filt.func$Treatment)=="C"] <- "One-year rough"

levels(brd.filt.func$Functional)[levels(brd.filt.func$Functional)=="Frugivore"] <- "Disperser"
brd.filt.func$Week<-as.Date(brd.filt.func$Week)

p2 <- brd.filt.func %>% 
	group_by(Treatment,Site,Week,Functional) %>% 
	summarize(Count = n()) %>% 
	group_by(Treatment, Week, Functional) %>% 
	summarize(Sum = sum(Count)) %>%
	ggplot(aes(x = Week, y = Sum, color = Functional))+
	geom_line(size=1.5, alpha=0.5)+
	geom_point(size=3.5)+
	geom_point(shape = 1,size = 3.5,colour = "black")+
	theme_bw()+
	theme(text = element_text(size = 22),
				legend.title = element_blank(),
				legend.position = 'none',
				plot.title = element_text(hjust = 0.5),
				strip.background = element_blank(),
   			strip.text.y = element_blank(),
				axis.title.x = element_text(face='bold', vjust=-2.5),
				axis.title.y = element_text(face='bold', vjust=3),
				strip.text.x = element_text(size = 18,face ='bold'),
				axis.text.x = element_text(size=10),
				legend.spacing.x = unit(0.5, 'cm'),
				plot.margin = unit(c(1,1.2,0.9,1.2),"cm"),
				legend.box.spacing = unit(1.2,'cm'),
				axis.ticks.x = element_line(size=1.2),
				axis.ticks.y = element_line(size=1.2))+
	scale_color_manual(values = c("#00AFBB", "#E7B800"))+
	scale_y_continuous(breaks=c(0,10,20,30), limits = c(0,30))+
	scale_x_date(date_breaks = "1 week", labels = function(x) format(x, "%d-%b"))+
	ylab("Total detections")+
	xlab("Sampling period")+
	facet_wrap(~Treatment,dir = 'v')
###################### COMBINE FIGURE ########################################## 
library(lemon)
grid_arrange_shared_legend(p1,p2)
###################### ANIMATED FIGURE ########################################## 
library(data.table)
library(sf)
library(gganimate)
library(gifski)
library(transformr)

p3 <- brd.filt.func %>% 
	group_by(Treatment,Site,Week,Functional) %>% 
	summarize(Count = n()) %>% 
	group_by(Treatment, Week, Functional) %>% 
	summarize(Sum = sum(Count)) %>%
	ggplot(aes(x = Week, y = Sum, color = Functional))+
	geom_line(size=1.5, alpha=0.5)+
	geom_point(size=3.5)+
	transition_reveal(Week)+
	theme_bw()+
	theme(text = element_text(size = 22),
				legend.title = element_blank(),
				legend.position = 'right',
				plot.title = element_text(hjust = 0.5),
				strip.background = element_blank(),
   			strip.text.y = element_blank(),
				axis.title.x = element_text(face='bold', vjust=-2.5),
				axis.title.y = element_text(face='bold', vjust=3),
				strip.text.x = element_text(size = 18,face ='bold'),
				axis.text.x = element_text(size=15,angle = 90),
				legend.spacing.x = unit(0.5, 'cm'),
				plot.margin = unit(c(1,1.2,0.9,1.2),"cm"),
				legend.box.spacing = unit(1.2,'cm'),
				axis.ticks.x = element_line(size=1.2),
				axis.ticks.y = element_line(size=1.2))+
	scale_color_manual(values = c("#00AFBB", "#E7B800"))+
	scale_y_continuous(breaks=c(0,2,4,6,8,10), limits = c(0,11))+
	scale_x_date(date_breaks = "1 week", date_labels = "%D")+
	ylab("Total detections")+
	xlab("Sampling period")+
	facet_wrap(~Treatment)

p3_anim <- gganimate::animate(p3, duration = 10)
gganimate::anim_save("p3_anim.gif", animation = p3_anim)