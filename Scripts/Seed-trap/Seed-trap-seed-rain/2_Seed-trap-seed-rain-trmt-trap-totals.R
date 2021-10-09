# 08 October 2021—David S. Mason—Exploring treatment and trap totals
# Set up ####
d <- read.csv("Raw-data/Seed-trap/2021-10-08_Seed-trap-seed-rain.csv")
dim(d)
str(d)

d$DATE <- mdy(d$DATE)
d$TREATMENT <- revalue(d$TREATMENT, c("B"="BURN", "C"="1-YEAR ROUGH"))

# Calculate seed detections in each trap and sampling period ####
d <- d %>% 
	dplyr::mutate(TOTAL.SEEDS = rowSums(d[, 5:88]))

sum(d$TOTAL.SEEDS) # 9381 seeds caught total

# Calculate adjusted seed detections  ####

# WHY WON'T THIS WORK!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# for(i in 1:nrow(d)){
#	if(d$TREATMENT[i] == "BURN"){
#	 	 d$TOTAL.SEEDS.ADJ = d$TOTAL.SEEDS
#		} else {
#		d$TOTAL.SEEDS.ADJ = (d$TOTAL.SEEDS*0.3352113)
#	}
# }

# Separate 1-year rough and burn because the for loop won't work

d.burn <- filter(d,TREATMENT=="BURN")
d.rough <- filter(d,TREATMENT=="1-YEAR ROUGH")

# Calculate adjusted totals

d.burn <- mutate(d.burn, TOTAL.SEEDS.ADJ = TOTAL.SEEDS)
d.rough <- mutate(d.rough, TOTAL.SEEDS.ADJ = TOTAL.SEEDS*0.3352113)

# Bring back together
d.adj <- rbind(d.burn, d.rough)

# Summarize seed detections in each trap  ####

# Adjusted totals
tot.trap.seed.det.adj <- d.adj %>% group_by(SITE, TREATMENT, TRAP) %>% 
	dplyr::summarise(sum(TOTAL.SEEDS.ADJ))

# Not adjusted
tot.trap.seed.det <- d %>% group_by(SITE, TREATMENT, TRAP) %>% 
	dplyr::summarise(sum(TOTAL.SEEDS))

# Fix the names of the summarize column  ####
# Adjusted
names(tot.trap.seed.det.adj)[names(tot.trap.seed.det.adj) 
														 == "sum(TOTAL.SEEDS.ADJ)"] <- "TOTAL.SEEDS.ADJ"

# Not adjusted
names(tot.trap.seed.det)[names(tot.trap.seed.det) 
														 == "sum(TOTAL.SEEDS)"] <- "TOTAL.SEEDS"

# Calculate mean, sd, se, and CI for seed detections by treatment ####

# Adjusted
means.tot.trap.seed.det.adj <- tot.trap.seed.det.adj %>%
 	dplyr::group_by(TREATMENT) %>%
  dplyr::summarise(smean = mean(TOTAL.SEEDS.ADJ, na.rm = TRUE),
            ssd = sd(TOTAL.SEEDS.ADJ, na.rm = TRUE),
  					count = n()) %>%
  dplyr::mutate(se = ssd / sqrt(count),
         lower_ci = lower_ci(smean, se, count),
         upper_ci = upper_ci(smean, se, count))

# Not adjsuted
means.tot.trap.seed.det <- tot.trap.seed.det %>%
 	dplyr::group_by(TREATMENT) %>%
  dplyr::summarise(smean = mean(TOTAL.SEEDS, na.rm = TRUE),
            ssd = sd(TOTAL.SEEDS, na.rm = TRUE),
  					count = n()) %>%
  dplyr::mutate(se = ssd / sqrt(count),
         lower_ci = lower_ci(smean, se, count),
         upper_ci = upper_ci(smean, se, count))


# Combine the dataframes
means.tot.trap.seed.det.comb <- rbind(means.tot.trap.seed.det.adj, 
																 means.tot.trap.seed.det)

# Drop one of the burns because it is not adjusted
means.tot.trap.seed.det.comb <- means.tot.trap.seed.det.comb[-3,]

# Rename the treatment
means.tot.trap.seed.det.comb$TREATMENT <- c("BURN","1-YEAR-ROUGH-ADJ", 
																						"1-YEAR-ROUGH")

# Convert to factor
means.tot.trap.seed.det.comb$TREATMENT <- as_factor(means.tot.trap.seed.det.comb$TREATMENT)

# Make bar graphs of seed totals ####
ggplot(means.tot.trap.seed.det.comb, aes(y = smean, x = TREATMENT, 
																				 col = TREATMENT, fill = TREATMENT))+
	geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
								 position=position_dodge(width=0.75))+
	geom_bar(position = 'dodge', stat = 'identity')+
	scale_color_manual(values = c('black', 'black', 'black'))+
	scale_fill_manual(values = c('orange', '#1F9E89FF', '#1F9E89FF'))+
	ylab("Mean seeds per trap")+
	xlab("Treatment")+
	scale_y_continuous(expand = c(0,0),
										 limits = c(0,600),
										 breaks = c(0,100,200,300,400,500,600))+
	theme_classic()+
	theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))+
	theme(legend.position = "none")
	
ggsave("Figures/Seed-trap-seed-rain-mean-trap-totals-barplot.svg", 
				device = "svg", dpi = 300)

# Create a dataframe of the trap totals by site ####
tot.trap.seed.det.adj$SITE <- as_factor(tot.trap.seed.det.adj$SITE)
tot.trap.seed.det$SITE <- as_factor(tot.trap.seed.det$SITE)

# Make the column names the same
names(tot.trap.seed.det.adj)[names(tot.trap.seed.det.adj) 
														 == "TOTAL.SEEDS.ADJ"] <- "TOTAL.SEEDS"

# Drop the burns from one of the dataframes
tot.trap.seed.det.adj <- filter(tot.trap.seed.det.adj,
																TREATMENT == "1-YEAR ROUGH")

# Rename the treatment to account for the adjustment
tot.trap.seed.det.adj$TREATMENT <- plyr::revalue(tot.trap.seed.det.adj$TREATMENT, 
																					c("1-YEAR ROUGH"="1-YEAR-ROUGH-ADJ"))

# Combine the dataframes
tot.trap.seed.det.comb <- rbind(tot.trap.seed.det.adj, tot.trap.seed.det)

# Make scatterplots of trap totals by each site to show variation ####

ggplot(tot.trap.seed.det.comb, aes(x = TREATMENT, y = TOTAL.SEEDS,
																	fill = TREATMENT))+
	geom_jitter(size = 4, alpha = 0.5, width = 0.15, height = 0.15,
							pch=21, color = "black")+
	scale_fill_manual(values = c('orange', '#1F9E89FF', '#1F9E89FF'))+
	ylim(limits = c(0,3000))+
	facet_wrap(~SITE)

# Only adjusted

# Adjusted
tot.trap.seed.det.adj <- d.adj %>% group_by(SITE, TREATMENT, TRAP) %>% 
	dplyr::summarise(sum(TOTAL.SEEDS.ADJ))

# Adjusted
names(tot.trap.seed.det.adj)[names(tot.trap.seed.det.adj) 
														 == "sum(TOTAL.SEEDS.ADJ)"] <- "TOTAL.SEEDS.ADJ"

# Create a dataframe of the adjusted trap totals by site ####
tot.trap.seed.det.adj$SITE <- as_factor(tot.trap.seed.det.adj$SITE)

# Rename the treatment to account for the adjustment
tot.trap.seed.det.adj$TREATMENT <- plyr::revalue(tot.trap.seed.det.adj$TREATMENT, 
																					c("1-YEAR ROUGH"="1-YEAR-ROUGH-ADJ"))

# Rename the sites for better labels
tot.trap.seed.det.adj$SITE <- plyr::revalue(tot.trap.seed.det.adj$SITE, 
																					c("1"="SITE 1", "4"="SITE 4",
																						"6"="SITE 6", "7"="SITE 7",
																						"8"="SITE 8", "16"="SITE 16",
																						"17"= "SITE 17"))

# Make scatterplots of adjusted trap totals by each site to show variation ####

ggplot(tot.trap.seed.det.adj, aes(x = TREATMENT, y = TOTAL.SEEDS.ADJ,
																	fill = TREATMENT))+
	geom_jitter(size = 4, alpha = 0.5, width = 0.15, height = 0.15,
							pch=21, color = "black")+
	scale_fill_manual(values = c('orange', '#1F9E89FF', '#1F9E89FF'))+
	scale_y_continuous(limits=c(0,1000))+
	ylab("Total seeds")+
	xlab("")+
	facet_wrap(~SITE)+
	theme_bw()+
	theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))+
	scale_x_discrete(labels = NULL, breaks = NULL)

ggsave("Figures/Seed-trap-seed-rain-per-trap-totals-scatter.svg", 
				device = "svg", dpi = 300)
