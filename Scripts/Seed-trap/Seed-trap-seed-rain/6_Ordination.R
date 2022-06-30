# 2021-10-14—David S. Mason—Creating an ordination figure
###################### NMDS ####################################################
# Load in the data
d <- read.csv("Clean-data/Seed-trap/Seed-trap-composition-cleaned.csv")

# Drop the extraneous row
d <- d[, 2:34]

# Separate environmental and community/species matrix
env <- d[, 1:3]
spec <- d[, 4:33]

# Run NMDS ordination
library(vegan)
ord <- metaMDS(spec)
###################### EXTRACT NMDS SCORES #####################################

# Using the scores function from vegan to extract 
# the site scores and convert to a data.frame
site.scores <- as.data.frame(scores(ord))

# create a column of site names, from the rownames of data.scores
site.scores <- cbind(env, site.scores)

# Using the scores function from vegan to extract 
# the species scores and convert to a data.frame
spec.scores <- as.data.frame(scores(ord, "species"))  

# create a column of species, from the rownames of species.scores
spec.scores$SPECIES <- rownames(spec.scores)  

###################### PLOT NMDS ###############################################
library(tidyverse)

site.scores$SITE <- as.factor(site.scores$SITE)

ord.spec.lab <- ggplot() + 
	ggrepel::geom_text_repel(data=spec.scores,
													 aes(x=NMDS1,y=NMDS2, label=SPECIES))+
	geom_point(data=site.scores,
						 aes(x=NMDS1,y=NMDS2,colour=TREATMENT),size=6)+
	coord_equal(xlim = c(-2, 1.5), ylim = c(-1, 1))+
	theme_classic()+
	theme(text = element_text(size=22))+
	theme(plot.margin = unit(c(0, -2, 0, 0), "in"))+
	scale_color_manual(values = c('#1F9E89FF','orange'))
	

ggsave("Outputs/Seed-trap-seed-rain-trmt-comp-ord-fig-lab.png")

ord.spec  <- ggplot() + 
	geom_point(data=site.scores,
						 aes(x=NMDS1,y=NMDS2,colour=TREATMENT),size=6, alpha=0.6)+
	coord_equal(xlim = c(-2, 1.5), ylim = c(-1, 1))+
	theme_classic()+
	theme(text = element_text(size=22))+
	theme(plot.margin = unit(c(0, -2, 0, 0), "in"))+
	scale_color_manual(values = c('#1F9E89FF','orange'))

ggsave("Outputs/Seed-trap-seed-rain-trmt-comp-ord-fig.png")
