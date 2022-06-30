# 2021-10-14—David S. Mason—Cleaning site by species matrix
###################### SUMMARIZE BY TRAP #######################################
library(tidyverse)

d <- read.csv("Raw-data/Seed-trap/2021-10-08_Seed-trap-seed-rain.csv")

d$TREATMENT <- plyr::revalue(d$TREATMENT, c("B"="BURN", "C"="1-YEAR ROUGH"))

d.lg <- pivot_longer(d, 5:88, names_to = "SPECIES", values_to = "COUNT")

d.lg.sum <- group_by(d.lg, SITE, TREATMENT, TRAP, SPECIES) %>% 
						summarise(COUNT = sum(COUNT))

d.wd.sum  <- pivot_wider(d.lg.sum, names_from = "SPECIES", values_from = "COUNT")
###################### REMOVE RARE & COM SPEC ##################################

# Separate into environmental and species matrix
env <- as.data.frame(d.wd.sum[,1:3])
spec <- as.matrix(d.wd.sum[,4:87])
spec <- spec[, colSums(spec != 0) > 0] # drop columns without observtions

# Determine rare and common species
source("Functions/biostats.r")
occur <- foa.plots(spec)
rare <- which(occur[,2]<5)
common <- which(occur[,2]>95)

# Create reduced community matrix
red_spec <- as.data.frame(spec[,-c(rare,common)])

# Bring back the environmental to enable filtering
d.wd.sum.red <- cbind(env,red_spec)

# Filter by treatment
d.wd.sum.red.burn <- filter(d.wd.sum.red, TREATMENT=="BURN")
d.wd.sum.red.con <- filter(d.wd.sum.red, TREATMENT=="1-YEAR ROUGH")

###################### ADJUST FOR FRUIT AVAIL ##################################

# Multiply values in 1-year-rough (control) matrix by the correction constant
d.wd.sum.red.con.spec <- d.wd.sum.red.con[,4:33]
for(i in 1:21){
	for(j in 1:30){
		d.wd.sum.red.con.spec[i,j] = d.wd.sum.red.con.spec[i,j]*0.3352113
	}
}

# Separate environmental components from filtered control matrix
d.wd.sum.red.con.env <- d.wd.sum.red.con[,1:3]

# Combine the control environmental and adjusted community matrix
d.wd.sum.red.con <- cbind(d.wd.sum.red.con.env, d.wd.sum.red.con.spec)

# Combine the adjusted control matrix with the burn matrix
d.wd.sum.red.adj <- rbind(d.wd.sum.red.con, d.wd.sum.red.burn)

# Separate environmental and community components of the new combined matrix
env.adj <- as.vector(d.wd.sum.red.adj[,2])
env.adj <- as.vector(as.factor(env.adj))
spec.adj <- as.matrix(round(d.wd.sum.red.adj[,4:33]))

# Save the cleaned data set 
write.csv(d.wd.sum.red.adj,"Clean-data/Seed-trap/Seed-trap-composition-cleaned.csv")