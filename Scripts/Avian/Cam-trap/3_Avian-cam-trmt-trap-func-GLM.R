# 2021 October 14—David S. Mason—Analzying avian camera trap data
###################### SUMMARIZE TOTALS ########################################
library(tidyverse)

d  <- read.csv("Clean-data/Avian/Avian-cam-clean.csv")

# Data to compare all bird observations by treatment
tot.obs <- d %>% 
	group_by(Treatment,Site,Trap,Functional) %>% 
	summarize(Count = n())

# If func type is missing, the NA represents a true zero
tot.obs.wide <- pivot_wider(tot.obs, names_from = Functional,
														values_from = Count)

# Replace NA with zero
for(i in 1:35){
	if(is.na(tot.obs.wide$Frugivore[i])==TRUE){
		tot.obs.wide$Frugivore[i] <- 0
	} else {
			if(is.na(tot.obs.wide$Other[i])==TRUE){
				tot.obs.wide$Other[i] <- 0
		}
	}
}

# Return to long format
tot.obs <- pivot_longer(tot.obs.wide, 4:5,
												names_to = "Functional",
												values_to = "Count")

###################### CREATE THE TOTAL MODELS #################################
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(MuMIn)
library(DHARMa)
library(RVAideMemoire)

# GLM
smpl.mod <- glm(Count~Treatment*Functional,data=tot.obs, family = 'poisson')
summary(smpl.mod)

sim.smpl.mod <- simulateResiduals(fittedModel = smpl.mod, n = 250)
plot(sim.smpl.mod) # warnings

### Test for overdispersion (ie. more variance in the data than expected)
testDispersion(sim.smpl.mod) # overdispersion

### Test for zero-inflation (ie. more zeros in the data than expected)
testZeroInflation(sim.smpl.mod) # not zero-inflated

##### GLMM
nst.mod <- glmer(Count~Treatment*Functional+(1|Site),data=tot.obs, family = 'poisson')
summary(nst.mod)

###### Diagnose GLMM model fit 

sim.nst.mod <- simulateResiduals(fittedModel = nst.mod, n = 250)
plot(sim.nst.mod) # little better than before

overdisp.glmer(nst.mod) # ratio should be 1 (its high)

####### GLM OD
tot.obs$obs <- 1:length(tot.obs$Count) # adding dummy variable as RV

nst.OD.mod <- glmer(Count~Treatment*Functional+(1|Site)+(1|obs),data=tot.obs, family = 'poisson')
summary(nst.OD.mod)

######## Diagnose the OD GLMM
sim.nst.OD.mod <- simulateResiduals(fittedModel = nst.OD.mod, n = 250)
plot(sim.nst.OD.mod) # looks good

testUniformity(sim.nst.mod) # Failed
testDispersion(sim.nst.mod) # Failed
testZeroInflation(sim.nst.mod) # passed

######### Negative binomial
nst.nb.mod <- glmer.nb(Count~Treatment*Functional+(1|Site), data=tot.obs)
summary(nst.nb.mod)

########## Diagnose the nb
sim.nst.nb.mod <- simulateResiduals(fittedModel = nst.nb.mod, n = 250)
plot(sim.nst.nb.mod)

########### Poisson with Zero Inflation (note no overdispersion). Is it enough to account for zero inflation without correcting for overdispersion? 
library(glmmTMB) ## new package. similar to lme4 but more flexibility and options

nst.ZI.mod <- glmmTMB(Count~Treatment*Functional+(1|Site), 
                      family=poisson, 
                      zi=~Treatment*Functional, 
                      data=tot.obs)

############ Diagnose the OD GLMM
sim.nst.ZI.mod <- simulateResiduals(fittedModel = nst.ZI.mod, n = 250)
plot(sim.nst.ZI.mod) # still doesn't look good

############# Poisson corrected for zero-inflation AND overdispersion
nst.ZI.OD.mod <- glmmTMB(Count~Treatment*Functional+(1|Site)+(1|obs), 
                     family=poisson, 
                     zi=~Treatment*Functional, 
                     data=tot.obs)
summary(nst.ZI.OD.mod)

############## Diagnose the ZI and OD GLMM 
sim.nst.ZI.OD.mod <- simulateResiduals(fittedModel = nst.ZI.OD.mod, n = 250)
plot(sim.nst.ZI.OD.mod) # Looks good

##############  ZI negative binomial
nst.ZI.nb.mod <- glmmTMB(Count~Treatment*Functional+(1|Site),
                     family=nbinom2, 
                     zi=~Treatment*Functional, 
                     data=tot.obs)

############### Diagnose the model
sim.nst.ZI.nb.mod <- simulateResiduals(fittedModel = nst.ZI.nb.mod, n = 250)
plot(sim.nst.ZI.nb.mod) # Looks decent

###################### COMPARE THE TOTAL MODELS ################################
summary(smpl.mod) ## summary for simple poisson model
Anova(smpl.mod)  

summary(nst.mod) ## summary for nested glmm
Anova(nst.mod)  

summary(nst.OD.mod)  ## summary for overdispered nested glmm
Anova(nst.OD.mod)  

summary(nst.nb.mod)  ## summary for nested negative binomial
Anova(nst.nb.mod)

summary(nst.ZI.mod)  ## summary for zero-inflated poisson
Anova(nst.ZI.mod)

summary(nst.ZI.OD.mod)  ## summary Poisson corrected for ZI AND OD
Anova(nst.ZI.OD.mod)
library(glmmTMB)
library(insight)
library(performance)

get_variance(nst.ZI.OD.mod)
r2(nst.ZI.OD.mod)

summary(nst.ZI.nb.mod)  ## summary forzero-inflated nb
Anova(nst.ZI.nb.mod)

## which model is best?
AIC(smpl.mod,nst.mod,nst.OD.mod,nst.nb.mod,nst.ZI.mod,
		nst.ZI.OD.mod,nst.ZI.nb.mod)

### examine SD associated with random effects 
options (scipen = 999)
VarCorr(nst.ZI.OD.mod) 
SD <- matrix(c("Site","obs","total",0.0000012401,0.8552376549, sum(0.0000012401+0.8552376549)),nrow=3,ncol=2)
colnames(SD) <- c("Groups", "SD")
SD <- as.data.frame(SD)

emmeans(nst.ZI.OD.mod, ~Treatment, type = 'response', bias.adj = T,
				sigma = sqrt(0.8552389))
emmeans(nst.ZI.OD.mod, ~Functional, type = 'response', bias.adj = T,
				sigma = sqrt(0.8552389))

means <- as.data.frame(emmeans(nst.ZI.OD.mod, ~Treatment*Functional, type = 'response', bias.adj = T,
				sigma = sqrt(0.8552389)))

write.csv(means, "Clean-data/Avian/Avian-cam-means.csv")



###################### SUMMARIZE TOTALS BY DATE ################################

d$Week <- as.Date(d$Week)

# Data to compare all bird observations by treatment and date
time.obs <- d %>% 
	group_by(Week,Treatment,Site,Trap,Functional) %>% 
	summarize(Count = n())

# If func type is missing, the NA represents a true zero
time.obs.wide <- pivot_wider(time.obs, names_from = Functional,
														values_from = Count)

# Replace NA with zero
for(i in 1:70){
	if(is.na(time.obs.wide$Frugivore[i])==TRUE){
		time.obs.wide$Frugivore[i] <- 0
	} else {
			if(is.na(time.obs.wide$Other[i])==TRUE){
				time.obs.wide$Other[i] <- 0
		}
	}
}

# Return to long format
time.obs <- pivot_longer(time.obs.wide, 5:6,
												names_to = "Functional",
												values_to = "Count")


###################### CREATE THE TIME MODELS ##################################
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(MuMIn)
library(DHARMa)
library(RVAideMemoire)

# GLM
smpl.mod <- glm(Count~Treatment*Functional*Week,data=time.obs, family = 'poisson')
summary(smpl.mod)

sim.smpl.mod <- simulateResiduals(fittedModel = smpl.mod, n = 250)
plot(sim.smpl.mod) # warnings

### Test for overdispersion (ie. more variance in the data than expected)
testDispersion(sim.smpl.mod) # overdispersion

### Test for zero-inflation (ie. more zeros in the data than expected)
testZeroInflation(sim.smpl.mod) # not zero-inflated

##### GLMM
nst.mod <- glmer(Count~Treatment*Functional*Week+(1|Site),
								 data=time.obs, family = 'poisson')
summary(nst.mod)

###### Diagnose GLMM model fit 

sim.nst.mod <- simulateResiduals(fittedModel = nst.mod, n = 250)
plot(sim.nst.mod) # little better than before

overdisp.glmer(nst.mod) # ratio should be 1 (its high)

####### GLM OD
time.obs$obs <- 1:length(time.obs$Count) # adding dummy variable as RV

nst.OD.mod <- glmer(Count~Treatment*Functional*Week+(1|Site)+(1|obs),
										data=time.obs, family = 'poisson')
summary(nst.OD.mod)

######## Diagnose the OD GLMM
sim.nst.OD.mod <- simulateResiduals(fittedModel = nst.OD.mod, n = 250)
plot(sim.nst.OD.mod) # looks good

testUniformity(sim.nst.mod) # Failed
testDispersion(sim.nst.mod) # Failed
testZeroInflation(sim.nst.mod) # passed

######### Negative binomial
nst.nb.mod <- glmer.nb(Count~Treatment*Functional*Week+(1|Site), 
											 data=time.obs)
summary(nst.nb.mod)

########## Diagnose the nb
sim.nst.nb.mod <- simulateResiduals(fittedModel = nst.nb.mod, n = 250)
plot(sim.nst.nb.mod)

########### Poisson with Zero Inflation (note no overdispersion). Is it enough to account for zero inflation without correcting for overdispersion? 
library(glmmTMB) ## new package. similar to lme4 but more flexibility and options

nst.ZI.mod <- glmmTMB(Count~Treatment*Functional*Week+(1|Site), 
                      family=poisson, 
                      zi=~., 
                      data=time.obs)

############ Diagnose the OD GLMM
sim.nst.ZI.mod <- simulateResiduals(fittedModel = nst.ZI.mod, n = 250)
plot(sim.nst.ZI.mod) # still doesn't look good

############# Poisson corrected for zero-inflation AND overdispersion
nst.ZI.OD.mod <- glmmTMB(Count~Treatment*Functional*Week+(1|Site)+(1|obs), 
                     family=poisson, 
                     zi=~., 
                     data=time.obs)
summary(nst.ZI.OD.mod)

############## Diagnose the ZI and OD GLMM 
sim.nst.ZI.OD.mod <- simulateResiduals(fittedModel = nst.ZI.OD.mod, n = 250)
plot(sim.nst.ZI.OD.mod) # Looks good

##############  ZI negative binomial
nst.ZI.nb.mod <- glmmTMB(Count~Treatment*Functional+(1|Site),
                     family=nbinom2, 
                     zi=~Treatment*Functional, 
                     data=tot.obs)

############### Diagnose the model
sim.nst.ZI.nb.mod <- simulateResiduals(fittedModel = nst.ZI.nb.mod, n = 250)
plot(sim.nst.ZI.nb.mod) # Looks decent

###################### COMPARE THE TIME MODELS #################################
summary(smpl.mod) ## summary for simple poisson model
Anova(smpl.mod)  

summary(nst.mod) ## summary for nested glmm
Anova(nst.mod)  

summary(nst.OD.mod)  ## summary for overdispered nested glmm
Anova(nst.OD.mod)  

summary(nst.nb.mod)  ## summary for nested negative binomial
Anova(nst.nb.mod)

summary(nst.ZI.mod)  ## summary for zero-inflated poisson
Anova(nst.ZI.mod)

summary(nst.ZI.OD.mod)  ## summary Poisson corrected for ZI AND OD
Anova(nst.ZI.OD.mod)
library(glmmTMB)
library(insight)
library(performance)

get_variance(nst.ZI.OD.mod)
r2(nst.ZI.OD.mod)

summary(nst.ZI.nb.mod)  ## summary forzero-inflated nb
Anova(nst.ZI.nb.mod)

## which model is best?
AIC(smpl.mod,nst.mod,nst.OD.mod,nst.nb.mod,nst.ZI.mod,
		nst.ZI.OD.mod,nst.ZI.nb.mod)

### examine SD associated with random effects 
options (scipen = 999)
VarCorr(nst.ZI.OD.mod) 
SD <- matrix(c("Site","obs","total",0.0000012401,0.8552376549, sum(0.0000012401+0.8552376549)),nrow=3,ncol=2)
colnames(SD) <- c("Groups", "SD")
SD <- as.data.frame(SD)



###################### SUMMARIZE MAGNET TOTALS #################################
d.mag <- filter(d, Week == "2020-04-19" |
									 Week == "2020-04-26" |
									 Week == "2020-05-03")

# Data to compare all bird observations by treatment
mag.obs <- d.mag %>% 
	group_by(Treatment,Site,Trap,Functional) %>% 
	summarize(Count = n())

# If func type is missing, the NA represents a true zero
mag.obs.wide <- pivot_wider(mag.obs, names_from = Functional,
														values_from = Count)

# Replace NA with zero
for(i in 1:14){
	if(is.na(mag.obs.wide$Frugivore[i])==TRUE){
		mag.obs.wide$Frugivore[i] <- 0
	} else {
			if(is.na(mag.obs.wide$Other[i])==TRUE){
				mag.obs.wide$Other[i] <- 0
		}
	}
}

# Return to long format
mag.obs <- as.data.frame(pivot_longer(mag.obs.wide, 4:5,
												names_to = "Functional",
												values_to = "Count"))

library(fitdistrplus)

plotdist(mag.obs$Count, histo = TRUE, demp = TRUE)
descdist(mag.obs$Count, discrete=TRUE, boot=500) # Poisson

mag.mod <- glmer(Count~Treatment*Functional+(1|Site),data=mag.obs, 
								 family = 'poisson')
summary(mag.mod)
Anova(mag.mod)

######## Diagnose the OD GLMM
sim.mag.mod <- simulateResiduals(fittedModel = mag.mod, n = 250)
plot(sim.mag.mod) # looks good
