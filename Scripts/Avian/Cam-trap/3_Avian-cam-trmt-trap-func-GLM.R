# 2021 October 14—David S. Mason—Analzying avian camera trap data
brd.filt.func  <- read.csv("Clean-data/Avian/Avian-cam-clean.csv")

###################### SUMMARIZE THE DATA ######################################

# Data to compare all bird observations by treatment
tot.obs <- brd.filt.func %>% 
	group_by(Treatment,Site,Trap,Functional) %>% 
	summarize(Count = n())

# Data to visualize trend related to time
time.obs <- brd.filt.func %>% 
	group_by(Treatment,Site, Functional, Week) %>% 
	summarize(Count = n())

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
plot(sim.nst.OD.mod) # still doesn't look good

testUniformity(sim.nst.mod) # Failed
testDispersion(sim.nst.mod) # Failed
testZeroInflation(sim.nst.mod) # Failed

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
