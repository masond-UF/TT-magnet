# 2021-10-14—David S. Mason—Exploring composition 
###################### MANY GLM ANALYSIS #######################################
library(mvabund)
mod1 <- manyglm(spec.adj ~ d.wd.sum.red.adj$TREATMENT, family="poisson")
plot(mod1)
anova(mod1)

mod2 <- manyglm(spec.adj ~ d.wd.sum.red.adj$TREATMENT, family="negative_binomial")
plot(mod2)
anova(mod2, p.uni="adjusted")


