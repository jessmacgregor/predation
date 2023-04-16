#packages
install.packages("bbmle", dependencies=TRUE)
install.packages("lme4", dependencies=TRUE)
install.packages("lattice", dependencies=TRUE)
install.packages("multcomp", dependencies=TRUE)
install.packages("R2admb")

library(bbmle)
library(lme4)
library(lattice)
library(multcomp)
library(glmmTMB)

#data
head(pred1)
str(pred1)

#convert things to factors
pred1$Number=as.factor(pred1$Number) 
pred1$Treatment=as.factor(pred1$Treatment)
pred1$Site=as.factor(pred1$Site)
pred1$monthname=as.factor(pred1$monthname)


#GLMM with zero inflated poisson dist, this worked!
#need to find a paper or 2 to back up
#include shell height as a fixed effect nested in month?? for now no.
model1 = glmmTMB(Alive.after.24hr~Treatment + (Site) + (monthname) + (1|Number), 
                 family="poisson", ziformula = ~1, 
               data=pred1)
summary(model1)
library(car)
Anova(model1)
library(performance)
check_overdispersion(model1)
#just curious - tukey tests
tukey=glht(model1, linfct=mcp(Treatment="Tukey")) #Tukey tests
summary.tukey = summary(tukey)
summary(tukey)
#not significant at all

#what if we pooled treatment just to get effect of site

model2 <- glm(Alive.after.24hr~ Site + (monthname), 
                 family="poisson", 
                 data=pred1)
summary(model2)
Anova(model2)
tukey2=glht(model2, linfct=mcp(Site="Tukey")) #Tukey tests
summary.tukey = summary(tukey2)
summary(tukey2)
