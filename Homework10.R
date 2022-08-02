# Name: Byanca Moreno
#Date: 8/1/2022

# While diving off Pūpūkea Beach you notice an algae that you have never seen before.

# Question: Do parrotfish have an effect on invasive Macroalgae cover? 

# You suspect there might be an effect of water depth. 

# Any given location has an amount of native algae.  

# Do a cage experiment, excluding parrotfish from certain locations. 

# You have permits to work at 5 beaches, at both shallow and deep depths. 

# You have 160 cages. How do you design this experiment?

   # At each location there would be caged and non caged treatments and within those treatments shallow and deep. So each beach would have 4 total treatments: beach 1 would have caged with deep, caged with shallow, non caged with deep, and non caged with shallow. I would then have replicates of each. In this case, 8 replicates of each treatment. 

# Analyze the data with the appropriate statistical model and address the research question. Question: Do parrotfish have an effect on invasive Macroalgae cover? You suspect there might be an effect of water depth. 

####################### Notes for Byanca ##############
# fixed effects: all the possible values of a variable are fixed (treatment vs. control); identical for all groups in a population
# random effects: the set of potential values can change (e.g., family(nested designs) or blocks); permitted to differ from group to group

# 5 different locations (Sunset, Bowls, Queens, Kaena, DiamondHead) form population

# is water depth a fixed effect or a random effect? (fiixed + or random 1|), interaction *
# is location a fixed effect or a random effet?

######################################################
library(lme4)
algae<-read.csv("AlgaeProject.csv")
head(algae)

########## combining water depth and treatment #############
#this allows us to deal with the non-independence by assuming a different "baseline" of water depth and treatment for each location

comb.location.treat.waterdepth<-paste(algae[,2],algae[,3],algae[,4],sep="")
par(fig=c(0,1,.1,1))
boxplot(algae[,6]~comb.location.treat.waterdepth, las=2, cex.axis=.5 , ylab="mass", ylim=c(15,55), col=c("red"))
par(cex.axis=1, cex.lab=1, cex.main=2, cex.sub=1)
par(new=T)
boxplot(algae[,5]~comb.location.treat.waterdepth, las=2, cex.axis=.5 , ylab="", ylim=c(15,55), col=c("blue"))
legend("topleft", c("Invasive","Native"), col=c("red","blue"), pch=c(19,19), title="Legend")

#seems some variation in invasive but not in native


########################### model ###############

model<-lmer(Invasive.Algae.Mass~Native.Algae.Mass*Treatment*Water.Depth+(1+Native.Algae.Mass|Location)+(1+Treatment|Location)+(1+Water.Depth|Location), data=algae,REML=F)
summary(model)

library(lmerTest)
step(model)


######## notes for Byanca #################
# + indicates inclusion of an explanatory variable in the model (not addition);
# - indicates deletion of an explanatory variable from the model (not subtraction);
# * indicates inclusion of explanatory variables and interactions (not multiplication);
# / indicates nesting of explanatory variables in the model (not division);
# | indicates conditioning (not ‘or’), so that y~x | z is read as ‘y as a function of x given z’.


# A*B*C is the same as A+B+C+A:B+A:C+B:C+A:B:C;
# A/B/C is the same as A+B%in%A+C%in%B%in%A;
# (A+B+C)ˆ3 is the same as A*B*C;
# (A+B+C)ˆ2 is the same as A*B*C - A:B:C.

