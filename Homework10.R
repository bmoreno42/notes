# Name: Byanca Moreno

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


# Doesn't seem to be a difference in mass based on location/treatment in native species- mass is pretty consistent 
# Within the invasive species there is a difference since there is a gap between Queen, Sunset and the other locations

########## combining location and treatment #############
#this allows us to deal with the non-independence by assuming a different "baseline" of location and treatment for each beach

comb.location.treat<-paste(algae[,2],algae[,4],sep="")
par(fig=c(0,1,.1,1))
boxplot(algae[,6]~comb.location.treat, las=2, cex.axis=.5 , ylab="mass", ylim=c(15,55), col=c("red"))
par(cex.axis=1, cex.lab=1, cex.main=2, cex.sub=1)
par(new=T)
boxplot(algae[,5]~comb.location.treat, las=2, cex.axis=.5 , ylab="", ylim=c(15,55), col=c("blue"))
legend("topleft", c("Invasive","Native"), col=c("red","blue"), pch=c(19,19), title="Legend")

# for native there is really no difference in mass based on beach and treatment
# for invasice there is a difference in mass based on on beach and treatment

########## combining treatment and water depth #############
#this allows us to deal with the non-independence by assuming a different "baseline" for each treatment and water depth
comb.treat.depth<-paste(algae[,3],algae[,4],sep="")
par(fig=c(0,1,.1,1))
boxplot(algae[,6]~comb.treat.depth, las=2, cex.axis=.5 , ylab="mass", ylim=c(15,55), col=c("red"))
par(cex.axis=1, cex.lab=1, cex.main=2, cex.sub=1)
par(new=T)
boxplot(algae[,5]~comb.treat.depth, las=2, cex.axis=.5 , ylab="", ylim=c(15,55), col=c("blue"))
legend("topleft", c("Invasive","Native"), col=c("red","blue"), pch=c(19,19), title="Legend")

# for native there doesn't seem to be a difference in mass based on treatment and water depth
# for invasive there seems to be a difference between caged and non-caged treatments but not based on water depth


########## combining location and water depth #############
#this allows us to deal with the non-independence by assuming a different "baseline" for each location and water depth
comb.location.depth<-paste(algae[,2],algae[,4],sep="")
par(fig=c(0,1,.1,1))
boxplot(algae[,6]~comb.location.depth, las=2, cex.axis=.5 , ylab="mass", ylim=c(15,55), col=c("red"))
par(cex.axis=1, cex.lab=1, cex.main=2, cex.sub=1)
par(new=T)
boxplot(algae[,5]~comb.location.depth, las=2, cex.axis=.5 , ylab="", ylim=c(15,55), col=c("blue"))
legend("topleft", c("Invasive","Native"), col=c("red","blue"), pch=c(19,19), title="Legend")



# for native there doesn't seem to be a difference in mass based on location and water depth
# for invasive there seems to be a difference between Queens, Sunset beaches and the other beaches


########################### model ###############
model<-lm(Invasive.Algae.Mass~Native.Algae.Mass+Location+Treatment, data=algae)
summary(model)



mod<-lmer(Invasive.Algae.Mass~comb.treat.depth+(1|Location), data=algae) #location as a random effect
summary(mod)
anova(mod)


boxplot(Invasive.Algae.Mass~Location, data=algae)
boxplot(Invasive.Algae.Mass~Treatment, data=algae)
boxplot(Invasive.Algae.Mass~Water.Depth, data=algae)

mean.invasive.shallow<-mean(algae[algae[,4]=="shallow",6]) # 35.4605
mean.invasive.deep<-mean(algae[algae[,4]=="deep",6]) # 36.436

mean.invasive.caged<-mean(algae[algae[,3]=="caged",6]) # 32.94262
mean.invasive.noncaged<-mean(algae[algae[,3]=="non-caged",6]) #38.95388


mean.native.shallow<-mean(algae[algae[,4]=="shallow",5]) # 20.07625
mean.native.deep<-mean(algae[algae[,4]=="deep",5]) # 20.005

mean.native.caged<-mean(algae[algae[,3]=="caged",5]) # 20
mean.native.noncaged<-mean(algae[algae[,3]=="non-caged",5]) # 20.08125


# Std.Dev gives us a measure of how much variability in invasive algae mass is due to variation among location
#Residual is still what we're unable to explain in our model



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

