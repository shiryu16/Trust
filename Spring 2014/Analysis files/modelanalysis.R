require(ggplot2)
#read the model data
ModelLisp <- read.csv("~/GMU/Lab/Trust/Model/reward-via-lisp.csv")
ModelProd <- read.csv("~/GMU/Lab/Trust/Model/output3 model with punishment.csv")

#read experimental data
ExpData <- read.csv("~/GMU/Lab/Trust/trust_overview.12.9.2013-1.csv")
#create variable names which match experimental data
ModelProd$condition <- paste(ModelProd$TPR,"/",ModelProd$FPR)
ModelProd$switchcnt_tosound <- ModelProd$Switches_to_sound
ModelProd$switchcnt_extra <- ModelProd$switch.to.no.sound
ModelLisp = subset(ModelLisp, wait.reward==1)
ModelLisp$condition <- paste(ModelLisp$TPR,"/",ModelLisp$FPR)
ModelLisp$switchcnt_tosound <- ModelLisp$Switches_to_sound
ModelLisp$switchcnt_extra <- ModelLisp$switch.to.no.sound
ExpData$condition <- paste(ExpData$intended_hitrate, "/",ExpData$intended_falsealarmrate)

#make sure it looks right
head(ModelLisp)
head(ExpData)

mean(ExpData$totaltrials)

#matrix of graphs of parameter space
ParametersToSound <- ggplot(ModelData, aes(x=factor(condition), y=Switches_to_sound))
ParametersToSound + geom_boxplot() + facet_grid(BLUE_REWARD ~ wait.reward + RED_REWARD, labeller =label_both)

ParamNoSound <- ggplot(ModelData, aes(x=factor(condition), y=switch.to.no.sound))
ParamNoSound + geom_boxplot() + facet_grid(BLUE_REWARD ~ wait.reward + RED_REWARD, labeller =label_both)

#overlay graphs to emperical data
#first create graphs of emperical data
overlay_to_sound <- ggplot(ExpData, aes(x=factor(condition),y=switchcnt_tosound,fill=factor(condition)))
overlay_to_sound <- overlay_to_sound + stat_summary(fun.y="mean", geom="bar") + scale_fill_grey()
overlay_to_sound <- overlay_to_sound + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)

overlay_to_no_sound <- ggplot(ExpData, aes(x=factor(condition),y=switchcnt_extra,fill=factor(condition)))
overlay_to_no_sound <- overlay_to_no_sound + stat_summary(fun.y="mean", geom="bar") + scale_fill_grey()
overlay_to_no_sound + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
#example error bars ----bootstrapped
#p <- overlay_to_sound + stat_summary(fun.y="mean", geom="bar")
#p + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)

#Now overlay the graphs with the model data
overlay_to_sound <- overlay_to_sound + theme_bw() + stat_summary(data=ModelData, aes(x=factor(condition),y=switchcnt_tosound, group=RED_REWARD),fun.y="mean",geom="line",colour="red")
overlay_to_sound + facet_grid(RED_REWARD ~ EGS)

overlay_to_no_sound <- overlay_to_no_sound + theme_bw() + stat_summary(data=ModelData, aes(x=factor(condition),y=switchcnt_extra, group=RED_REWARD),fun.y="mean",geom="line",colour="red")
overlay_to_no_sound + facet_grid(RED_REWARD ~ EGS)

#so graphs are done... lets try to get some stats happening
#RMSD function from greg's code
rmsd <- function(l1, l2) {
  num <- sum((l1-l2)^2)
  den <- length(l1)
  return(sqrt(num/den))
}

### InRange takes a vector and a low number and a high number
### it returns the number of values in the vector that are >= low
### and <= high
### InRange also works with a vector and a corresponding low and high vector
InRange <- function(v, low, high) {
  
  return(sum(v >= low & v <= high))
  
}

ExpData_means <- summaryBy(switchcnt_tosound + switchcnt_extra~condition,data=ExpData,FUN=c(mean,sd))
ModelData_means <- summaryBy(switchcnt_tosound+ switchcnt_extra~RED_REWARD + EGS + condition, data=ModelData,FUN=c(mean,sd))
ExpData_means$toSound.ci <- 2 * ExpData_means$switchcnt_tosound.sd / sqrt(15)
ModelData_means$toSound.ci <- 2 * ModelData_means$switchcnt_tosound.sd / sqrt(15)
ExpData_means$to.noSound.ci <- 2 * ExpData_means$switchcnt_extra.sd / sqrt(15)
ModelData_means$to.noSound.ci <- 2 * ModelData_means$switchcnt_extra.sd / sqrt(15)

#i need to do a correlation analysis on items with the same condition.
#for RMSD i could do a loop for the modeldata df to compare to the expdata df
#the df should be set up to have the means already.
#gonna create a function that runs the loops over the two data frames 
#the data frames should have the same variables, but not necessarily the same length
#this is written so that the model df will be the longer one
CalculateFits <- function(modeldf, expdf) {
  #sink outputs to the speciefied file instead of the console
  sink("ModelFits.txt")
  fitsdf <- modeldf
  #start the loop for each egs
  for (egs in unique(modeldf$EGS)) {
    dummyegs <- subset(modeldf, EGS==egs)
    #nest the loop for each RED_Reward
    for(reward in unique(dummyegs$RED_REWARD)){
        dummyReward <- subset(dummyegs,RED_REWARD==reward)
        #nest again for each condition
        #for(condition in unique(dummyReward$condition)){
            #dummyCondition <- subset(dummyReward,condition==condition)
#this sub-section calculates the fits for the switch_cnt variable
            cat("egs =", dummyegs$EGS[1], " Red Reward =", dummyReward$RED_REWARD[1], "Switches to Sound",
                "r^2 = ",cor(dummyReward$switchcnt_tosound.mean,expdf$switchcnt_tosound.mean),
                "RMSD = ", rmsd(dummyReward$switchcnt_tosound.mean,expdf$switchcnt_tosound.mean),
                fill=T)
            GoodFits <- InRange(dummyReward$switchcnt_tosound.mean, expdf$switchcnt_tosound.mean - expdf$toSound.ci, 
                                expdf$switchcnt_tosound.mean + expdf$toSound.ci)
            cat("  Number of model points in CI for", egs,"/",reward,"= ", GoodFits)
          if (GoodFits == 4)
            cat(" *************")
          if (GoodFits == 3)
            cat(" +++++++++++++")
          cat(fill=T)
#this subsection calculates the fits for the extra switches
          cat("egs =", dummyegs$EGS[1], " Red Reward =", dummyReward$RED_REWARD[1], "Switches to No Sound",
                "r^2 = ",cor(dummyReward$switchcnt_extra.mean,expdf$switchcnt_extra.mean),
                "RMSD = ", rmsd(dummyReward$switchcnt_extra.mean,expdf$switchcnt_extra.mean),
                fill=T)
          GoodFits <- InRange(dummyReward$switchcnt_extra.mean, expdf$switchcnt_extra.mean - expdf$to.noSound.ci, 
                              expdf$switchcnt_extra.mean + expdf$to.noSound.ci)
          cat("  Number of model points in CI for", egs,"/",reward,"= ", GoodFits)
          if (GoodFits == 4)
              cat(" *************")
          if (GoodFits == 3)
              cat(" +++++++++++++")
          cat(fill=T)
          cat(fill=T)
        #}
    }
  }
  
}

testfits <- CalculateFits(ModelData_means,ExpData_means)
sink()


#overlay graphs to emperical data this time only use the best fit 
#first create graphs of emperical data
overlay_to_sound <- ggplot(ExpData, aes(x=factor(condition),y=switchcnt_tosound)) + theme_bw(base_size = 18)
overlay_to_sound <- overlay_to_sound + stat_summary(fun.y="mean", geom="bar",fill="dark grey")
overlay_to_sound <- overlay_to_sound + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
#separate the means to use for the fit based on fit output
bestfit <- subset(ModelData_means, subset= EGS == .7 & RED_REWARD == 8)
#plot the separated data
overlay_to_sound <- overlay_to_sound + geom_point(data=bestfit, aes(y=switchcnt_tosound.mean, x= factor(condition)),size=4)
overlay_to_sound +  labs(x = "Hit Rate/False Alarm Rate", y= "Mean Cued Switches")

# now do it for the uncued switches
overlay_to_no_sound <- ggplot(ExpData, aes(x=factor(condition),y=switchcnt_extra)) + theme_bw(base_size = 16)
overlay_to_no_sound <- overlay_to_no_sound + stat_summary(fun.y="mean", geom="bar",fill="dark grey")
overlay_to_no_sound <-overlay_to_no_sound + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
overlay_to_no_sound <- overlay_to_no_sound + geom_point(data=bestfit, aes(y=switchcnt_extra.mean, x= factor(condition)),size=4)
overlay_to_no_sound +  labs(x = "Hit Rate/False Alarm Rate", y= "Mean Uncued Switches")

#calculate anova for model switches
#first separate the best fits data 
bestfitRaw <- subset(ModelData, subset= wait.reward == na)
#then calculate the anova
anovaModelCued <- aov(switchcnt_tosound ~ condition, data=bestfitRaw)
summary(anovaModelCued)
anovaModelUncued <- aov(switchcnt_extra ~ condition, data=bestfitRaw)
summary(anovaModelUncued)

head(bestfit)