require(ggplot2)
require(doBy)
#read the model data

ModelLisp <- read.csv("~/GitHub/ACT_R/Instance_Learning.csv")
#ModelProd <- read.csv("~/GMU/Lab/Trust/Model/output3 model with punishment.csv")

#read experimental data
ExpData <- read.csv("~/GMU/Lab/Trust/Data/trust_overview.1.2.2014.csv")
#create variable names which match experimental data
# ModelProd$condition <- paste(ModelProd$TPR,"/",ModelProd$FPR)
# ModelProd$switchcnt_tosound <- ModelProd$Switches_to_sound
# ModelProd$switchcnt_extra <- ModelProd$switch.to.no.sound
str(ModelLisp)
#ModelLisp = subset(ModelLisp, wait.reward==1)
ModelLisp$condition <- paste(ModelLisp$TPR,"/",ModelLisp$FPR)
ModelLisp$switchcnt_tosound <- ModelLisp$CuedSwitch
ModelLisp$switchcnt_extra <- ModelLisp$UnCuedSwitch

ExpData$condition <- paste(ExpData$intended_hitrate, "/",ExpData$intended_falsealarmrate)

#make sure it looks right
head(ModelLisp)
head(ExpData)

mean(ExpData$totaltrials)

#matrix of graphs of parameter space
ParametersToSound <- ggplot(ModelLisp, aes(x=factor(condition), y=CuedSwitch))
ParametersToSound + geom_boxplot() + facet_grid(~ans)

ParamNoSound <- ggplot(ModelLisp, aes(x=factor(condition), y=UnCuedSwitch))
ParamNoSound + geom_boxplot() + facet_grid(~ans)

#overlay graphs to emperical data
#first create graphs of emperical data
overlay_to_sound <- ggplot(ExpData, aes(x=factor(condition),y=switchcnt_tosound,fill=factor(condition)))
overlay_to_sound <- overlay_to_sound + stat_summary(fun.y="mean", geom="bar") + scale_fill_grey()
overlay_to_sound <- overlay_to_sound + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
#print(overlay_to_sound)


overlay_to_no_sound <- ggplot(ExpData, aes(x=factor(condition),y=switchcnt_extra,fill=factor(condition)))
overlay_to_no_sound <- overlay_to_no_sound + stat_summary(fun.y="mean", geom="bar") + scale_fill_grey()
overlay_to_no_sound <- overlay_to_no_sound + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
#print(overlay_to_no_sound)
#example error bars ----bootstrapped
#p <- overlay_to_sound + stat_summary(fun.y="mean", geom="bar")
#p + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)

##melt th

#Now overlay the graphs with the model data
overlay_to_sound <- overlay_to_sound + theme_bw() + stat_summary(data=ModelLisp, aes(x=factor(condition),y=switchcnt_tosound,group=ans),fun.y="mean",geom="line")
overlay_to_sound <- overlay_to_sound + facet_grid(~ans)
ggsave(plot=overlay_to_sound, "~/GMU/Lab/Trust/Spring 2014/graphs/Instance Model/All_parameters_ans_overlay Cued.png")
print(overlay_to_sound)

overlay_to_no_sound <- overlay_to_no_sound + theme_bw() + stat_summary(data=ModelLisp, aes(x=factor(condition),y=switchcnt_extra,group=ans),fun.y="mean",geom="line")
overlay_to_no_sound <- overlay_to_no_sound + facet_grid(~ans)
ggsave(plot=overlay_to_no_sound, "~/GMU/Lab/Trust/Spring 2014/graphs/Instance Model/All_parameters_ans_overlay Uncued.png")
print(overlay_to_no_sound)
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
ModelLisp_means <- summaryBy(switchcnt_tosound+ switchcnt_extra~ ans + condition, data=ModelLisp,FUN=c(mean,sd))
ExpData_means$toSound.ci <- 2 * ExpData_means$switchcnt_tosound.sd / sqrt(15)
ModelLisp_means$toSound.ci <- 2 * ModelLisp_means$switchcnt_tosound.sd / sqrt(15)
ExpData_means$to.noSound.ci <- 2 * ExpData_means$switchcnt_extra.sd / sqrt(15)
ModelLisp_means$to.noSound.ci <- 2 * ModelLisp_means$switchcnt_extra.sd / sqrt(15)

#i need to do a correlation analysis on items with the same condition.
#for RMSD i could do a loop for the ModelLisp df to compare to the expdata df
#the df should be set up to have the means already.
#gonna create a function that runs the loops over the two data frames 
#the data frames should have the same variables, but not necessarily the same length
#this is written so that the model df will be the longer one
CalculateFits <- function(modeldf, expdf) {
  #sink outputs to the speciefied file instead of the console
  sink("Instance Learning Model Fits to Exp 1.txt")
  #fitsdf <- modeldf
  
  #start the loop for each egs
  for (ANS in unique(modeldf$ans)) {
    dummyANS <- subset(modeldf, subset= ans==ANS)
  
    #nest the loop for each RED_Reward
#     for(reward in unique(dummyegs$ans)){
#         dummyReward <- subset(dummyegs,ans==reward)
        #nest again for each condition
        #for(condition in unique(dummyReward$condition)){
            #dummyCondition <- subset(dummyReward,condition==condition)
#this sub-section calculates the fits for the switch_cnt variable
            cat("ans =", dummyANS$ans[1],"|| Switches to Sound",
                "r^2 = ",cor(dummyANS$switchcnt_tosound.mean,expdf$switchcnt_tosound.mean),
                "RMSD = ", rmsd(dummyANS$switchcnt_tosound.mean,expdf$switchcnt_tosound.mean),
                fill=T)
            GoodFits <- InRange(dummyANS$switchcnt_tosound.mean, expdf$switchcnt_tosound.mean - expdf$toSound.ci, 
                                expdf$switchcnt_tosound.mean + expdf$toSound.ci)
            cat("  Number of model points in CI for",ANS,"= ", GoodFits)
          if (GoodFits == 4)
            cat(" *************")
          if (GoodFits == 3)
            cat(" +++++++++++++")
          cat(fill=T)
#this subsection calculates the fits for the extra switches
          cat("ans =", dummyANS$ans[1],"|| Switches to No Sound",
                "r^2 = ",cor(dummyANS$switchcnt_extra.mean,expdf$switchcnt_extra.mean),
                "RMSD = ", rmsd(dummyANS$switchcnt_extra.mean,expdf$switchcnt_extra.mean),
                fill=T)
          GoodFits <- InRange(dummyANS$switchcnt_extra.mean, expdf$switchcnt_extra.mean - expdf$to.noSound.ci, 
                              expdf$switchcnt_extra.mean + expdf$to.noSound.ci)
          cat("  Number of model points in CI for", ANS ,"= ", GoodFits)
          if (GoodFits == 4)
              cat(" *************")
          if (GoodFits == 3)
              cat(" +++++++++++++")
          cat(fill=T)
          cat(fill=T)
        #}
    #}
  }
  
}

testfits <- CalculateFits(ModelLisp_means,ExpData_means)
sink()


#overlay graphs to emperical data this time only use the best fit 
#first create graphs of emperical data
overlay_to_sound <- ggplot(ExpData, aes(x=factor(condition),y=switchcnt_tosound)) + theme_bw(base_size = 18)
overlay_to_sound <- overlay_to_sound + stat_summary(fun.y="mean", geom="bar",fill="dark grey")
overlay_to_sound <- overlay_to_sound + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
#separate the means to use for the fit based on fit output
bestfit <- subset(ModelLisp_means, subset= ans==.6)
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
bestfitRaw <- subset(ModelLisp, subset= wait.reward == na)
#then calculate the anova
anovaModelCued <- aov(switchcnt_tosound ~ condition, data=bestfitRaw)
summary(anovaModelCued)
anovaModelUncued <- aov(switchcnt_extra ~ condition, data=bestfitRaw)
summary(anovaModelUncued)

head(bestfit)