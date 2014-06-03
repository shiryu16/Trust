require(ggplot2)
require(doBy)
#read the model data
#model with wait reward given through lisp with punishment for blue box
ModelLisp <- read.csv("~/GMU/Lab/Trust/Model/reward-via-lisp.csv")
#model with wait reward given through lisp but no punishment for blue box
#ModelPunis <- read.csv("~/GMU/Lab/Trust/Model/reward-v-lisp-no-punish.csv")
#model with with production-firing wait reward and blue box punishment
ModelProd <- read.csv("~/GMU/Lab/Trust/Model/model with punishment.csv")

#read experimental data
ExpData <- read.csv("~/GMU/Lab/Trust/Spring 2014/Data/trust_overview.4.28.2014_subset.csv")

# #create variable names which match experimental data
# ModelPunis$condition <- paste(ModelPunis$TPR,"/",ModelPunis$FPR)
# ModelPunis$switchcnt_tosound <- ModelPunis$Switches_to_sound
# ModelPunis$switchcnt_extra <- ModelPunis$switch.to.no.sound

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
head(ModelProd)
View(ExpData)

mean(ExpData$totaltrials)

#matrix of graphs of parameter space
prodParamsToSound <- ggplot(ModelProd, aes(x=factor(condition), y=Switches_to_sound)) + labs(title = "Production Based Wait Reward")
prodParamsToSound + geom_boxplot() + facet_grid(BLUE_REWARD ~ wait.reward + RED_REWARD, labeller =label_both)

ModelProd <- subset(ModelProd, wait.reward==1)

prodParamNoSound <- ggplot(ModelProd, aes(x=factor(condition), y=switch.to.no.sound)) + labs(title = "Production Based Wait Reward")
prodParamNoSound + geom_boxplot() + facet_grid(BLUE_REWARD ~ wait.reward + RED_REWARD, labeller =label_both)


lispParamToSound <- ggplot(ModelLisp, aes(x=factor(condition), y=Switches_to_sound)) + labs(title = "Lisp based Wait Reward")
lispParamToSound + geom_boxplot() + facet_grid(BLUE_REWARD ~ wait.reward + RED_REWARD, labeller =label_both)

#ModelPunis <- subset(ModelPunis, wait.reward==1)
lispParamNoSound <- ggplot(ModelLisp, aes(x=factor(condition), y=switch.to.no.sound)) + labs(title = "Lisp based Wait Reward")
lispParamNoSound + geom_boxplot() + facet_grid(BLUE_REWARD ~ wait.reward + RED_REWARD, labeller =label_both)

#overlay graphs to emperical data
#first create graphs of emperical data
overlay_to_sound <- ggplot(ExpData, aes(x=condition,y=switchcnt_tosound)) + theme_bw()
overlay_to_sound <- overlay_to_sound + stat_summary(fun.y="mean", geom="bar")
overlay_to_sound <- overlay_to_sound + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)

overlay_to_no_sound <- ggplot(ExpData, aes(x=condition,y=switchcnt_extra))
overlay_to_no_sound <- overlay_to_no_sound + stat_summary(fun.y="mean", geom="bar") + theme_bw()
overlay_to_no_sound  <- overlay_to_no_sound + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
#example error bars ----bootstrapped
#p <- overlay_to_sound + stat_summary(fun.y="mean", geom="bar")
#p + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)

#Now overlay the graphs with the model data
overlay_to_sound <- overlay_to_sound + stat_summary(data=ModelLisp, aes(x=condition,y=switchcnt_tosound, group=RED_REWARD),fun.y="mean",geom="line",colour="red")
overlay_to_sound <- overlay_to_sound + facet_grid(RED_REWARD ~ BLUE_REWARD,labeller=label_both)  + labs(title="CS_w_Blue_punish_via_lisp", y="Cued Switches")
ggsave(overlay_to_sound, "~/GMU/Lab/Trust/Spring 2014/graphs/UL_model/param_explore_CS_w_Blue_punish_via_lisp.png")
print(overlay_to_sound)

overlay_to_no_sound <- overlay_to_no_sound + stat_summary(data=ModelLisp, aes(x=condition,y=switchcnt_extra, group=RED_REWARD),fun.y="mean",geom="line",colour="red")
overlay_to_no_sound  <- overlay_to_no_sound + facet_grid(RED_REWARD ~ BLUE_REWARD,labeller=label_both) + labs(title="US_w_Blue_punish_via_lisp", y= "Uncued Switches")
print(overlay_to_no_sound)# does an ok job of fitting the data 

###to analyze the alternate model, use the same base, then use the following two sections
overlay_to_sound <- overlay_to_sound + stat_summary(data=ModelProd, aes(x=condition,y=switchcnt_tosound, group=RED_REWARD),fun.y="mean",geom="line",colour="red")
overlay_to_sound <- overlay_to_sound + facet_grid(RED_REWARD ~ BLUE_REWARD) + labs(title="CS_w_Blue_punish_via_production")
ggsave(overlay_to_sound, "~/GMU/Lab/Trust/Spring 2014/graphs/UL_model/param_explore_CS_w_Blue_punish_via_lisp.png")
print(overlay_to_sound)

overlay_to_no_sound <- overlay_to_no_sound + stat_summary(data=ModelProd, aes(x=condition,y=switchcnt_extra, group=RED_REWARD),fun.y="mean",geom="line",colour="red")
overlay_to_no_sound  <- overlay_to_no_sound + facet_grid(RED_REWARD ~ BLUE_REWARD) + labs(title="US_w_Blue_punish_via_production", y="Uncued Switches")
print(overlay_to_no_sound) ##turns out this model greatly under-predicts the emperical data


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
ModelData_means <- summaryBy(switchcnt_tosound+ switchcnt_extra~RED_REWARD + BLUE_REWARD + condition,
                             data=ModelLisp,FUN=c(mean,sd))
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
CalculateFits <- function(modeldf, expdf, path) {
  #sink outputs to the speciefied file instead of the console
  sink(path)
  fitsdf <- modeldf
  #start the loop for each blue_reward
  for (blue_reward in unique(modeldf$BLUE_REWARD)) {
    dummyblue_reward <- subset(modeldf, BLUE_REWARD==blue_reward)
    #nest the loop for each RED_Reward
    for(reward in unique(dummyblue_reward$RED_REWARD)){
        dummyReward <- subset(dummyblue_reward,RED_REWARD==reward)
        #nest again for each condition
        #for(condition in unique(dummyReward$condition)){
            #dummyCondition <- subset(dummyReward,condition==condition)
#this sub-section calculates the fits for the switch_cnt variable
            cat("blue_reward =", dummyblue_reward$BLUE_REWARD[1], " Red Reward =", dummyReward$RED_REWARD[1], "Switches to Sound",
                "r^2 = ",cor(dummyReward$switchcnt_tosound.mean,expdf$switchcnt_tosound.mean),
                "RMSD = ", rmsd(dummyReward$switchcnt_tosound.mean,expdf$switchcnt_tosound.mean),
                fill=T)
            GoodFits <- InRange(dummyReward$switchcnt_tosound.mean, expdf$switchcnt_tosound.mean - expdf$toSound.ci, 
                                expdf$switchcnt_tosound.mean + expdf$toSound.ci)
            cat("  Number of model points in CI for", blue_reward,"/",reward,"= ", GoodFits)
          if (GoodFits == 4)
            cat(" *************")
          if (GoodFits == 3)
            cat(" +++++++++++++")
          cat(fill=T)
#this subsection calculates the fits for the extra switches
          cat("blue_reward =", dummyblue_reward$BLUE_REWARD[1], " Red Reward =", dummyReward$RED_REWARD[1], "Switches to No Sound",
                "r^2 = ",cor(dummyReward$switchcnt_extra.mean,expdf$switchcnt_extra.mean),
                "RMSD = ", rmsd(dummyReward$switchcnt_extra.mean,expdf$switchcnt_extra.mean),
                fill=T)
          GoodFits <- InRange(dummyReward$switchcnt_extra.mean, expdf$switchcnt_extra.mean - expdf$to.noSound.ci, 
                              expdf$switchcnt_extra.mean + expdf$to.noSound.ci)
          cat("  Number of model points in CI for", blue_reward,"/",reward,"= ", GoodFits)
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

testfits <- CalculateFits(ModelData_means,ExpData_means,"~/GMU/Lab/Trust/Spring 2014/Analysis files/Spr14_fits_w_punish_via_Lisp.txt")
sink()

#overlay graphs to emperical data this time only use the best fit 
#first create graphs of emperical data
overlay_to_sound <- ggplot(ExpData, aes(x=factor(condition),y=switchcnt_tosound)) + theme_bw(base_size = 18)
overlay_to_sound <- overlay_to_sound + stat_summary(fun.y="mean", geom="bar",fill="dark grey")
overlay_to_sound <- overlay_to_sound + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)

#separate the means to use for the fit based on fit output
bestfit <- subset(ModelData_means, subset= BLUE_REWARD == -4 & RED_REWARD == 4)

#plot the separated data
overlay_to_sound <- overlay_to_sound + geom_point(data=bestfit, aes(y=switchcnt_tosound.mean, x= factor(condition)),size=4)
overlay_to_sound <- overlay_to_sound +  labs(x = "Hit Rate/False Alarm Rate", y= "Mean Cued Switches")
ggsave(overlay_to_sound,"~/GMU/Lab/Trust/Spring 2014/graphs/UL_model/MeanCS_BestFit R4-Bneg4.png")
# now do it for the uncued switches
overlay_to_no_sound <- ggplot(ExpData, aes(x=factor(condition),y=switchcnt_extra)) + theme_bw(base_size = 18)
overlay_to_no_sound <- overlay_to_no_sound + stat_summary(fun.y="mean", geom="bar",fill="dark grey")
overlay_to_no_sound <-overlay_to_no_sound + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
overlay_to_no_sound <- overlay_to_no_sound + geom_point(data=bestfit, aes(y=switchcnt_extra.mean, x= factor(condition)),size=4)
overlay_to_no_sound <- overlay_to_no_sound +  labs(x = "Hit Rate/False Alarm Rate", y= "Mean Uncued Switches")
ggsave(overlay_to_sound,"~/GMU/Lab/Trust/Spring 2014/graphs/UL_model/MeanUS_BestFit R4-Bneg4.png")

#calculate anova for model switches
#first separate the best fits data 
bestfitRaw <- subset(ModelData, subset= wait.reward == na)
#then calculate the anova
anovaModelCued <- aov(switchcnt_tosound ~ condition, data=bestfitRaw)
summary(anovaModelCued)
anovaModelUncued <- aov(switchcnt_extra ~ condition, data=bestfitRaw)
summary(anovaModelUncued)

head(bestfit)

#######################################################################################################
##Model data analysis by block

#read the block file
modelLispByID  <- read.csv("~/GMU/Lab/Trust/Spring 2014/Data/Utility_Learning_byBlock_processed.csv")
str(modelLispByID)
unique(modelLispByID$ID)

test <- subset(modelLispByID, subset= ID < 4)


###for each subject separate the blocks then add it to the overall df
addBlocks <- function(original.df){
  finalFrame  <- original.df[0,] # make an empty df with the headers
for (subj in unique(original.df$ID)){
  temp.df  <-  subset(original.df, subset= ID==subj) #separate each set of subjects
  totalTime  <- temp.df$time[length(temp.df$time)] #find out that subjects total time
  block_Delimeter  <- totalTime/4 #divide that time into 4 
  temp.df$block <- ifelse(temp.df$time > block_Delimeter*3, 4, #if it's greater than 3 times the delimeter it's block 4
         ifelse(temp.df$time > block_Delimeter*2, 3, #to block 3
                ifelse(temp.df$time > block_Delimeter, 2, #to block 2
                       ifelse(temp.df$time < block_Delimeter,1,
                              NA)))) #if it's not one of the first 3 blocks then it's block 4
  finalFrame <- rbind(finalFrame,temp.df)
  }
return(finalFrame)
}

modelLispbyBlock <- addBlocks(modelLispByID)


