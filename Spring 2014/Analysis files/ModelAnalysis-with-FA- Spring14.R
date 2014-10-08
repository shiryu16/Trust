#######Model Analysis With False Alarms#########################
###### load the packages
require(ggplot2)
require(doBy)
require(plyr)

##### read in the data
ExpData <- read.csv("~/GMU/Lab/Trust/Spring 2014/Data/trust_overview.4.28.2014_subset.csv")
modelLispByID  <- read.csv("~/GMU/Lab/Trust/Spring 2014/Data/.csv")
str(modelLispByID)


######## functions i'll need
CalculateFits <- function(modeldf, expdf, path) {
  #sink outputs to the speciefied file instead of the console
  sink(path)
  
  #start the loop for each blue_reward
  for (blue in unique(modeldf$blue_reward)) {
    dummyblue <- subset(modeldf, blue_reward==blue)
    #nest the loop for each RED_Reward
    for(red in unique(dummyblue$red_reward)){
      dummyRed <- subset(dummyblue,red_reward==red)
      #nest again for each condition
      for(EGS in unique(dummyRed$egs)){
        dummyegs <- subset(dummyRed,egs==EGS)
        #this sub-section calculates the fits for the switch_cnt variable
        cat("blue_reward =", blue, " Red Reward =", red, " EGS =", EGS, "Switches to Sound",
            "r^2 = ",cor(dummyegs$switchcnt_tosound.mean,expdf$switchcnt_tosound.mean),
            "RMSD = ", rmsd(dummyegs$switchcnt_tosound.mean,expdf$switchcnt_tosound.mean),
            fill=T)
        GoodFits <- InRange(dummyegs$switchcnt_tosound.mean, expdf$switchcnt_tosound.mean - expdf$toSound.ci, 
                            expdf$switchcnt_tosound.mean + expdf$toSound.ci)
        cat("  Number of model points in CI for", blue,"/",red,"/",EGS,"= ", GoodFits)
        if (GoodFits == 4)
          cat(" *************")
        if (GoodFits == 3)
          cat(" +++++++++++++")
        cat(fill=T)
        #this subsection calculates the fits for the extra switches
        cat("blue_reward =", blue, " Red Reward =", red, " EGS =",EGS,"Switches to No Sound",
            "r^2 = ",cor(dummyegs$switchcnt_extra.mean,expdf$switchcnt_extra.mean),
            "RMSD = ", rmsd(dummyegs$switchcnt_extra.mean,expdf$switchcnt_extra.mean),
            fill=T)
        GoodFits <- InRange(dummyegs$switchcnt_extra.mean, expdf$switchcnt_extra.mean - expdf$to.noSound.ci, 
                            expdf$switchcnt_extra.mean + expdf$to.noSound.ci)
        cat("  Number of model points in CI for", blue,"/",red,"/",EGS,"= ", GoodFits)
        if (GoodFits == 4)
          cat(" *************")
        if (GoodFits == 3)
          cat(" +++++++++++++")
        cat(fill=T)
        ##another subsection must go here to calculate the fits to the ignored cues. 
        #this subsection calculates the fits for the extra switches
        cat("blue_reward =", blue, " Red Reward =", red, " EGS =",EGS,"Ignored Cues",
            "r^2 = ",cor(dummyegs$IgnoredSwitch.mean,expdf$noswitchcnt_tosound.mean),
            "RMSD = ", rmsd(dummyegs$IgnoredSwitch.mean,expdf$noswitchcnt_tosound.mean),
            fill=T)
        GoodFits <- InRange(dummyegs$IgnoredSwitch.mean, expdf$noswitchcnt_tosound.mean - expdf$IgnoredSwitch.ci, 
                            expdf$noswitchcnt_tosound.mean + expdf$IgnoredSwitch.ci)
        cat("  Number of model points in CI for", blue,"/",red,"/",EGS,"= ", GoodFits)
        if (GoodFits == 4)
          cat(" *************")
        if (GoodFits == 3)
          cat(" +++++++++++++")
        cat(fill=T)
        cat(fill=T)
      }
    }
  }
  sink()
  sink()
}

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

CalculateFits_ByBlock <- function(modeldf, expdf, path) {
  #sink outputs to the speciefied file instead of the console
  sink(path)
  
  #start the loop for each blue_reward
  for (blue in unique(modeldf$blue_reward)) {
    dummyblue <- subset(modeldf, blue_reward==blue)
    #nest the loop for each RED_Reward
    for(red in unique(dummyblue$red_reward)){
      dummyRed <- subset(dummyblue,red_reward==red)
      #nest again for each condition
      for(EGS in unique(dummyRed$egs)){
        dummyegs <- subset(dummyRed,egs==EGS)
        for(COND in unique(dummyegs$condition)){
          dummyCondition_model  <- subset(dummyegs,condition==COND)
          dummyCondition_exp  <- subset(expdf,condition==COND)
          #this sub-section calculates the fits for the switch_cnt variable
          cat("blue_reward =", blue, " Red Reward =", red, " EGS =", EGS," Condition",COND, "Switches to Sound",
              "r^2 = ",cor(dummyCondition_model$switchcnt_tosound.mean,dummyCondition_exp$switchcnt_tosound.mean),
              "RMSD = ", rmsd(dummyCondition_model$switchcnt_tosound.mean,dummyCondition_exp$switchcnt_tosound.mean),
              fill=T)
          GoodFits <- InRange(dummyCondition_model$switchcnt_tosound.mean, dummyCondition_exp$switchcnt_tosound.mean - dummyCondition_exp$toSound.ci, 
                              dummyCondition_exp$switchcnt_tosound.mean + dummyCondition_exp$toSound.ci)
          cat("  Number of model points in CI for", blue,"/",red,"/",EGS,"/",COND,"= ", GoodFits)
          if (GoodFits == 4)
            cat(" *************")
          if (GoodFits == 3)
            cat(" +++++++++++++")
          cat(fill=T)
          #this subsection calculates the fits for the extra switches
          cat("blue_reward =", blue, " Red Reward =", red, " EGS =",EGS," Cond= ",COND,"Switches to No Sound",
              "r^2 = ",cor(dummyCondition_model$switchcnt_extra.mean,dummyCondition_exp$switchcnt_extra.mean),
              "RMSD = ", rmsd(dummyCondition_model$switchcnt_extra.mean,dummyCondition_exp$switchcnt_extra.mean),
              fill=T)
          GoodFits <- InRange(dummyCondition_model$switchcnt_extra.mean, dummyCondition_exp$switchcnt_extra.mean - dummyCondition_exp$to.noSound.ci, 
                              dummyCondition_exp$switchcnt_extra.mean + dummyCondition_exp$to.noSound.ci)
          cat("  Number of model points in CI for", blue,"/",red,"/",EGS,"/",COND,"= ", GoodFits)
          if (GoodFits == 4)
            cat(" *************")
          if (GoodFits == 3)
            cat(" +++++++++++++")
          cat(fill=T)
          ###Ignored cues analysis 
          cat("blue_reward =", blue, " Red Reward =", red, " EGS =",EGS," Cond= ",COND,"Ignored Cues",
              "r^2 = ",cor(dummyCondition_model$IgnoredSwitch.mean,dummyCondition_exp$IgnoredSwitch.mean),
              "RMSD = ", rmsd(dummyCondition_model$IgnoredSwitch.mean,dummyCondition_exp$IgnoredSwitch.mean),
              fill=T)
          GoodFits <- InRange(dummyCondition_model$IgnoredSwitch, dummyCondition_exp$IgnoredSwitch.mean - dummyCondition_exp$IgnoredSwitch.ci, 
                              dummyCondition_exp$IgnoredSwitch.mean + dummyCondition_exp$IgnoredSwitch.ci)
          cat("  Number of model points in CI for", blue,"/",red,"/",EGS,"/",COND,"= ", GoodFits)
          if (GoodFits == 4)
            cat(" *************")
          if (GoodFits == 3)
            cat(" +++++++++++++")
          cat(fill=T)
          cat(fill=T)
        
        }
      }
    }
  } 
}

graph_by_condition <- function(model.df,exp.df,DV){
  if(DV=="Cued"){
    ##selection for DV based on case structure
    ## then separate different graphs by condition
    for (COND in unique(model.df$condition)){
      condName <- as.data.frame(strsplit(as.character(COND),"/"))
      dummyCond  <- subset(model.df,condition == COND)
      
      ovly_fits_by_block_CS  <- ggplot(exp.df,aes(x=Block,y=CuedSwitch))+ theme_bw(base_size = 18)
      ovly_fits_by_block_CS <- ovly_fits_by_block_CS + stat_summary(fun.y="mean", geom="bar",fill="dark grey")
      ovly_fits_by_block_CS <- ovly_fits_by_block_CS + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
      ovly_fits_by_block_CS <- ovly_fits_by_block_CS + geom_line(data=dummyCond, aes(x=Block,y=switchcnt_tosound.mean,colour=factor(egs)))
      ovly_fits_by_block_CS <- ovly_fits_by_block_CS + facet_grid(red_reward ~ blue_reward,labeller=label_both)
      ovly_fits_by_block_CS <- ovly_fits_by_block_CS + labs(title=paste(COND,"---",DV),y="Cued Switches", x="Block")
      print(ovly_fits_by_block_CS)
      ggsave(paste("~/GMU/Lab/Trust/Spring 2014/graphs/UL_model/",condName[1,],"-",condName[2,],"---",DV,".png"),height=9,width=16,dpi=300)
    }
  }else{
    if(DV=="Uncued"){
      for (COND in unique(model.df$condition)){
        condName <- as.data.frame(strsplit(as.character(COND),"/"))
        dummyCond  <- subset(model.df,condition == COND)
        
        ovly_fits_by_block_US  <- ggplot(exp.df,aes(x=Block,y=UncuedSwitch))+ theme_bw(base_size = 18)
        ovly_fits_by_block_US <- ovly_fits_by_block_US + stat_summary(fun.y="mean", geom="bar",fill="dark grey")
        ovly_fits_by_block_US <- ovly_fits_by_block_US + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
        ovly_fits_by_block_US <- ovly_fits_by_block_US + geom_line(data=dummyCond, aes(x=Block,y=switchcnt_extra.mean,colour=factor(egs)))
        ovly_fits_by_block_US <- ovly_fits_by_block_US + facet_grid(red_reward ~ blue_reward,labeller=label_both)
        ovly_fits_by_block_US <- ovly_fits_by_block_US + labs(title=paste(COND,"---",DV),y="UnCued Switches", x="Block")
        print(ovly_fits_by_block_US)
        ggsave(paste("~/GMU/Lab/Trust/Spring 2014/graphs/UL_model/",condName[1,],"-",condName[2,],"---",DV,".png"),height=9,width=16,dpi=300)
      }
    }else{
      if (DV== "Ignore"){
        for (COND in unique(model.df$condition)){
            condName <- as.data.frame(strsplit(as.character(COND),"/"))
            dummyCond  <- subset(model.df,condition == COND)
            
            ovly_fits_by_block_Ig  <- ggplot(exp.df,aes(x=Block,y=IgnoredSwitch))+ theme_bw(base_size = 18)
            ovly_fits_by_block_Ig <- ovly_fits_by_block_Ig + stat_summary(fun.y="mean", geom="bar",fill="dark grey")
            ovly_fits_by_block_Ig <- ovly_fits_by_block_Ig + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
            ovly_fits_by_block_Ig <- ovly_fits_by_block_Ig + geom_line(data=dummyCond, aes(x=Block,y=IgnoredSwitch.mean,colour=factor(egs)))
            ovly_fits_by_block_Ig <- ovly_fits_by_block_Ig + facet_grid(red_reward ~ blue_reward,labeller=label_both)
            ovly_fits_by_block_Ig <- ovly_fits_by_block_Ig + labs(title=paste(COND,"---",DV),y="Ignored Cues", x="Block")
            print(ovly_fits_by_block_Ig)
            ggsave(paste("~/GMU/Lab/Trust/Spring 2014/graphs/UL_model/",condName[1,],"-",condName[2,],"---",DV,".png"),height=9,width=16,dpi=300)
          }
        }
    }
  }
}

####SETUP THE DATA FRAME FOR THE EXPERIMENTAL DATA AS BY BLOCK 
###need to get the means for the ExpData by block
ExpData$condition <- paste(ExpData$intended_hitrate, "/",ExpData$intended_falsealarmrate)
ExpData$condition <- as.factor(ExpData$condition)
str(ExpData)
Uncued_dataByBlock <- reshape(ExpData, varying = c("switchcnt_extra_1","switchcnt_extra_2","switchcnt_extra_3","switchcnt_extra_4"),
                              v.names= "UncuedSwitch",
                              timevar ="Block", times=c(1,2,3,4),direction = "long")

Cued_dataByBlock <- reshape(ExpData, varying = c("switchcnt_tosound_1","switchcnt_tosound_2","switchcnt_tosound_3","switchcnt_tosound_4"),
                            v.names= "CuedSwitch",
                            timevar ="Block", times=c(1,2,3,4),direction = "long")

Ignored_dataByBlock <- reshape(ExpData, varying = c("noswitchcnt_tosound_1","noswitchcnt_tosound_2","noswitchcnt_tosound_3","noswitchcnt_tosound_4"),
                               v.names= "IgnoredSwitch",
                               timevar ="Block", times=c(1,2,3,4),direction = "long")

RT_dataByBlock <- reshape(ExpData, varying = c("averageRT_tosound_1","averageRT_tosound_2","averageRT_tosound_3","averageRT_tosound_4"),
                          v.names= "RTimeToSound",
                          timevar ="Block", times=c(1,2,3,4),direction = "long")
hitcnt_dataByBlock <- reshape(ExpData, varying = c("hitcnt_1","hitcnt_2","hitcnt_3","hitcnt_4"),
                              v.names= "alarmHits",
                              timevar ="Block", times=c(1,2,3,4),direction = "long")

misscnt_dataByBlock <- reshape(ExpData, varying = c("misscnt_1","misscnt_2","misscnt_3","misscnt_4"),
                               v.names= "alarmMisses",
                               timevar ="Block", times=c(1,2,3,4),direction = "long")

facnt_dataByBlock <- reshape(ExpData, varying = c("falsealarmcount_1","falsealarmcount_2","falsealarmcount_3","falsealarmcount_4"),
                             v.names= "alarmFA",
                             timevar ="Block", times=c(1,2,3,4),direction = "long")

merged1 <- merge(Uncued_dataByBlock,Cued_dataByBlock)
merged1 <- merge(merged1,Ignored_dataByBlock)
merged1 <- merge(merged1,RT_dataByBlock)
merged1 <- merge(merged1,hitcnt_dataByBlock)
merged1 <- merge(merged1,misscnt_dataByBlock)
merged1 <- merge(merged1,facnt_dataByBlock)

ExpData_ByBlock  <- subset(merged1,select=c(subject,condition,Block,UncuedSwitch,CuedSwitch,
                                            IgnoredSwitch,RTimeToSound))
##now that i have allt eh block data separated into long format, i need to summarize it
ExpData_ByBlock_means <- ddply(.data=ExpData_ByBlock,.(condition,Block),summarize,
                               switchcnt_tosound.mean=mean(CuedSwitch),switchcnt_tosound.sd=sd(CuedSwitch),
                               switchcnt_extra.mean=mean(UncuedSwitch),switchcnt_extra.sd=sd(UncuedSwitch),
                               IgnoredSwitch.mean=mean(IgnoredSwitch),IgnoredSwitch.sd=sd(IgnoredSwitch))

#####SETUP THE MODEL DATA BY BLOCK
modelLispByID$condition <- paste(modelLispByID$TPR,"/",modelLispByID$FPR)
modelLispByID$condition <- as.factor(modelLispByID$condition)
#I need to summarize the data so that there are 4 data points per ID in order to emulate the data from participants
#this will emulate the data from participants
summary(modelLispByID$condition)

#i have to first be able to count the switches. 
modelLispByID$switchcnt_tosound <- ifelse(modelLispByID$action=="CUED",1,0)
modelLispByID$switchcnt_extra <- ifelse(modelLispByID$action=="UNCUED",1,0)
modelLispByID$Ignored <- ifelse(modelLispByID$action=="IGNORE",1,0)

#now that i have the counts for each trial, i should summarize to smaller dataset
model_Utility_byBlock <- ddply(.data=modelLispByID,.(ID,condition,Block,red_reward,blue_reward,egs),
                               summarize,switchcnt_tosound=sum(switchcnt_tosound),
                               switchcnt_extra=sum(switchcnt_extra),
                               IgnoredSwitch=sum(Ignored))
#model_Utility_byBlock <- summaryBy(switchcnt_tosound + switchcnt_extra ~ ID+condition+block+red_reward+blue_reward+egs,data=modelLispByID,FUN=c(sum))

str(model_Utility_byBlock)
summary(model_Utility_byBlock)

total_Utility_byBlock <- ddply(.data=model_Utility_byBlock,.(ID,condition,red_reward,blue_reward,egs),
                               summarize,switchcnt_tosound=sum(switchcnt_tosound),
                               switchcnt_extra=sum(switchcnt_extra),
                               IgnoredSwitch=sum(IgnoredSwitch))

str(total_Utility_byBlock)
str(ExpData)

#matrix of graphs of parameter space
#CUED
overlay_to_sound <- ggplot(ExpData, aes(x=factor(condition),y=switchcnt_tosound)) + theme_bw(base_size = 18)
overlay_to_sound <- overlay_to_sound + stat_summary(fun.y="mean", geom="bar",fill="dark grey")
overlay_to_sound <- overlay_to_sound + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
overlay_to_sound <- overlay_to_sound + stat_summary(data=total_Utility_byBlock, aes(x=condition,y=switchcnt_tosound,group=egs,colour=factor(egs)),fun.y="mean",geom="line")
overlay_to_sound <- overlay_to_sound + facet_grid(red_reward ~ blue_reward,labeller=label_both)  + labs(title="noWaitreward-:u2 Cued Switches", y="Cued Switches", x="Condition")
print(overlay_to_sound)
ggsave("~/GMU/Lab/Trust/Spring 2014/graphs/UL_model/CS -red-blue-egs-nowait-u2 - with FA.png",height=9,width=16,dpi=300)

#UNCUED
overlay_Uncued <- ggplot(ExpData, aes(x=factor(condition),y=switchcnt_extra)) + theme_bw(base_size = 18)
overlay_Uncued <- overlay_Uncued + stat_summary(fun.y="mean", geom="bar",fill="dark grey")
overlay_Uncued <- overlay_Uncued + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
overlay_Uncued <- overlay_Uncued + stat_summary(data=total_Utility_byBlock, aes(x=condition,y=switchcnt_extra,group=egs,colour=factor(egs)),fun.y="mean",geom="line")
overlay_Uncued <- overlay_Uncued + facet_grid(red_reward ~ blue_reward,labeller=label_both)  + labs(title="noWaitreward-:u2 Uncued Switches", y="UnCued Switches", x="Condition")
print(overlay_Uncued)
ggsave("~/GMU/Lab/Trust/Spring 2014/graphs/UL_model/US nowaitu2 blue-red-egs- with FA.png",height=9,width=16,dpi=300)

#IGNORE
overlay_ignored <- ggplot(ExpData, aes(x=factor(condition),y=noswitchcnt_tosound)) + theme_bw(base_size = 18)
overlay_ignored <- overlay_ignored + stat_summary(fun.y="mean", geom="bar",fill="dark grey")
overlay_ignored <- overlay_ignored + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
overlay_ignored <- overlay_ignored + stat_summary(data=total_Utility_byBlock, aes(x=condition,y=IgnoredSwitch,group=egs,colour=factor(egs)),fun.y="mean",geom="line")
overlay_ignored <- overlay_ignored + facet_grid(red_reward ~ blue_reward,labeller=label_both)  + labs(title="noWaitreward-:u2 Ignored Cues", y="Ignored Cues", x="Condition")
print(overlay_ignored)
ggsave("~/GMU/Lab/Trust/Spring 2014/graphs/UL_model/Ig nowaitu2 blue-red-egs - with FA.png",height=9,width=16,dpi=300)



### After evaluating the genaral data i need to look at the block data
### To do that i need to summarize it

### The ExpData was summarized earlier, but i still need to summarize Model Data
ModelData_ByBlock_means <- ddply(.data=model_Utility_byBlock,.(condition,Block,red_reward,egs,blue_reward),summarize,
                                 switchcnt_tosound.mean=mean(switchcnt_tosound),switchcnt_tosound.sd=sd(switchcnt_tosound),
                                 switchcnt_extra.mean=mean(switchcnt_extra),switchcnt_extra.sd=sd(switchcnt_extra),
                                 IgnoredSwitch.mean=mean(IgnoredSwitch),IgnoredSwitch.sd=sd(IgnoredSwitch))

#to get hte confidence interval you need the sqrt(number of subjects)
ExpData_ByBlock_means$toSound.ci <- 2 * ExpData_ByBlock_means$switchcnt_tosound.sd / sqrt(17)
ModelData_ByBlock_means$toSound.ci <- 2 * ModelData_ByBlock_means$switchcnt_tosound.sd / sqrt(17)
ExpData_ByBlock_means$to.noSound.ci <- 2 * ExpData_ByBlock_means$switchcnt_extra.sd / sqrt(17)
ModelData_ByBlock_means$to.noSound.ci <- 2 * ModelData_ByBlock_means$switchcnt_extra.sd / sqrt(17)
ExpData_ByBlock_means$IgnoredSwitch.ci <- 2 * ExpData_ByBlock_means$IgnoredSwitch.sd / sqrt(17)
ModelData_ByBlock_means$IgnoredSwitch.ci <- 2 * ModelData_ByBlock_means$IgnoredSwitch.sd / sqrt(17)

##now i can graph by condition
graph_by_condition(ModelData_ByBlock_means,ExpData_ByBlock,"Cued")
graph_by_condition(ModelData_ByBlock_means,ExpData_ByBlock,"Uncued")
graph_by_condition(ModelData_ByBlock_means,ExpData_ByBlock,"Ignore")


##i want to explore the fits of the FA model to the no cost study
####
###
###

noCostExp  <- read.csv("~/GMU/Lab/Trust/Fall 2013/Data/trust_overview.12.9.2013-1.csv")

noCostExp$condition <- paste(noCostExp$intended_hitrate, "/",noCostExp$intended_falsealarmrate)
noCostExp$condition <- as.factor(noCostExp$condition)

##remove subject from formula to caluclate fits
##add subject to formula to create graphs by condition
noCostExp_means <- summaryBy(switchcnt_tosound + switchcnt_extra + averageRT_tosound + noswitchcnt_tosound ~ subject + condition,data=noCostExp,FUN=c(mean,sd))

modelLispByID  <- read.csv("~/GMU/Lab/Trust/Spring 2014/Data/UL_With_FA_noCost_byBlock_processed.csv")

#####SETUP THE MODEL DATA BY BLOCK
modelLispByID$condition <- paste(modelLispByID$TPR,"/",modelLispByID$FPR)
modelLispByID$condition <- as.factor(modelLispByID$condition)
#I need to summarize the data so that there are 4 data points per ID in order to emulate the data from participants
#this will emulate the data from participants

#i have to first be able to count the switches. 
modelLispByID$switchcnt_tosound <- ifelse(modelLispByID$action=="CUED",1,0)
modelLispByID$switchcnt_extra <- ifelse(modelLispByID$action=="UNCUED",1,0)
modelLispByID$Ignored <- ifelse(modelLispByID$action=="IGNORE",1,0)

#now that i have the counts for each trial, i should summarize to smaller dataset
model_Utility_byBlock <- ddply(.data=modelLispByID,.(ID,condition,Block,red_reward,blue_reward,egs),
                               summarize,switchcnt_tosound=sum(switchcnt_tosound),
                               switchcnt_extra=sum(switchcnt_extra),
                               IgnoredSwitch=sum(Ignored))
#model_Utility_byBlock <- summaryBy(switchcnt_tosound + switchcnt_extra ~ ID+condition+block+red_reward+blue_reward+egs,data=modelLispByID,FUN=c(sum))

total_Utility_byBlock <- ddply(.data=model_Utility_byBlock,.(ID,condition,red_reward,blue_reward,egs),
                               summarize,switchcnt_tosound=sum(switchcnt_tosound),
                               switchcnt_extra=sum(switchcnt_extra),
                               IgnoredSwitch=sum(IgnoredSwitch))

#now i have to create the data frames for analysing the false alarms 
noCost_Model <- summaryBy(switchcnt_tosound + switchcnt_extra + IgnoredSwitch ~ condition + red_reward + blue_reward + egs,data=total_Utility_byBlock,FUN=c(mean,sd))


#cued
overlay_to_sound <- ggplot(noCostExp_means, aes(x=factor(condition),y=switchcnt_tosound.mean)) + theme_bw(base_size = 18)
overlay_to_sound <- overlay_to_sound + stat_summary(fun.y="mean", geom="bar",fill="dark grey")
overlay_to_sound <- overlay_to_sound + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
overlay_to_sound <- overlay_to_sound + stat_summary(data=noCost_Model, aes(x=condition,y=switchcnt_tosound.mean,group=egs,colour=factor(egs)),fun.y="mean",geom="line")
overlay_to_sound <- overlay_to_sound + facet_grid(red_reward ~ blue_reward,labeller=label_both)  + labs(title="noWaitreward-:u2 Cued Switches", y="Cued Switches", x="Condition")
print(overlay_to_sound)
ggsave("~/GMU/Lab/Trust/Fall 2013/graphs/UL_model/CS-noCost-nowait-u2-with FA.png",height=9,width=16,dpi=300)



#uncued
overlay_Uncued <- ggplot(noCostExp_means, aes(x=factor(condition),y=switchcnt_extra.mean)) + theme_bw(base_size = 18)
overlay_Uncued <- overlay_Uncued + stat_summary(fun.y="mean", geom="bar",fill="dark grey")
overlay_Uncued <- overlay_Uncued + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
overlay_Uncued <- overlay_Uncued + stat_summary(data=noCost_Model, aes(x=condition,y=switchcnt_extra.mean,group=egs,colour=factor(egs)),fun.y="mean",geom="line")
overlay_Uncued <- overlay_Uncued + facet_grid(red_reward ~ blue_reward,labeller=label_both)  + labs(title="noWaitreward-:u2 Uncued Switches", y="UnCued Switches", x="Condition")
print(overlay_Uncued)
ggsave("~/GMU/Lab/Trust/Fall 2013/graphs/UL_model/US-noCost-nowait-u2-with FA.png",height=9,width=16,dpi=300)

#IGNORE
overlay_ignored <- ggplot(noCostExp_means, aes(x=factor(condition),y=noswitchcnt_tosound.mean)) + theme_bw(base_size = 18)
overlay_ignored <- overlay_ignored + stat_summary(fun.y="mean", geom="bar",fill="dark grey")
overlay_ignored <- overlay_ignored + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
overlay_ignored <- overlay_ignored + stat_summary(data=noCost_Model, aes(x=condition,y=IgnoredSwitch.mean,group=egs,colour=factor(egs)),fun.y="mean",geom="line")
overlay_ignored <- overlay_ignored + facet_grid(red_reward ~ blue_reward,labeller=label_both)  + labs(title="noWaitreward-:u2 Ignored Cues", y="Ignored Cues", x="Condition")
print(overlay_ignored)
ggsave("~/GMU/Lab/Trust/Fall 2013/graphs/UL_model/IS-noCost-nowait-u2-with FA.png",height=9,width=16,dpi=300)



#to get hte confidence interval you need the sqrt(number of subjects)
noCostExp_means$toSound.ci <- 2 * noCostExp_means$switchcnt_tosound.sd / sqrt(15)
noCost_Model$toSound.ci <- 2 * noCost_Model$switchcnt_tosound.sd / sqrt(15)
noCostExp_means$to.noSound.ci <- 2 * noCostExp_means$switchcnt_extra.sd / sqrt(15)
noCost_Model$to.noSound.ci <- 2 * noCost_Model$switchcnt_extra.sd / sqrt(15)
noCostExp_means$IgnoredSwitch.ci <- 2 * noCostExp_means$noswitchcnt_tosound.sd / sqrt(15)
noCost_Model$IgnoredSwitch.ci <- 2 * noCost_Model$IgnoredSwitch.sd / sqrt(15)

CalculateFits(noCost_Model,noCostExp_means,"~/GMU/Lab/Trust/Spring 2014/Analysis files/noCost-FA_Model-fits.txt")

