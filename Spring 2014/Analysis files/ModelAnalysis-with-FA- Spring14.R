#######Model Analysis With False Alarms#########################
#load the packages
require(ggplot2)
require(doBy)
require(plyr)

#read in the data
ExpData <- read.csv("~/GMU/Lab/Trust/Spring 2014/Data/trust_overview.4.28.2014_subset.csv")
modelLispByID  <- read.csv("~/GMU/Lab/Trust/Spring 2014/Data/UL_With_FA_byBlock_processed.csv")
str(modelLispByID)
#functions i'll need
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
        cat(fill=T)
      }
    }
  } 
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
        ovly_fits_by_block_CS  <- ggplot(exp.df,aes(x=Block,y=UncuedSwitch))+ theme_bw(base_size = 18)
        ovly_fits_by_block_CS <- ovly_fits_by_block_CS + stat_summary(fun.y="mean", geom="bar",fill="dark grey")
        ovly_fits_by_block_CS <- ovly_fits_by_block_CS + stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = .25)
        ovly_fits_by_block_CS <- ovly_fits_by_block_CS + geom_line(data=dummyCond, aes(x=Block,y=switchcnt_extra.mean,colour=factor(egs)))
        ovly_fits_by_block_CS <- ovly_fits_by_block_CS + facet_grid(red_reward ~ blue_reward,labeller=label_both)
        ovly_fits_by_block_CS <- ovly_fits_by_block_CS + labs(title=paste(COND,"---",DV),y="Cued Switches", x="Block")
        print(ovly_fits_by_block_CS)
        #cat("~/GMU/Lab/Trust/Spring 2014/graphs/UL_model/",COND,"---",DV,".png")
        ggsave(paste("~/GMU/Lab/Trust/Spring 2014/graphs/UL_model/",condName[1,],"-",condName[2,],"---",DV,".png"),height=9,width=16,dpi=300)
      }
    }
  }
}
