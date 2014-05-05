#Data Analysis File for Trust Study. 
#get important packages
library(plyr)
library(doBy)
library(ggplot2)
library(ez)
library(GGally)
library(car)
source("~/GMU/Lab/VigilanceSpring2013/TraftonLabPlotting.r")

#read most current data file (change filename)
EmpericData <- read.csv("~/GMU/Lab/Trust/Spring 2014/Data/trust_overview.4.28.2014_subset.csv")

#histogram of each count variable
hist(EmpericData$switchcnt_tosound, plot=T)
#suitable to use regular LM since the distribution is normal
hist(EmpericData$noswitchcnt_tosound,plot=T)
#poussant distribution
hist(EmpericData$switchcnt_extra,plot=T)
#poussant Distribution
#doublecheck group balabce

#explore data
head(EmpericData)
str(EmpericData)
summary(EmpericData$condition)
EmpericData$subject <- as.factor(EmpericData$subject)
EmpericData$condition <- as.factor(paste(EmpericData$intended_hitrate, "/",EmpericData$intended_falsealarmrate))
#actual hit/false alarms and total trials by condition
summary(EmpericData)
summaryBy(actual_hitrate + actual_falsealarmrate + totaltrials ~ condition, data=EmpericData)
#some other variables of initial interest by condition
summaryBy(finalscore + averageRT_tosound ~ condition, data=EmpericData)

#pull out the good stuff from EmpericData
EmpericData.clean <- subset(EmpericData,select=c("subject","condition","actual_hitrate",
                                          "actual_falsealarmrate","finalscore","averageRT_tosound",
                                          "noswitchcnt_tosound","switchcnt_tosound","switchcnt_extra"))

# Scatterplot Matrices from the car Package
scatterplotMatrix(~ condition + 
                    averageRT_tosound + noswitchcnt_tosound + 
                    switchcnt_tosound + switchcnt_extra, data=EmpericData.clean,
                    diagonal="histogram",
                    #groups= condition, by.groups=T, ###not working correctly
                    main="Correlation Matrix")

cor(EmpericData.clean[,3:9])
####example!!
# ggpairs(df, 
#         upper = list(params = c(size = 10)),
#         lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue"))
# )


##ggpairs is very slow


ggpairs(EmpericData.clean[,3:9],
                       upper = list(param=c(size=10)),
                       lower = list(continuous = "smooth",param=c(method="loess", fill = "blue")),
                       diag = list(continuous = "density")
                        )

##don't print scatterplot... it takes way too long 
<<<<<<< HEAD
#ggsave(scatterplot,"~/GMU/Lab/Trust/Spring 2014/graphs/scatterplot matrix - all counts.png")
#print(scatterplot)
=======
# print(scatterplot)
>>>>>>> 4edc4006059f105d25afc02ba1500f6f2012d2fe
#calculate infomration sampling by taking total switches and removing switches due to cue
#data$info_sampling <- data$totalswitches - data$hitcnt - data$falsealarmcnt
#plot(summaryBy(info_sampling ~ condition+subject, data=data))

#investigate switching behavior 
summaryBy(switchcnt_extra + switchcnt_tosound + noswitchcnt_tosound ~ condition, data=EmpericData.clean)
View(EmpericData.clean)
#plot
#uncued switches
plotExtra <- ggplot(data = EmpericData, aes(condition, switchcnt_extra)) + theme_bw(base_size = 16)
plotExtra <- plotExtra + stat_summary(fun.y="mean",geom="bar",fill="dark grey") + 
                          stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=.25)
plotExtra  <- plotExtra +labs(x= "Hit Rate/False Alarm Rate",y= "UnCued Switches")
ggsave(plot=plotExtra,"~/GMU/Lab/Trust/Spring 2014/Graphs/unCuedSwitches by condition.png")
print(plotExtra)

#cued switches
plotToSound <- ggplot(data = EmpericData, aes(condition, switchcnt_tosound)) + theme_bw(base_size = 16)
plotToSound <- plotToSound + stat_summary(fun.y="mean",geom="bar",fill="dark grey") + 
                              stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=.25)
plotToSound  <- plotToSound +labs(x= "Hit Rate/False Alarm Rate",y= "Cued Switches")
ggsave(plot=plotToSound, "~/GMU/Lab/Trust/Spring 2014/Graphs/CuedSwitches by condition.png")
print(plotToSound)

#ignored cues
plotNoSwitch <- ggplot(data = EmpericData, aes(condition, noswitchcnt_tosound)) + theme_bw(base_size = 16)
plotNoSwitch <- plotNoSwitch + stat_summary(fun.y="mean",geom="bar",fill="dark grey") + 
                              stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=.25)
plotNoSwitch <- plotNoSwitch +labs(x= "Hit Rate/False Alarm Rate",y= "Ignored Cues")
ggsave(plot=plotNoSwitch, "~/GMU/Lab/Trust/Spring 2014/Graphs/No Switch by Condition.png")
print(plotNoSwitch)

#reaction time
plotRT <- ggplot(data = EmpericData, aes(condition, averageRT_tosound)) +theme_bw(base_size = 16)
plotRT <- plotRT + stat_summary(fun.y="mean",geom="bar",fill="dark grey") + 
                  stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=.25)
plotRT <- plotRT +labs(x= "Hit Rate/False Alarm Rate",y= "Response Time")
ggsave(plot=plotRT, "~/GMU/Lab/Trust/Spring 2014/Graphs/RT by condition.png")
print(plotRT)

#response Rate
plotRR <- ggplot(data = EmpericData, aes(condition, ResponseRatetoCue)) +theme_bw(base_size = 16)
plotRR <- plotRR + stat_summary(fun.y="mean",geom="bar",fill="dark grey") + 
  stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=.25)
plotRR <- plotRR +labs(x= "Hit Rate/False Alarm Rate",y= "Response Rate")
ggsave(plot=plotRR, "~/GMU/Lab/Trust/Spring 2014/Graphs/RR by condition.png")
print(plotRR)

#calculate significance
#Cued switches
anovaCued <- aov(switchcnt_tosound ~ condition, data=EmpericData.clean)
summary(anovaCued) ###significant 
TukeyHSD(anovaCued)

glmCued <- glm(switchcnt_tosound ~ actual_hitrate + actual_falsealarmrate, 
               data=EmpericData.clean, family="poisson")
#glmCued <- glm(switchcnt_tosound ~ condition, data=EmpericData.clean, family="poisson")
summary(glmCued) ###significant

#Uncued Switches
anovaUnCued <- aov(switchcnt_extra ~ condition, data=EmpericData.clean)
summary(anovaUnCued)###significant
TukeyHSD(anovaUnCued)

glmUncued <- glm(switchcnt_extra ~ actual_hitrate + actual_falsealarmrate,
                 data=EmpericData.clean, family="poisson" ) ### Weird, i'm not sure how to interpret this

#glmUncued <- glm(switchcnt_extra ~ condition,data=EmpericData.clean, family="poisson" )
summary(glmUncued)##signtificant

#response time
anovaRT <- aov(averageRT_tosound ~ condition, data=EmpericData.clean)
summary(anovaRT)##non-significant

glmRT  <-  glm(averageRT_tosound ~ actual_hitrate + actual_falsealarmrate, data=EmpericData.clean, family="poisson")
#glmRT  <-  glm(averageRT_tosound ~ condition, data=EmpericData.clean, family="poisson")
summary(glmRT)## signficant 
##throws some warnings because the values of RT are nto integers? 
#is this important for poisson distribution?

#ignored cues
anovaIgnore <- aov(noswitchcnt_tosound ~ condition, data=EmpericData.clean)
summary(anovaIgnore) #no signficance

glmIgnored <- glm(noswitchcnt_tosound ~ actual_hitrate + actual_falsealarmrate, data =EmpericData.clean, family="poisson")

#glmIgnored <- glm(noswitchcnt_tosound ~ condition, data =EmpericData.clean, family="poisson")
summary(glmIgnored) ##signficant when using condition
                    ###marginal when using hitrate and false alarm rate

# #setup to replace missing condition from errors
# missing <- sample(c("C","A","A","B","D","B","A"))
# missing     

test <- summaryBy(actual_hitrate + actual_falsealarmrate + totaltrials ~ condition, data=EmpericData)
test[,"actual_hitrate.mean"] <- round(test[,"actual_hitrate.mean"],2)
test[,"actual_falsealarmrate.mean"] <- round(test[,"actual_falsealarmrate.mean"],2)
test[,"totaltrials.mean"] <- round(test[,"totaltrials.mean"],2)
test

#read the new data in with block information
dataByBlock <- read.csv("~/GMU/Lab/Trust/Spring 2014/Data/trust_overview.4.28.2014_subset.csv")
head(dataByBlock)
str(dataByBlock)

#In order to play with it i need to make one variable for the counts at different blocks
#First subset only the variables i want in dataByBlock (i did this in excel and renamed to file with the suffix _subset)

extra_dataByBlock <- reshape(dataByBlock, varying = c("switchcnt_extra_1","switchcnt_extra_2","switchcnt_extra_3","switchcnt_extra_4"),
                             v.names= "switchWithNoSound",
                             timevar ="Block", times=c(1,2,3,4),direction = "long")

tosound_dataByBlock <- reshape(dataByBlock, varying = c("switchcnt_tosound_1","switchcnt_tosound_2","switchcnt_tosound_3","switchcnt_tosound_4"),
                               v.names= "switchToSound",
                               timevar ="Block", times=c(1,2,3,4),direction = "long")

Notosound_dataByBlock <- reshape(dataByBlock, varying = c("noswitchcnt_tosound_1","noswitchcnt_tosound_2","noswitchcnt_tosound_3","noswitchcnt_tosound_4"),
                                v.names= "noSwitchToSound",
                                timevar ="Block", times=c(1,2,3,4),direction = "long")

RT_dataByBlock <- reshape(dataByBlock, varying = c("averageRT_tosound_1","averageRT_tosound_2","averageRT_tosound_3","averageRT_tosound_4"),
                                v.names= "RTimeToSound",
                                timevar ="Block", times=c(1,2,3,4),direction = "long")
hitcnt_dataByBlock <- reshape(dataByBlock, varying = c("hitcnt_1","hitcnt_2","hitcnt_3","hitcnt_4"),
                             v.names= "alarmHits",
                             timevar ="Block", times=c(1,2,3,4),direction = "long")

misscnt_dataByBlock <- reshape(dataByBlock, varying = c("misscnt_1","misscnt_2","misscnt_3","misscnt_4"),
                               v.names= "alarmMisses",
                               timevar ="Block", times=c(1,2,3,4),direction = "long")

facnt_dataByBlock <- reshape(dataByBlock, varying = c("falsealarmcount_1","falsealarmcount_2","falsealarmcount_3","falsealarmcount_4"),
                                 v.names= "alarmFA",
                                 timevar ="Block", times=c(1,2,3,4),direction = "long")

merged1 <- merge(extra_dataByBlock,tosound_dataByBlock)
merged1 <- merge(merged1,Notosound_dataByBlock)
merged1 <- merge(merged1,RT_dataByBlock)
merged1 <- merge(merged1,hitcnt_dataByBlock)
merged1 <- merge(merged1,misscnt_dataByBlock)
merged1 <- merge(merged1,facnt_dataByBlock)

#subest the merged df to the variables I care about
mergedByBlock <- subset(merged1, select = c("subject","condition","Block","finalscore",
                                            "actual_hitrate","actual_falsealarmrate",
                                            "intended_hitrate","intended_falsealarmrate",
                                            "switchWithNoSound","switchToSound",
                                            "noSwitchToSound","RTimeToSound",
                                            "alarmHits","alarmMisses","alarmFA",
                                            "hitcnt","falsealarmcnt","misscnt")) 
mergedByBlock$condition <- paste(mergedByBlock$intended_hitrate, "/",mergedByBlock$intended_falsealarmrate)
mergedByBlock$condition <- factor(mergedByBlock$condition)
summary(mergedByBlock)
View(mergedByBlock)
#write.csv(mergedByBlock, file = "trust_overview_1_9_2014_withBlocks.csv")
EmpericData$ResponseRatetoCue <- (EmpericData$switchcnt_tosound / (EmpericData$hitcnt + EmpericData$falsealarmcnt))
mergedByBlock$ResponseRatetoCue <- (mergedByBlock$switchToSound / (mergedByBlock$hitcnt + mergedByBlock$falsealarmcnt))


#first we should explore the distributions
hist(mergedByBlock$switchWithNoSound,plot=T) #pousaant
hist(mergedByBlock$switchToSound,plot=T) #normal
hist(mergedByBlock$noSwitchToSound,plot=T) #poussant

#these have the same shape as before that's good, i did not mess up the data

#plots by block
##uncued switches
plotExtra <- ggplot(data = mergedByBlock, aes(Block, switchWithNoSound)) + theme_bw(base_size = 16)
plotExtra <- plotExtra + stat_summary(fun.y="mean",geom="line") + facet_wrap(~ condition)
plotExtra <- plotExtra + labs(x="Block",y= "UnCued Switches")
ggsave(plot=plotExtra, "~/GMU/Lab/Trust/Spring 2014/graphs/Uncued switches by block.png")
print(plotExtra)


##cued switches
plotToSound <- ggplot(data = mergedByBlock, aes(Block, switchToSound))+ theme_bw(base_size = 16)
plotToSound <- plotToSound + stat_summary(fun.y="mean",geom="line") + facet_wrap(~ condition)
plotToSound <- plotToSound +labs(x="Block", y="Cued Switches")
ggsave(plot=plotToSound, "~/GMU/Lab/Trust/Spring 2014/graphs/Cued Switchs by block.png")
print(plotToSound)

##ignored Cues
plotNoSwitch <- ggplot(data = mergedByBlock, aes(Block, noSwitchToSound)) + theme_bw(base_size=16)
plotNoSwitch <- plotNoSwitch + stat_summary(fun.y="mean",geom="line") + facet_wrap(~ condition)
plotNoSwitch <- plotNoSwitch + labs(x="Block",y="Ignored Cues")
ggsave(plot=plotNoSwitch, "~/GMU/Lab/Trust/Spring 2014/graphs/Ignored Cues by Block.png")
print(plotNoSwitch)

##Response time
plotRT <- ggplot(data = mergedByBlock, aes(Block, RTimeToSound)) + theme_bw(base_size=16)
plotRT <- plotRT + stat_summary(fun.y="mean",geom="line") + facet_wrap(~ condition)
plotRT <- plotRT + labs(x="Block",y="Response Time")
ggsave(plot=plotRT, "~/GMU/Lab/Trust/Spring 2014/graphs/RT by block.png")
print(plotRT)

#individual responses
#Cued switches
subjToSound <- ggplot(mergedByBlock, aes(x=Block, y=switchToSound,group=subject,colour=condition))
subjToSound <- subjToSound + geom_line() + geom_point() + facet_wrap(~subject)
ggsave(plot=subjToSound, "~/GMU/Lab/Trust/Spring 2014/graphs/Individual cued switches by block.png")
print(subjToSound)

#uncued switches
subjToNoSound<- ggplot(mergedByBlock, aes(x=Block, y=switchWithNoSound,group=subject,colour=condition))
subjToNoSound <- subjToNoSound + geom_line() + geom_point() + facet_wrap(~subject)
ggsave(plot=subjToNoSound, "~/GMU/Lab/Trust/Spring 2014/graphs/Individual unCued switches by block.png")
print(subjToNoSound)

#ignore cue
subjIgnored <- ggplot(mergedByBlock, aes(x=Block, y=noSwitchToSound,group=subject,colour=condition))
subjIgnored <- subjIgnored + geom_line() + geom_point() + facet_wrap(~subject)
ggsave(plot=subjIgnored, "~/GMU/Lab/Trust/Spring 2014/graphs/Individual ignored cues by block.png")
print(subjIgnored)

#RT
subjRT <- ggplot(mergedByBlock, aes(x=Block, y=RTimeToSound,group=subject,colour=condition))
subjRT <- subjRT + geom_line() + geom_point() + facet_wrap(~subject)
#mergedByBlock$Block <- factor(mergedByBlock$Block) should treat blocks as a covariate, not a factor (they are actually sequential)
ggsave(plot=subjRT, "~/GMU/Lab/Trust/Spring 2014/graphs/Individual RTs by block.png")
print(subjRT)

#anovas for responses by blocks
aovByBlockCued <- aov(switchToSound ~ condition *Block + Error(subject/Block), mergedByBlock)
summary(aovByBlockCued)


#analysis of rate of response to cues
View(mergedByBlock)

subjRateToSound <- ggplot(mergedByBlock, aes(x=Block, y=ResponseRatetoCue,group=subject,colour=condition))
subjRateToSound <- subjRateToSound + geom_line() + geom_point() + facet_wrap(~subject)
ggsave(plot=subjRateToSound, "~/GMU/Lab/Trust/Spring 2014/graphs/Individual cued switch rate by block.png")
print(subjRateToSound)

summaryBy(ResponseRatetoCue ~ condition , EmpericData, FUN = c(mean, var, sd))
respRateAOV <- aov(ResponseRatetoCue ~ condition, data = EmpericData)
summary(respRateAOV)
mean(EmpericData$ResponseRatetoCue)


#order with the participants needed to balance the effects
# balance <- c("B","B","B","B","B","D","D","D")
# balance$order <- sample(balance)
# balance$order
