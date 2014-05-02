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
EmpericData <- read.csv("~/GMU/Lab/Trust/Spring 2014/trust_overview.4.28.2014_subset.csv")

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
scatterplotMatrix(~actual_hitrate +actual_falsealarmrate + 
                    averageRT_tosound + noswitchcnt_tosound + 
                    switchcnt_tosound + switchcnt_extra, data=EmpericData.clean,
                    diagonal="histogram",main="test")

cor(EmpericData.clean[,3:9])
####example!!
# ggpairs(df, 
#         upper = list(params = c(size = 10)),
#         lower = list(continuous = "smooth", params = c(method = "loess", fill = "blue"))
# )

ggpairs(EmpericData.clean[,3:9],
                       upper = list(param=c(size=10)),
                       lower = list(continuous = "smooth",param=c(method="loess", fill = "blue")),
                       diag = list(continuous = "density",discrete="bar")
                        )
##don't print scatterplot... it takes way too long 
ggsave(scatterplot,"~/GMU/Lab/Trust/Spring 2014/graphs/scatterplot matrix - all counts.png")
print(scatterplot)
#calculate infomration sampling by taking total switches and removing switches due to cue
#data$info_sampling <- data$totalswitches - data$hitcnt - data$falsealarmcnt
#plot(summaryBy(info_sampling ~ condition+subject, data=data))

#investigate switching behavior 
summaryBy(switchcnt_extra + switchcnt_tosound + noswitchcnt_tosound ~ condition, data=EmpericData)

#plot
#uncued switches
plotExtra <- ggplot(data = EmpericData, aes(condition, switchcnt_extra))
plotExtra <- plotExtra + geom_boxplot()
ggsave(plot=plotExtra,"~/GMU/Lab/Trust/Spring 2014/Graphs/unCuedSwitches by condition.png")
print(plotExtra)

#cued switches
plotToSound <- ggplot(data = EmpericData, aes(condition, switchcnt_tosound))
plotToSound <- plotToSound + geom_boxplot()
ggsave(plot=plotToSound, "~/GMU/Lab/Trust/Spring 2014/Graphs/CuedSwitches by condition.png")
print(plotToSound)

#ignored cues
plotNoSwitch <- ggplot(data = EmpericData, aes(condition, noswitchcnt_tosound))
plotNoSwitch <- plotNoSwitch + geom_boxplot()
ggsave(plot=plotNoSwitch, "~/GMU/Lab/Trust/Spring 2014/Graphs/NoSwitch by condition.png")
print(plotNoSwitch)
#reaction time
plotRT <- ggplot(data = EmpericData, aes(condition, averageRT_tosound))
plotRT <- plotRT + geom_boxplot()
ggsave(plot=plotRT, "~/GMU/Lab/Trust/Spring 2014/Graphs/RT by condition.png")
print(plotRT)
#calculate significance
anovaCued <- aov(switchcnt_tosound ~ condition, data=EmpericData)
summary(anovaCued)
10934/(10934+11352)
TukeyHSD(anovaCued)

anovaUnCued <- aov(switchcnt_extra ~ condition, data=EmpericData)
summary(anovaUnCued)
TukeyHSD(anovaUnCued)

anovaRT <- aov(averageRT_tosound ~ condition, data=EmpericData)
summary(anovaRT)

anovaIgnore <- aov(noswitchcnt_tosound ~ condition, data=EmpericData)
summary(anovaIgnore)

newLM <- glm(noswitchcnt_tosound ~ intended_hitrate + intended_falsealarmrate, EmpericData,family="poisson" )
summary(newLM)
# #setup to replace missing condition from errors
# missing <- sample(c("C","A","A","B","D","B","A"))
# missing     

test <- summaryBy(actual_hitrate + actual_falsealarmrate + totaltrials ~ condition, data=EmpericData)
test[,"actual_hitrate.mean"] <- round(test[,"actual_hitrate.mean"],2)
test[,"actual_falsealarmrate.mean"] <- round(test[,"actual_falsealarmrate.mean"],2)
test[,"totaltrials.mean"] <- round(test[,"totaltrials.mean"],2)
test

#read the new data in with block information
dataByBlock <- read.csv("~/GMU/Lab/Trust/Spring 2014/trust_overview.4.28.2014_subset.csv")
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


#now that i have reshaped the data and saved it i can wite the temporary dfs if i need memory and just open the file generated at the end


#first we should explore the distributions
hist(mergedByBlock$switchWithNoSound,plot=T) #pousaant
hist(mergedByBlock$switchToSound,plot=T) #normal
hist(mergedByBlock$noSwitchToSound,plot=T) #poussant

#these have the same shape as before that's good, i did not mess up the data

#plot
plotExtra <- ggplot(data = mergedByBlock, aes(Block, switchWithNoSound))
plotExtra <- plotExtra + stat_summary(fun.y="mean",geom="line") + facet_wrap(~ condition)
ggsave(plot=plotExtra, "~/GMU/Lab/Trust/Spring 2014/graphs/Uncued switches by block.png")
print(plotExtra)

plotToSound <- ggplot(data = mergedByBlock, aes(Block, switchToSound))
plotToSound <- plotToSound + stat_summary(fun.y="mean",geom="line") + facet_wrap(~ condition)
ggsave(plot=plotToSound, "~/GMU/Lab/Trust/Spring 2014/graphs/Cued Switchs by block.png")
print(plotToSound)

plotNoSwitch <- ggplot(data = mergedByBlock, aes(Block, noSwitchToSound))
plotNoSwitch <- plotNoSwitch + stat_summary(fun.y="mean",geom="line") + facet_wrap(~ condition)
ggsave(plot=plotNoSwitch, "~/GMU/Lab/Trust/Spring 2014/graphs/Ignored Cues by Block.png")
print(plotNoSwitch)

plotRT <- ggplot(data = mergedByBlock, aes(Block, RTimeToSound))
plotRT <- plotRT + stat_summary(fun.y="mean",geom="line") + facet_wrap(~ condition)
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
