require(ggplot2)
require(reshape)
require(doBy)

source("~/Documents/models/vigilance/RecentFiles.r")

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

GetFiles <- function(input.directory) {

  alldata.df <- NULL
  fnames <- list.files(input.directory, pattern=".*.csv", full.names=F)
  for (fname in fnames) {
    df <- read.csv(paste(input.directory, fname, sep=""))
#                   col.names=c("subj", "Period1", "Period2", "Period3", "Period4", "Nothing", "egs", "ut", "signal.reward", "alpha"))
##    bar <<- df <- df[,c("Period1", "Period2", "Period3", "Period4", "egs", "ut", "signal.reward", "noise.reward")]
#    bar <<- df <- df[,c("Period1", "Period2", "Period3", "Period4", "egs", "ut", "signal.reward", "alpha")]
#    nn <<- m <- colMeans(df)
#    egs <- df$egs[1]
#    ut <- df$ut[1]
#    signal.reward <- df$signal.reward[1]
#    alpha <- df$alpha[1]
#    noise.reward <- df$noise.reward[1]
#    egs <- as.numeric(substr(fname, 22, 22))
#    ut <- as.numeric(substr(fname, 26, 26))
#    rew <- as.numeric(substr(fname, 31, 32))
#    cat(fname, fill=T)
##    cat("egs =", egs, " ut =", ut, " signal reward =", signal.reward, " noise reward = ", noise.reward, fill=T)
#    cat("egs =", egs, " ut =", ut, " signal reward =", signal.reward, " alpha =", alpha, fill=T)
#    print(m)
#    cat(fill=T)
##    foo <<- alldata.df <- rbind(alldata.df, c(m, egs, ut, signal.reward, noise.reward))
##    foo <<- alldata.df <- rbind(alldata.df, c(m, egs, ut, signal.reward))
#    foo <<- alldata.df <- rbind(alldata.df, c(m))
  }
#  alldata.df <- data.frame(alldata.df)
##  colnames(alldata.df) <- c("Period1", "Period2", "Period3", "Period4", "egs", "ut", "signal.reward", "noise.reward")
#  colnames(alldata.df) <- c("Period1", "Period2", "Period3", "Period4", "egs", "ut", "signal.reward", "alpha")
  print(df)
  return(df)
}

### takes individual filename instead of a whole directory
GetFile <- function(fname) {
  df <- read.csv(fname)
  print(df)
  return(df)
}

GetRecentFile <- function(input.directory) {

  MostRecentFile <- GetMostRecentFile(input.directory)
  return(MostRecentFile)

}

#df <- read.csv("~/Documents/models/vigilance/outputfile.8.9.11.egs2.csv",
#               col.names=c("subj", "Period1", "Period2", "Period3", "Period4", "Nothing"))
#df <- read.csv("~/Documents/models/vigilance/outputfiles/OUTPUTFILE.8.9.11.egs1.ut0.csv",
#               col.names=c("subj", "Period1", "Period2", "Period3", "Period4", "Nothing"))
#df <- df[,c("Period1", "Period2", "Period3", "Period4")]
#m <- colMeans(df)
#print(m)

##df$subj <- 1:nrow(df)

input.directory <- "~/Documents/models/vigilance/outputfiles/"
##alldata.df <- GetFiles("~/Documents/models/vigilance/outputfiles.5.subjects.explore/")
#alldata.df <- GetFiles("~/Documents/models/vigilance/outputfiles/")
#fname <- "~/Documents/models/vigilance/outputfiles/outputfile.8.25.11.csv"
fname <- GetRecentFile(input.directory)
###fname.nameonly <- tail(fname, 15) ### not exactly, but close.
cat("Analyzing", fname, fill=T)
cat(fill=T)
alldata.df <- GetFile(fname)
alldata.df$Period1 <- as.numeric(alldata.df$Period1)
alldata.df$Period2 <- as.numeric(alldata.df$Period2)
alldata.df$Period3 <- as.numeric(alldata.df$Period3)
alldata.df$Period4 <- as.numeric(alldata.df$Period4)

df.doby <- summaryBy(Period1 + Period2 + Period3 + Period4 ~ egs + ut + signal.reward + alpha, data = alldata.df,
 	 FUN = function(x) { c(m = mean(x)) } )
# 	 FUN = function(x) { c(m = mean(x, na.rm=T)) } )
## model.run should probably be model.avg
df.doby$model.run <- 1:nrow(df.doby)

#alldata.df$model.run <- 1:nrow(alldata.df)

df.melt <- melt(df.doby,
                id=c("model.run", "egs", "ut", "signal.reward", "alpha"))
df.melt$value <- df.melt$value * 10

#df.melt <- melt(alldata.df,
#                id=c("model.run", "egs", "ut", "signal.reward", "alpha"))
#df.melt$value <- df.melt$value * 10

### from hitchcock03.r  No-Cue conditions  TIES paper
hitchcock.nocue <- data.frame(value=c(90, 77.5, 79.4, 71.9, 71.2, 58.8, 63.1, 55),
                              sd   =c(2.74,5.52,4.69, 6.27, 5.76, 7.52, 6.44, 8.61),
                              variable=c("Period1.m", "Period1.m",
                                "Period2.m", "Period2.m",
                                "Period3.m", "Period3.m",
                                "Period4.m", "Period4.m"),
                              model.run=c("High", "Low"))
hitchcock.nocue$ci <- 2 * hitchcock.nocue$sd / sqrt(8)
### original data
## > exp4.means <- ddply(exp4.summaryby, .(Condition, block), summarise, Correct = mean(Correct.m), sd = sd(Correct.m))
## > exp4.means1
##   Condition block   Correct        sd
## 1       400     1 0.9074074 0.1141050
## 2       400     2 0.8666667 0.1176697
## 3       400     3 0.7703704 0.2366673
## 4       400     4 0.8111111 0.2118296
## 5       800     1 0.9269231 0.1218448
## 6       800     2 0.9076923 0.1354196
## 7       800     3 0.8730769 0.2050516
## 8       800     4 0.8461538 0.2436896
### after shiva has run more participants:
## exp4.means
##   Condition block   Correct         sd
## 1       400     1 0.9193548 0.11081322
## 2       400     2 0.8516129 0.13132902
## 3       400     3 0.7870968 0.23344084
## 4       400     4 0.8129032 0.21407465
## 5       800     1 0.9344828 0.11108511
## 6       800     2 0.9241379 0.09507581
## 7       800     3 0.9068966 0.12798168
## 8       800     4 0.8931034 0.16675695
### all data from shiva
##   Condition block   Correct        sd
## 1       400     1 0.8925000 0.1575249
## 2       400     2 0.8400000 0.1614041
## 3       400     3 0.7775000 0.2391089
## 4       400     4 0.7900000 0.2097618
## 5       800     1 0.9341463 0.1039465
## 6       800     2 0.9146341 0.1108174
## 7       800     3 0.9121951 0.1228721
## 8       800     4 0.8829268 0.1610938
### only good eyetracking data from shiva
##   Condition block   Correct        sd
## 1       400     1 0.8939394 0.1579941
## 2       400     2 0.8242424 0.1696141
## 3       400     3 0.7575758 0.2424231
## 4       400     4 0.7696970 0.2172050
## 5       800     1 0.9242424 0.1118881
## 6       800     2 0.9181818 0.1044466
## 7       800     3 0.9151515 0.1227834
## 8       800     4 0.8666667 0.1726026

### original data
## exp4.df <- data.frame(value=c(90.7, 92.7, 86.7, 90.8, 77.0, 87.3, 81.1, 84.6),
##                       sd   =c(11.4, 12.1, 11.7, 13.5, 23.6, 20.5, 21.1, 24.4),
##                       variable=c("Period1.m", "Period1.m",
##                         "Period2.m", "Period2.m",
##                         "Period3.m", "Period3.m",
##                         "Period4.m", "Period4.m"),
##                       model.run=c("400", "800"))  ## 400, 800

### updated shiva data (incomplete still, but closer)
## exp4.df <- data.frame(value=c(91.9, 93.4, 85.1, 92.4, 78.7, 90.6, 81.3, 89.3),
##                       sd   =c(11.1, 11.1, 13.1, 9.5, 23.3, 12.8, 21.4, 16.7),
##                       variable=c("Period1.m", "Period1.m",
##                         "Period2.m", "Period2.m",
##                         "Period3.m", "Period3.m",
##                         "Period4.m", "Period4.m"),
##                       model.run=c("400", "800"))  ## 400, 800
### all data from shiva...
exp4.df <- data.frame(value=c(89.25, 93.41, 84, 91.46, 77.75, 91.22, 79, 88.29),
                      sd   =c(15.75, 10.39, 16.14, 11.08, 23.91, 12.28, 20.98, 16.11),
                      variable=c("Period1.m", "Period1.m",
                        "Period2.m", "Period2.m",
                        "Period3.m", "Period3.m",
                        "Period4.m", "Period4.m"),
                      model.run=c("400", "800"))  ## 400, 800
exp4.df$ci <- 2 * exp4.df$sd / sqrt(26)

### 95% CI
#limits <- aes(ymax = hitchcock.nocue$value + hitchcock.nocue$ci,
#              ymin=  hitchcock.nocue$value - hitchcock.nocue$ci)
limits <- aes(ymax = exp4.df$value + exp4.df$ci,
              ymin=  exp4.df$value - exp4.df$ci)
dodge <- position_dodge(width=0.9)

##df.melt <- subset(df.melt, alpha < 0.1)
#df.melt <- subset(df.melt, signal.reward >= 40)
#df.melt <- subset(df.melt, signal.reward < 55)
##p <- ggplot(subset(df.melt, ut == -6), aes(x=variable, y=value, group=model.run))
p <- ggplot(df.melt, aes(x=variable, y=value, group=model.run))
##p <- ggplot(subset(df.melt, alpha< 0.1), aes(x=variable, y=value, group=model.run))
##p <- p + geom_point(aes(size=2))
p <- p + ggtitle(fname)
##p <- p + geom_point(aes(shape=factor(ut)))
p <- p + geom_point(aes(shape=factor(egs)))
#p <- p + geom_point()
###p <- p + geom_line(aes(colour = cue.group)) + facet_grid(. ~ salience)
#p <- p + geom_line()
#p <- p + geom_line(aes(colour=model.run)) + facet_grid(signal.reward ~ egs)
##p <- p + geom_line() + facet_grid(ut + signal.reward ~ egs)
######p <- p + geom_line(aes(colour = as.factor(ut))) + facet_grid(signal.reward ~ egs)
##p <- p + geom_line(aes(colour = as.factor(alpha))) + facet_grid(signal.reward ~ egs)
###p <- p + geom_line(aes(linetype = as.factor(ut))) + facet_grid(signal.reward ~ egs)
p <- p + geom_line(aes(linetype = as.factor(egs))) + facet_grid(signal.reward ~ ut)
#p <- p + geom_line(aes(linetype = as.factor(alpha)))
##p <- p + geom_line(aes(linetype = as.factor(alpha))) + facet_grid(signal.reward ~ egs + ut)
#p <- p + geom_line() + facet_grid(alpha ~ ut)
#p <- p + geom_line(aes(colour=model.run))
#p <- p + geom_line(aes(colour=model.run)) + facet_grid(ut + signal.reward ~ egs)
#p <- p + geom_line(data=hitchcock.nocue)
#####p <- p + geom_line(data=hitchcock.nocue, aes(x = variable, y = value, group = model.run))
p <- p + geom_line(data=exp4.df, aes(x = variable, y = value, group = model.run, colour=model.run))
#p <- p + geom_errorbar(data=hitchcock.nocue, limits, width=.2)
#p <- p + geom_errorbar(limits, width=0.25)
print(p)

CalculateFits <- function(df, vigilance.df) {

  v <- NULL
  v.ci <- NULL
  for (run in unique(vigilance.df$model.run)) {
    v[[run]] <- subset(vigilance.df, model.run==run)$value
    v.ci[[run]] <- subset(vigilance.df, model.run==run)$ci
#    vh <<- vigilance.high <- subset(vigilance.df, model.run=="High")$value
#    vh.ci <<- vigilance.high.ci <- subset(vigilance.df, model.run=="High")$ci
#    hl <<- vigilance.low <- subset(vigilance.df, model.run=="Low")$value
#    hlci <<- vigilance.low.ci <- subset(vigilance.df, model.run=="Low")$ci
  }

  for (run in unique(df$model.run)) {
    model.run.data <- subset(df, model.run==run)
    model.data <- model.run.data$value
    cat("egs =", model.run.data$egs[1], " ut =", model.run.data$ut[1], " signal.reward =", model.run.data$signal.reward[1], " alpha =", model.run.data$alpha[1], fill=T)
    print(model.data)
    for (condition.run in unique(vigilance.df$model.run)) {
      cat(condition.run, "correlation = ", cor(v[[condition.run]], model.data), "rmsd = ", rmsd(v[[condition.run]], model.data), fill=T)
      GoodFits <- InRange(model.data, v[[condition.run]] - v.ci[[condition.run]], v[[condition.run]] + v.ci[[condition.run]])
      cat("  Number of model points in CI for", condition.run,"= ", GoodFits)
      if (GoodFits == 4)
        cat(" *")
      if (GoodFits == 3)
        cat(" +")
      cat(fill=T)
    }
    cat(fill=T)
    ## cat("low saliency correlation = ", cor(hitchcock.low, model.data), "rmsd = ", rmsd(hitchcock.low, model.data), fill=T)
    ## GoodFits <- InRange(model.data, hitchcock.low - hitchcock.low.ci, hitchcock.low + hitchcock.low.ci)
    ## cat("Number of model points in CI for low saliency = ", GoodFits)
    ## if (GoodFits == 4)
    ##   cat(" *")
    ## if (GoodFits == 3)
    ##   cat(" +")
    ## cat(fill=T)
    ## cat(fill=T)
  }

}

##CalculateFits(df.melt, hitchcock.nocue)
CalculateFits(df.melt, exp4.df)
