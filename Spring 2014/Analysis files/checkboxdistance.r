source("Spring 2014/utilities.mal.r")

dir.box <- paste(getwd(),"/Spring 2014/Trust2.box/",sep="")

box.files <- list.files(dir.box)
box.files.l <- list.files(dir.box, full.names=T)

getFile <- function(subj, filelist) {
	index <- which(grepl(subj, filelist) == T)
	fname <- filelist[index]
	return(fname)
}

getDistBetweenTwoPoints <- function(x1, y1, x2, y2) {
	dist <- sqrt((x2-x1)^2+(y2-y1)^2)
	return(dist)
}

for (z in 1:length(box.files)) {

	subj <- substr(box.files[z],0,3)
	printv("subj", T)

	box.fname <- getFile(subj, box.files.l)
	box.df <- read.csv(box.fname, col.names=c("X","Y","Time","Form"))
	
	newdf <- data.frame()
	for (i in 1:(length(box.df$X)-1)) {
		dist <- getDistBetweenTwoPoints(box.df[i,]$X, box.df[i,]$Y, box.df[i+1,]$X, box.df[i+1,]$Y)
		newdf[i,"dist"] <- dist
	}
	
	write.csv(newdf, paste(subj,".dists.csv",sep=""), row.names=F)
	
} #z
