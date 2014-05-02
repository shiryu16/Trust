# Malcolm, 10/22/2010, trying to create the simplest cat I can think of
# var here should be a string representation of the variable you want to print
printv <- function(var, dbg=F) {

	# if the global debug flag is true then printv will always run
	# otherwise, you must pass in a T as the 2nd parameter
	if (exists("debug", mode="logical")) {
		if (debug)
			dbg <- T
	}		
	
	if (dbg == T) {
		index <- which(names(as.list(parent.frame())) == var)
		cat(names(as.list(parent.frame())[index]), paste("=",as.list(parent.frame())[index],sep=" "), fill=T)
	}
}


setdirectory <- function(dirname) {
	directory <- paste(getwd(),"/",dirname,"/",sep="")
	return(directory)
}

getcsv <- function(dirname, fname, header=TRUE) {
	f <- read.csv(paste(dirname,fname,sep=""),header=header)
	return(f)
}

# set the text up for a new working directory
getNewWD <- function(dirtxt) {

	newdir <- paste(getwd(),"/experiment.dirs/",dirtxt,sep="")
	return(newdir)
}

gettxt <- function(dirname, fname, skip, sep="\t", header=T) {

	f <- read.table(paste(dirname,"/",fname, sep=""), sep=sep, skip=skip, header=header)
	return(f)
}

cat.debug <- function(var, debug, msg="Var = ") {
	if (debug == TRUE) {
		cat(msg, var, fill=T)
	}
}

getdistbetweentwopoints <- function(x1,y1,x2,y2) {
	
		dist <- sqrt(((x1 - x2)*(x1 - x2)) + ((y1 - y2)*(y1 - y2)))
		
		return(dist)
}