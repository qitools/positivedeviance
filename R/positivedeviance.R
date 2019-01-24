positivedeviance <- function(content, topic, outcome, outcome_type, threshold, benchmark, benchmark_type, type, theme) {
	#myframe <- data.frame (mymatrix) # For testing
	
	first.row <- substr(content, 1, regexpr("\n",content))
	num.columns <- str_count(first.row, ",")

	temp <- content
	# Uses package meta http://cran.r-project.org/web/packages/meta/
	# http://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html
	#temp <- gsub('\n', '', fixed = TRUE, temp, perl = TRUE)
	#temp <- gsub("\\s+$", "", temp, perl = TRUE) #Removing trailing whitespace
	#temp <- gsub(",+$", "", temp, perl = TRUE) #Remove trailing comma if accidentally added by user online
	temp <- gsub("\r", ' ', fixed = TRUE, temp)
	temp <- gsub("\n", ' ', fixed = TRUE, temp)
	temp <- gsub("\t", ' ', fixed = TRUE, temp)
	temp <- gsub(',', '","', fixed = TRUE, temp)

	temp <- paste('"',temp,'"',sep = '')
	temp <- paste('Mymatrix <- matrix(c(',temp,'), ncol=',num.columns,', byrow=TRUE)')
	x<-eval(parse(file = "", n = NULL, text = temp))
	# Delete first row if contains column labels (detected by as.numeric(year) = false)
	first.row.header <- FALSE
	if (is.na(as.numeric(x[1,2])) == TRUE){first.row.header <- TRUE}
	if (first.row.header == TRUE){x <- x[-c(1),]}
	# Delete terminal rows if contains instructions (detected by as.numeric(year) = false)
	x <- x[!(is.na(as.numeric(x[,2])) == TRUE),]

	column.names <- c("rowname","numerator", "denominator")
	dimnames(x) <- list(NULL, column.names)

	myframe <- data.frame (x)
	remove(x)
	
	#stop(paste("Dataframe rows: ",nrow(myframe), sep=""))
	stop(paste("myframe: ",myframe, sep=""))

	myframe$numerator   <-as.numeric(myframe$numerator)
	myframe$denominator <-as.numeric(myframe$denominator)
	
	stop(paste("myframe$denominator: ",myframe$denominator, sep=""))

	subjectlabel <- topic
	test <- 0.10
	(proportion.population = sum(myframe$numerator)/sum(myframe$denominator))
	variance <- sum(myframe$denominator)*(proportion.population*(1-proportion.population))
	(std.dev <- sqrt(variance))
	(probability <- pbinom(0.1, size = 1000, prob = proportion.population, lower.tail = TRUE, log = FALSE))#  4.22 interquartile range using openmetaanalysis methods
	# http://www.stat.yale.edu/Courses/1997-98/101/binom.htm
	(probability <- pnorm(test, mean = proportion.population, sd = std.dev, log = FALSE))#  4.22 interquartile range using openmetaanalysis methods

	stop(paste("std.dev: ",std.dev, sep=""))

	# Plot
    x <- seq(0, 1, by = 0.01)
    densities<-dnorm(x, mean = proportion.population, sd = std.dev, log = FALSE) # *adjust
    #x <- seq(0, 100, by = 1)
    #densities<-dbinom(x, size = 1, prob = proportion.population , log = FALSE) # *adjust
    plot (x, densities, type = "n", xlab=paste("Results: Percentage ",outcome,sep=""), ylab = "Probablity of result",
          main = paste("Distribution of ",outcome," by ", subjectlabel,sep="")# 
          ,xlim=c(0,1),ylim=c(0,1))
    s <- spline(x, densities, xout=seq(0,1,by=0.01))
    lines(s)
    points(test,probability, col="red", pch=19)

    #Indicate local rate
    axis(1,at=proportion.population,labels="Clinic rate", col.ticks="red", col.axis="red", col="red", las = 2)

    # Display details
    textout1 <- paste("Probability of ",test,"% is ",round(probability,3),"%",sep="")
    textout2 <- paste("Population size: ",sum(myframe$denominator), sep="")
    if (proportion.population > 50){
      text(par("usr")[1]+1*strwidth("A"),par("usr")[4]-1.5*strheight("A"),textout1,adj=c(0,0), cex=0.8)
      text(par("usr")[1]+1*strwidth("A"),par("usr")[4]-3 * strheight("A"),textout2,adj=c(0,0), cex=0.8)
    }else{
      text(par("usr")[2]-1*strwidth("A"),par("usr")[4]-1.5*strheight("A"),textout1,adj=c(1,0), cex=0.8)
      text(par("usr")[2]-1*strwidth("A"),par("usr")[4]-3 * strheight("A"),textout2,adj=c(1,0), cex=0.8)
    }

    #text(par("usr")[2]/3,par("usr")[3]+(par("usr")[4]-par("usr")[3])/2, "This example is three doctors, each with 1000 patients, \nwho have outcomes rates of 10%, 15%, 20%.\nWhat population percentile is the doctor with 10%?")
	
}
