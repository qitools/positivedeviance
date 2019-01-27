positivedeviance <- function(content, topic, subjectlabel, outcome, outcome_type, threshold_count, threshold_value,benchmark_value, benchmark_type, type, theme) {
	#data <- data.frame (mymatrix) # For testing 
	if (!topic=="99"){stop("This web app is under constrution") }
 	#stop("Request received") #Works	
	first.row <- substr(content, 1, regexpr("\n",content))
	num.columns <- str_count(first.row, ",")

	#stop(paste("num.columns: ",num.columns, sep="")) # Works

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

	#stop(paste("x: ",x, sep="")) # Works
	
	# Delete first row if contains column labels (detected by as.numeric(year) = false)
	first.row.header <- FALSE
	if (is.na(as.numeric(x[1,2])) == TRUE){first.row.header <- TRUE}
	if (first.row.header == TRUE){x <- x[-c(1),]}
	# Delete terminal rows if contains instructions (detected by as.numeric(year) = false)
	x <- x[!(is.na(as.numeric(x[,2])) == TRUE),]

	column.names <- c("Subject","Outcomes", "Observations")
	dimnames(x) <- list(NULL, column.names)

	data <- data.frame (x)
	remove(x)
	
	#stop(paste("Dataframe rows: ",nrow(data),"\n","data: ","\n",data, sep="")) # Works
  
	#### Start here if running locally
	
	data$Outcomes<-as.numeric(as.character(str_trim(data$Outcomes)))
	data$Observations<-as.numeric(as.character(str_trim(data$Observations)))
	data$Outcome.rate<-data$Outcomes/data$Observations
	size = nrow(data)
	adjust = 100/size # disp.ave / ave
	
	#stop(paste("data$Observations: ",data$Observations, sep="")) # Works

	benchmark_value <- as.numeric(benchmark_value)
	threshold_value <- as.numeric(threshold_value)
	threshold_count <- as.numeric(threshold_count)
	(total <- sum(data$Observations))
	(proportion.population <- sum(data$Outcomes)/total)
	variance <- sum(data$Observations)*(proportion.population*(1-proportion.population))
	(std.dev <- sqrt(variance))
	test <- 0.10
	(probability <- pbinom(test, size = size, prob = proportion.population, lower.tail = TRUE, log = FALSE))#  4.22 interquartile range using openmetaanalysis methods
	# http://www.stat.yale.edu/Courses/1997-98/101/binom.htm
	(probability <- pnorm(test, mean = proportion.population, sd = std.dev, log = FALSE))#  4.22 interquartile range using openmetaanalysis methods
	
	#stop(paste("std.dev: ",std.dev, sep="")) # Works

	# Plot
	x <- seq(0, size, by = 1)
	#x <- seq(0, 1, by = 0.01) # No work
	#stop(paste("x: ",x, sep="")) # Works
	if (type == "n"){
		densities<-dnorm(x, mean = proportion.population, sd = std.dev, log = FALSE) # *adjust
		}
	if (type == "b"){
		densities<-dbinom(x, size = size, prob = proportion.population , log = FALSE) # *adjust
	}
	#stop(paste("densities: ",densities, sep="")) # Works
	plot (x*adjust, densities, type = "n", xlab=paste("Results: Percentage ",outcome,sep=""), ylab = "Probablity of result",
	      main = paste("Distribution of ",outcome," by ", subjectlabel,sep=""),xlim=c(0,100), ylim=c(0,1))
	s <- spline(x*adjust, densities, xout=seq(0,100,by=1))
	lines(s)
	
	#stop(paste("data$Observations:",data$Observations,sep=""))
	#stop(paste("threshold_count:",threshold_count,sep=""))

	#Ratings that qualify (> threshold_count observations)
	data.threshold <-data[which(data$Observations >= threshold_count),]
	(nrow(data.threshold))
	#OOPS - for some reason getting only 1 row
	#stop(paste("nrow(data.threshold): ",nrow(data.threshold),sep=""))
	
	#Plot point(s) that qualify
	points(data.threshold$Outcome.rate*100,s$y[data.threshold$Outcome.rate*100], col="black", pch=19, cex = 1)
	temp.list  <- unique(data.threshold$Outcome.rate, incomparables = FALSE)
	for(i in 1:length(temp.list))
	{
	  temp.data <- data.threshold[which(data.threshold$Outcome.rate == temp.list[i]),] 
	  temp.value <- nrow(temp.data)
	  text(temp.list[i]*100,s$y[temp.data$Outcome.rate*100],temp.value, pos=3, col="black", font=1)
	}
	
	#stop(paste("benchmark_value:",benchmark_value,sep=""))
	#benchmark
	segments(benchmark_value*100,0,benchmark_value*100,s$y[benchmark_value*100], col="green")
	axis(1,at=benchmark_value*100,labels="Benchmark", col.ticks="green", col.axis="green", col="green", las = 2)

	#Indicate local rate
	axis(1,at=proportion.population*100,labels="Mean", col.ticks="red", col.axis="red", col="red", las = 2)
	#axis(1,at=proportion.population*size*adjust,labels="", col.ticks="red", col.axis="red", col="red")
	#text(proportion.population*size*adjust,0,paste("Mean rate: ",round(100*proportion.population,0),"%",sep=""),col="red")
	
	#Plot deviants in red
	if (!outcome_type == "NA"){
  	deviants <- data.threshold[which(data.threshold$Observations >= threshold_count & data.threshold$Outcome.rate < threshold_value),]
  	if (outcome_type =="g"){deviants <- data.threshold[which(data.threshold$Observations >= threshold_count & data.threshold$Outcome.rate > threshold_value),]}
  	(nrow(deviants))
  	#points(deviants$outcome*100,s$y[deviants$outcome*1000], col="red", pch=19, cex=1 + 0.5*(temp.value - 1))
  	(temp.list  <- unique(deviants$Outcome.rate, incomparables = FALSE))
  	#temp.value <- min(deviants$outcome)
  	for(i in 1:length(temp.list))
  	{
  	  temp.data <- deviants[which(deviants$Outcome.rate == temp.list[i]),] 
  	  temp.value <- nrow(temp.data)
  	  point.size <- 1 + 0.25*(temp.value - 1)
  	  point.size <- 1
  	  points(temp.data$Outcome.rate*100,s$y[temp.data$Outcome.rate*100], col="red", pch=19, cex=point.size)
  	  text(temp.list[i]*100,s$y[temp.data$Outcome.rate*100],temp.value, pos=3, col="red", font=2)
  	  #temp.data <- deviants[which(deviants$outcome != temp.value),] 
  	}
  	
  	temp.list  <- unique(deviants$Subject, incomparables = FALSE)
  	temp.list
	}	
	#Details
	textout1 <- paste("Number of ",topic," assessed: ",size," (observations: ",total,")",sep="")
	textout2 <- paste("Not displayed are ", subjectlabel, " with less than ",threshold_count," observations",sep="")
	textout3 <- paste("Range of ",outcome," among all ",subjectlabel,": ",round(100*min(data$Outcome.rate),0),"% to ",round(100*max(data$Outcome.rate),0),"%",sep="")
	textout4 <- paste("       ... among ",subjectlabel," with ", threshold_count," or more observations: ",round(100*min(data.threshold$Outcome.rate),0),"% to ",round(100*max(data.threshold$Outcome.rate),0),"%",sep="")
	if (!outcome_type == "NA"){textout5 <- paste("       ... among positive deviants: ",round(100*min(deviants$Outcome.rate),0),"% to ",round(100*max(deviants$Outcome.rate),0),"%",sep="")}
	textout6 <- paste("Result of ",benchmark_type,": " ,benchmark_value,"%",sep="")
	textout10 <- paste("Numbers indicate number of ",subjectlabel," with that result",sep="")
	if (proportion.population > 0.50){
	  xpos <- par("usr")[1] + 2.0*strwidth("A")
	  text(xpos,par("usr")[4]-1.5*strheight("A"),textout1,adj=c(0,0), cex=0.8)
	  text(xpos,par("usr")[4]-3.0*strheight("A"),textout2,adj=c(0,0), cex=0.8)
	  text(xpos,par("usr")[4]-4.5*strheight("A"),textout3,adj=c(0,0), cex=0.8)
	  text(xpos,par("usr")[4]-6.5*strheight("A"),textout4,adj=c(0,0), cex=0.8)
	  if (!outcome_type == "NA"){text(xpos,par("usr")[4]-8*strheight("A"),textout5,adj=c(0,0), cex=0.8)}
	  if (benchmark_value < 101){text(xpos,par("usr")[4]-9.5*strheight("A"),textout6,adj=c(0,0), cex=0.8, col="green")}
	  text(par("usr")[2] - 2.0*strwidth("A"),par("usr")[4]-1.5*strheight("A"),textout10,adj=c(1,0), cex=0.8)
	}else{
	  xpos = par("usr")[2] - 2.0*strwidth("A")
	  text(xpos,par("usr")[4]-1.5*strheight("A"),textout1,adj=c(1,0), cex=0.8)
	  text(xpos,par("usr")[4]-3.0*strheight("A"),textout2,adj=c(1,0), cex=0.8)
	  text(xpos,par("usr")[4]-4.5*strheight("A"),textout3,adj=c(1,0), cex=0.8)
	  text(xpos,par("usr")[4]-6.5*strheight("A"),textout4,adj=c(1,0), cex=0.8)
	  if (!outcome_type == "NA"){text(xpos,par("usr")[4]-8*strheight("A"),textout5,adj=c(1,0), cex=0.8)}
	  if (benchmark_value < 101){text(xpos,par("usr")[4]-9.5*strheight("A"),textout6,adj=c(1,0), cex=0.8, col="green")}
	  text(par("usr")[1] + 2.0*strwidth("A"),par("usr")[4]-1.5*strheight("A"),textout10,adj=c(0,0), cex=0.8)
	}
	#text(par("usr")[2]/3,par("usr")[3]+(par("usr")[4]-par("usr")[3])/2, "This example is three doctors, each with 1000 patients, \nwho have outcomes rates of 10%, 15%, 20%.\nWhat population percentile is the doctor with 10%?")
	
}
