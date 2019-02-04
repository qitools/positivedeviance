positivedeviance <- function(content, topic, subjectlabel, outcome, outcome_type, threshold_count, threshold_value,benchmark_value, benchmark_type, type, theme) {
  if (!topic=="99"){stop("This web app is under constrution") }
  #stop("Request received") #Works	
  
  if (is.data.frame(content)){
    # Script is being run locally on a desktop and not online at openCPU
    #Column names of local file must be 
    x <- content
  }else{
    # Script is being run online at openCPU and not locally on a desktop
    # Special handling if needed of content
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
  }
  
  #stop(paste("x: ",x, sep="")) # Works
  
  # Delete first row if contains column labels (detected by as.numeric(year) = false)
  first.row.header <- FALSE
  if (is.na(as.numeric(x[1,2])) == TRUE){first.row.header <- TRUE}
  if (first.row.header == TRUE){x <- x[-c(1),]}
  # Delete terminal rows if contains instructions (detected by as.numeric(year) = false)
  x <- x[!(is.na(as.numeric(x[,2])) == TRUE),]
  
  column.names <- c("Subject","Site", "Outcomes", "Observations")
  #dimnames(x) <- list(NULL, column.names)
  colnames(x) <- column.names
  
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
  #par(mar=c(5.1,4.1,4.1,2.1), mfrow=c(1,1))
  plot (x*adjust, densities, type = "n", xlab="", ylab = "Probablity of result",
        main = paste("Distribution of ",outcome," by ", subjectlabel,sep=""),xlim=c(0,100), ylim=c(0,1))
  mtext(paste("Results: Percentage ",outcome,sep=""), side=1, line = 3)
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
  axis(1,at=benchmark_value*100,labels="B", col.ticks="green", col.axis="green", col="green", font=2, lwd=2, padj=1.2, las = 1)
  
  #Indicate local rate
  axis(1,at=proportion.population*100,labels="M", col.ticks="blue", col.axis="blue", col="blue", lwd=2, padj=1.2, las = 1)
  #axis(1,at=proportion.population*size*adjust,labels="", col.ticks="blue", col.axis="blue", col="blue")
  #text(proportion.population*size*adjust,0,paste("Mean rate: ",round(100*proportion.population,0),"%",sep=""),col="blue")
  
  #Plot deviants in green
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
      points(temp.data$Outcome.rate*100,s$y[temp.data$Outcome.rate*100], col="green", pch=19, cex=point.size)
      text(temp.list[i]*100,s$y[temp.data$Outcome.rate*100],temp.value, pos=3, col="green", font=2)
      #temp.data <- deviants[which(deviants$outcome != temp.value),] 
    }
    
    temp.list  <- unique(deviants$Subject, incomparables = FALSE)
    temp.list
  }	
  #Details
  textout0 <- paste("Summary:",sep="")
  textout1 <- paste("Number of ",subjectlabel," assessed: ",size," (observations: ",total,")",sep="")
  textout2 <- paste("Not displayed are ", subjectlabel, " with less than ",threshold_count," observations",sep="")
  textout3 <- paste("Range of ",outcome," among all ",subjectlabel,": ",round(100*min(data$Outcome.rate),0),"% to ",round(100*max(data$Outcome.rate),0),"%",sep="")
  textout4 <- paste("       ... among ",subjectlabel," with ", threshold_count," or more observations: ",round(100*min(data.threshold$Outcome.rate),0),"% to ",round(100*max(data.threshold$Outcome.rate),0),"%",sep="")
  if (!outcome_type == "NA"){textout5 <- paste("       ... among positive deviants: ",round(100*min(deviants$Outcome.rate),0),"% to ",round(100*max(deviants$Outcome.rate),0),"%",sep="")}
  textout10 <- paste("Legend:",sep="")
  textout11 <- paste("Numbers indicate number of ",subjectlabel," with that result",sep="")
  textout12 <- paste("M: mean is " ,round(proportion.population*100,0),"%",sep="")
  textout13 <- paste("B: ",benchmark_type,": " ,benchmark_value*100,"%",sep="")
  #if (proportion.population > 0){ #So always use this arrangement
  xpos <- par("usr")[1] + 2.0*strwidth("A")
  text(xpos,par("usr")[4]-2.0*strheight("A"),textout0,adj=c(0,0), cex=1.0, font = 2)
  text(xpos,par("usr")[4]-3.5*strheight("A"),textout1,adj=c(0,0), cex=0.8)
  text(xpos,par("usr")[4]-5.0*strheight("A"),textout2,adj=c(0,0), cex=0.8)
  text(xpos,par("usr")[4]-6.5*strheight("A"),textout3,adj=c(0,0), cex=0.8)
  text(xpos,par("usr")[4]-8.0*strheight("A"),textout4,adj=c(0,0), cex=0.8)
  if (!outcome_type == "NA"){text(xpos,par("usr")[4]-9*strheight("A"),textout5,col="green", font=2, adj=c(0,0), cex=0.8)}
  xpos <- par("usr")[2] - 2.0*strwidth("A")
  text(xpos,par("usr")[4]-2.0*strheight("A"),textout10,adj=c(1,0), cex=1.0, font = 2)
  text(xpos,par("usr")[4]-3.5*strheight("A"),textout11,adj=c(1,0), cex=0.8)
  text(xpos,par("usr")[4]-5.0*strheight("A"),textout12,adj=c(1,0), cex=0.8, col="blue")
  text(xpos,par("usr")[4]-5.0*strheight("A"),textout13,adj=c(1,0), cex=0.8, col="green")
  if (benchmark_value < 101)  {text(par("usr")[2] - 2.0*strwidth("A"),par("usr")[4]-65*strheight("A"),textout13,adj=c(1,0), font=2, cex=0.8, col="green")}
  
  #text(par("usr")[2]/3,par("usr")[3]+(par("usr")[4]-par("usr")[3])/2, "This example is three doctors, each with 1000 patients, \nwho have outcomes rates of 10%, 15%, 20%.\nWhat population percentile is the doctor with 10%?")
  
}
