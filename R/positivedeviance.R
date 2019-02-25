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
    #stop(paste("temp: ",temp, sep="")) # Works
    x<-eval(parse(file = "", n = NULL, text = temp))
  }
  
  #stop(x) # Works
  #stop(paste("x: ",x, sep="")) # Works
  
  # Delete first row if contains column labels (detected by as.numeric(Observations) = false)
  first.row.header <- FALSE
  if (is.na(as.numeric(x[1,3])) == TRUE){first.row.header <- TRUE}
  if (first.row.header == TRUE){x <- x[-c(1),]}

  column.names <- c("Subject","Site", "Outcomes", "Observations")
  #dimnames(x) <- list(NULL, column.names)
  colnames(x) <- column.names
  
  data <- data.frame (x)
  remove(x)
  
  #stop( paste("Dataframe rows: ",nrow(data),"\n","data: ","\n",data, sep="") ) # Works
  
  #### Start here if running locally
  
  #Calculations
  data$Outcomes<-as.numeric(as.character(str_trim(data$Outcomes)))
  data$Observations<-as.numeric(as.character(str_trim(data$Observations)))
  data$Outcome.rate<-data$Outcomes/data$Observations
  size = nrow(data)
  size_population = sum(data$Observations)
  (total <- sum(data$Observations))
  (proportion.population <- sum(data$Outcomes)/total)
  variance <- sum(data$Observations)*(proportion.population*(1-proportion.population))
  (std.dev <- sqrt(variance))
  
  benchmark_value <- as.numeric(benchmark_value)
  threshold_value <- as.numeric(threshold_value)
  threshold_count <- as.numeric(threshold_count)
  test <- 0.10
  (probability <- pbinom(test, size = size, prob = proportion.population, lower.tail = TRUE, log = FALSE))#  4.22 interquartile range using openmetaanalysis methods
  # http://www.stat.yale.edu/Courses/1997-98/101/binom.htm
  (probability <- pnorm(test, mean = proportion.population, sd = std.dev, log = FALSE))#  4.22 interquartile range using openmetaanalysis methods
  
  #stop(paste("std.dev: ",std.dev, sep="")) # Works
  
  # Plot
  # Which size to use?
  size <- nrow(data)
  #size <- size_population
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
  adjust = 100/size
  plot (x*adjust, densities/adjust, type = "n", xlab="", ylab = "Probablity of result",
        main = paste("Distribution of ",outcome," by ", subjectlabel,sep=""),xlim=c(0,100), ylim=c(0,1))
  mtext(paste("Results: Percentage ",outcome,sep=""), side=1, line = 3)
  #####
  x <- seq(0, size, by = 1)
  densities <-dbinom(x, size = size, prob = proportion.population , log = FALSE) # *adjust
  s <- spline(x*adjust, densities, xout=seq(0,100,by=1))
  lines(s, col = "black")
  #####EXPECTED
  #xpos <- par("usr")[1] + 2.0*strwidth("A")
  #text(xpos,par("usr")[4]-2.0*strheight("A"),"Black line is expected distribution",adj=c(0,0), cex=0.8)
  # Dotted line is expected distribution
  adjust = 100/size_population
  x <- seq(0, size_population, by = 1)
  densities_pop <-dbinom(x, size = size_population, prob = proportion.population , log = FALSE) # *adjust
  ss <- spline(x*adjust, densities_pop*10, xout=seq(0,100,by=1))
  #lines(ss, lty = "dashed", col = "black")
  #text(xpos,par("usr")[4]-3.5*strheight("A"),"Dashed line is observed distribution",adj=c(0,0), cex=0.8)
  #####
  
  #benchmark
  # '1+' in line below is key 02/24/2019
  segments(benchmark_value*100,0,benchmark_value*100,s$y[1+benchmark_value*100], col="green")
  axis(1,at=benchmark_value*100,labels="B", col.ticks="green", col.axis="green", col="green", font=2, lwd=2, padj=1.2, las = 1)
  
  #Indicate local rate
  axis(1,at=proportion.population*100,labels="M", col.ticks="blue", col.axis="blue", col="blue", lwd=2, padj=1.2, las = 1)
  #text(proportion.population*size*adjust,0,paste("Mean rate: ",round(100*proportion.population,0),"%",sep=""),col="blue")
  
  #Ratings that qualify (> threshold_count observations)
  data.threshold <-data[which(data$Observations >= threshold_count),]
  (nrow(data.threshold))
  
  #Plot point(s) that qualify
  # '1+' in line below is key 02/24/2019
  points(data.threshold$Outcome.rate*100,s$y[1+data.threshold$Outcome.rate*100], col="black", pch=19, cex = 1)
  temp.list  <- unique(data.threshold$Outcome.rate, incomparables = FALSE)
  
  for(i in 1:length(temp.list))
  {
    temp.data <- data.threshold[which(data.threshold$Outcome.rate == temp.list[i]),] 
    temp.value <- nrow(temp.data)
    if (temp.value > 1)
    {# '1+' in line below is key 02/24/2019
      text(temp.list[i]*100,s$y[1+temp.data$Outcome.rate*100],temp.value, pos=3, col="black", font=1)
    }
  }
  
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
      # '1+' in line below is key 02/24/2019
      points(temp.data$Outcome.rate*100,s$y[1+temp.data$Outcome.rate*100], col="green", pch=19, cex=point.size)
      if (temp.value > 1)
      {# '1+' in line below is key 02/24/2019
        text(temp.list[i]*100,s$y[1+temp.data$Outcome.rate*100],temp.value, pos=3, col="green", font=2)
      }
      #temp.data <- deviants[which(deviants$outcome != temp.value),] 
    }
    
    temp.list  <- unique(deviants$Subject, incomparables = FALSE)
    deviant.Outcomes <- sum(deviants$Outcomes)
    deviant.Observations <- sum(deviants$Observations)
    deviant.rate <- deviant.Outcomes/deviant.Observations
  }	
  #Details
  textout0 <- paste("Summary:",sep="")
  textout1 <- paste("Number of ",subjectlabel," assessed: ",size," (observations: ",total,")",sep="")
  textout2 <- paste("Not displayed are ", subjectlabel, " with less than ",threshold_count," observations",sep="")
  textout3 <- paste("Range of ",outcome," among all ",subjectlabel,": ",round(100*min(data$Outcome.rate),0),"% to ",round(100*max(data$Outcome.rate),0),"%",sep="")
  textout4 <- paste("       ... among ",subjectlabel," with ", threshold_count," or more observations: ",round(100*min(data.threshold$Outcome.rate),0),"% to ",round(100*max(data.threshold$Outcome.rate),0),"%",sep="")
  if (!outcome_type == "NA"){
    textout5 <- paste("       ... among positive deviants: ",round(100*min(deviants$Outcome.rate),0),"% to ",round(100*max(deviants$Outcome.rate),0),"%",sep="")
    textout6 <- paste("Among positive deviant ",subjectlabel,": ",round(100*deviant.rate,0),"% (",deviant.Outcomes," of ",deviant.Observations,") have ",outcome,sep="")
  }
  textout10 <- paste("Legend:",sep="")
  textout11 <- paste("Numbers indicate number of ",subjectlabel," with that result (if > 1)",sep="")
  textout12 <- paste("M: mean is " ,round(proportion.population*100,0),"%",sep="")
  textout13 <- paste("B: ",benchmark_type,": " ,benchmark_value*100,"%",sep="")
  #if (proportion.population > 0){ #So always use this arrangement
  xpos <- par("usr")[1] + 2.0*strwidth("A")
  text(xpos,par("usr")[4]-2.0*strheight("A"),textout0,adj=c(0,0), cex=1.0, font = 2)
  text(xpos,par("usr")[4]-3.5*strheight("A"),textout1,adj=c(0,0), cex=0.8)
  text(xpos,par("usr")[4]-5.0*strheight("A"),textout2,adj=c(0,0), cex=0.8)
  text(xpos,par("usr")[4]-6.5*strheight("A"),textout3,adj=c(0,0), cex=0.8)
  text(xpos,par("usr")[4]-8.0*strheight("A"),textout4,adj=c(0,0), cex=0.8)
  if (!outcome_type == "NA"){
    text(xpos,par("usr")[4]-9*strheight("A"),textout5,col="green", font=2, adj=c(0,0), cex=0.8)
    text(xpos,par("usr")[4]-10.5*strheight("A"),textout6,col="green", font=2, adj=c(0,0), cex=0.8)
  }
  xpos <- par("usr")[2] - 2.0*strwidth("A")
  text(xpos,par("usr")[4]-2.0*strheight("A"),textout10,adj=c(1,0), cex=1.0, font = 2)
  text(xpos,par("usr")[4]-3.5*strheight("A"),textout11,adj=c(1,0), cex=0.8)
  text(xpos,par("usr")[4]-5.0*strheight("A"),textout12,adj=c(1,0), cex=0.8, col="blue")
  if (benchmark_value < 101)  {text(par("usr")[2] - 2.0*strwidth("A"),par("usr")[4]-6.5*strheight("A"),textout13,adj=c(1,0), font=2, cex=0.8, col="green")}
  
  #text(par("usr")[2]/3,par("usr")[3]+(par("usr")[4]-par("usr")[3])/2, "This example is three doctors, each with 1000 patients, \nwho have outcomes rates of 10%, 15%, 20%.\nWhat population percentile is the doctor with 10%?")
  
}
