positivedeviance <- function(content, topic, subject_label, outcome_label, outcome_type, threshold_count, threshold_value,benchmark_value, benchmark_label, distribution_type, x_min, x_max, theme) {
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

  column.names <- c("Subject","Site", "Rate_or_Mean", "Observations")
  #dimnames(x) <- list(NULL, column.names)
  colnames(x) <- column.names
  
  data <- data.frame (x)
  remove(x)
  
  #stop( paste("Dataframe rows: ",nrow(data),"\n","data: ","\n",data, sep="") ) # Works
  
  #### Start here if running locally
  
  #Conversions
  benchmark_value <- as.numeric(benchmark_value)
  threshold_value <- as.numeric(threshold_value)
  threshold_count <- as.numeric(threshold_count)
  x_min <- as.numeric(x_min)
  x_max <- as.numeric(x_max)
  data$Rate_or_Mean<-as.numeric(as.character(str_trim(data$Rate_or_Mean)))
  data$Observations<-as.numeric(as.character(str_trim(data$Observations)))

  #Calculations
  size = nrow(data)
  (size_population <- sum(data$Observations))
  (Rate_or_Mean_Population <- sum(data$Rate_or_Mean*data$Observations)/size_population)

  #test <- 0.10
  #(probability <- pbinom(test, size = size, prob = Rate_or_Mean_Population, lower.tail = TRUE, log = FALSE))#  4.22 interquartile range using openmetaanalysis methods
  # http://www.stat.yale.edu/Courses/1997-98/101/binom.htm
  #(probability <- pnorm(test, mean = Rate_or_Mean_Population, sd = std.dev, log = FALSE))#  4.22 interquartile range using openmetaanalysis methods
  
  #stop(paste("std.dev: ",std.dev, sep="")) # Works
  
  # Plot
  x <- seq(0, size, by = 1)
  #stop(paste("x: ",x, sep="")) # Works
  if (distribution_type == "n"){
    (variance <- var(data$Rate_or_Mean))
    (std.dev <- sd(data$Rate_or_Mean))
    densities<-dnorm(x, mean = Rate_or_Mean_Population, sd = std.dev, log = FALSE) # *adjust
    adjust.axis <- 1
    xlab = paste("Results: mean of ", outcome_label,sep="")
  }
  if (distribution_type == "b"){
    (variance <- sum(data$Observations)*(Rate_or_Mean_Population*(1-Rate_or_Mean_Population)))
    (std.dev <- sqrt(variance))
    densities<-dbinom(x, size = size, prob = Rate_or_Mean_Population , log = FALSE) # *adjust
    adjust.axis <- 100
    xlab = paste("Results: rate of ", outcome_label,sep="")
  }
  adjust = (x_max-x_min)/size
  #stop(paste("densities: ",densities, sep="")) # Works
  # Make plot but *NO* points yet as these will be equally distributed
  plot (x*adjust*adjust.axis, densities, type = "p", xlab=xlab, ylab = "Probablity of result", yaxt='n',
        main = paste("Distribution of ",outcome_label," by ", subject_label,sep=""),xlim=c(x_min,x_max*adjust.axis), ylim=c(0,1.5*max(densities)))
  axis(2,at=0.05,labels="0.05", col.ticks="green", col.axis="green", col="green", font=2, lwd=2, padj=0.5, las = 1)
  #Lines smoothed. Note that we make x=100 rather than x=size
  s <- spline(x*adjust*adjust.axis, densities, xout=seq(x_min,x_max*adjust.axis,by=adjust.axis/100))
  lines(s, col = "black")
  #####OBSERVED VS EXPECTED
  #xpos <- par("usr")[1] + 2.0*strwidth("A")
  #text(xpos,par("usr")[4]-2.0*strheight("A"),"Black line is expected distribution",adj=c(0,0), cex=0.8)
  # Dotted line is expected distribution
  x <- seq(0, size_population, by = 1)
  adjust = (x_max-x_min)/size_population
  densities_pop <-dbinom(x, size = size_population, prob = Rate_or_Mean_Population , log = FALSE) # *adjust
  ss <- spline(x*adjust*adjust.axis, densities_pop*(max(densities)/max(densities_pop)), xout=seq(x_min,x_max*adjust.axis,by=x_max*adjust.axis/100))
  #lines(ss, lty = "dashed", col = "black")
  #text(xpos,par("usr")[4]-3.5*strheight("A"),"Dashed line is observed distribution",adj=c(0,0), cex=0.8)
  #####
  
  #benchmark
  # '1+' in line below is key 02/24/2019
  #segments(benchmark_value*adjust.axis,0,benchmark_value*adjust.axis,s$y[1+benchmark_value*adjust.axis], col="green")
  axis(1,at=benchmark_value*adjust.axis,labels="B", col.ticks="green", col.axis="green", col="green", font=2, lwd=2, padj=1.2, las = 1)
  
  #Indicate local rate
  axis(1,at=Rate_or_Mean_Population*adjust.axis,labels="M", col.ticks="blue", col.axis="blue", col="blue", lwd=2, padj=1.2, las = 1)
  #text(Rate_or_Mean_Population*size*adjust,0,paste("Mean rate: ",round(100*Rate_or_Mean_Population,0),"%",sep=""),col="blue")
  
  #Ratings that qualify (> threshold_count observations)
  data.threshold <-data[which(data$Observations >= threshold_count),]
  (nrow(data.threshold))
  
  #Plot point(s) that qualify
  # '1+' in line below is key 02/24/2019
  points(data.threshold$Rate_or_Mean*adjust.axis,s$y[1+data.threshold$Rate_or_Mean*adjust.axis], col="black", pch=19, cex = 1)
  temp.list  <- unique(data.threshold$Rate_or_Mean, incomparables = FALSE)
  
  for(i in 1:length(temp.list))
  {
    temp.data <- data.threshold[which(data.threshold$Rate_or_Mean == temp.list[i]),] 
    temp.value <- nrow(temp.data)
    if (temp.value > 1)
    {# '1+' in line below is key 02/24/2019
      text(temp.list[i]*adjust.axis,s$y[1+temp.data$Rate_or_Mean*adjust.axis],temp.value, pos=3, col="black", font=1)
    }
  }
  
  #Plot deviants in green
  if (!outcome_type == "NA"){
    deviants <- data.threshold[which(data.threshold$Observations >= threshold_count & data.threshold$Rate_or_Mean < threshold_value),]
    if (outcome_type =="g"){deviants <- data.threshold[which(data.threshold$Observations >= threshold_count & data.threshold$Rate_or_Mean > threshold_value),]}
    (nrow(deviants))
    #points(deviants$outcome*100,s$y[deviants$outcome*1000], col="red", pch=19, cex=1 + 0.5*(temp.value - 1))
    (temp.list  <- unique(deviants$Rate_or_Mean, incomparables = FALSE))
    #temp.value <- min(deviants$outcome)
    for(i in 1:length(temp.list))
    {
      temp.data <- deviants[which(deviants$Rate_or_Mean == temp.list[i]),] 
      temp.value <- nrow(temp.data)
      point.size <- 1 + 0.25*(temp.value - 1)
      point.size <- 1
      # '1+' in line below is key 02/24/2019
      points(temp.data$Rate_or_Mean*100,s$y[1+temp.data$Rate_or_Mean*100], col="green", pch=19, cex=point.size)
      if (temp.value > 1)
      {# '1+' in line below is key 02/24/2019
        text(temp.list[i]*100,s$y[1+temp.data$Rate_or_Mean*100],temp.value, pos=3, col="green", font=2)
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
  textout1 <- paste("Number of ",subject_label," assessed: ",size," (observations: ",size_population,")",sep="")
  textout2 <- paste("Not displayed are ", subject_label, " with less than ",threshold_count," observations",sep="")
  textout3 <- paste("Range of ",outcome_label," among all ",subject_label,": ",round(100*min(data$Outcome.rate),0),"% to ",round(100*max(data$Outcome.rate),0),"%",sep="")
  textout4 <- paste("       ... among ",subject_label," with ", threshold_count," or more observations: ",round(100*min(data.threshold$Outcome.rate),0),"% to ",round(100*max(data.threshold$Outcome.rate),0),"%",sep="")
  if (!outcome_type == "NA"){
    textout5 <- paste("       ... among positive deviants: ",round(100*min(deviants$Outcome.rate),0),"% to ",round(100*max(deviants$Outcome.rate),0),"%",sep="")
    textout6 <- paste("Among positive deviant ",subject_label,": ",round(100*deviant.rate,0),"% (",deviant.Outcomes," of ",deviant.Observations,") have ",outcome,sep="")
  }
  textout10 <- paste("Legend:",sep="")
  textout11 <- paste("Numbers indicate number of ",subject_label," with that result (if > 1)",sep="")
  textout12 <- paste("M: mean is " ,round(Rate_or_Mean_Population*100,0),"%",sep="")
  textout13 <- paste("B: ",benchmark_label,": " ,benchmark_value*100,"%",sep="")
  #if (Rate_or_Mean_Population > 0){ #So always use this arrangement
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
