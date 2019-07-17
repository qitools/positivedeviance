positivedeviance <- function(content, topic, subject_label, outcome_label, outcome_type, threshold_count, threshold_value,benchmark_value, benchmark_label, distribution_type, x_min, x_max, theme) {
# Current not used: x_min, x_max,
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
  if (is.na(as.numeric(x[1,3])) == TRUE){first.row.header <- TRUE}
  if (first.row.header == TRUE){x <- x[-c(1),]}
  # Delete terminal rows if contains instructions (detected by as.numeric(year) = false)
  x <- x[!(is.na(as.numeric(x[,3])) == TRUE),]
  
  column.names <- c("Subject","Site", "Outcomes", "Observations")
  #dimnames(x) <- list(NULL, column.names)
  colnames(x) <- column.names
  
  data <- data.frame (x)
  #remove(x)
  
  #stop(paste("Dataframe rows: ",nrow(data),"\n","data: ","\n",data, sep="")) # Works
  
  #### Start here if running locally
  
  #Calculations
  data$Outcomes<-as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data$Outcomes)))))
  data$Observations<-as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data$Observations)))))
  if (distribution_type == "n"){
    data$Mean<-as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data$Mean)))))
    data$Outcome.value <-data$Mean
    }
  if (distribution_type == "b"){
    data$Outcome.value <-data$Outcomes/data$Observations
    (proportion.population <- sum(data$Outcomes)/sum(data$Observations))
    variance <- sum(data$Observations)*(proportion.population*(1-proportion.population))
    (std.dev <- sqrt(variance))
  }
  size = nrow(data)
  size_population = sum(data$Observations)
  (total <- sum(data$Observations))

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
  n <- seq(0, size, by = 1)
  #x <- seq(0, 1, by = 0.01) # No work
  #stop(paste("x: ",x, sep="")) # Works
  if (distribution_type == "n"){
    densities<-dnorm(n, mean = proportion.population, sd = std.dev, log = FALSE) # *adjust
  }
  if (distribution_type == "b"){
    densities<-dbinom(n, size = size, prob = proportion.population , log = FALSE) # *adjust
  }
  #stop(paste("densities: ",densities, sep="")) # Works
  adjust = 100/size
  adjust.axis = 100
  if (distribution_type == "n"){
    adjust.axis<-1
  }
  plot (n*adjust, densities/adjust, type = "n", xlab="", ylab = "Probablity of result",
        main = paste("Distribution of ",outcome_label," by ", subject_label,sep=""),xlim=c(0,100), ylim=c(0,0.5))
  mtext(paste("Results: proportion with ",outcome_label,sep=""), side=1, line = 3)
  #####
  x <- seq(0, size, by = 1)
  densities <-dbinom(x, size = size, prob = proportion.population , log = FALSE) # *adjust
  s <- spline(x*adjust, densities, xout=seq(0,100,by=1))
  lines(s, col = "black")

  #benchmark
  # '1+' in line below is key 02/24/2019
  segments(benchmark_value*adjust.axis,0,benchmark_value*adjust.axis,s$y[1+benchmark_value*adjust.axis], col="green")
  axis(1,at=benchmark_value*adjust.axis,labels="B", col.ticks="green", col.axis="green", col="green", font=2, lwd=2, padj=1.2, las = 1)
  
  #Indicate local rate
  axis(1,at=proportion.population*adjust.axis,labels="M", col.ticks="blue", col.axis="blue", col="blue", lwd=2, padj=1.2, las = 1)
  #text(proportion.population*size*adjust,0,paste("Mean rate: ",round(100*proportion.population,0),"%",sep=""),col="blue")
  
  #Ratings that qualify (> threshold_count observations)
  data.threshold <-data[which(data$Observations >= threshold_count),]
  (nrow(data.threshold))
  
  #Plot point(s) that qualify
  # '1+' in line below is key 02/24/2019
  points(data.threshold$Outcome.value*adjust.axis,s$y[1+data.threshold$Outcome.value*adjust.axis], col="black", pch=19, cex = 1)
  temp.list  <- unique(data.threshold$Outcome.value, incomparables = FALSE)

  duplicates <- 0
  for(i in 1:length(temp.list))
  {
    temp.data <- data.threshold[which(data.threshold$Outcome.value == temp.list[i]),] 
    temp.value <- nrow(temp.data)
    if (temp.value > 1)
      {# '1+' in line below is key 02/24/2019
      duplicates <- 1.5
      text(temp.list[i]*adjust.axis,s$y[1+temp.data$Outcome.value*adjust.axis],temp.value, pos=3, col="black", font=1)
    }
  }
  
  #Plot deviants in green
  if (!outcome_type == "NA"){
    deviants <- data.threshold[which(data.threshold$Observations >= threshold_count & data.threshold$Outcome.value < threshold_value),]
    if (outcome_type =="g"){deviants <- data.threshold[which(data.threshold$Observations >= threshold_count & data.threshold$Outcome.value > threshold_value),]}
    (nrow(deviants))
    #points(deviants$outcome*100,s$y[deviants$outcome*1000], col="red", pch=19, cex=1 + 0.5*(temp.value - 1))
    (temp.list  <- unique(deviants$Outcome.value, incomparables = FALSE))
    #temp.value <- min(deviants$outcome)
    for(i in 1:length(temp.list))
    {
      temp.data <- deviants[which(deviants$Outcome.value == temp.list[i]),] 
      temp.value <- nrow(temp.data)
      point.size <- 1 + 0.25*(temp.value - 1)
      point.size <- 1
      # '1+' in line below is key 02/24/2019
      points(temp.data$Outcome.value*adjust.axis,s$y[1+temp.data$Outcome.value*adjust.axis], col="green", pch=19, cex=point.size)
      if (temp.value > 1)
        {# '1+' in line below is key 02/24/2019
        text(temp.list[i]*adjust.axis,s$y[1+temp.data$Outcome.value*adjust.axis],temp.value, pos=3, col="green", font=2)
        }
      #temp.data <- deviants[which(deviants$outcome != temp.value),] 
    }
    
    (temp.list  <- unique(deviants$Subject, incomparables = FALSE))
    (deviant.Outcomes <- sum(deviants$Outcomes))
    (deviant.Observations <- sum(deviants$Observations))
    (deviant.rate <- deviant.Outcomes/deviant.Observations)
  }	
  
  # Meta-analysis
  data <- data[order(data$Outcome.value),]
  row.names(data)
  meta1 <- metaprop(data$Outcomes, data$Observations, studlab = row.names(data),data=data, sm="PRAW", hakn = TRUE, method = "Inverse",comb.fixed = FALSE,incr=0.5)
  summary(meta1)
  (TE = round(meta1$TE.random,2))
  (TE_text = paste("Rate: ",TE," (",round(meta1$lower.random,2)," - ",round(meta1$upper.random,2),")",sep=""))
  I2 = round(meta1$I2*100,1)
  I2.L = round(meta1$lower.I2*100,1)
  I2.U = round(meta1$upper.I2*100,1)
  #text(par("usr")[2],(par("usr")[4]-1.4*strheight("A"))                     ,cex=1.2,adj=c(1,0),TE_text, font=1, col="black")
  #text(par("usr")[2],(par("usr")[4]-3.0*strheight("A"))                     ,cex=1.2,adj=c(1,0),paste("I2 = ",I2,"% (",I2.L," to ",I2.U,")", sep=""), font=1, col="black")
  
  # And a Forest
  #forest(meta1, leftcols=c("studlab","event","n"),leftlabs=c(subject_label,outcome_label,"Observations"),print.tau2=FALSE, print.Q=FALSE,print.pval.Q=FALSE,studlab=1:nrow(data),xlim=c(0,1))
  
  #Details
  textout0 <- paste("Summary:",sep="")
  textout1 <- paste("Number of ",subject_label," assessed: ",size," (observations: ",total,")",sep="")
  textout2 <- paste("Heterogeneity (I2): ",I2,"%",sep="")
  textout3 <- paste("       ... among ",subject_label," with ", threshold_count," or more observations: ",round(100*min(data.threshold$Outcome.value),0),"% to ",round(100*max(data.threshold$Outcome.value),0),"%",sep="")
  if (!outcome_type == "NA"){
    textout4 <- paste("       ... among positive deviants: ",round(100*min(deviants$Outcome.value),0),"% to ",round(100*max(deviants$Outcome.value),0),"%",sep="")
    textout5 <- paste("Among local positive deviants:",sep="")
    textout6 <- paste("  ",round(100*deviant.rate,0),"% (",deviant.Outcomes," of ",deviant.Observations," observations) have ",outcome_label,sep="")
    if (distribution_type == "n"){
      textout6 <- paste("  ",4.75," is mean rating",sep="")
      }
  }
  textout10 <- paste("Legend:",sep="")
  textout11 <- paste("Points indicate ", subject_label, " with  ",threshold_count," or more observations",sep="")
  textout12 <- paste("Numbers indicate number of ",subject_label," with that result (if > 1)",sep="")
  textout13 <- paste("M: mean is " ,round(proportion.population*100,0),"%",sep="")
  if (distribution_type == "n"){
    textout13 <- paste("M: mean is " ,4.0,sep="")
    }
  textout14 <- paste("B: ",benchmark_type,": " ,benchmark_value*100,"%",sep="")
  if (distribution_type == "n"){
    textout14 <- paste("B: ",benchmark_type,": " ,benchmark_value,sep="")
  }
  # Now write the texts
  # Left-hand side of plot
  #if (proportion.population > 0){ #So always use this arrangement
  xpos <- par("usr")[1] + 2.0*strwidth("A")
  text(xpos,par("usr")[4]-2.0*strheight("A"),textout0,adj=c(0,0), cex=1.0, font = 2)
  #text(xpos,par("usr")[4]-3.5*strheight("A"),textout1,adj=c(0,0), cex=0.8)
  text(xpos,par("usr")[4]-3.5*strheight("A"),textout2,adj=c(0,0), cex=0.8)
  #text(xpos,par("usr")[4]-6.5*strheight("A"),textout3,adj=c(0,0), cex=0.8)
  #text(xpos,par("usr")[4]-8.0*strheight("A"),textout4,adj=c(0,0), cex=0.8)
  if (!outcome_type == "NA"){
    text(xpos,par("usr")[4]-5*strheight("A"),textout5,col="black", font=1, adj=c(0,0), cex=0.8)
    text(xpos,par("usr")[4]-6.5*strheight("A"),textout6,col="black", font=1, adj=c(0,0), cex=0.8)
  }
  # Right-hand side of plot
  xpos <- par("usr")[2] - 2.0*strwidth("A")
  text(xpos,par("usr")[4]-2.0*strheight("A"),textout10,adj=c(1,0), cex=1.0, font = 2)
  text(xpos,par("usr")[4]-3.5*strheight("A"),textout11,adj=c(1,0), cex=0.8)
  if (duplicates == 1.5)  {text(xpos,par("usr")[4]-5.0*strheight("A"),textout12,adj=c(1,0), cex=0.8)}
  text(xpos,par("usr")[4]-(5.0+duplicates)*strheight("A"),textout13,adj=c(1,0), cex=0.8, col="blue")
  if (benchmark_value != 0)  {text(par("usr")[2] - 2.0*strwidth("A"),par("usr")[4]-(6.5+duplicates)*strheight("A"),textout14,adj=c(1,0), font=2, cex=0.8, col="green")}
  
  text(par("usr")[2]/3,par("usr")[3]+(par("usr")[4]-par("usr")[3])/2, "This example is three doctors, each with 1000 patients, \nwho have outcomes rates of 10%, 15%, 20%.\nWhat population percentile is the doctor with 10%?")
  
}
