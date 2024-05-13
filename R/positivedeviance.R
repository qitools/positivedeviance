positivedeviance <- function(content, topic, Name_label, subgroup, outcome_label, outcome_type, displaynames, threshold_observations, threshold_value,benchmark_value, benchmark_label, data_type, output_type, x_min, x_max, theme) {
  
  #if (!topic=="99"){stop("This web app is under construction") }
  #stop("Request received") #Works	
  
`%notin%` <- Negate(`%in%`)

 #stop(paste("nMade it so far:\nx_min: ", x_min, sep="")) # Works
	
  if (is.na(benchmark_value) | benchmark_value == 'NULL') {benchmark_value <- NA}

  benchmark_value <- as.numeric(benchmark_value)
  threshold_value <- as.numeric(threshold_value)
  threshold_observations <- as.numeric(threshold_observations)
  x_min <- as.numeric(x_min)
  x_max <- as.numeric(x_max)

  # stop(paste("nMade it so far:\nx_min: ", x_min, sep="")) # Works
	
  study_results <- ifelse(displaynames == 'groups', FALSE, TRUE)
  subgroup <- ifelse(displaynames =='groups', 'YES',subgroup)

	
  if (is.data.frame(content)){ 
    # Script is being run locally on a desktop and not online at openCPU
    # Column names of local file must be 
    x <- content
  }else{
    # Script is being run online at openCPU and not locally on a desktop
    # Special handling if needed of content
    first.row <- substr(content, 1, regexpr("\n",content))
    num.columns <- str_count(first.row, ",")

    #stop(paste("nMade it so far:\nnum.columns: ",num.columns, sep="")) # Works
    
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
  
  #stop(paste("Made it so far - matrix made:\nx: ",x, sep="")) # Works
  
  # Delete first row if contains column labels (detected by as.numeric(column4) = false)
  first.row.header <- FALSE
  if (is.na(as.numeric(x[1,5])) == TRUE){first.row.header <- TRUE}
  if (first.row.header == TRUE){x <- x[-c(1),]}
  # Delete terminal rows if contains instructions (detected by as.numeric(column4) = false)
  x <- x[!(is.na(as.numeric(x[,5])) == TRUE),]
  
  data <- data.frame (x)
  #stop(paste("Made it so far - dataframe made!\ncolums: ", ncol(x),"\nColumn names: ",paste(colnames(data),collapse=', '),"\nrows: ",nrow(x), "\n", sep=""))
  remove(x)
  
column.names <- c("Name",'ID',"Group", "Outcomes", "Observations")
colnames(data) <- column.names
if (data_type == "m"){
	data$PosParenth1 <- regexpr("(", data$Outcomes, fixed=TRUE)
	data$PosParenth2 <- regexpr(")", data$Outcomes, fixed=TRUE)
	data$Mean   <- as.numeric(substring(data$Outcomes, 1, data$PosParenth1 - 1))
	data$sd     <- as.numeric(substring(data$Outcomes, data$PosParenth1 + 1, data$PosParenth2 - 1))
	#stop(data$Mean)
	}else{ # Replace empty outcome cells with zeros. This is common need for Excel. THIS IS NOT WORKING
	data$Outcomes <- ifelse(is.na(data$Outcomes),0,data$Outcomes)
	}

data$Name		<- str_trim(as.character(data$Name)) 
data$ID      		<- str_trim(as.character(data$ID)) 
data$Observations	<- as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data$Observations)))))
size = nrow(data)
size_population = sum(data$Observations)

#stop(paste("Made it so far - before summary stats!\ncolums: ", ncol(data),"\nColumn names: ",paste(colnames(data),collapse=', '),"\nrows: ",nrow(data), "\n", sep=""))
  
## Summary descriptive stats ----------------------------------------------------------------------------
  
  if (data_type == "m"){
  	data$Mean<-as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data$Mean)))))
	data$Outcome.value <-data$Mean
	data$sd<-as.numeric(data$sd)
	data$product <- (data$Mean * data$Observations)
    	mean_population <- (sum(data$product))/size_population
	data <- data[order(data$Mean),]
    }
  if (data_type == "p"){
	#Calculations
	data$Outcomes<-as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data$Outcomes)))))
	data$Outcome.value <-data$Outcomes/data$Observations
	(proportion_population <- sum(data$Outcomes)/sum(data$Observations))
	variance <- sum(data$Observations)*(proportion_population*(1-proportion_population))
	(std.dev <- sqrt(variance))
	test <- 0.10
	(probability <- pbinom(test, size = size, prob = proportion_population, lower.tail = TRUE, log = FALSE))#  4.22 interquartile range using openmetaanalysis methods
	# http://www.stat.yale.edu/Courses/1997-98/101/binom.htm
	(probability <- pnorm(test, mean = proportion_population, sd = std.dev, log = FALSE))#  4.22 interquartile range using openmetaanalysis methods
	data <- data[order(data$Outcome.value),]
  }
  if (data_type == "c"){ # Counts with Poisson distribution
	#Calculations
	data$Outcomes<-as.numeric(as.numeric(gsub(",", "", as.character(str_trim(data$Outcomes)))))
	
	for(i in 1:nrow(data))
	{
	  data$ci.l[i] <- poisson.test(data$Outcomes[i])$conf.int[1]
	  data$ci.u[i] <- poisson.test(data$Outcomes[i])$conf.int[2]
	}
	
	# Estimate standard errors assuming Poisson distribution
	#mydata$se <- sqrt(mydata$pe)
	
	# Calculate standard error
	z_score <- qnorm(0.975)  # Z-score for 95% confidence interval
	data$se <- (data$ci.u - data$ci.l) / (2 * z_score)	  
	data <- data[order(data$Outcomes),]
  }

#stop(data$Outcome.value)
#stop(paste("Made it so far - after summary stats!\ncolums: ", ncol(data),"\nColumn names: ",paste(colnames(data),collapse=', '), "\nOutcome values: ", paste(data$Outcome.value,collapse=', '), "\nrows: ",nrow(data), "\n", sep=""))

  ## Meta-analysis ------------------------------------

  if (data_type == "p"){
	  # Method to GLMM 02/19/2021
	  if (subgroup =='YES'){
		meta1 <- metaprop(Outcomes, Observations, studlab = Name, subgroup = Group, data=data, method = 'GLMM', hakn = TRUE, fixed=FALSE, title='Rates')
	  }else{
		meta1 <- metaprop(Outcomes, Observations, studlab = Name, data=data, method = 'GLMM', hakn = TRUE, fixed=FALSE, title='Rates')
	  }
  (paste(meta1$lower,inv.logit(meta1$TE),meta1$upper))
	}

if (data_type == "m"){
	  if (subgroup =='YES'){
		meta1 <- metamean(Observations,Mean,sd,studlab = Name,subgroup = Group, data=data, fixed=FALSE, sm = "MLN", hakn=TRUE, title='Means')
	  }else{
		meta1 <- metamean(Observations,Mean,sd,studlab = Name, data=data, fixed=FALSE, sm = "MLN", hakn=TRUE, title='Means')
	  }
  (paste(meta1$lower,meta1$TE,meta1$upper))
  }

if (data_type == "c"){ #metagen(Outcomes, se, data = data)
	  if (subgroup =='YES'){
		meta1 <- metagen(Outcomes,se,studlab = Name,subgroup = Group, data=data, fixed=FALSE, title='Counts')
	  }else{
		meta1 <- metagen(Outcomes,se,studlab = Name, data=data, fixed=FALSE, title='Counts')
	  }
  (paste(meta1$lower,meta1$TE,meta1$upper))
  }

summary(meta1)
  (TE = round(meta1$TE.random,2))
  (TE_text = paste("Rate: ",TE," (",round(meta1$lower.random,2)," - ",round(meta1$upper.random,2),")",sep=""))
  I2 = round(meta1$I2*100,1)
  I2.L = round(meta1$lower.I2*100,1)
  I2.U = round(meta1$upper.I2*100,1)
	
  #stop(paste("Success so far! Ready to plot:\nI2 = ", I2, '\nTE: ', TE_text,"\noutput_type: ", output_type , sep=""))

 ## Density plots---------------------------------------------------------------------------------------

  if (output_type == "d")
	  {
	  # Which size to use?
	  size <- nrow(data)
	  #size <- size_population
	  n <- seq(0, size, by = 1)
	  #x <- seq(0, 1, by = 0.01) # No work
	  #stop(paste("x: ",x, sep="")) # Works
	  if (data_type == "m"){
		densities<-dnorm(n, mean = proportion_population, sd = std.dev, log = FALSE) # *adjust
	  }
	  if (data_type == "p"){
		densities<-dbinom(n, size = size, prob = proportion_population , log = FALSE) # *adjust
	  }
	  #stop(paste("densities: ",densities, sep="")) # Works
	  adjust = 100/size
	  adjust.axis = 100
	  if (data_type == "m"){
		adjust.axis<-1
	  }
	  plot (n*adjust, densities/adjust, type = "n", xlab="", ylab = "Probablity of result",
			main = paste("Distribution of ",outcome_label," by ", Name_label,sep=""),xlim=c(0,100), ylim=c(0,0.5))
	  mtext(paste("Results: proportion with ",outcome_label,sep=""), side=1, line = 3)
	  #####
	  x <- seq(0, size, by = 1)
	  densities <-dbinom(x, size = size, prob = proportion_population , log = FALSE) # *adjust
	  s <- spline(x*adjust, densities, xout=seq(0,100,by=1))
	  lines(s, col = "black")

	  #benchmark
	  # '1+' in line below is key 02/24/2019
	  segments(benchmark_value*adjust.axis,0,benchmark_value*adjust.axis,s$y[1+benchmark_value*adjust.axis], col="green")
	  axis(1,at=benchmark_value*adjust.axis,labels="B", col.ticks="green", col.axis="green", col="green", font=2, lwd=2, padj=1.2, las = 1)
	  
	  #Indicate local rate
	  axis(1,at=proportion_population*adjust.axis,labels="M", col.ticks="blue", col.axis="blue", col="blue", lwd=2, padj=1.2, las = 1)
	  #text(proportion_population*size*adjust,0,paste("Mean rate: ",round(100*proportion_population,0),"%",sep=""),col="blue")
	  
	  #Ratings that qualify (> threshold_observations observations)
	  data.threshold <-data[which(data$Observations >= threshold_observations),]
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
	  if (outcome_type != "NA"){
		deviants <- data.threshold[which(data.threshold$Observations >= threshold_observations & data.threshold$Outcome.value < threshold_value),]
		if (outcome_type =="g"){deviants <- data.threshold[which(data.threshold$Observations >= threshold_observations & data.threshold$Outcome.value > threshold_value),]}
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
		
		(temp.list  <- unique(deviants$Name, incomparables = FALSE))
		(deviant.Outcomes <- sum(deviants$Outcomes))
		(deviant.Observations <- sum(deviants$Observations))
		(deviant.rate <- deviant.Outcomes/deviant.Observations)
	  }	
	  
	  #Details
	  textout0 <- paste("Summary:",sep="")
	  textout1 <- paste("Number of ",Name_label," assessed: ",size," (observations: ", size_population,")",sep="")
	  textout2 <- paste("Heterogeneity (I2): ",I2,"%",sep="")
	  textout3 <- paste("       ... among ",Name_label," with ", threshold_observations," or more observations: ",round(100*min(data.threshold$Outcome.value),0),"% to ",round(100*max(data.threshold$Outcome.value),0),"%",sep="")
	  if (outcome_type != "NA"){
		textout4 <- paste("       ... among positive deviants: ",round(100*min(deviants$Outcome.value),0),"% to ",round(100*max(deviants$Outcome.value),0),"%",sep="")
		textout5 <- paste("Among local positive deviants:",sep="")
		textout6 <- paste("  ",round(100*deviant.rate,0),"% (",deviant.Outcomes," of ",deviant.Observations," observations) have ",outcome_label,sep="")
		if (data_type == "m"){
		  textout6 <- paste("  ",4.75," is mean rating",sep="")
		  }
	  }
	  textout10 <- paste("Legend:",sep="")
	  textout11 <- paste("Points indicate ", Name_label, " with  ",threshold_observations," or more observations",sep="")
	  textout12 <- paste("Numbers indicate number of ",Name_label," with that result (if > 1)",sep="")
	  textout13 <- paste("M: mean is " ,round(proportion_population*100,0),"%",sep="")
	  if (data_type == "m"){
		textout13 <- paste("M: mean is " ,4.0,sep="")
		}
	  textout14 <- paste("B: ",benchmark_label,": " ,benchmark_value*100,"%",sep="")
	  if (data_type == "m"){
		textout14 <- paste("B: ",benchmark_label,": " ,benchmark_value,sep="")
	  }
	  # Now write the texts
	  # Left-hand side of plot
	  #if (proportion_population > 0){ #So always use this arrangement
	  xpos <- par("usr")[1] + 2.0*strwidth("A")
	  text(xpos,par("usr")[4]-2.0*strheight("A"),textout0,adj=c(0,0), cex=1.0, font = 2)
	  #text(xpos,par("usr")[4]-3.5*strheight("A"),textout1,adj=c(0,0), cex=0.8)
	  text(xpos,par("usr")[4]-3.5*strheight("A"),textout2,adj=c(0,0), cex=0.8)
	  #text(xpos,par("usr")[4]-6.5*strheight("A"),textout3,adj=c(0,0), cex=0.8)
	  #text(xpos,par("usr")[4]-8.0*strheight("A"),textout4,adj=c(0,0), cex=0.8)
	  if (outcome_type != "NA"){
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
	  
	  #text(par("usr")[2]/3,par("usr")[3]+(par("usr")[4]-par("usr")[3])/2, "This example is three doctors, each with 1000 patients, \nwho have outcomes rates of 10%, 15%, 20%.\nWhat population percentile is the doctor with 10%?")
	}

 ## Forest plots---------------------------------------------------------------------------------------
  if (output_type == "f"){
	
	#stop(paste("Success so far!:\n ", 'Ready for forest plots - displaying deviants ' , sep="")) # Works
	
	##* Identify deviants------------------
	left.deviants <- NULL
	right.deviants <- NULL
	for(i in 1:length(meta1$TE)){
	  if (data_type == "p"){
		if (meta1$upper[i]<inv.logit(meta1$TE.random)){
		  left.deviants <- rbind(left.deviants,meta1$TE[i])}
		if (meta1$lower[i]>inv.logit(meta1$TE.random)){
		  right.deviants <- rbind(right.deviants,meta1$TE[i])}
		}  
	  if (data_type == "m"){
		if (meta1$upper[i]<(meta1$TE.random)){
		  left.deviants <- rbind(left.deviants,meta1$TE[i])}
		if (meta1$lower[i]>(meta1$TE.random)){
		  right.deviants <- rbind(right.deviants,meta1$TE[i])}
	  }  
	}
	(left.deviants)
	(right.deviants)

##** Display attribution/names------------------
for(i in 1:length(meta1$TE)){
  # Replacing names as needed
  if (displaynames == 'none'){meta1$studlab[i] <- meta1$data$ID[i]}
  if (displaynames == 'selected'){
  if (data_type == "p"){
	if (outcome_type == 'b' & meta1$upper[i] >= inv.logit(meta1$TE.random)){meta1$studlab[i] <- meta1$data$ID[i]}
	if (outcome_type == 'g' & meta1$lower[i] <= inv.logit(meta1$TE.random)){meta1$studlab[i] <- meta1$data$ID[i]}
	    }
  if (data_type == "m"){
	if (outcome_type == 'b' & meta1$upper[i] >= meta1$TE.random){meta1$studlab[i] <- meta1$data$ID[i]}
	if (outcome_type == 'g' & meta1$lower[i] <= meta1$TE.random){meta1$studlab[i] <- meta1$data$ID[i]}
	    }
    }
  }

##** Asterisk to deviants------------------
for(i in 1:length(meta1$TE)){
  # Simple adding of asterisk to deviants
  if (data_type == "p"){
    if (meta1$upper[i]<(inv.logit(meta1$TE.random))){
    meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="")}
  if (meta1$lower[i] > (inv.logit(meta1$TE.random))){
    meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="")}
  }
  if (data_type == "m"){
    if (meta1$upper[i] <  meta1$TE.random){
      meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="")}
    if (meta1$lower[i] >= meta1$TE.random){
      meta1$studlab[i] <- paste(meta1$studlab[i],"*",sep="")}
  }
}
meta1$studlab

##* Forest plot ----------------------------------------------------

#if (data_type == "m"){stop(paste("Success so far!:\n ", 'Ready for forest plots - actual plotting: ' , x_min, , ' , ', x_max, sep=""))} # Works
# 2022-05-04 - made it to here

		if (data_type == "p"){
			xlim <- c(0,1)
			leftcols=c("studlab","event","n")
			leftlabs=c(Name_label,outcome_label,"Observations")
			}
		else{
			xlim <- c(x_min, x_max)
			leftcols=c("studlab", "n")
			leftlabs=c(Name_label,"Observations")
			}
		forest(meta1, 
			 leftcols=leftcols,
			 leftlabs=leftlabs, 
			 sortbar = meta1$TE,
			 study.results = study_results,
			 bysort = TRUE,
			 ref = benchmark_value,
			 xlim = xlim, print.I2.ci = TRUE, print.tau2=FALSE, print.Q=FALSE,print.pval.Q=FALSE,studlab= meta1$studlab)
		
		# Title
		grid.text(topic, 0.5, 0.95, gp=gpar(cex=1.4))
		
		#Footer
		if (!is.na(benchmark_value)){
			grid.text('Notes:', 0.08, 0.08, hjust=0, gp=gpar(cex=1, font=2))
			Footer <- NULL
			Footer <- paste(Footer,"Goal is ", benchmark_label, ": ", benchmark_value, " (solid vertical line)")
			grid.text(Footer, 0.08, 0.06, hjust=0, gp=gpar(cex=1, font=1))
			}
  		}
} 
#
