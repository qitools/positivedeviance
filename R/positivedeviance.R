positivedeviance <- function(content, topic, outcome, counted, timeperiod, goalu, goall, type, theme) {
    #myframe <- data.frame (mymatrix) # For testing
    myframe <- data.frame (content)
    test <- 0.10
    #test <- test * 100
    (proportion.population = sum(myframe$numerator)/sum(myframe$denominator))
    #(probability <- pbinom(test, 100, prob = proportion.population, lower.tail = TRUE, log.p = FALSE))
    variance <- sum(myframe$denominator)*(proportion.population*(1-proportion.population))
    (std.dev <- sqrt(variance))
    (probability <- pbinom(0.1, size = 1000, prob = proportion.population, lower.tail = TRUE, log = FALSE))#  4.22 interquartile range using openmetaanalysis methods
    # http://www.stat.yale.edu/Courses/1997-98/101/binom.htm
    (probability <- pnorm(test, mean = proportion.population, sd = std.dev, log = FALSE))#  4.22 interquartile range using openmetaanalysis methods

	    # Plot
    x <- seq(0, 1, by = 0.01)
    densities<-dnorm(x, mean = proportion.population, sd = std.dev, log = FALSE) # *adjust
    #x <- seq(0, 100, by = 1)
    #densities<-dbinom(x, size = 1, prob = proportion.population , log = FALSE) # *adjust
    plot (x, densities, type = "n", xlab=paste("Results: Percentage ",outcome,sep=""), ylab = "Probablity of result",
          main = paste("Distribution of ",outcome," by ", subjectlabel,sep="")# 
          ,xlim=c(0,1))#,ylim=c(0,1))
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

    text(par("usr")[2]/3,par("usr")[3]+(par("usr")[4]-par("usr")[3])/2, "This example is three doctors, each with 1000 patients, \nwho have outcomes rates of 10%, 15%, 20%.\nWhat population percentile is the doctor with 10%?")
	
}
