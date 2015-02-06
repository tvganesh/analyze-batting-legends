#############################################################################################
#
# Analysis of cricket's batting legends - Through the mirage with R
# Designed and developed by: Tinniam V Ganesh    
# Date: 5 Feb 2015
# More details - https://gigadom.wordpress.com
# 
##############################################################################################

# Plot the batting performance as a bar plot of runs versus frequency
battingperf <- function(df, name) {
  
  # Remove rows where the batsman 'did not bat' - DNB
  a <- df$Runs != "DNB"
  batsman <- df[a,]

  # Remove rows with 'TDNB'
  c <- batsman$Runs != "TDNB"
  batsman <- batsman[c,]

  # Divide batting into groups of 20 runs
  batsman$Runs <- as.numeric(gsub("\\*","",batsman$Runs))
 
  # Divide the runs into 20 run ranges from 0 to 400
  f <- cut(batsman$Runs, breaks=seq(from=0,to=400,by=20))

  # Create a table
  g <- table(f)
  
  # Create a vector to store the runs frequency
  v <- as.vector(g)

  # Compute percentage of runs in the overall run total
  percentRuns <- (g/sum(g))*100
  runfreq <- c(name, round(percentRuns,1), "\n")
  
  # Write the output in the ./output directory
  setwd("./output")
  cat(runfreq, file="runs-frequency.txt", sep=",",append=TRUE)
  setwd("..")
  
  # Save the plot
  setwd("./plots")
  runfreqplot <- paste(name,"-runs-frequency.jpg")
  jpeg(runfreqplot)

  atitle <- paste(name,"'s", " batting career - Percentage times (%) vs Run ranges (Runs)")
  # Plot the batting performance 
  barplot(percentRuns, main = atitle ,xlab="Runs scored",
          ylab="Percentage times runs scored in range (%)",ylim=c(0,100))
  axis(side=2, at=seq(0, 100, by=5))
  
  dev.off()
  setwd("..")
  
  plotPerf(percentRuns, name)
  
  rate4(batsman, name)
  rate6(batsman,name)
  dismissal(batsman, name)
  
  
}

# Plot the performance of the batsman as a continous graph
plotPerf <- function(percentRuns,name){
  a <- as.data.frame(percentRuns)
  g <- seq(from=10,to=390,by=20)
  
  # Save the plot
  setwd("./plots")
  perfplot <- paste(name,"-perf-plot.jpg")
  jpeg(perfplot)
  
  atitle = paste(name,"'s", "Overall batting performance")
  plot(g,a$Freq, xlab="Runs", ylab = "Run percentages (%)", main = atitle, ylim=c(0,100))
  lines(g,a$Freq)
  axis(side=2, at=seq(0, 100, by=5))
  
  dev.off()
  setwd("..")
}

# Plot the Percentage of runs in 4s vs Runs scored
rate4 <- function(batsman, name) {
   
   x4s <- as.numeric(as.vector(batsman$X4s))
   
   # Check if there are NAs in 4s
   a <- is.na(x4s)
   
   # Get all the 4's
   x4s <- x4s[!a] 
   
   runs <- as.numeric(batsman$Runs)
   runs <- runs[!a]
   
   # Save the plot
   setwd("./plots")
   plot4s <- paste(name,"-4s-plot.jpg")
   jpeg(plot4s)
   
   atitle = paste(name,"-","Runs vs No of 4s" )
   
   # Plot no of 4s and a 2nd order curve fit   
   plot(runs,x4s, xlab = "Runs", ylab = "Number of 4's", main = atitle ) 
   
   # Second order polynomial used
   fit2 <- lm(x4s~poly(runs,2,raw=TRUE))
   
   xx <- seq(from=0,to = max(runs),by=20)
   yy <- NULL
   for (i in seq_along(xx)) {
     yy[i] <- fit2$coefficients[3] * xx[i]^2 + fit2$coefficients[2] * xx[i] + fit2$coefficients[1] 
    
   }
   lines(xx,yy,col="blue")
   
   dev.off()
   setwd("..")
  
   fours <-c(name, round(yy), "\n")
   
   # Write the output in the ./output directory
   setwd("./output")
   cat(fours, file="fours-of-batsman.txt", sep=",",append=TRUE)
   setwd("..")  
}

# Plot the Percentage of runs in 6s vs Runs scored
rate6 <- function(batsman, name) {
  
  x6s <- as.numeric(as.vector(batsman$X6s))
  
  # Check if there are NAs in 6s
  a <- is.na(x6s)
  
  # Get all the 6's
  x6s <- x6s[!a] 
  
  runs <- as.numeric(batsman$Runs)
  runs <- runs[!a]
  
  # Save the plot
  setwd("./plots")
  
  plot6s <- paste(name,"-6s-plot.jpg")
  jpeg(plot6s)
  
  atitle = paste(name,"-","Runs vs Percentage of runs in 6s" )
  # Calculate the percentage runs contributed by 6s
  percent <- (x6s/runs) *100
  plot(runs,percent, xlab = "Runs", ylab = "Percentage of runs in 6's", main = atitle )
  
  # Second order polynomial used
  fit2 <- lm(percent~poly(runs,2,raw=TRUE))
  xx <- seq(from=0,to = max(runs),by=20)
  yy <- NULL
  for (i in seq_along(xx)) {
    yy[i] <- fit2$coefficients[3] * xx[i]^2 + fit2$coefficients[2] * xx[i] + fit2$coefficients[1] 
    
  }
  lines(xx,yy,col="purple")
  
  dev.off()
  setwd("..")
  
}

# Plot the dismissals of the batsman as a pie chart
dismissal <- function(df, name) {
  
  # Save the plot
  setwd("./plots")
  dismissalsplot <- paste(name,"'s-dismissals.jpg")
  jpeg(dismissalsplot)
  a <- df$Runs != "DNB"
  batsman <- df[a,]
  
  # Remove rows with 'TDNB'
  c <- batsman$Runs != "TDNB"
  batsman <- batsman[c,]
  
  # Divide batting into groups of 20 runs
  batsman$Runs <- as.numeric(gsub("\\*","",batsman$Runs))
  lbls <- NULL
  
  d <- batsman$Dismissal
  
  # Convert to data frame
  dismissal <- data.frame(table(d))
  
  # Remove "-"
  dismissal <- dismissal[dismissal$d != "-",]
  
  lbls <- dismissal$d
  slices <- dismissal$Freq
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  atitle <- paste("Pie chart of dismissals for ", name)
  
  # Important note: Ensure the number of labels & slices match
  pie3D(slices, labels=lbls,explode=0.1, main= atitle,pty="s")
  dev.off()
  setwd("..")
  
  
}

# Helper function that calculates the percentage of runs in ranges of 20
percentRuns <- function(df, name) {
  
  # Remove rows where the batsman 'did not bat' - DNB
  a <- df$Runs != "DNB"
  batsman <- df[a,]
  
  # Remove rows with 'TDNB'
  c <- batsman$Runs != "TDNB"
  batsman <- batsman[c,]
  
  # Divide batting into groups of 20 runs
  batsman$Runs <- as.numeric(gsub("\\*","",batsman$Runs))
  
  f <- cut(batsman$Runs, breaks=seq(from=0,to=400,by=20))
  
  # Create a table
  g <- table(f)
  
  # Compute percentage
  percentRuns <- (g/sum(g))*100
  percentRuns
}

# Overall performance used for comparing relative performance of batsman
overallperf <- function(frame, name,color,flag=FALSE) {
 
    name ="abc"
    g <- seq(from=10,to=390,by=20)
    pR <- percentRuns(frame,name)
    a <- as.data.frame(pR)
    if(flag == TRUE) {
        plot(g,a$Freq, cex=0.8, xlab="Runs", ylab = "Run Percentages (%)", main = "Overall batting performances", ylim=c(0,50))
    }
    lines(g,a$Freq,col=color, lwd=2.5)
    
}

# Compute the moving average of the time series
movingaverage <- function(df,name) {

  # Remove rows where the batsman 'did not bat' - DNB
  a <- df$Runs != "DNB"
  batsman <- df[a,]
  
  # Remove rows with 'TDNB'
  c <- batsman$Runs != "TDNB"
  batsman <- batsman[c,]
  
  # Divide batting into groups of 20 runs
  batsman$Runs <- as.numeric(gsub("\\*","",batsman$Runs))
  
  # Check if there are NAs in runs
  a <- is.na(batsman$Runs)
  
  # Remove na's
  batsman <- batsman[!a,] 
  
  
  batsman$Start.Date <- as.Date(batsman$Start.Date,format="%d-%B-%y")
  runs <- batsman$Runs
  date <-  batsman$Start.Date 
  timeframe <- data.frame(runs,date)
  
  # Save the plot
  setwd("./plots")
  movingavgplot <- paste(name,"moving-avg.jpg")
  jpeg(movingavgplot)
  
  atitle <- paste(name,"'s moving average (runs) over career")
  plot(timeframe$date,timeframe$runs,type="o",col="grey", xlab ="Year", ylab = "Moving average", main=atitle)
  # Use loess regression to fit the moving average
  lines(timeframe$date,predict(loess(runs~as.numeric(date),timeframe)),lwd=2)
  dev.off()
  setwd("..")
  
}

