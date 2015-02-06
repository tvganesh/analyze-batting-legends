#############################################################################################
#
# Analysis of cricket's batting legends - Through the mirage with R
# Designed and developed by: Tinniam V Ganesh    
# Date: 5 Feb 2015
# More details - https://gigadom.wordpress.com
# 
##############################################################################################

#Install package plotrix for pie3D
library(plotrix)

setwd("C:\\software\\R\\cricket")
source("battingperf.R")
# Create an output directory if it does not exist
if(!file.exists("output")) {
  dir.create("output")
}

if(!file.exists("plots")) {
  dir.create("plots")
}

# Create a heading vector for the file
ranges <- c("Name", "0-20","20-40","40-60","60-80","80-100",
            "100-120,120-140","140-160","160-180","180-200",
            "200-220,220-240","240-260","260-280","280-300",
            "300-320,320-340","340-360","360-380","380-400","\n")

setwd("./output")
# Create a file in the output directory
file.create("runs-frequency.txt")
cat(ranges, file="runs-frequency.txt", s
    ep=",",append=TRUE)
getwd()
setwd("..")


# Write to a file
# Create a file in the output directory for 4s
setwd("./output")
file.create("fours-of-batsman.txt")
r <- seq(from=0,to=240,by=20)
r <- c("Name",r,"\n")
cat(r, file="fours-of-batsman.txt", sep=",",append=TRUE)
getwd()
setwd("..")

# Read data
richards = read.csv("richards.csv")
name <- 'Vivian Richards'
battingperf(richards,name)

villiers = read.csv("villiers.csv")
name <- 'AB De Villiers'
battingperf(villiers,name)

# Read data
lara = read.csv("lara.csv")
name <- 'Brian Lara'
battingperf(lara,name)

# Sachin Tendulkar's batting performance
tendulkar = read.csv("tendulkar.csv")
name <- 'Sachin Tendulkar'
battingperf(tendulkar,name)

# Sunil Gavaskar's batting performance
gavaskar = read.csv("gavaskar.csv")
name <- 'Sunil Gavaskar'
battingperf(gavaskar,name)

# Rahul Dravid's batting performance
dravid = read.csv("dravid.csv")
name <- 'Rahul Dravid'
battingperf(dravid,name)

# Ricky Ponting's batting performance
ponting = read.csv("ponting.csv")
name <- 'Ricky Ponting'
battingperf(ponting,name)

# Sir Don Bradman's batting performance
bradman = read.csv("bradman.csv")
name <- 'Sir Don Bradman'
battingperf(bradman,name)



# Comparing all the batting legends
overallperf(lara,name,"green",flag = "TRUE")
overallperf(tendulkar,name,"red")
overallperf(gavaskar,name,"darkmagenta")
overallperf(dravid,name,"green")
overallperf(ponting,name,"blue")
overallperf(bradman,name,"darkgoldenrod1")
legend(x="right",c("Lara","Tendulkar","Gavaskar","Dravid","Ponting","Bradman"), lty=c(1,1,1,1,1,1),   
       lwd=c(2.5,2.5,2.5,2.5,2.5,2.5),col=c("green", "red","darkmagenta","green","blue","darkgoldenrod1")) 

# Comparing Tendulkar, Lara and Ponting
overallperf(lara,name,"green",flag = "TRUE")
overallperf(tendulkar,name,"red")
overallperf(ponting,name,"blue")
overallperf(villiers,name,"black")
legend(x="right",c("Lara","Tendulkar","Ponting","villiers"), lty=c(1,1,1),   
       lwd=c(2.5,2.5,2.5),col=c("green", "red","blue","black")) 

# Comparing Tendulkar, Gavaskar, Dravid
overallperf(tendulkar,name,"green",flag = "TRUE")
overallperf(gavaskar,name,"red")
overallperf(dravid,name,"blue")
legend(x="right",c("Tendulkar","Gavaskar","Dravid"), lty=c(1,1,1),   
       lwd=c(2.5,2.5,2.5),col=c("green", "red","blue")) 

# Plot the moving averages of the players
movingaverage(lara,"Brian Lara")
movingaverage(tendulkar,"Sachin Tendulkar")
movingaverage(gavaskar,"Sunil Gavaskar")
movingaverage(dravid,"Rahul Dravid")
movingaverage(ponting,"Ricky Ponting")
movingaverage(bradman,"Sir Don Bradman")
movingaverage(villiers,"AB De Villiers")
movingaverage(richards,"Vivian Richards")




