#Data analysis of Anne Arundel Community College 5K Turkey Trot
#RNovember 27 2019
setwd("~/Desktop/RProgramming")
install.packages(c("dplyr", "tidyr", "readr", "readxl"))
library(readr)
library(dplyr)
library(tidyverse)
#Read ACC csv file
AACC<-read.csv("ACC.csv")
#Inspect ACC data
tibble(ACC)
#Reaveal typeof data 
typeof(ACC)
#Convert to character
ACC.char<-as.character(ACC[,1])
print(ACC.char)
#Extract time with a regular expression
times<-str_extract(ACC.char, "[0-9][0-9]:[0-9][0-9].[0-9]")
print(times)
#Display number of finishers
length(times)
#The following lines convert **:**.* fomratted times to seconds
mins<-as.numeric(substr(times[1:length(times)],1,2))
secs<-as.numeric(substr(times[1:length(times)],4,5))
tenths<-as.numeric(substr(times[1:length(times)],7,7))/10
time.in.secs<-60*mins+secs+tenths
print(time.in.secs)
#Display 5 number summary
summary(time.in.secs)
#Display Histogram of times in seconds
hist(time.in.secs, main="AACC 5K 11/27/19 Turkey Trot Times",xlim = c(1000,4100), 
     xlab="Seconds (bin width = 60 secs)",
     ylab = "Number of Runners (119 Total)",
     breaks=c(seq(1000,4100,60)), 
     col = c("blue", "blue","blue", "red","blue",
             "blue","blue","blue",
             "blue","blue","blue",
             "blue","blue","blue",
             "blue","blue","blue",
             "blue","blue","blue",
             "blue","blue","blue",
             "blue","blue","blue",
             "blue","blue"))
#Sort times ascending 
time.in.secs<-time.in.secs[order(time.in.secs)]
#A funtion to convert time in seconds to **:**.* format
time.format <- function(time.in){
  m<-time.in %/% 60
  s<-round(time.in-60*m, digits = 1)
  time.out<-paste(as.character(m),":",as.character(s))
  return(time.out)
}
#Use the function to convert and display time in **:**.* format
print(time.format(time.in.secs)) 
#Convert 5 number summary to **:**.* format
time.format(c(   1034  ,  1523   , 1798   , 1876  ,  2170   , 4001 ))
#Generate conversions between time and **:**.* format
conv<-seq(1000, 4000  , 60)
temp<-time.format(seq(1000, 4000  , 60))
names(conv)<-temp
print(conv)
#Generate all data in seconds and **:**.* format
temp<-time.format(time.in.secs)
names(time.in.secs)<-temp
print(time.in.secs)
#End of file
