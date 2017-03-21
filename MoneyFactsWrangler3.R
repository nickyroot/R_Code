#GET THE LIBRARYS I NEED

install.packages("reshape2")

library(dplyr)
library(data.table)
library(reshape2)
library(lubridate)

#IMPORT THE MASTER DATE FILE AND CREATE MULTIPLE VERSONS OF DATE

MasterDate <- read.csv("C:/Users/8410106/Documents/Data/MasterDate.csv")
MasterDate$Date2 <- as.Date(MasterDate$Date, "%d/%m/%Y" )
#create abbreviated date
short.date = strftime(MasterDate$Date2, "%Y/%m")
MasterDate$ChrDate <- short.date
#create some string dates and months for model features
MasterDate$ChrMonth <- substr(MasterDate$ChrDate,6,7)
MasterDate$ChrYear <- substr(MasterDate$ChrDate,1,4)


###IMPORT MONEYFACTS DATA
#NOTE IF WE LIMIT IT TO 2014 AND 2015 WE ARE FINE, BUT THERE ARE GAPS PRE THEN
Price1 <- read.csv("C:/Users/8410106/Documents/Data/MoneyFacts 2007-Present (Merged).csv",header=TRUE,dec=".",sep=",",strip.white=TRUE, na.strings="?")

#Create a smaller table with only the rows we are interested in
Price2 <- Price1[Price1$Prodlvl1=="Fixed"&Price1$Prodlvl2=="12m",] 

#Create a new column that merges the two date fields and also create a proper date field that R recognises as a date
Price2$YrMo<- paste(Price2$Load.Year,"-",Price2$Load.Month,"-","01", sep="")
#strDates <- Price2$YrMo
#Price2$YrMoDate <- as.Date(strDates, "%Y-%M-%D")

#Produce our own Aermax column as we know the max in the data is not complete
n<-dim(Price2)[1]
for (i in 1:n){
Price2$Aermax2[i] <- max(Price2[i,12:21])
}

#produce rank by month-yr
Price3 <- as.data.table(Price2)
Price3[,monthYrRank:=rank(-Aermax2,ties.method="first"),by=YrMo]

#Get rid of multiple labels for BM
Price3[Price3=="BM Savings"]<-"Birmingham Midshires"

#Now we need to focus on BM 
Price4 <- Price3[Company.Name=="Birmingham Midshires",] 

#now we need to group up the Prodlvl3 column - we only want Online TD as branch is really old business
Price5 <- Price4[Price4$Prodlvl3=="Online TD",]

#Table is too big, lets take out some columns
Price5[, (1:30) := NULL]

###END MONEYFACTS DATA IMPORT


###IMPORT PACKING DATA

PackingData <- read.csv("C:/Users/8410106/Documents/Data/BMPackingData.csv")

#Create a proper date field
StrDates2 <- PackingData$BDCAL8
PackingData$Date <- gsub("X","",StrDates2)
PackingData$Date2 <-ifelse(nchar(PackingData$Date)==7, paste("0", PackingData$Date, sep=""), PackingData$Date)
PackingData$Date3 <- as.Date(PackingData$Date2, "%d%m%Y")

#still need to aggregate by month and filter by desired product
PackingData2 <- PackingData[PackingData$PDDESCR=="1 Year Fixed Rate Yearly Interest Bond" | PackingData$PDDESCR=="1 Year Fixed Rate Monthly Interest Bond",]

# aggregate packing data by date
short.date = strftime(PackingData2$Date3, "%Y/%m")
aggr.stat = aggregate(PackingData2$PRODUCTC05 ~ short.date, FUN = sum)
PackingData3 <- aggr.stat

###END PACKING DATA IMPORT


#MERGE ALL DATASETS INTO A SINGLE TABLE - not working yet

Merge(MasterDate,Price5, by.MasterDate ="Date2" ,byPrice5 = "YrMo" , all.MasterDate=TRUE, all.Price5=FALSE) 





