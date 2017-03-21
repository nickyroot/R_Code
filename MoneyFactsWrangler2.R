#GET THE LIBRARYS I NEED
library(dplyr)
library(data.table)
install.packages("reshape2")
library(reshape2)

###IMPORT MONEYFACTS DATA
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

###END MONEYFACTS DATA IMPORT


###IMPORT PACKING DATA

PackingData <- read.csv("C:/Users/8410106/Documents/Data/PackingData.csv")
PackingData_Long <- melt(PackingData, id.vars=c("Row.Labels"))

#Create a proper date field
StrDates2 <- PackingData_Long$variable
PackingData_Long$Date <- gsub("X","",StrDates2)
PackingData_Long$Date2 <-ifelse(nchar(PackingData_Long$Date)==7, paste("0", PackingData_Long$Date, sep=""), PackingData_Long$Date)
PackingData_Long$Date3 <- as.Date(PackingData_Long$Date2, "%d%M%Y")
