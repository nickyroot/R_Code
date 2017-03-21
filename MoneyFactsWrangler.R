library(dplyr)

#import
Price1 <- read.csv("C:/Users/8410106/Documents/Data/MoneyFacts 2007-Present (Merged).csv",header=TRUE,dec=".",sep=",",strip.white=TRUE, na.strings="?")

#Create a smaller table with only the rows we are interested in
Price2 <- Price1[Prodlvl1=="Fixed"&Prodlvl2=="12m",] 

#Create a new column that merges the two date fields
Price2$YrMo<- paste(Price2$Load.Year,"-",Price2$Load.Month, sep="")

#Ensure the Aermax column is numerical
Price2$Aermax_num<-as.numeric(levels(Price2$Aermax))[Price2$Aermax]

tes<-ddply(Price2,.(YrMo),transform,CompanyRank=max(Aermax))

#Produce our own Aermax column as we know the max in the data is not complete
n<-dim(Price2)[1]
for (i in 1:n){
Price2$Aermax2[i] <- max(Price2[i,12:21])
}

#produce rank by month-yr
library(data.table)
Price3 <- as.data.table(Price2)
Price3[,monthYrRank:=rank(-Aermax2,ties.method="first"),by=YrMo]

#Get rid of multiple labels for BM
Price3[Price3=="BM Savings"]<-"Birmingham Midshires"

#Now we need to focus on BM 
Price4 <- Price3[Company.Name=="Birmingham Midshires",] 

#now we need to group up the Prodlvl3 column

