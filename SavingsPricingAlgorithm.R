#GET THE LIBRARYS I NEED

install.packages("reshape2")
install.packages("ggplot2")

library(dplyr)
library(data.table)
library(reshape2)
library(lubridate)
library(ggplot2)

#IMPORT THE MASTER DATE FILE AND CREATE MULTIPLE VERSONS OF DATE

MasterDate <- read.csv("C:/Users/8410106/Documents/Data/MasterDate.csv")
MasterDate$Date1 <- as.Date(MasterDate$Date, "%d/%m/%Y" )
#create character date fields
MasterDate$Date2 <- as.character(MasterDate$Date1)
short.date = strftime(MasterDate$Date1, "%Y/%m")
MasterDate$ChrDate2 <- short.date
#create some string dates and months for model features
MasterDate$ChrMonth <- substr(MasterDate$ChrDate2,6,7)
MasterDate$ChrYear <- substr(MasterDate$ChrDate2,1,4)


###IMPORT MONEYFACTS DATA
#NOTE IF WE LIMIT IT TO 2014 AND 2015 WE ARE FINE, BUT THERE ARE GAPS PRE THEN
Price1 <- read.csv("C:/Users/8410106/Documents/Data/MoneyFacts 2007-Present (Merged).csv",header=TRUE,dec=".",sep=",",strip.white=TRUE, na.strings="?")

#Create a smaller table with only the rows we are interested in
Price2 <- Price1[Price1$Prodlvl1=="Fixed"&Price1$Prodlvl2=="12m",] 

#Create a new column that merges the two date fields and also create a proper date field that R recognises as a date
Price2$YrMo<- paste(Price2$Load.Year,"-",Price2$Load.Month,"-","01", sep="")
strDates <- Price2$YrMo
Price2$YrMoDate <- as.Date(strDates, "%Y-%m-%d")
#But we will need to merge on a character not a date, so lets create a character field
Price2$YrMoChar <- as.character(Price2$YrMoDate)

#Produce our own Aermax column as we know the max in the data is not complete
n<-dim(Price2)[1]
for (i in 1:n){
Price2$Aermax2[i] <- max(Price2[i,12:21])
}

#Produce our own AerMIN column so we can work out if there is a tiered pricing system
#convert the zeros in the price sextion to NA so the min function ignores zeros
Price2[, 12:22][Price2[, 12:22] == 0] <- NA

n<-dim(Price2)[1]
for (i in 1:n){
Price2$Aermin[i] <- min(Price2[i,12:22],na.rm = TRUE)
}
#Produce tiered indicator
Price2$TieredPrice <- ifelse(Price2$Aermin == Price2$Aermax2, 0, 1)

#We dont need the Aermin column anymore
Price2$Aermin <- NULL

#produce rank by month-yr
Price3 <- as.data.table(Price2)
Price3[,monthYrRank:=rank(-Aermax2,ties.method="max"),by=YrMo]

#Now we need to focus on BM 
Price4 <- Price3[Company.Name=="Birmingham Midshires" | Company.Name=="BM Savings",] 

#now we need to group up the Prodlvl3 column - we only want Online TD as branch is really old business
Price5 <- Price4[Price4$Prodlvl3=="Online TD",]

#Table is too big, lets take out some columns
Price5[, (1:32) := NULL]

#Remove the duplicate months (where they changed price mid month) by averaging the rows
P5melted <- melt(Price5, id = "YrMoChar")
Price6 <- dcast(P5melted, YrMoChar ~ variable, mean)

###END MONEYFACTS DATA IMPORT


###IMPORT PACKING DATA

PackingData <- read.csv("C:/Users/8410106/Documents/Data/BMPackingData_updated.csv")

#Create a proper date field
StrDates2 <- PackingData$BDCAL8
PackingData$Date <- gsub("X","",StrDates2)
PackingData$Date1 <-ifelse(nchar(PackingData$Date)==7, paste("0", PackingData$Date, sep=""), PackingData$Date)
PackingData$Date2 <- as.Date(PackingData$Date1, "%d%m%Y")

#still need to aggregate by month and filter by desired product
#PackingData2 <- PackingData[PackingData$PDDESCR=="1 Year Fixed Rate Yearly Interest Bond" | PackingData$PDDESCR=="1 Year Fixed Rate Monthly Interest Bond" | PackingData$PDDESCR=="1 Year Fixed Rate 3.50% Yearly Interest Bond" | PackingData$PDDESCR=="1 Year Fixed Rate 3.45% Monthly Interest Bond"
# | PackingData$PDDESCR=="Internet 1 Year Fixed 3.50% Yearly" | PackingData$PDDESCR=="Internet 1 Year Fixed 3.45% Monthly" | 
# PackingData$PDDESCR=="1 Year Test Matrix Bond" | PackingData$PDDESCR=="1 Year Fixed Rate 2.75% Bond" | "1 Year Fixed Rate 3.50% Bond",]


PackingData2 <- PackingData[PackingData$PDDESCR %in% c("1 Year Fixed Rate Yearly Interest Bond", "1 Year Fixed Rate Monthly Interest Bond",
 "1 Year Fixed Rate 3.50% Yearly Interest Bond", "1 Year Fixed Rate 3.45% Monthly Interest Bond", "Internet 1 Year Fixed 3.50% Yearly",
"Internet 1 Year Fixed 3.45% Monthly", "1 Year Test Matrix Bond", "1 Year Fixed Rate 2.75% Bond",  "1 Year Fixed Rate 3.50% Bond"),]



# aggregate packing data by date
short.date = strftime(PackingData2$Date2, "%Y-%m")
CharDate = paste(short.date,"01", sep = "-", collapse = NULL)
aggr.stat = aggregate(PackingData2$PRODUCTC05 ~ CharDate, FUN = sum)
PackingData3 <- aggr.stat
names(PackingData3)<-c("Date2","PackingFigures")

###END PACKING DATA IMPORT

###IMPORT FLOW DATA

BM_NewMoneyInflows <- read.csv("C:/Users/8410106/Documents/Data/BM_NewMoneyFlow.csv")
BM_NewMoneyInflows_Long <- melt(BM_NewMoneyInflows, id.vars=c("X"))
BM_NewMoneyInflows_Long$Date1 <- as.Date(paste("01",BM_NewMoneyInflows_Long$variable,sep="."),"%d.%b.%y")
BM_NewMoneyInflows_Long$Date2 <- as.character(BM_NewMoneyInflows_Long$Date1)
#filter 1 year fixed rate
BM_NewMoneyInflows_Long <- BM_NewMoneyInflows_Long[BM_NewMoneyInflows_Long$X=="1 Year Fixed Rate",]

### END IMPORT FLOW DATA


#MERGE ALL DATASETS INTO A SINGLE TABLE 

names(Price6)<-c("Date2","Aermax2", "TieredPrice", "monthYrRank")
FinalTable1 <- merge(MasterDate,Price6, by = "Date2" , all.x=TRUE, all.y=FALSE) 
#table(MasterDate$Date2==Price6$Date2)
FinalTable2 <- merge(FinalTable1,PackingData3, by = "Date2" , all.x=TRUE, all.y=FALSE)
FinalTable3 <- merge(FinalTable2,BM_NewMoneyInflows_Long, by = "Date2" , all.x=TRUE, all.y=FALSE)

#Lets clean the table up by removing some unwanted columns
FinalTable3$X <- NULL
FinalTable3$variable <- NULL
FinalTable3$Date <- NULL
FinalTable3$Date1.x <- NULL
FinalTable3$Date1.y <- NULL
FinalTable3$ChrDate2 <- NULL
names(FinalTable3)<-c("Date2","ChrMonth","ChrYear","Max Price", "TieredPrice", "BestBuyRank", "PackingFigures", "NewMoneyFlow" ) #BE CAREFUL HERE IF YOU ADD COLUMNS







#Remove non-sequential rows as no good for time lagging - CAREFUL HERE TOP 2 ROWS WONT ALWAYS BE NON SEQUENTIAL
FinalTable3 <- FinalTable3[-c(1, 2), ]


#Lets lag the packing figures

FinalTable3$QuarterOfpackingFigs<- (FinalTable3$NewMoneyFlow * 0.25)
FinalTable3$ThreeQuartersOfPackingFigs <- (FinalTable3$NewMoneyFlow * 0.75)
FinalTable3$PackingFiguresLagged <- (FinalTable3$ThreeQuartersOfPackingFigs + shift(FinalTable3$QuarterOfpackingFigs, n=1L, fill=NA, type=c("lead"), give.names=FALSE))



#lets lag the flow figures

FinalTable3$HalfOfNMFlow <- (FinalTable3$NewMoneyFlow * 0.5)
FinalTable3$QuarterOfNMFlow <- (FinalTable3$NewMoneyFlow * 0.25)
FinalTable3$ThreeQuartersOfNMFlow <- (FinalTable3$NewMoneyFlow * 0.75)
FinalTable3$NewMoneyLagged <- (FinalTable3$ThreeQuartersOfNMFlow + shift(FinalTable3$QuarterOfNMFlow, n=1L, fill=NA, type=c("lead"), give.names=FALSE))






#Try making the rank a factor to capture the NA's, (remember you need to not exclude the NA's later on)
FinalTable3$BestBuyRank[is.na(FinalTable3$BestBuyRank)]<-"IWasNA"
FinalTable3$BestBuyRank <- as.factor(FinalTable3$BestBuyRank)


# Make Tiered Price a Factor in the same way
FinalTable3$TieredPrice[is.na(FinalTable3$TieredPrice)]<-"IWasNA"
FinalTable3$TieredPrice <- as.factor(FinalTable3$TieredPrice)

#Remove ALL NA's
#FinalTable3 <- FinalTable3[complete.cases(FinalTable3),]

#Remove just the NAs in the NewMoneyLagged column
FinalTable3 <- FinalTable3[!is.na(FinalTable3$NewMoneyLagged),]

#


#str(FinalTable3)
View(FinalTable3)




######## LINEAR MODDELLING #########



#MODELS FOR THE PACKING


#Create the linear model of the packing figures
linear.fit <- lm(PackingFigures ~ BestBuyRank, FinalTable3)
summary(linear.fit)
P1 <- ggplot(FinalTable3, aes(y=PackingFigures, x=BestBuyRank)) + geom_point() + geom_abline(intercept = coef(linear.fit)[1], slope = coef(linear.fit)[2], color='red')



#It doesnt look linear, lets create a log version of the model
log.fit <- lm(log(PackingFigures) ~ log(BestBuyRank), FinalTable3)
summary(log.fit)
P2 <- ggplot(FinalTable3, aes(y=log(PackingFigures), x=log(BestBuyRank))) + geom_point() + geom_abline(intercept = coef(log.fit)[1], slope = coef(log.fit)[2], color='red')

log.fit3 <- lm(log(PackingFiguresLagged) ~ log(BestBuyRank), FinalTable3)
summary(log.fit3)
P4 <- ggplot(FinalTable3, aes(y=log(PackingFiguresLagged), x=log(BestBuyRank))) + geom_point() + geom_abline(intercept = coef(log.fit3)[1], slope = coef(log.fit3)[2], color='red')




#MODELS FOR THE FLOWS

#what about the flows
linear.fit2 <- lm(NewMoneyFlow ~ BestBuyRank, data = FinalTable3)
summary(linear.fit2)
P4 <- ggplot(FinalTable3, aes(y=NewMoneyFlow, x=BestBuyRank)) + geom_point() + geom_abline(intercept = coef(linear.fit2)[1], slope = coef(linear.fit2)[2], color='red')


#log model for the FLOWS
log.fit2 <- lm(log(NewMoneyFlow) ~ log(BestBuyRank), FinalTable3)
summary(log.fit2)
P5 <- ggplot(FinalTable3, aes(y=log(NewMoneyFlow), x=log(BestBuyRank))) + geom_point() + geom_abline(intercept = coef(log.fit2)[1], slope = coef(log.fit2)[2], color='red')
P5


#log model for the LAGGED flows
log.fit4 <- lm(log(NewMoneyLagged) ~ log(BestBuyRank), data = FinalTable3)
summary(log.fit4)

#USE THIS MODEL
log.fit4 <- lm(log(FinalTable3$NewMoneyLagged +1) ~ FinalTable3$BestBuyRank + FinalTable3$TieredPrice)
summary(log.fit4)
P3 <- ggplot(FinalTable3, aes(y=log(NewMoneyLagged), x=log(FinalTable3$BestBuyRank))) + geom_point() + geom_abline(intercept = coef(log.fit4)[1], slope = coef(log.fit4)[2], color='red')
P3

#Much better!


write.csv(FinalTable3, file = "C:/Users/8410106/Documents/Data/FinalTable3.csv")


