
library(dplyr)
library(data.table)
library(reshape2)
library(lubridate)
library(ggplot2)



Conversion_Atv_Data <- read.csv("C:/Users/8410106/Documents/Data/ConversionAndATV.csv")


#Create a real date field
StrDates2 <- Conversion_Atv_Data$BDCAL8
Conversion_Atv_Data$Date <- gsub("X","",StrDates2)
Conversion_Atv_Data$Date1 <-ifelse(nchar(Conversion_Atv_Data$Date)==7, paste("0", Conversion_Atv_Data$Date, sep=""), Conversion_Atv_Data$Date)
Conversion_Atv_Data$Date2 <- as.Date(Conversion_Atv_Data$Date1, "%d%m%Y")



#Filter by the correct products
Conversion_Atv_Data <- Conversion_Atv_Data[Conversion_Atv_Data$PDDESCR %in% c("1 Year Fixed Rate Yearly Interest Bond", "1 Year Fixed Rate Monthly Interest Bond",
 "1 Year Fixed Rate 3.50% Yearly Interest Bond", "1 Year Fixed Rate 3.45% Monthly Interest Bond", "Internet 1 Year Fixed 3.50% Yearly",
"Internet 1 Year Fixed 3.45% Monthly", "1 Year Test Matrix Bond", "1 Year Fixed Rate 2.75% Bond",  "1 Year Fixed Rate 3.50% Bond"),]

#Aggregate application and conversion by date
short.date = strftime(Conversion_Atv_Data$Date2, "%Y-%m")
CharDate = paste(short.date,"01", sep = "-", collapse = NULL)

#This is the volume bit
aggr.stat <- aggregate(Conversion_Atv_Data$Application.Volumes ~ CharDate, FUN = sum)
Conversion_Atv_Data2 <- aggr.stat

#This is the Conversion bit
aggr.stat2 <- aggregate(Conversion_Atv_Data$Converted ~ CharDate, FUN = sum)
Conversion_Atv_Data3 <- aggr.stat2

#This is the ATV bit
aggr.stat3 <- aggregate(Conversion_Atv_Data$ATV ~ CharDate, FUN = mean)
Conversion_Atv_Data4 <- aggr.stat3


#merge
Conversion_Atv_Data5 <- merge(Conversion_Atv_Data2,Conversion_Atv_Data3, by = "CharDate" , all.x=TRUE, all.y=FALSE)
Conversion_Atv_Data6 <- merge(Conversion_Atv_Data5,Conversion_Atv_Data4, by = "CharDate" , all.x=TRUE, all.y=FALSE)

#Replace all the NAs with zero
Conversion_Atv_Data5 <- Conversion_Atv_Data4[complete.cases(Conversion_Atv_Data5),]

write.csv(Conversion_Atv_Data6, file = "C:/Users/8410106/Documents/Data/ConversionData.csv")

#add the % calculation
#Conversion_Atv_Data4 <- transform(Conversion_Atv_Data4, ConversionRate = Conversion_Atv_Data3$Converted / Conversion_Atv_Data3$Application.Volumes)
#Conversion_Atv_Data4$ConversionRate <-  Conversion_Atv_Data3$Converted / Conversion_Atv_Data3$Application.Volumes
