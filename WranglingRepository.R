# Author: Nick Root

#######################################################################
# Installing & loading librarys
####################################################################### 

install.packages("clustergram")
library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(lubridate)
library(ggplot2)


#######################################################################
# The basics
####################################################################### 


library(reshape2)
View(iris)
head(iris)

  
#######################################################################
# Reshaping dataframes
####################################################################### 

# Wide to long (using Reshape 2)
BM_Inflows_Long <- melt(BM_Inflows, id.vars=c("X"))


#######################################################################
# Importing & Exporting a dataset
####################################################################### 
Price1 <- read.csv("C:/Users/8410106/Documents/Data/MoneyFacts 2007-Present (Merged).csv",header=TRUE,dec=".",sep=",",strip.white=TRUE, na.strings="?")
write.table(dataset, file= "dormPcaActvActv_Train90days_29022016_MODEL.txt", sep = ",", col.names = TRUE)


#######################################################################
# Subsetting/Filtering dataframes
####################################################################### 

# Filtering by 2 fields (&)
Price2 <- Price1[Price1$Prodlvl1=="Fixed"&Price1$Prodlvl2=="12m",] 

# Filtering by 2 fields (or)
Price4 <- Price3[Company.Name=="Birmingham Midshires" | Company.Name=="BM Savings",] 


# Filtering a single field with some logic
dataset = dataset[which(dataset$intervalBtwnMand1506End_Mo >= 12), ]


# Delete a single column
Price2$Aermin <- NULL

# Delete columns by numerical reference
Price5[, (1:32) := NULL]


# Define and remove columns from a dataset
col_names = c('intervalBtwnVisit_DayP7509'
              , 'intervalBtwnVisit_DayP7506'
              , 'intervalBtwnVisit_DayP7503'
              , 'd_ct_DbtTxnMoLLg_PrevMo'
              , 'd_ct_CrTxnMoLLg_PrevMo'
              , 'dp_ct_CrTxnMoLLg_Prev3MoAvg'
              , 'dp_ct_DbtTxnMoLLg_Prev3MoAvg'
              , 'ageBand.1')
dataset = dataset[ , -which(names(dataset) %in% col_names )]


# Group by with mean.. but can be changed for max, min etc
P5melted <- melt(Price5, id = "YrMoChar")
Price6 <- dcast(P5melted, YrMoChar ~ variable, mean)


#######################################################################
## NULL  &  NA  & FACTORS Threatment
#######################################################################
# Cleanning Columns
# Removing rows with NA Values
train_dataset = train_dataset[complete.cases(train_dataset),]
# remove all rows with non-finite values
train_dataset = train_dataset[!rowSums(!is.finite(as.matrix(train_dataset))), ]
# Removing columns with NA, NaN or Inf values
train_dataset = train_dataset[,sapply(train_dataset, function(x) !any(!is.finite(x)))]
# Factors columns to numeric...
index <- sapply(train_dataset, is.factor)
train_dataset[index] <- lapply(train_dataset[index], function(x) as.numeric(as.character(x)))
# Removing Columns with Factor
train_dataset = train_dataset[,!sapply(train_dataset, function(x) is.factor(x))]

# Modifying NULL to a value in some columns
dataset <- dataset %>% mutate(dp_ct_VstsMoLastLg_PrevMo = ifelse(is.na(dp_ct_VstsMoLastLg_PrevMo),200,dp_ct_VstsMoLastLg_PrevMo))
dataset <- dataset %>% mutate(dp_ct_LgsMoLastLg_PrevMo = ifelse(is.na(dp_ct_LgsMoLastLg_PrevMo),200,dp_ct_LgsMoLastLg_PrevMo))

# Removing Rows With NULL values 
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

dataset = completeFun(dataset, desired_columns_to_remove_nulls)
desired_columns_to_remove_nulls = c('p_ct_LgsDevDskOLogonsTot12',
                                    'p_ct_LgsDevMobOLogonsTot12',)


#Make N/A's into a factor (they might be helpful as a model input)
FinalTable3$BestBuyRank[is.na(FinalTable3$BestBuyRank)]<-"IWasNA"
FinalTable3$BestBuyRank <- as.factor(FinalTable3$BestBuyRank)


#######################################################################
# Manipulating date formats
#######################################################################

# Standard date format
MasterDate$Date1 <- as.Date(MasterDate$Date, "%d/%m/%Y" )
#create character date fields
MasterDate$Date2 <- as.character(MasterDate$Date1)
short.date = strftime(MasterDate$Date1, "%Y/%m")
MasterDate$ChrDate2 <- short.date
#create some string dates and months for model features
MasterDate$ChrMonth <- substr(MasterDate$ChrDate2,6,7)
MasterDate$ChrYear <- substr(MasterDate$ChrDate2,1,4)


#Create a new column that merges the two date fields and also create a proper date field that R recognises as a date
Price2$YrMo<- paste(Price2$Load.Year,"-",Price2$Load.Month,"-","01", sep="")
strDates <- Price2$YrMo
Price2$YrMoDate <- as.Date(strDates, "%Y-%m-%d")
#But we will need to merge on a character not a date, so lets create a character field
Price2$YrMoChar <- as.character(Price2$YrMoDate)


# Auto create field of dates
install.packages("year")

days=365*2;
date = seq(as.Date("2000-01-01"),length=days,by="day")
year = year(date)
month = month(date)


#######################################################################
# Leading & Lagging fields
#######################################################################


FinalTable3$QuarterOfpackingFigs<- (FinalTable3$NewMoneyFlow * 0.25)
FinalTable3$ThreeQuartersOfPackingFigs <- (FinalTable3$NewMoneyFlow * 0.75)
FinalTable3$PackingFiguresLagged <- (FinalTable3$ThreeQuartersOfPackingFigs + shift(FinalTable3$QuarterOfpackingFigs, n=1L, fill=NA, type=c("lead"), give.names=FALSE))



#######################################################################
# Writing a list as a dataframe (for exporting as a CSV)
#######################################################################
xx <- lapply(x, unlist)
max <- max(sapply(xx, length))
final_dataframe = do.call(rbind, lapply(xx, function(z)c(z, rep(NA, max-length(z)))))

head(final_dataframe)



#######################################################################
# Creating extra columns/features
#######################################################################

#Produce an extra column with the horizontal max value of other specified columns
n<-dim(Price2)[1]
for (i in 1:n){
  Price2$Aermax2[i] <- max(Price2[i,12:21])
}

# The same but for the min value
n<-dim(Price2)[1]
for (i in 1:n){
  Price2$Aermin[i] <- min(Price2[i,12:22],na.rm = TRUE)
}


#######################################################################
# Merging tables
#######################################################################


names(Price6)<-c("Date2","Aermax2", "TieredPrice", "monthYrRank")
FinalTable1 <- merge(MasterDate,Price6, by = "Date2" , all.x=TRUE, all.y=FALSE) 
FinalTable2 <- merge(FinalTable1,PackingData3, by = "Date2" , all.x=TRUE, all.y=FALSE)
FinalTable3 <- merge(FinalTable2,BM_NewMoneyInflows_Long, by = "Date2" , all.x=TRUE, all.y=FALSE)


#######################################################################
# Some flashy stuff
#######################################################################


# Quicky check correlation with clusters
pairs(iris)











