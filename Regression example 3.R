rm(list=ls()) # this cleans you enviroment.. get rid of all the earlier variables (rm means remove)
dat3 <- read.table('http://www.ats.ucla.edu/stat/examples/chp/p081.txt', sep='\t', h=T)
head(dat3)


summary(dat3)
dat3$State <- NULL

fit <- lm(Sales~., data=dat3) 
summary(fit)


  fit2 <- update(fit, .~. -Black -Female -HS -Age)
  summary(fit2)
  
  # now they are all statisically significant
  
