dat1 <- read.table('http://statistics.ats.ucla.edu/stat/examples/chp/p054.txt', sep='\t', h=T)
head(dat1)

pairs(dat1) # this is awesome - quickly shows you the correlation between all the variables

fit <- lm(Y~., data=dat1) # notice the dot means all other features


#the variable elimination method is where you just remove all the shit features, notice that X5 has the lowest t-value - thats bad. The t value and P value usually go in opposite directions.

#updates fit with all the same variables but leaves out variable 5
fit2 <- update(fit, .~. -X5)
summary(fit2)