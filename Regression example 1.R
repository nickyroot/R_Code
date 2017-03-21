
#######################################################################
# Linear Regression
#######################################################################

install.packages("ggplot2", dependencies = TRUE)

library(ggplot)
download.file("http://bit.ly/PaceData", destfile="p.txt",method="curl", extra="-L")
dat<- read.csv("p.txt", header=T)
head(dat)

ggplot(dat, aes(y=speed, x=pop)) + geom_point()

linear.fit <- lm(speed ~ pop, data=dat)
summary(linear.fit)

P1 <- ggplot(dat, aes(y=speed, x=pop)) + geom_point() + scale_y_continuous(limits=c(0,3)) + geom_abline(intercept = coef(linear.fit)[1], slope = coef(linear.fit)[2], color='red')

P1

log.fit <- lm(log(speed) ~ log(pop), data=dat)
summary(log.fit)

p2 <- ggplot(dat, aes(y=log(speed), x=log(pop))) + geom_point() + geom_abline(intercept = coef(log.fit)[1], slope = coef(log.fit)[2], color='red')
p2           
             
             
            