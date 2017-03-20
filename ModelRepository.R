# Author: Javier Abascal, Jose Alatrista, Matthew Dominguez
# Starting date: 2016-02-08
# Different models to work with a dataset

# Libraries for Modelling...
library(devtools)
library(woe)
library(pROC)         # ROC
library(dplyr)        # Subsetting libraries
library(FactoMineR)   # PCA
library(factoextra)   # PCA
library(e1071)        # SVM
library(ROCR)         # ROCR
library(party)        # Visualiazing Trees


#######################################################################
# Splitting the Data
#######################################################################
set.seed(12345)
trainIndex <- createDataPartition(dataset[,1], p = 0.8, list = FALSE, times = 1)
train_dataset = dataset[trainIndex,]
test_dataset = dataset[-trainIndex,]
# Calls to Model Functions requires to write the specific target_variable and can't be passed by parameter
target_variable = "Ind_DormAsOfScoring"

save(train_dataset, file = "train_dataset.RData")
save(test_dataset, file = "test_dataset.RData")
saveRDS(train_dataset, file = "train_dataset.rds")
saveRDS(test_dataset, file = "test_dataset.rds")


#######################################################################
# Logistic Regression Model RMSE & ROC curve
# Link = "logit" (binomial)
#######################################################################
# Preparing TARGET VARIABLE
train_dataset[, target_variable] = as.integer(train_dataset[, target_variable])

# Generatinig Logistic Regression

# Family - Link types
# binomial	          (link = "logit")
# gaussian	          (link = "identity")
# Gamma             	(link = "inverse")
# inverse.gaussian	  (link = "1/mu^2")
# poisson	            (link = "log")
# quasi	              (link = "identity", variance = "constant")
# quasibinomial	      (link = "logit")
# quasipoisson	      (link = "log")
tic()
set.seed(12345)
model_lm = glm(Ind_DormAsOfScoring ~ . ,data=train_dataset, family = binomial()) #Family binomial
saveRDS(model_lm, file = "model_lm.rds")
save(model_lm, file = "model_lm.RData")
exectime <- toc()
exectime <- exectime$toc - exectime$tic

model_lm.prob = predict(model_lm, type = "response", test_dataset, probability = TRUE, decision.values = TRUE)

# Evaluating test_dataset...
model_lm.prob.rocr <- prediction(model_lm.prob, test_dataset[,target_variable])

# Plot ROC Curve (Area under the Curve)
model_lm.perf_1 = performance(model_lm.prob.rocr, "tpr", "fpr")
plot(model_lm.perf_1, col = 2, lwd = 2)
lines(0:1,0:1, type = "l", col= 1, lwd = 2)
title(main= "ROC Curve")

# ChiSquare Test
ChiSq_Test = anova(model_lm, "ChiSq")
# Summary Model
summary(model_lm)

# Calculating RMSE (may not make sense for binary responses)
performance(model_lm.prob.rocr, "rmse")
error = model_lm.prob - as.numeric(test_dataset[,target_variable])
rmse(error)

# Calculating - Area under the curve
auc = performance(model_lm.prob.rocr, measure ='auc')
print(paste("AUC: ",attr(auc, "y.values")))

# Kolmogorov-Smirnov Lift
perftest<-performance(model_lm.prob.rocr,"tpr","fpr")
ks_lift = max(attr(perftest,'y.values')[[1]]-attr(perftest,'x.values')[[1]])
ks_lift

# Lift
lift = performance(model_lm.prob.rocr, "lift","rpp")
l = as.data.frame(attr(lift, "y.values"))
quantile(l, c(1, .9, .8, .7, .6, .5, .4, .3, .2, .1, 0), na.rm = TRUE)
plot(lift, main = "Lift Curve")

# Gain
gain = performance(model_lm.prob.rocr, "tpr","rpp")
g = as.data.frame(attr(gain, "y.values"))
quantile(g, c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), na.rm = TRUE)
plot(gain, main = "Gain Curve")

# Plot precision/recall curve
model_lm.perf_2 = performance(model_lm.prob.rocr, "prec", "rec")
plot(model_lm.perf_2)

# Plot accuracy as function of threshold
model_lm.perf_3 = performance(model_lm.prob.rocr, "acc")
plot(model_lm.perf_3)


rm(model_lm.perf_1)
rm(model_lm.perf_2)
rm(model_lm.perf_3)
rm(ChiSq_Test)
rm(error)
rm(model_lm.prob.rocr)
rm(model_lm.prob)
rm(g)
rm(l)
rm(lift)
rm(ks_lift)
rm(perftest)


#######################################################################
# GBM Model RMSE & ROC curve
# Distribution = Bernoulli
#######################################################################
# Preparing TARGET VARIABLE
train_dataset[, target_variable] = as.integer(train_dataset[, target_variable])

# Training the Model GBM
tic()
set.seed(12345)
model_gbm = gbm(Ind_DormAsOfScoring ~ . ,data=train_dataset
                ,distribution = "bernoulli",n.trees=500,shrinkage=.05
                ,n.minobsinnode=10,interaction.depth = 5, cv.folds = 5
                ,bag.fraction = 0.5,n.cores=2,keep.data = TRUE
                ,train.fraction = 0.7, verbose = TRUE)
saveRDS(model_gbm, file = "model_gbm.rds")
save(model_gbm, file = "model_gbm.RData")
exectime <- toc()
exectime <- exectime$toc - exectime$tic

model_gbm.prob = predict(model_gbm, type = "response", test_dataset, probability = TRUE, decision.values = TRUE)

# Evaluating test_dataset...
model_gbm.prob.rocr <- prediction(model_gbm.prob, test_dataset[,target_variable])

# Plot ROC Curve (Area under the Curve)
model_gbm.perf_1 = performance(model_gbm.prob.rocr, "tpr", "fpr")
plot(model_gbm.perf_1, col = 2, lwd = 2)
lines(0:1,0:1, type = "l", col= 1, lwd = 2)
title(main= "ROC Curve")

# Summary Model
summary(model_gbm)

# Calculating RMSE (may not make sense for binary responses)
performance(model_gbm.prob.rocr, "rmse")
error = model_gbm.prob - as.numeric(test_dataset[,target_variable])
rmse(error)

# Calculating - Area under the curve
auc = performance(model_gbm.prob.rocr, measure ='auc')
print(paste("AUC: ",attr(auc, "y.values")))

# Kolmogorov-Smirnov Lift
perftest<-performance(model_gbm.prob.rocr,"tpr","fpr")
ks_lift = max(attr(perftest,'y.values')[[1]]-attr(perftest,'x.values')[[1]])
ks_lift

# Lift
lift = performance(model_gbm.prob.rocr, "lift","rpp")
l = as.data.frame(attr(lift, "y.values"))
quantile(l, c(1, .9, .8, .7, .6, .5, .4, .3, .2, .1, 0), na.rm = TRUE)
plot(lift, main = "Lift Curve")

# Gain
gain = performance(model_gbm.prob.rocr, "tpr","rpp")
g = as.data.frame(attr(gain, "y.values"))
quantile(g, c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), na.rm = TRUE)
plot(gain, main = "Gain Curve")

# Plot precision/recall curve
model_gbm.perf_2 = performance(model_gbm.prob.rocr, "prec", "rec")
plot(model_gbm.perf_2)

# Plot accuracy as function of threshold
model_gbm.perf_3 = performance(model_gbm.prob.rocr, "acc")
plot(model_gbm.perf_3)


rm(model_gbm.perf_1)
rm(model_gbm.perf_2)
rm(model_gbm.perf_3)
rm(error)
rm(model_gbm.prob.rocr)
rm(model_gbm.prob)
rm(g)
rm(l)
rm(lift)
rm(ks_lift)
rm(perftest)


#######################################################################
# SVM Model RMSE & ROC curve
# Kernel = linear
#######################################################################
# Preparing TARGET VARIABLE
train_dataset[, target_variable] = as.factor(train_dataset[, target_variable])

# Training the Model SVM
tic()
set.seed(12345)
model_svm = svm(Ind_DormAsOfScoring ~ ., data = train_dataset, kernel = "linear",
                probability = TRUE) # Look below for parameter tunning
saveRDS(model_svm, file = "model_svm.rds")
save(model_svm, file = "model_svm.RData")
exectime <- toc()
exectime <- exectime$toc - exectime$tic

model_svm.prob = predict(model_svm, type = "prob", test_dataset, probability = TRUE, decision.values = TRUE)

# Model Accuracy (0.5 threshold may not be the right choice)
model_svm$accuracies
model_svm$tot.accuracy

# Evaluating test_dataset...
# https://heuristically.wordpress.com/2009/12/23/compare-performance-machine-learning-classifiers-r/
model_svm.prob.rocr <- prediction(attr(model_svm.prob, "probabilities")[,2], test_dataset[,target_variable])

# Plot ROC Curve (Area under the Curve)
model_svm.perf_1 = performance(model_svm.prob.rocr, "tpr", "fpr")
plot(model_svm.perf_1, col = 2, lwd = 2)
lines(0:1,0:1, type = "l", col= 1, lwd = 2)
title(main= "ROC Curve")

# Summary Model
summary(model_svm)

# Calculating RMSE (may not make sense for binary responses)
performance(model_svm.prob.rocr, "rmse")
error = model_svm.prob - as.numeric(test_dataset[,target_variable])
rmse(error)

# Kolmogorov-Smirnov Lift
perftest<-performance(model_svm.prob.rocr,"tpr","fpr")
ks_lift = max(attr(perftest,'y.values')[[1]]-attr(perftest,'x.values')[[1]])
ks_lift

# Lift
lift = performance(model_svm.prob.rocr, "lift","rpp")
l = as.data.frame(attr(lift, "y.values"))
quantile(l, c(1, .9, .8, .7, .6, .5, .4, .3, .2, .1, 0), na.rm = TRUE)
plot(lift, main = "Lift Curve")

# Gain
gain = performance(model_svm.prob.rocr, "tpr","rpp")
g = as.data.frame(attr(gain, "y.values"))
quantile(g, c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), na.rm = TRUE)
plot(gain, main = "Gain Curve")

# Plot precision/recall curve
model_svm.perf_2 = performance(model_svm.prob.rocr, "prec", "rec")
plot(model_svm.perf_2)

# Plot accuracy as function of threshold
model_svm.perf_3 = performance(model_svm.prob.rocr, "acc")
plot(model_svm.perf_3)

rm(model_svm.perf_1)
rm(model_svm.perf_2)
rm(model_svm.perf_3)
rm(error)
rm(model_svm.prob.rocr)
rm(model_svm.prob)
rm(g)
rm(l)
rm(lift)
rm(ks_lift)
rm(perftest)

#######################################################################
# Tunning values for epsilon & cost
#### WARNING ####
#### LONG TIME EXECUTION ####
#### WARNING ####
#######################################################################
tic()
tuneResult <- tune(svm, Ind_DormAsOfScoring ~ .,  data = train_dataset, 
                   ranges = list(epsilon = seq(0,1,0.5), cost = 2^(2:10)))
exectime <- toc()
exectime <- exectime$toc - exectime$tic

print(tuneResult)
plot(tuneResult)


#######################################################################
# RF Model RMSE & ROC curve
#######################################################################
# Preparing TARGET VARIABLE
train_dataset[, target_variable] = as.factor(train_dataset[, target_variable])

# Training the Model SVM
tic()
set.seed(12345)
model_rf <- randomForest(Ind_DormAsOfScoring ~ ., data=train_dataset, ntree = 500,
                         importance=TRUE, proximity=TRUE, replace = TRUE,
                         na.action = na.omit)
saveRDS(model_rf, file = "model_rf.rds")
save(model_rf, file = "model_rf.RData")
exectime <- toc()
exectime <- exectime$toc - exectime$tic

model_rf.prob = predict(model_rf, type = "prob", test_dataset, probability = TRUE, decision.values = TRUE)

# Evaluating test_dataset...
model_rf.prob.rocr <- prediction(model_rf.prob[1:19,2], test_dataset[,target_variable])

# Plot ROC Curve (Area under the Curve)
model_rf.perf_1 = performance(model_rf.prob.rocr, "tpr", "fpr")
plot(model_rf.perf_1, col = 2, lwd = 2)
lines(0:1,0:1, type = "l", col= 1, lwd = 2)
title(main= "ROC Curve")

# Kolmogorov-Smirnov Lift
perftest<-performance(model_rf.prob.rocr,"tpr","fpr")
ks_lift = max(attr(perftest,'y.values')[[1]]-attr(perftest,'x.values')[[1]])
ks_lift

# Lift
lift = performance(model_rf.prob.rocr, "lift","rpp")
l = as.data.frame(attr(lift, "y.values"))
quantile(l, c(1, .9, .8, .7, .6, .5, .4, .3, .2, .1, 0), na.rm = TRUE)
plot(lift, main = "Lift Curve")

# Gain
gain = performance(model_rf.prob.rocr, "tpr","rpp")
g = as.data.frame(attr(gain, "y.values"))
quantile(g, c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), na.rm = TRUE)
plot(gain, main = "Gain Curve")

# Summary Model
summary(model_rf)
print(model_rf)

# Calculating RMSE (may not make sense for binary responses)
performance(model_rf.prob.rocr, "rmse")
error = model_rf.prob - as.numeric(test_dataset[,target_variable])
rmse(error)

# Plot precision/recall curve
model_rf.perf_2 = performance(model_rf.prob.rocr, "prec", "rec")
plot(model_rf.perf_2)

# Plot accuracy as function of threshold
model_rf.perf_3 = performance(model_rf.prob.rocr, "acc")
plot(model_rf.perf_3)

rm(model_rf.perf_1)
rm(model_rf.perf_2)
rm(model_rf.perf_3)
rm(error)
rm(model_rf.prob.rocr)
rm(model_rf.prob)
rm(g)
rm(l)
rm(lift)
rm(ks_lift)
rm(perftest)







