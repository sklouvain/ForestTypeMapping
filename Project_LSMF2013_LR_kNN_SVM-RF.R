#Report
#LSMF2013 Quantitative Data Analysis (UCLouvain)
#Project: Forset Type Mapping
#Authors: S. Kurin, L. Parmentier, and M. Goudjil
#Supervisor: Professor M. Saerens (UCLouvain)

#Feature Engineering: PCA, LDA
#Classification Models: Logistic Regression, k-NN, SVM-RBF, Random Forest


#Packages 
library(ISLR)
library(leaps)
library(gam)
library(fmsb)
library(class)
library(e1071)
library(caret)
library(rafalib)
library(rpart)
library(ROSE)
library(rgl)
library(MASS)
library(PerformanceAnalytics)
library(nnet)
library(outliers)
library(randomForest)


#Datasets

Auto=read.csv("C:/Users/serge_000/Desktop/DD/DD Spring/ADQ/ForestTypes/training.csv", sep=",",dec = ".",header=TRUE)
str(Auto)
Auto1=read.csv("C:/Users/serge_000/Desktop/DD/DD Spring/ADQ/ForestTypes/testing.csv", sep=",",dec = ".",header=TRUE)

names(Auto)
names(Auto1)

dim(Auto)
dim(Auto1)

summary(Auto)
summary(Auto1)
typeof(Auto$class)

Auto = rbind(Auto, Auto1)

summary(Auto)

Auto = Auto[sample(nrow(Auto)),] ###shuffle data
Auto

names(Auto)
dim(Auto)
summary(Auto)

boxplot(Auto[,2:28])
chart.Correlation(Auto[,2:10])
#cor(Auto [,-1]) #correlation between features

######################Dealing with outliers (for LDA)

for (i in 2:28){
  outlier = boxplot.stats(Auto[,i])$out
  print(outlier)
}


outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  #par(mfrow=c(2, 2), oma=c(0,0,3,0))
  #boxplot(var_name, main="With outliers")
  #hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, m1, var_name)
  #boxplot(var_name, main="Without outliers")
  #hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  #title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  m2 <- mean(var_name, na.rm = T)
  dt[as.character(substitute(var))] <- invisible(var_name)
  assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
  return(invisible(dt))
}

outlierKD(Auto, b1)
outlierKD(Auto, b2)
outlierKD(Auto, b3)
outlierKD(Auto, b4)
outlierKD(Auto, b5)
outlierKD(Auto, b6)
outlierKD(Auto, b7)
outlierKD(Auto, b8)
outlierKD(Auto, b9)
outlierKD(Auto, pred_minus_obs_H_b1)
outlierKD(Auto, pred_minus_obs_H_b2)
outlierKD(Auto, pred_minus_obs_H_b3)
outlierKD(Auto, pred_minus_obs_H_b4)
outlierKD(Auto, pred_minus_obs_H_b5)
outlierKD(Auto, pred_minus_obs_H_b6)
outlierKD(Auto, pred_minus_obs_H_b7)
outlierKD(Auto, pred_minus_obs_H_b8)
outlierKD(Auto, pred_minus_obs_H_b9)
outlierKD(Auto, pred_minus_obs_S_b1)
outlierKD(Auto, pred_minus_obs_S_b2)
outlierKD(Auto, pred_minus_obs_S_b3)
outlierKD(Auto, pred_minus_obs_S_b4)
outlierKD(Auto, pred_minus_obs_S_b5)
outlierKD(Auto, pred_minus_obs_S_b6)
outlierKD(Auto, pred_minus_obs_S_b7)
outlierKD(Auto, pred_minus_obs_S_b8)
outlierKD(Auto, pred_minus_obs_S_b9)

boxplot(Auto[,2:28])

################################################

#Scaling & Centering
Auto[,2:28] = scale(x=Auto[,2:28], center=TRUE, scale=TRUE)

#Accuracy

acc=function(CM)   ### classification accuracy
{
  diag=0
  for(d in 1:4){
    diag = diag + CM[d,d]
  }
  diag
  length(test.Y)
  error = (length(test.Y) - diag)/length(test.Y)
  accuracy = 1 - error
  accuracy
}

###########################folds for CV###########
set.seed(1)
idx=createFolds(Auto$class, k=10)
idx
sapply(idx, length)

#Auto[-idx[[1]], ] 

#head(train.X[idx[[1]], 1:3])

sapply(idx, function(i) table(Auto$class[i]))

Auto$class <- as.factor(Auto$class)

###################Stepwise Logistic Regression####################

Auto.initial = multinom(Auto$class ~ 1, data=Auto)#!!!!!!!

stepwise = stepAIC(Auto.initial, scope=Auto$class ~ Auto$b1 + Auto$b2 + Auto$b3 + Auto$b4 + Auto$b5 + 
                     Auto$b6 + Auto$b7 + Auto$b8 + Auto$b9 + Auto$pred_minus_obs_H_b1 + Auto$pred_minus_obs_H_b2 + 
                     Auto$pred_minus_obs_H_b3 + Auto$pred_minus_obs_H_b4 + Auto$pred_minus_obs_H_b5 + 
                     Auto$pred_minus_obs_H_b6 + Auto$pred_minus_obs_H_b7 + Auto$pred_minus_obs_H_b8 + 
                     Auto$pred_minus_obs_H_b9 + Auto$pred_minus_obs_S_b1 + Auto$pred_minus_obs_S_b2 +
                     Auto$pred_minus_obs_S_b3 + Auto$pred_minus_obs_S_b4 + Auto$pred_minus_obs_S_b5 +
                     Auto$pred_minus_obs_S_b6 + Auto$pred_minus_obs_S_b7 + Auto$pred_minus_obs_S_b8 +
                     Auto$pred_minus_obs_S_b9, direction="both", steps=1000, trace=2)

stepwise

SLR = multinom(formula = Auto$class ~ Auto$b2 + Auto$pred_minus_obs_H_b1 + 
                 Auto$b8 + Auto$pred_minus_obs_S_b1 + Auto$pred_minus_obs_H_b7 + 
                 Auto$b9 + Auto$b3, 
               family=binomial, data = Auto)

SLR = multinom(formula = class ~ ., family=binomial, data = Auto)






CMt=data.frame(matrix(nrow=4, ncol=4))
for (i in 1:4){
  for (j in 1:4){
    CMt[i,j]=0 
  }
}
CMt  ###CM calculation



res.lr = sapply(seq_along(idx), function(i){
  #for (i in 1:10){
   ##loop over each of the 10 cross-validation folds
  
  SLR = multinom(formula = class ~ ., family=binomial, data = Auto[-idx[[i]],])
  
  prediction = predict(SLR, Auto[idx[[i]],-1], type="class")
  
  ##the ratio of misclassified samples
  #mean(Auto$class[idx[[i]]] != prediction)
  CM=table(Auto$class[ idx[[i]] ], prediction)
  CM
  CMt = CMt + CM
  CMt
  #accuracy = sum(diag(CM))/sum(CM)
  #accuracy
})
#CMt  
  
mean(res.lr)


res.lr = sapply(seq_along(idx), function(i) {
  ##loop over each of the 10 cross-validation folds
  
  SLR1 = multinom(formula = class ~ b2 + pred_minus_obs_H_b1 + 
                   b8 + pred_minus_obs_S_b1 + pred_minus_obs_H_b7 + 
                   b9 + b3, family=binomial, data = Auto[-idx[[i]],])
  
  prediction = predict(SLR1, Auto[idx[[i]],-1], type="class")
  
  ##the ratio of misclassified samples
  #mean(Auto$class[ idx[[i]] ] != prediction)
  CM=table(Auto$class[ idx[[i]] ], prediction)
  accuracy = sum(diag(CM))/sum(CM)
  accuracy
})
mean(res.lr)


#####################Outliers were replaced by the mean values
res.lr = sapply(seq_along(idx), function(i) {
  ##loop over each of the 10 cross-validation folds
  
  SLR1 = multinom(formula = class ~ b2 + pred_minus_obs_H_b1 + 
                    pred_minus_obs_H_b9 + pred_minus_obs_H_b7 + b4 + 
                    b8 + b3 + pred_minus_obs_S_b5 + pred_minus_obs_H_b6 + 
                    pred_minus_obs_H_b3 + b9, family=binomial, data = Auto[-idx[[i]],])
  
  prediction = predict(SLR1, Auto[idx[[i]],-1], type="class")
  
  ##the ratio of misclassified samples
  #mean(Auto$class[ idx[[i]] ] != prediction)
  CM=table(Auto$class[ idx[[i]] ], prediction)
  accuracy = sum(diag(CM))/sum(CM)
  accuracy
})
mean(res.lr)

###################k-NN model########################

set.seed(1)
ks = 1:12
res = sapply(ks, function(k) {
  ##try out each version of k from 1 to 12
  res.k = sapply(seq_along(idx), function(i) {
    ##loop over each of the 10 cross-validation folds
    ##predict the held-out samples using k nearest neighbors
    prediction1 = knn(Auto[ -idx[[i]], -1],
               Auto[ idx[[i]], -1],
               Auto$class[ -idx[[i]] ], k = k)
    ##the ratio of misclassified samples
    mean(Auto$class[ idx[[i]] ] != prediction1)
  })
  ##average over the 10 folds
  mean(res.k)
})

plot(ks, res, type="o",ylab="misclassification error")
res

res.kKNN = sapply(seq_along(idx), function(i) {
  
  ##loop over each of the 10 cross-validation folds
  ##predict the held-out samples using k nearest neighbors
  prediction2 = knn(Auto[ -idx[[i]], -1],
                   Auto[ idx[[i]], -1],
                   Auto$class[ -idx[[i]] ], k = 2)
  ##the ratio of misclassified samples
  #mean(Auto$class[ idx[[i]] ] != prediction)
  CM=table(Auto$class[ idx[[i]] ], prediction2)
  CM
  CMt = CMt + CM
  CMt
  accuracy = sum(diag(CM))/sum(CM)
  accuracy
  #print(CM)
})
#CMt
mean(res.kKNN)


###################SVM-RBF model#######################

set.seed(1)
res.lrSVM = sapply(seq_along(idx), function(i) {
  #for (i in 1:10){
  ##loop over each of the 5 cross-validation folds
  
  set.seed(1)
  dat=data.frame(x=Auto[-idx[[i]],-1], y=as.factor(Auto$class[-idx[[i]]])) 
  dat.te=data.frame(x=Auto[idx[[i]],-1], y=as.factor(Auto$class[idx[[i]]]))
  
  #10-fold cross validation
  tune.out=tune(svm, y ~ ., data=dat, kernel ="radial",
                ranges=list(cost=c(0.1, 1, 10, 100, 1010),
                            gamma=c(0.001, 0.003, 0.01, 1, 1.95)))
  summary(tune.out)# error rate 
  plot(tune.out)$cost
  
  prediction3=predict(tune.out$best.model, newdata=dat.te)
  prediction3
  
  CM=table(Auto$class[ idx[[i]] ], prediction3)
  CM
  CMt = CMt + CM
  CMt
  accuracy = sum(diag(CM))/sum(CM)
  accuracy
  
})
#CMt
#accuracy
mean(res.lrSVM)


###################Random Forest#######################


res.lrRF = sapply(seq_along(idx), function(i) {
  
  ##loop over each of the 5 cross-validation folds
  
  set.seed(1)
  
  rf = randomForest (x = Auto[-idx[[i]],-1], y = Auto$class[-idx[[i]]],
                     Auto[idx[[i]],-1], Auto$class[idx[[i]]], ntree=100)
  

  
  modelFit <- randomForest(class ~., data = Auto[-idx[[i]],])
  prediction4 <- predict(modelFit, Auto[idx[[i]],], type="class")
  
  CM=table(Auto$class[ idx[[i]] ], prediction4)
  accuracy = sum(diag(CM))/sum(CM)
  accuracy
})
mean(res.lrRF)

######################PCA#############################

barplot(table(Auto$class))
pca=prcomp(Auto[,2:28], center=TRUE, scale.=TRUE)
print(pca)
summary(pca)
plot(pca, type="l")

pca_var=(pca$sdev)^2
sum_var=sum(pca_var)
plot(cumsum(pca_var/sum_var), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
cumsum(pca_var/sum_var)

xyplot(pca$x[,2] ~ pca$x[,1], Auto, groups=Auto$class, xlab = "Z1", ylab = "Z2")

Auto_PCA=data.frame(matrix(nrow=nrow(Auto), ncol=7))
#Auto1_PCA=data.frame(matrix(nrow=nrow(Auto1), ncol=5))

colnames(Auto_PCA)=c("class","PC1","PC2","PC3","PC4", "PC5", "PC6")
for (i in 2:7){
  Auto_PCA[,i]=pca$x[,i]
}
Auto_PCA[,1]=Auto$class
dim(Auto_PCA)
Auto_PCA

Auto=Auto_PCA

############################LDA#############################

lda1=lda(formula=class ~ ., data=Auto)
#lda1$prior
#lda1$counts
plot(lda1)

lda.pred=predict(lda1)
lda.pred$x
lda.pred$class
names(lda.pred) 
dim(lda.pred$x)

Auto_LDA=data.frame(matrix(nrow=nrow(lda.pred$x), ncol=4))   ###Auto
#Auto1_LDA=data.frame(matrix(nrow=nrow(Auto[test,]), ncol=4))

colnames(Auto_LDA) = c("class", "LD1", "LD2", "LD3")

for (i in 2:4){
  Auto_LDA[,i]=lda.pred$x[,i-1]
}

length(Auto$class)
length(lda.pred$class)
Auto_LDA[,1] = lda.pred$class  ###Auto
Auto$class

Auto=Auto_LDA
Auto