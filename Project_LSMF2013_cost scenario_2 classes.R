#Report
#LSMF2013 Quantitative Data Analysis (UCLouvain)
#Project: Forset Type Mapping
#Authors: S. Kurin, L. Parmentier, and M. Goudjil
#Supervisor: Professor M. Saerens (UCLouvain)

#3 classes merged for studying cost scenario and reject option
#Classification Models: Logistic Regression

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


#Working directory

Auto=read.csv("C:/Users/serge_000/Desktop/DD/DD Spring/ADQ/ForestTypes/training.csv", sep=",",dec = ".",header=TRUE)


Auto1=read.csv("C:/Users/serge_000/Desktop/DD/DD Spring/ADQ/ForestTypes/testing.csv", sep=",",dec = ".",header=TRUE)
#summary(Auto)
#summary(Auto1)

names(Auto)

dim(Auto)

Auto = rbind(Auto, Auto1)

Auto$class = as.numeric(Auto$class)

n=0

for (i in 1:length(Auto$class)){
  if (Auto$class[i] != 3) {
    Auto$class[i] = 0
  }
  else {
    Auto$class[i] = 1
    n=n+1
  }
}

pr=n/length(Auto$class)
pr
Auto$class = as.factor(Auto$class)

summary(Auto)

Auto[,2:28] = scale(x=Auto[,2:28], center=TRUE, scale=TRUE)

acc=function(CM)
{
  diag=0
  for(d in 1:2){
    diag = diag + CM[d,d]
  }
  diag
  length(test.Y)
  error = (length(test.Y) - diag)/length(test.Y)
  accuracy = 1 - error
  accuracy
}

test=199:523
train.X=Auto[-test, -1]
test.X=Auto[test, -1]
train.Y=Auto[-test, 1]
test.Y=Auto[test, 1]
w=data.frame(matrix(nrow=length(test.Y), ncol=3))

SLR = multinom(formula = class ~ ., family=binomial, data = Auto)

prediction = predict(SLR, Auto[test,-1], type="prob")

length(Auto$class)

CM=table(test.Y, prediction > 0.5)
CM

acc(CM) 

w[,1] = test.Y
w[,2] = 1-prediction
w[,3] = prediction
w

out = capture.output(w)
cat(out, file="C:/Users/serge_000/Desktop/DD/DD Spring/ADQ/ForestTypes/output.txt", sep=",", fill = TRUE, append=FALSE)