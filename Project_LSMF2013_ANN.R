#Report
#LSMF2013 Quantitative Data Analysis (UCLouvain)
#Project: Forset Type Mapping
#Authors: S. Kurin, L. Parmentier, and M. Goudjil
#Supervisor: Professor M. Saerens (UCLouvain)

#Classification Model: Artificial Neural Network


#Packages
library(rpart)
library(nnet)
library(MASS)
library(neuralnet)
library(chemometrics)
library(randomForest)


#Datasets
Auto=read.csv("C:/Users/serge_000/Desktop/DD/DD Spring/ADQ/ForestTypes/training.csv", sep=",",dec = ".",header=TRUE)
Auto1=read.csv("C:/Users/serge_000/Desktop/DD/DD Spring/ADQ/ForestTypes/testing.csv", sep=",",dec = ".",header=TRUE)

names(Auto)
dim(Auto)
dim(Auto1)


Auto = rbind(Auto, Auto1)
Auto = Auto[sample(nrow(Auto)),]
#set.seed(123)
index <- sample(1:nrow(Auto),round(0.7*nrow(Auto)))
index


#train <- Auto[index,]
#test <- Auto[-index,]

## 1) Decision tree

DT = function(){
  
index <- sample(1:nrow(Auto),round(0.7*nrow(Auto)))  
modelT <- rpart(Auto[index,]$class ~. ,method="class", data=Auto[index,])

prediction = predict(modelT, newdata = Auto[-index,-1], type="class")
confusion_matrix = table(Auto[-index,]$class,prediction)
Accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix) #accuracy
Accuracy}
#acc(accuracy,Auto[-index,]$class) }

## 2) Random Forest

RF = function(){
#modelT <- randomForest(Auto[index,]$class ~. ,method="class", data=Auto[index,])
modelT <- randomForest(Auto$class ~. ,method="class", data=Auto)  

confusion_matrix = modelT$confusion[,-5]
Accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix) #accuracy
Accuracy

#prediction = predict(modelT, newdata = Auto[-index,-1], type="class")
#accuracy = table(Auto[-index,]$class,prediction)

#acc(accuracy,Auto[-index,]$class)
}

## 3) Artificial Neural Network

ANN = function(){
index <- sample(1:nrow(Auto),round(0.7*nrow(Auto)))
ideal <- class.ind(Auto$class)

AutoANN = nnet(Auto[index,-1], ideal[index,],size=20, softmax=TRUE)

prediction = predict(AutoANN,Auto[-index,-1], type="class")

confusion_matrix = table(Auto[-index,]$class,prediction)
accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy

}


### MONTE-CARLO SIMULATION

MC = function(sim,fct){  ### sim = Number of Simulations # fct => 1= Decision tree 2= Random Forest 3= ANN
  
  
  vec.sim = vector(mode="numeric",length=sim)
  

  if (fct ==1){
    
    for(f in 1:sim){
      vec.sim[f] = DT()
    }
    
  } else if (fct == 2){
    
    for(f in 1:sim){
      vec.sim[f] = RF()
    }
    
    
  } else if (fct ==3){
    
    for(f in 1:sim){
      vec.sim[f] = ANN()
    }
    
  }
  
  
  vec.sim
  
}

test = MC(100,3)

hist(test)