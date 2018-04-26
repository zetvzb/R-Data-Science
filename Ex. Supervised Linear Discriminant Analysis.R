
Animals <- read.csv("master.csv", header=T)
names(Animals)
set.seed(1)
train.set = sample(1:nrow(Animals),.7*nrow(Animals),replace=FALSE)
animals.train=Animals[train.set,]
animals.test=Animals[-train.set,]

animals.train$AVERAGE.LIFESPAN..YEARS. = as.factor(animals.train$AVERAGE.LIFESPAN..YEARS.)
animals.train$AboutToDie = as.factor(animals.train$AboutToDie)
animals.train$Hypo = as.factor(animals.train$Hypo)


#Fit a LDA model on the training set, with M chosen by cross validation. Report the new test error obtained, along with the value of M selected by cross-validation.  

library(pls)
attach(Animals)

library(dplyr)
animals.Train.New <- animals.train[,c(5,7,9,11,12,13,14,15,16,20,21,22,23,30,29)]
animals.Test.New <- animals.test[,c(5,7,9,11,12,13,14,15,16,20,21,22,23,30,29)]

data.frame = data.frame(animals.Test.New)

#Perform Linear Discriminant Analysis
library(MASS)
str(animals.Train.New)

lda.fit = lda(OutcomeType~AnimalType+AgeuponOutcome+Color+Hunting.Dog+AVERAGE.LIFESPAN..YEARS.+Sex+Fixed+Age+Mix+Hypo+Size+Hair+AboutToDie+TimeofDay, data=animals.Train.New)
summary(lda.fit)
lda.fit

animals.Train.New$AVERAGE.LIFESPAN..YEARS. = as.factor(animals.Train.New$AVERAGE.LIFESPAN..YEARS.)
animals.Train.New$Hypo = as.factor(animals.Train.New$Hypo)
animals.Train.New$AboutToDie = as.factor(animals.Train.New$AboutToDie)

animals.Test.New$AVERAGE.LIFESPAN..YEARS. = as.factor(animals.Test.New$AVERAGE.LIFESPAN..YEARS.)
animals.Test.New$Hypo = as.factor(animals.Test.New$Hypo)
animals.Test.New$AboutToDie = as.factor(animals.Test.New$AboutToDie)


lda.pred = predict(lda.fit, animals.Test.New)
p <-predict(lda.fit, animals.Test.New)
ldahist(data=p$x[,1], g=animals.Test.New$OutcomeType)

#Bi-Plot 
library(devtools)
#install_github("fawda123/ggord")
library(ggord)
ggord(lda.fit, animals.Train.New$OutcomeType, xlim=c(-4,4))

#Partition plot
library(klaR)
partimat(OutcomeType~chegg., data=animals.Train.New, method="lda", mar=c(100,100,100,100))

eval = data.frame(lda.pred$class, animals.Test.New$OutcomeType)
eval$misclass = ifelse(eval$lda.pred.class == eval$animals.Test.New.OutcomeType, 0, 1)
sum(eval$misclass)
3009/8019

eval$Adopted = ifelse(eval$animals.Test.New.OutcomeType == "Adoption", 1, 0)
eval$Transfered = ifelse(eval$animals.Test.New.OutcomeType == "Transfer", 1,0) 
eval$RTO = ifelse(eval$animals.Test.New.OutcomeType == "Return_to_owner", 1,0) 
eval$Euth =ifelse(eval$animals.Test.New.OutcomeType == "Euthanasia", 1, 0)
eval$Died = ifelse(eval$animals.Test.New.OutcomeType == "Died", 1,0)

sum(eval$Adopted)/8019
sum(eval$Transfered)/8019
sum(eval$RTO)/8019
sum(eval$Euth)/8019
sum(eval$Died)/8019

plot(lda.pred$x[,1], lda.pred$x[,2])
plot(lda.pred$x[,1], lda.pred$x[,2], col = as.integer(animals.Test.New$OutcomeType), pch=20)
plot(lda.pred$x[,3], lda.pred$x[,4], col = as.integer(animals.Test.New$OutcomeType), pch=20)
plot(lda.pred$x[,1], lda.pred$x[,4], col = as.integer(animals.Test.New$OutcomeType), pch=20)
plot(lda.pred$x[,1], lda.pred$x[,3], col = as.integer(animals.Test.New$OutcomeType), pch=20)
plot(lda.pred$x[,2], lda.pred$x[,3], col = as.integer(animals.Test.New$OutcomeType), pch=20)

options()
plot(lda.fit, dimen=20)
pairs(OutcomeType~AnimalType+AgeuponOutcome+Color+Hunting.Dog+AVERAGE.LIFESPAN..YEARS.+Sex+Fixed+Age+Mix+Hypo+Size+Hair+AboutToDie+TimeofDay, data=animals.Train.New)




