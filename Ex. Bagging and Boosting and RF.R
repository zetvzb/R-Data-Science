
setwd("/Users/zachtallevast/Box Sync/STAT 4002/")
Tumor <- read.csv("tumor.csv", header=T)
set.seed(1)
#(a) Create a training set containing a random sample of 90% of the observations, and a test set containing the remaining 10% of the observations. 
#(a) Set Seed to 1 for consistent results 
Sample = sample(1:nrow(Tumor), 0.9*nrow(Tumor), replace=FALSE)
#Sample = sample(c(TRUE,FALSE),nrow(Tumor),replace=T, prob=c(0.9,0.1)) #512 obs is 90% of Observations
train = Tumor[Sample,]
Tumor.test <- Tumor[-Sample,]
#(b) Fit a tree to the training data, with Diagnosis as the response and the other variables as the predictors. Use the summary() function to proudce summary statistics for the tree
#(b) and describe the results obtained. What is the training error rate? How many terminal nodes does the tree have? 
library(tree)
library(ISLR)

tree.Train = tree(Diagnosis ~.,train)
summary(tree.Train)
#The Training Error Rate is 3.32% 
#Number of Terminal Nodes: 9

#(c) Type in the name of the tree object to get a detailed text output. 
#Pick one of the terminal nodes and interpret the information displayed. 
tree.Train
#For instance, Area <696.25 with 302 observations in that branch, 73.880 deviance, 97.35% Benign and 2.65% Malignant

#(d) Create a Plot of the Tree, and interpret the results. 
plot(tree.Train)
text(tree.Train, pretty=0)

#(e) Predict the response on the test data, and produce a confusion matrix comparing the test labels to the predicted test labels. What is the test error rate? 
set.seed(1)
tree.pred = predict(tree.Train, Tumor.test, type="class")
table(Tumor.test$Diagnosis, tree.pred)
7/57 
#1.22%


#(f) Apply the cv.tree() function to the training set in order to determine the optimal tree size. 
cv.train = cv.tree(tree.Train, FUN=prune.misclass)
cv.train
#The optimal tree size is 9 because it has the lowest deviation of 34

#(g) Produce a plot with tree size on the x-axis and CV classification error rate on the Y-Axis 
plot(cv.train$size, cv.train$dev, xlab="Tree Size", ylab ="Deviation", main = 'Trees CV Plot')
abline(h=34)

#(h) Produce a pruned tree corresponding to the optimal tree size obtained using CV. If CV does not lead to selection of a pruned tree, then create a pruned tree with five terminal noeds 
prune.train = prune.misclass(tree.Train,best=9)
plot(prune.train)
text(prune.train, pretty=0)

#(i) Compare the training error rates between the pruned and unpruned trees. Which is higher? 
summary(prune.train)
#3.32% so slightly higher for pruned trees

#(j) Compare the test error rates between the pruned and unpruned trees. Which is higher? 
prune.pred = predict(prune.train, Tumor.test, type="class")
table(prune.pred, Tumor.test$Diagnosis)
#Same Error Rate. 

#(k) Now apply bagging to the training set. What is the test misclassifcation rate for this approach? 
library(randomForest)
set.seed(1)
bag.train = randomForest(Diagnosis ~., data=train, mtry=10, importance = TRUE)
bag.train
#Test Error is 5.6%
yhat.bag = predict(bag.train, newdata=Tumor.test)


#(l) Perform Boosting on the training set with 1000 trees for a range of values of the shrinkage paramater. Produce a plot with different shrinkage values on the x-axis and the corresponding training misclassification rate on the y-axis. Use 0.5 as the cut point for classification of Benign and Malignant 
#install.packages("gbm")
library(gbm)
library(dplyr)
Train = mutate(train, Diagnosis = ifelse(train$Diagnosis == 'Malignant',1,0))
set.seed(1)
pows = seq(-10, -0.2, by = 0.1)
lambdas = 10^pows
length.lambdas = length(lambdas)
train.errors = rep(NA, length.lambdas)
errorrate = rep(NA, 99)
for (i in 1:length.lambdas) {
  boost.tumor = gbm(Diagnosis ~ ., data = Train, distribution = "bernoulli", 
                    n.trees = 1000, shrinkage = lambdas[i])
  train.prob=rep(0,509)
  train.pred = predict(boost.tumor, Train, n.trees = 1000, type='response')
  train.prob[train.pred>.5] = 1
  train.errors[i] = sum(abs(Train$Diagnosis - train.prob))
  errorrate = (train.errors/509)}
plot(lambdas, errorrate, type = "b", xlab = "Shrinkage", ylab = "Train Misclassification", 
     col = "blue", pch = 20, xlim=c(0,.7), ylim=c(0,0.4))
errorrate
train.errors


#(m) Produce a plot with different shrinkage values on the x-axis and the corresponding test misclassifcation rate on the y-axis 
library(gbm)
Test = mutate(Tumor.test, Diagnosis = ifelse(Tumor.test$Diagnosis == 'Malignant',1,0))
set.seed(1)
test.errors = rep(NA, 99)


for (i in 1:length.lambdas) {
  boost.tumor = gbm(Diagnosis ~ ., data = Train, distribution = "bernoulli", 
                    n.trees = 1000, shrinkage = lambdas[i])
  test.pred = predict(boost.tumor, Test, n.trees = 1000, type='response')
  test.prob = rep(0,60)
  test.prob[test.pred>.5] = 1
  test.errors[i] = sum(abs(Test$Diagnosis - test.prob))}
test.prob
test.errorrate = test.errors/60
test.errorrate
plot(lambdas, test.errorrate, type = "b", xlab = "Shrinkage", ylab = "Test Misclassification", 
     col = "blue", pch = 20, xlim=c(0,.7), ylim=c(0,0.4))
test.errors


#(n) Use random forests to analyze this data. What test misclassification rate do you obtain (again use 0.5 to classifcy malignant or bening? Use the importance() function to determine which variables are most important. Descrive the effect of m, the number of variables considered at each split, on the error rate obtained)
#RERUN TOP
library(randomForest)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)
library(rpart)
set.seed(1)
forest.tumor = randomForest(Diagnosis~., data=train, mtry =6, type = "class", importance = TRUE )
importance = importance(forest.tumor)

varImportance <- data.frame(variables = row.names(importance), Importance = round(importance[ , 'MeanDecreaseGini'], 2))

rankImportance <-varImportance %>% 
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x=reorder(variables, Importance), 
                           y=Importance)) + 
  geom_bar(stat='identity', color = 'black')+
  geom_text(aes(x=variables, y=0.5, label = Rank), 
            hjust = 0, vjust = 0.55, size=4, color = 'lavender', 
            fontface = 'bold')+
  labs(x='Variables', title = 'Relative Variable Importance') + 
  coord_flip()+
  theme_few()

preds = predict(forest.tumor, newdata = Tumor.test)
train.prob = as.numeric(train.prob)
train.prob
train.prob = rep(0,57)
train.prob[preds >0.5] = 1
sum(abs(train.prob-Tumor.test$Diagnosis))
forest.tumor

#Test Error is 5.66%




#2: Bagging Simulations 
#This code write a manually bagging method using Boston data. 
library(MASS)
library(tree)
library(boot)
data(Boston)
set.seed(1)
train = sample(1:nrow(Boston), .5*nrow(Boston))
Boston.train<-Boston[train,]
Boston.test <-Boston[-train,]
#(a) Initialize a matrix called fhat with a number of rows equal to the number of rows in the test data and number of columns equal to the number of trees we will fit. Say B=100. Each column will represent the predictions from a particular iteration. 
fhat = matrix(NA, nrow = 253, ncol = 100)

B = rep(1:100)
n = nrow(Boston.train)
iterations =100
variables = 14
output = list()
for(i in 1:iterations){
  output[[i]] <- Boston.train[sample(nrow(Boston.train), 25),]  
}


boot.fn1 = tree(crim~., data = output[[1]])
tree.pred1 = predict(boot.fn1, newdata=Boston.test)
boot.fn2 = tree(crim~., data = output[[2]])
tree.pred2 = predict(boot.fn2, newdata=Boston.test)
boot.fn3 = tree(crim~., data = output[[3]])
tree.pred3 = predict(boot.fn3, newdata=Boston.test)
boot.fn4 = tree(crim~., data = output[[4]])
tree.pred4 = predict(boot.fn4, newdata=Boston.test)
boot.fn5 = tree(crim~., data = output[[5]])
tree.pred5 = predict(boot.fn5, newdata=Boston.test)



boot.fn6 = tree(crim~., data = output[[6]])
tree.pred6 = predict(boot.fn6, newdata=Boston.test)
boot.fn7 = tree(crim~., data = output[[7]])
tree.pred7 = predict(boot.fn7, newdata=Boston.test)
boot.fn8 = tree(crim~., data = output[[8]])
tree.pred8 = predict(boot.fn8, newdata=Boston.test)
boot.fn9 = tree(crim~., data = output[[9]])
tree.pred9 = predict(boot.fn9, newdata=Boston.test)
boot.fn10 = tree(crim~., data = output[[10]])
tree.pred10 = predict(boot.fn10, newdata=Boston.test)

tree.preds1 = data.frame(tree.pred6, tree.pred7, tree.pred8, tree.pred9, tree.pred10)
cumulative1 = rowMeans(tree.preds1)

boot.fn11 = tree(crim~., data = output[[11]])
tree.pred11 = predict(boot.fn11, newdata=Boston.test)
boot.fn12 = tree(crim~., data = output[[12]])
tree.pred12 = predict(boot.fn12, newdata=Boston.test)
boot.fn13 = tree(crim~., data = output[[13]])
tree.pred13 = predict(boot.fn13, newdata=Boston.test)
boot.fn14 = tree(crim~., data = output[[14]])
tree.pred14 = predict(boot.fn14, newdata=Boston.test)
boot.fn15 = tree(crim~., data = output[[15]])
tree.pred15 = predict(boot.fn15, newdata=Boston.test)

tree.preds2 = data.frame(tree.pred11, tree.pred12, tree.pred13, tree.pred14, tree.pred15)
cumulative2 = rowMeans(tree.preds2)



boot.fn16 = tree(crim~., data = output[[16]])
tree.pred16 = predict(boot.fn16, newdata=Boston.test)
boot.fn17 = tree(crim~., data = output[[17]])
tree.pred17 = predict(boot.fn17, newdata=Boston.test)
boot.fn18 = tree(crim~., data = output[[18]])
tree.pred18 = predict(boot.fn18, newdata=Boston.test)
boot.fn19 = tree(crim~., data = output[[19]])
tree.pred19 = predict(boot.fn19, newdata=Boston.test)
boot.fn20 = tree(crim~., data = output[[20]])
tree.pred20 = predict(boot.fn20, newdata=Boston.test)

tree.preds3 = data.frame(tree.pred16, tree.pred17, tree.pred18, tree.pred19, tree.pred20)
cumulative3 = rowMeans(tree.preds3)


boot.fn21 = tree(crim~., data = output[[21]])
tree.pred21 = predict(boot.fn21, newdata=Boston.test)
boot.fn22 = tree(crim~., data = output[[22]])
tree.pred22 = predict(boot.fn22, newdata=Boston.test)
boot.fn23 = tree(crim~., data = output[[23]])
tree.pred23 = predict(boot.fn23, newdata=Boston.test)
boot.fn24 = tree(crim~., data = output[[24]])
tree.pred24 = predict(boot.fn24, newdata=Boston.test)
boot.fn25 = tree(crim~., data = output[[25]])
tree.pred25 = predict(boot.fn25, newdata=Boston.test)

tree.preds4 = data.frame(tree.pred21, tree.pred22, tree.pred23, tree.pred24, tree.pred25)
cumulative4 = rowMeans(tree.preds4)

boot.fn26 = tree(crim~., data = output[[26]])
tree.pred26 = predict(boot.fn26, newdata=Boston.test)
boot.fn27 = tree(crim~., data = output[[27]])
tree.pred27 = predict(boot.fn27, newdata=Boston.test)
boot.fn28 = tree(crim~., data = output[[28]])
tree.pred28 = predict(boot.fn28, newdata=Boston.test)
boot.fn29 = tree(crim~., data = output[[29]])
tree.pred29 = predict(boot.fn29, newdata=Boston.test)
boot.fn30 = tree(crim~., data = output[[30]])
tree.pred30 = predict(boot.fn30, newdata=Boston.test)

tree.preds5 = data.frame(tree.pred26, tree.pred27, tree.pred28, tree.pred29, tree.pred30)
cumulative5 = rowMeans(tree.preds5)

boot.fn31 = tree(crim~., data = output[[31]])
tree.pred31 = predict(boot.fn31, newdata=Boston.test)
boot.fn32 = tree(crim~., data = output[[32]])
tree.pred32 = predict(boot.fn32, newdata=Boston.test)
boot.fn33 = tree(crim~., data = output[[33]])
tree.pred33 = predict(boot.fn33, newdata=Boston.test)
boot.fn34 = tree(crim~., data = output[[34]])
tree.pred34 = predict(boot.fn34, newdata=Boston.test)
boot.fn35 = tree(crim~., data = output[[35]])
tree.pred35 = predict(boot.fn35, newdata=Boston.test)

tree.preds6 = data.frame(tree.pred31, tree.pred32, tree.pred33, tree.pred34, tree.pred35)
cumulative6 = rowMeans(tree.preds6)

boot.fn36 = tree(crim~., data = output[[36]])
tree.pred36 = predict(boot.fn36, newdata=Boston.test)
boot.fn37 = tree(crim~., data = output[[37]])
tree.pred37 = predict(boot.fn37, newdata=Boston.test)
boot.fn38 = tree(crim~., data = output[[38]])
tree.pred38 = predict(boot.fn38, newdata=Boston.test)
boot.fn39 = tree(crim~., data = output[[39]])
tree.pred39 = predict(boot.fn39, newdata=Boston.test)
boot.fn40 = tree(crim~., data = output[[40]])
tree.pred40 = predict(boot.fn40, newdata=Boston.test)

tree.preds7 = data.frame(tree.pred36, tree.pred37, tree.pred38, tree.pred39, tree.pred40)
cumulative7 = rowMeans(tree.preds7)


boot.fn41 = tree(crim~., data = output[[41]])
tree.pred41 = predict(boot.fn41, newdata=Boston.test)
boot.fn42 = tree(crim~., data = output[[42]])
tree.pred42 = predict(boot.fn42, newdata=Boston.test)
boot.fn43 = tree(crim~., data = output[[43]])
tree.pred43 = predict(boot.fn43, newdata=Boston.test)
boot.fn44 = tree(crim~., data = output[[44]])
tree.pred44 = predict(boot.fn44, newdata=Boston.test)
boot.fn45 = tree(crim~., data = output[[45]])
tree.pred45 = predict(boot.fn45, newdata=Boston.test)

tree.preds8 = data.frame(tree.pred41, tree.pred42, tree.pred43, tree.pred44, tree.pred45)
cumulative8 = rowMeans(tree.preds8)

boot.fn46 = tree(crim~., data = output[[46]])
tree.pred46 = predict(boot.fn46, newdata=Boston.test)
boot.fn47 = tree(crim~., data = output[[47]])
tree.pred47 = predict(boot.fn47, newdata=Boston.test)
boot.fn48 = tree(crim~., data = output[[48]])
tree.pred48 = predict(boot.fn48, newdata=Boston.test)
boot.fn49 = tree(crim~., data = output[[49]])
tree.pred49 = predict(boot.fn49, newdata=Boston.test)
boot.fn50 = tree(crim~., data = output[[50]])
tree.pred50 = predict(boot.fn50, newdata=Boston.test)

tree.preds9 = data.frame(tree.pred46, tree.pred47, tree.pred48, tree.pred49, tree.pred50)
cumulative9 = rowMeans(tree.preds9)

boot.fn51 = tree(crim~., data = output[[51]])
tree.pred51 = predict(boot.fn51, newdata=Boston.test)
boot.fn52 = tree(crim~., data = output[[52]])
tree.pred52 = predict(boot.fn52, newdata=Boston.test)
boot.fn53 = tree(crim~., data = output[[53]])
tree.pred53 = predict(boot.fn53, newdata=Boston.test)
boot.fn54 = tree(crim~., data = output[[54]])
tree.pred54 = predict(boot.fn54, newdata=Boston.test)
boot.fn55 = tree(crim~., data = output[[55]])
tree.pred55 = predict(boot.fn55, newdata=Boston.test)

tree.preds10 = data.frame(tree.pred51, tree.pred52, tree.pred53, tree.pred54, tree.pred55)
cumulative10 = rowMeans(tree.preds10)

boot.fn56 = tree(crim~., data = output[[56]])
tree.pred56 = predict(boot.fn56, newdata=Boston.test)
boot.fn57 = tree(crim~., data = output[[57]])
tree.pred57 = predict(boot.fn57, newdata=Boston.test)
boot.fn58 = tree(crim~., data = output[[58]])
tree.pred58 = predict(boot.fn58, newdata=Boston.test)
boot.fn59 = tree(crim~., data = output[[59]])
tree.pred59 = predict(boot.fn59, newdata=Boston.test)
boot.fn60 = tree(crim~., data = output[[60]])
tree.pred60 = predict(boot.fn60, newdata=Boston.test)

tree.preds11 = data.frame(tree.pred56, tree.pred57, tree.pred58, tree.pred59, tree.pred60)
cumulative11 = rowMeans(tree.preds11)


boot.fn61 = tree(crim~., data = output[[61]])
tree.pred61 = predict(boot.fn61, newdata=Boston.test)
boot.fn62 = tree(crim~., data = output[[62]])
tree.pred62 = predict(boot.fn62, newdata=Boston.test)
boot.fn63 = tree(crim~., data = output[[63]])
tree.pred63 = predict(boot.fn63, newdata=Boston.test)
boot.fn64 = tree(crim~., data = output[[64]])
tree.pred64 = predict(boot.fn64, newdata=Boston.test)
boot.fn65 = tree(crim~., data = output[[65]])
tree.pred65 = predict(boot.fn65, newdata=Boston.test)

tree.preds12 = data.frame(tree.pred61, tree.pred62, tree.pred63, tree.pred64, tree.pred65)
cumulative12 = rowMeans(tree.preds12)


boot.fn66 = tree(crim~., data = output[[66]])
tree.pred66 = predict(boot.fn66, newdata=Boston.test)
boot.fn67 = tree(crim~., data = output[[67]])
tree.pred67 = predict(boot.fn67, newdata=Boston.test)
boot.fn68 = tree(crim~., data = output[[68]])
tree.pred68 = predict(boot.fn68, newdata=Boston.test)
boot.fn69 = tree(crim~., data = output[[69]])
tree.pred69 = predict(boot.fn69, newdata=Boston.test)
boot.fn70 = tree(crim~., data = output[[70]])
tree.pred70 = predict(boot.fn70, newdata=Boston.test)

tree.preds13 = data.frame(tree.pred66, tree.pred67, tree.pred68, tree.pred69, tree.pred70)
cumulative13 = rowMeans(tree.preds13)


boot.fn71 = tree(crim~., data = output[[71]])
tree.pred71 = predict(boot.fn71, newdata=Boston.test)
boot.fn72 = tree(crim~., data = output[[72]])
tree.pred72 = predict(boot.fn72, newdata=Boston.test)
boot.fn73 = tree(crim~., data = output[[73]])
tree.pred73 = predict(boot.fn73, newdata=Boston.test)
boot.fn74 = tree(crim~., data = output[[74]])
tree.pred74 = predict(boot.fn74, newdata=Boston.test)
boot.fn75 = tree(crim~., data = output[[75]])
tree.pred75 = predict(boot.fn75, newdata=Boston.test)

tree.preds14 = data.frame(tree.pred71, tree.pred72, tree.pred73, tree.pred74, tree.pred75)
cumulative14 = rowMeans(tree.preds14)

boot.fn76 = tree(crim~., data = output[[76]])
tree.pred76 = predict(boot.fn76, newdata=Boston.test)
boot.fn77 = tree(crim~., data = output[[77]])
tree.pred77 = predict(boot.fn77, newdata=Boston.test)
boot.fn78 = tree(crim~., data = output[[78]])
tree.pred78 = predict(boot.fn78, newdata=Boston.test)
boot.fn79 = tree(crim~., data = output[[79]])
tree.pred79 = predict(boot.fn79, newdata=Boston.test)
boot.fn80 = tree(crim~., data = output[[80]])
tree.pred80 = predict(boot.fn80, newdata=Boston.test)

tree.preds15 = data.frame(tree.pred76, tree.pred77, tree.pred78, tree.pred79, tree.pred80)
cumulative15 = rowMeans(tree.preds15)


boot.fn81 = tree(crim~., data = output[[81]])
tree.pred81 = predict(boot.fn81, newdata=Boston.test)
boot.fn82 = tree(crim~., data = output[[82]])
tree.pred82 = predict(boot.fn82, newdata=Boston.test)
boot.fn83 = tree(crim~., data = output[[83]])
tree.pred83 = predict(boot.fn83, newdata=Boston.test)
boot.fn84 = tree(crim~., data = output[[84]])
tree.pred84 = predict(boot.fn84, newdata=Boston.test)
boot.fn85 = tree(crim~., data = output[[85]])
tree.pred85 = predict(boot.fn85, newdata=Boston.test)

tree.preds16 = data.frame(tree.pred81, tree.pred82, tree.pred83, tree.pred84, tree.pred85)
cumulative16 = rowMeans(tree.preds16)

boot.fn86 = tree(crim~., data = output[[86]])
tree.pred86 = predict(boot.fn86, newdata=Boston.test)
boot.fn87 = tree(crim~., data = output[[87]])
tree.pred87 = predict(boot.fn87, newdata=Boston.test)
boot.fn88 = tree(crim~., data = output[[88]])
tree.pred88 = predict(boot.fn88, newdata=Boston.test)
boot.fn89 = tree(crim~., data = output[[89]])
tree.pred89 = predict(boot.fn89, newdata=Boston.test)
boot.fn90 = tree(crim~., data = output[[90]])
tree.pred90 = predict(boot.fn90, newdata=Boston.test)

tree.preds17 = data.frame(tree.pred86, tree.pred87, tree.pred88, tree.pred89, tree.pred90)
cumulative17 = rowMeans(tree.preds17)

boot.fn91 = tree(crim~., data = output[[91]])
tree.pred91 = predict(boot.fn91, newdata=Boston.test)
boot.fn92 = tree(crim~., data = output[[92]])
tree.pred92 = predict(boot.fn92, newdata=Boston.test)
boot.fn93 = tree(crim~., data = output[[93]])
tree.pred93 = predict(boot.fn93, newdata=Boston.test)
boot.fn94 = tree(crim~., data = output[[94]])
tree.pred94 = predict(boot.fn94, newdata=Boston.test)
boot.fn95 = tree(crim~., data = output[[95]])
tree.pred95 = predict(boot.fn95, newdata=Boston.test)

tree.preds18 = data.frame(tree.pred91, tree.pred92, tree.pred93, tree.pred94, tree.pred95)
cumulative18 = rowMeans(tree.preds18)

boot.fn96 = tree(crim~., data = output[[96]])
tree.pred96 = predict(boot.fn96, newdata=Boston.test)
boot.fn97 = tree(crim~., data = output[[97]])
tree.pred97 = predict(boot.fn97, newdata=Boston.test)
boot.fn98 = tree(crim~., data = output[[98]])
tree.pred98 = predict(boot.fn98, newdata=Boston.test)
boot.fn99 = tree(crim~., data = output[[99]])
tree.pred99 = predict(boot.fn99, newdata=Boston.test)
boot.fn100 = tree(crim~., data = output[[100]])
tree.pred100 = predict(boot.fn100, newdata=Boston.test)

tree.preds19 = data.frame(tree.pred96, tree.pred97, tree.pred98, tree.pred99, tree.pred100)
cumulative19 = rowMeans(tree.preds19)

tree.predsfinal1 = data.frame(tree.preds, tree.preds1)
cum1 = rowMeans(tree.predsfinal1)

tree.predsfinal2 = data.frame(tree.preds, tree.preds1, tree.preds2)
cum2 = rowMeans(tree.predsfinal2)

tree.predsfinal3 = data.frame(tree.preds, tree.preds1, tree.preds2, tree.preds3)
cum3 = rowMeans(tree.predsfinal3)

tree.predsfinal4 = data.frame(tree.preds, tree.preds1, tree.preds2, tree.preds3, tree.preds4)
cum4 = rowMeans(tree.predsfinal4)

tree.predsfinal5 = data.frame(tree.preds, tree.preds1, tree.preds2, tree.preds3, tree.preds4, tree.preds5)
cum5 = rowMeans(tree.predsfinal5)

tree.predsfinal6 = data.frame(tree.preds, tree.preds1, tree.preds2, tree.preds3, tree.preds4, tree.preds5, tree.preds6)
cum6 = rowMeans(tree.predsfinal6)

tree.predsfinal7 = data.frame(tree.preds, tree.preds1, tree.preds2, tree.preds3, tree.preds4, tree.preds5, tree.preds6, tree.preds7)
cum7 = rowMeans(tree.predsfinal7)

tree.predsfinal8 = data.frame(tree.preds, tree.preds1, tree.preds2, tree.preds3, tree.preds4, tree.preds5, tree.preds6, tree.preds7, tree.preds8)
cum8 = rowMeans(tree.predsfinal8)

tree.predsfinal9 = data.frame(tree.preds, tree.preds1, tree.preds2, tree.preds3, tree.preds4, tree.preds5, tree.preds6, tree.preds7, tree.preds8,
                              tree.preds9)
cum9 = rowMeans(tree.predsfinal9)

tree.predsfinal10 = data.frame(tree.predsfinal9, tree.preds10)
cum10 =rowMeans(tree.predsfinal10)

tree.predsfinal11 = data.frame(tree.predsfinal10, tree.preds11)
cum11 =rowMeans(tree.predsfinal11)

tree.predsfinal12 = data.frame(tree.predsfinal11, tree.preds12)
cum12 =rowMeans(tree.predsfinal12)

tree.predsfinal13 = data.frame(tree.predsfinal12, tree.preds13)
cum13 =rowMeans(tree.predsfinal13)

tree.predsfinal14 = data.frame(tree.predsfinal13, tree.preds14)
cum14 =rowMeans(tree.predsfinal14)

tree.predsfinal15 = data.frame(tree.predsfinal14, tree.preds15)
cum15 =rowMeans(tree.predsfinal15)

tree.predsfinal16 = data.frame(tree.predsfinal15, tree.preds16)
cum16 =rowMeans(tree.predsfinal16)

tree.predsfinal17 = data.frame(tree.predsfinal16, tree.preds17)
cum17 =rowMeans(tree.predsfinal17)

tree.predsfinal18 = data.frame(tree.predsfinal17, tree.preds18)
cum18 =rowMeans(tree.predsfinal18)

tree.predsfinal19 = data.frame(tree.predsfinal18, tree.preds19)
cum19 =rowMeans(tree.predsfinal19)

cum19

mse_data = data.frame(cum1, cum2, cum3, cum4, cum5, cum6, cum7, cum8, 
                      cum9, cum10, cum11, cum12, cum13, cum14, cum15, cum16, cum17,
                      cum18, cum19, Boston.test$crim)
crim = Boston.test$crim
mse1 = (abs(cum1-crim)^2)
mse2 = (abs(cum2-crim)^2)
mse3 = (abs(cum3-crim)^2)
mse4 = (abs(cum4-crim)^2)
mse5 = (abs(cum5-crim)^2)
mse6 = (abs(cum6-crim)^2)
mse7 = (abs(cum7-crim)^2)
mse8 = (abs(cum8-crim)^2)
mse9 = (abs(cum9-crim)^2)
mse10 = (abs(cum10-crim)^2)
mse11 = (abs(cum11-crim)^2)
mse12 = (abs(cum12-crim)^2)
mse13 = (abs(cum13-crim)^2)
mse14 = (abs(cum14-crim)^2)
mse15 = (abs(cum15-crim)^2)
mse16 = (abs(cum16-crim)^2)
mse17 = (abs(cum17-crim)^2)
mse18 = (abs(cum18-crim)^2)
mse19 = (abs(cum19-crim)^2)

mse_data2 = data.frame(mse1, mse2, mse3, mse4, mse5, mse6, mse7, mse8, mse9, mse10, mse11,
                       mse12, mse13, mse14, mse15, mse16, mse17, mse18, mse19)

plot(mse_data2)

MSE= colMeans(mse_data2)
plot(MSE, type = 'b')



