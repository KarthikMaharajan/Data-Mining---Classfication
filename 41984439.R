myC45Iris <- function(){
  library(caret)
set.seed(4139)
trainind<-createDataPartition(iris$Species, p = .8,
                              list = FALSE,
                              times = 1)
irisTrain <- iris[ trainind,]
irisTest  <- iris[-trainind,]
summary(irisTrain)
summary(irisTest)
library(RWeka)
library(datasets)
data("iris")
fit<-J48(Species~., data=irisTrain)
prediction<-predict(fit,data=irisTrain)
table(prediction,irisTrain$Species)
print(fit)
library(partykit)
plot(fit)
summary(fit)
predictiontest<-predict(fit,newdata=irisTest)
plot(predictiontest)
table(predictiontest,irisTest$Species)
}

myC45LifeExpectancy <- function(){
  MyData <- read.csv(file="/Users/KarthikMaharajan/Documents/Fall 2015/Introduction to Data Mining/Project 1/life_expectancy.csv", header=TRUE, sep=",")
  str(MyData)
  library(caret)
  set.seed(4139)
  trainindle<-createDataPartition(MyData$Continent, p = .8,
                                  list = FALSE,
                                  times = 1)
  leTrain <- MyData[ trainindle,]
  leTest  <- MyData[-trainindle,]
  summary(leTrain)
  summary(leTest)
  library(RWeka)
  library(datasets)
  fitle<-J48(Continent~., data=leTrain)
  predictionle<-predict(fitle,data=leTrain)
  table(predictionle,leTrain$Continent)
  print(fitle)
  library(partykit)
  plot(fitle)
  summary(fitle)
  predictiontestle<-predict(fitle,newdata=leTest)
  plot(predictiontestle)
  table(predictiontestle,leTest$Continent)
}

myRipperIris <- function(){
  library(caret)
  set.seed(4139)
  trainind<-createDataPartition(iris$Species, p = .8,
                                list = FALSE,
                                times = 1)
  irisTrain <- iris[ trainind,]
  irisTest  <- iris[-trainind,]
  summary(irisTrain)
  summary(irisTest)
  library(datasets)
  data("iris")
  rip=JRip(Species~.,data=irisTrain)
  print(rip)
  summary(rip)
  predictiontestrip<-predict(rip,newdata=irisTest)
  table(predictiontestrip,irisTest$Species)
  plot(predictiontestrip)
}

myRipperLifeExpectancy <- function(){
  MyData <- read.csv(file="/Users/KarthikMaharajan/Documents/Fall 2015/Introduction to Data Mining/Project 1/life_expectancy.csv", header=TRUE, sep=",")
  str(MyData)
  library(caret)
  set.seed(4139)
  le<-createDataPartition(MyData$Continent, p = .8,
                          list = FALSE,
                          times = 1)
  Trainle <- MyData[ le,]
  Testle  <- MyData[-le,]
  summary(Trainle)
  summary(Testle)
  library(datasets)
  riple=JRip(Continent~.,data=Trainle)
  print(riple)
  summary(riple)
  predictiontestlerip<-predict(riple,newdata=Testle)
  table(predictiontestlerip,Testle$Continent)
  plot(predictiontestlerip)
}

myObliqueIris <- function(){
  library(caret)
  set.seed(4139)
  ind<-createDataPartition(iris$Species, p = .8,
                           list = FALSE,
                           times = 1)
  Traindata <- iris[ ind,]
  Testdata  <- iris[-ind,]
  summary(Traindata)
  summary(Testdata)
  library(oblique.tree)
  library(datasets)
  iris_ob <- oblique.tree(Species~., data=Traindata,oblique.splits = "only")
  pred<-predict(iris_ob,data=Traindata)
  print(iris_ob)
  plot(iris_ob)
  text(iris_ob)
  summary(iris_ob)
  iris_ob1 <- oblique.tree(formula=Species~., data=Testdata,oblique.splits = "only")
  pred2<-predict(iris_ob1,data=Testdata)
  print(iris_ob1)
  plot(iris_ob1)
  text(iris_ob1)
}

myObliqueLifeExpectancy <- function(){
  MyData <- read.csv(file="/Users/KarthikMaharajan/Documents/Fall 2015/Introduction to Data Mining/Project 1/life_expectancy.csv", header=TRUE, sep=",")
  str(MyData)
  library(caret)
  set.seed(4139)
  ind<-createDataPartition(MyData$Continent, p = .8,
                           list = FALSE,
                           times = 1)
  myvar <- names(MyData) %in% c("Country") 
  newdata <- MyData[!myvar]
  Traindata <- newdata[ ind,]
  Testdata  <- newdata[-ind,]
  summary(Traindata)
  summary(Testdata)
  library(oblique.tree)
  library(datasets)
  iris_ob <- oblique.tree(Continent~., data=Traindata,oblique.splits = "only")
  pred<-predict(iris_ob,data=Traindata)
  print(iris_ob)
  plot(iris_ob)
  text(iris_ob)
  summary(iris_ob)
  iris_ob1 <- oblique.tree(Continent~., data=Testdata,oblique.splits = "only")
  pred2<-predict(iris_ob1,data=Testdata)
  print(iris_ob1)
  plot(iris_ob1)
  text(iris_ob1)
}

myNaiveBayesIris <- function(){
  library(caret)
  set.seed(4139)
  trainind<-createDataPartition(iris$Species, p = .8,
                                list = FALSE,
                                times = 1)
  irisTrain <- iris[ trainind,]
  irisTest  <- iris[-trainind,]
  summary(irisTrain)
  summary(irisTest)
  library(RWeka)
  library(datasets)
  library(e1071)
  classifiernb<-naiveBayes(irisTrain[,1:4], irisTrain[,5])
  print(classifiernb)
  summary(classifiernb)
  table(predict(classifiernb, irisTrain[,-5]), irisTrain[,5])
  table(predict(classifiernb, irisTest[,-5]), irisTest[,5])
}

myNaiveBayesLifeExpectancy <- function(){
  MyData <- read.csv(file="/Users/KarthikMaharajan/Documents/Fall 2015/Introduction to Data Mining/Project 1/life_expectancy.csv", header=TRUE, sep=",")
  str(MyData)
  library(caret)
  set.seed(4139)
  trainind<-createDataPartition(MyData$Continent, p = .8,
                                list = FALSE,
                                times = 1)
  leTrain <- MyData[ trainindle,]
  leTest  <- MyData[-trainindle,]
  summary(leTrain)
  summary(leTest)
  library(RWeka)
  library(datasets)
  library(e1071)
  classifiernble<-naiveBayes(leTrain[,1:7], leTrain[,8])
  print(classifiernble)
  summary(classifiernble)
  table(predict(classifiernble, leTrain[,-8]), leTrain[,8])
  table(predict(classifiernble, leTest[,-8]), leTest[,8])
}

myKnnIris <- function(){
  library(caret)
  set.seed(4139)
  ind<-createDataPartition(iris$Species, p = .8,
                           list = FALSE,
                           times = 1)
  Traindata2 <- iris[ ind,]
  Testdata2  <- iris[-ind,]
  summary(Traindata2)
  summary(Testdata2)
  library(RWeka)
  library(datasets)
  iris_pred <- IBk(Species~., data=Traindata2)
  pred<-predict(iris_pred,data=Traindata2)
  print(iris_pred)
  plot(pred)
  summary(iris_pred)
  prediction<-predict(iris_pred, newdata=Testdata2)
  table(prediction,Testdata2$Species)
  plot(prediction)
}

myKnnLifeExpectancy <- function(){
  MyData <- read.csv(file="/Users/KarthikMaharajan/Documents/Fall 2015/Introduction to Data Mining/Project 1/life_expectancy.csv", header=TRUE, sep=",")
  str(MyData)
  library(caret)
  set.seed(4139)
  indle<-createDataPartition(MyData$Continent, p = .8,
                             list = FALSE,
                             times = 1)
  Traindata2le <- MyData[ indle,]
  Testdata2le  <- MyData[-indle,]
  summary(Traindata2le)
  summary(Testdata2le)
  library(RWeka)
  library(datasets)
  cont_pred <- IBk(Continent~., data=Traindata2le)
  pred<-predict(cont_pred,data=Traindata2le)
  print(cont_pred)
  plot(pred)
  summary(cont_pred)
  prediction<-predict(cont_pred, newdata=Testdata2le)
  table(prediction,Testdata2le$Continent)
  plot(prediction)
}
