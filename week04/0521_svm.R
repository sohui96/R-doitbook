library(kernlab)

m1=ksvm(Species~. ,data=iris)
m1
head(predict(m1, newdata=iris))

m2=ksvm(Species~. ,data=iris, kernel="vanilladot")
m2

m3=ksvm(Species~. ,data=iris, kernel="polydot", kpar=list(degree=3))
m3

#SVM 파라미터 튜닝
library(e1071)
result=tune.svm(Species~. ,data=iris, gamma=2^(-1:1), cost=2^(2:4))

attributes(result)
result$best.parameters
result$best.parameters["gamma"]
result$best.parameters["cost"]

library(caret)
idx<-createDataPartition(iris$Species, p=0.7, list=F)
iris_train<-iris[idx,]
iris_test<-iris[-idx,]
table(iris_train$Species)
table(iris_test$Species)

svm.result=ksvm(Species ~., iris_train, kernel="rbfdot")
svm.pred=predict(svm.result, iris_test, type="response")
table(svm.pred, iris_test$Species)
confusionMatrix(svm.pred, iris_test$Species)

#126page
