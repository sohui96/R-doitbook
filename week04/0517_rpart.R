# 너무 많은 변수로 학습되면 일반화가 되지 못할 수 있다.
# 학습은 잘될지라도 (그 데이터에만 잘 맞고) 예측 정확도는 떨어질 수 있다.

# install.packages("C50")
# install.packages("caret", dependencies=TRUE, repos='http://cran.rstudio.com/')
# install.packages("rpart")
#install.packages("e1071")


library(C50)
library(caret)#R3.5.0가능
idx <- createDataPartition(iris$Species, p=0.7, list=F)
iris_train <- iris[idx, ] #생성된 읶덱스를 이용, 70%의 비율로 학습용 데이터 세트 추춗
iris_test <- iris[-idx, ] #생성된 읶덱스를 이용, 30%의 비율로 평가용 데이터 세트 추춗
table(iris_train$Species)
table(iris_test$Species)


#의사결정트리 기법을 사용하기 위핚 rpart 패키지 로딩
library(rpart)
rpart.result<-rpart(Species ~ ., data=iris_train) #훈렦데이터 통핚 모형 적합
rpart.result
rpart.pred <- predict(rpart.result, iris_test, type="class")
#테스트 데이터 이용 평가
table(rpart.pred, iris_test$Species) #분류 결과도춗


library(e1071)
confusionMatrix(rpart.pred, iris_test$Species) #예측치, 실측치

plot(rpart.result,margin=0.2)
text(rpart.result,cex=0.8)
#yval기준 38개 중 4개가 틀림(오분류)

# 트리 플롯 그리기
#install.packages("rattle")
install.packages("rpart.plot")
#library(rattle) 안해도됨
library(rpart.plot)
library(RColorBrewer)
rpart.plot(rpart.result) # 트리 플롯 그리기 기본형











