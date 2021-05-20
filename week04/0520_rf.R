library(randomForest)
rf <- randomForest(Species ~., data=iris)
rf
rf2 <- randomForest(iris[,1:4],iris[,5])
rf2
rf3 <- randomForest(Species ~., data=iris, importance=T)
importance(rf3)
varImpPlot(rf3, main="varImpPlot of iris")


library(cvTools)
library(foreach)
set.seed(719)
K=10
R=3
cv <- cvFolds(NROW(iris), K=K, R=R)
grid <- expand.grid(ntree=c(10,100,200), mtry=c(3,4))
grid

r=randomForest(Species ~., data=iris, ntree=grid$ntree[g], mtry=grid$mtry[g])


##데이터분할
t_index <- sample(1:nrow(iris), size=nrow(iris)*0.7, replace = FALSE)
train <- iris[t_index, ]
test <- iris[-t_index, ]
nrow(train); nrow(test)

##분석예측
set.seed(1)
iris_randomForest <- randomForest(Species ~., data=train, proximity=TRUE, importance=TRUE)
plot(iris_randomForest)

# %IncMSE = 정확도, IncNodePurity = 중요도
# MeanDecreaseAccuracy: 정확도 개선에 중요한 변수   
# MeanDecreaseGini: 노드 불순도 개선에 중요한 변수
importance(iris_randomForest) 
varImpPlot(iris_randomForest)

# 10-fold CV 데이터 나누기
t_index <- sample(1:nrow(iris), size=nrow(iris))
split_index <- split(t_index, 1:10)
split_index[[1]]

# k-fold CV 수행
accuracy_3 <- c() # 데이터를 받을 빈 벡터

for(g in 1:nrow(grid)){
for(i in 1:10){
  test <- iris[split_index[[i]],]  
  train <- iris[-split_index[[i]],]  
  
  set.seed(1004)
  edu_randomForest <- randomForest(Species ~., data=train, proximity=TRUE, importance=TRUE, ntree=grid$ntree[g],
                                   mtry=grid$mtry[g])
  
  plot(iris_randomForest)
  importance(iris_randomForest) 
  varImpPlot(iris_randomForest)  
  
  test_pred <- predict(iris_randomForest, test)
  table <- table(real=test$Species, predict=test_pred)
  
  #정확도
  accuracy_3[i] <- sum(diag(table))/sum(table)
  
  }
  print(mean(accuracy_3))
}









