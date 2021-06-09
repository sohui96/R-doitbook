##knn머신러닝적용코드; knn 다양한 관점에서 보는 라이브러리들 있다.
##데이터 분포 확인
library(ggvis)
library(dplyr)
iris %>% 
  ggvis(~Petal.Length,~Petal.Width, fill=~factor(Species)) %>%
  layer_points()

library(class) # k = 1 일 때
# set.seed(1234)
# knn_1 <- knn(train = train_x, test = valid_x, cl = train_y, k = 1)

set.seed(1234)
idx<-sample(1:NROW(iris),0.7*NROW(iris))
iris.train=iris[idx,]
iris.test=iris[-idx,]
iris_model<-knn(train=iris.train[,-5],
                test=iris.test[,-5],
                cl=iris.train$Species, k=3
)
summary(iris_model)
table(iris_model,iris.test$Species)

##---------------------##
##산점도 -train 산점도
par(mar=c(2,2,2,2))
plot(Sepal.Length~Sepal.Width,
     data=iris.train,
     col=c("purple","blue","green")[iris.train$Species],
     main="KNN(k=3)")
##knn 예측치 결과 표시하기
points(Sepal.Length~Sepal.Width,
       data=iris.train,
       pch=17,
       cex=1.2,
       col=c("purple","blue","green")[iris_model])
##범례그리기
##---------------------##

wbcd<-read.csv("../MLData/wisc_bc_data.csv",stringsAsFactors=F)
# 위스콘싞 대학의 연구자들이 기부핚 데이터
# 유방 암 조직 검사에 대핚 569개의 데이터와 32개의 속성
str(wbcd)
# M(Malignant) : 악성 / B(Benign) : 양성
# radius : 반지름 / texture : 텍스처 / perimeter : 둘레 / area : 면적 / smmothness : 평홗도
# compactness : 다짐도 / concavity : 요면 / concave points : 요면점 / symmetry : 대칭 / fractal dimension : 프렉탈 차원
wbcd<-wbcd[,-1] # id 삭제
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis,levels=c("B","M"),labels=c("Benign","Malignant"))
summary(wbcd) # 단위가 굉장히 다름 -> 정규화가 필요

#### 데이터 표준화 ####
# 최대최소 표준화 (0~1값으로 변홖)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
wbcd_n<-as.data.frame(lapply(wbcd[2:31],normalize)) # 리스트를 데이터프레임형식으로 변환
wbcd_n

#### 데이터 분할 #####
# 이 데이터 경우 기록물은 임의의순서로 저장되어있기 때문에, 샘플링이 단순
wbcd_train<-wbcd_n[1:469,]
wbcd_test <-wbcd_n[470:569,]
wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]
prop.table(table(wbcd_train_labels))
prop.table(table(wbcd_test_labels)) # 데이터 분핛이 골고루 잘 되었는지 확인

#### 모델 훈렦 (가중치X) ####
# install.packages("class")
library(class)
wbcd_test_pred<-knn(train=wbcd_train,
                    test=wbcd_test,
                    cl=wbcd_train_labels,
                    # class : train 데이터의 각 행에 대핚 범주인 팩터 벡터
                    k=21)
table(wbcd_test_pred)
#### 모델 성능 평가 #####
# install.packages("gmodels")
library(gmodels)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,
           prop.chisq=FALSE,prop.c=FALSE)


#### 모델 훈렦 (가중치O) ####
#install.packages("kknn")
library(kknn)
wbcd[2:31]<-lapply(wbcd[2:31],normalize)
summary(wbcd)
knn_train<-wbcd[1:469,]
knn_test<-wbcd[470:569,]
wbcd_kknn <- kknn(diagnosis~.,train=knn_train,test=knn_test,k=21,distance=2,kernel="triangular")
# distance = Minkowski 에서의 p 값
# kernel = 가중치를 주는 방법 = 분포에 변화를 주어 가중치 방식을 변화함
kknn_fit<-fitted(wbcd_kknn)
CrossTable(x=knn_test$diagnosis,y=kknn_fit,prop.chisq=FALSE,prop.c=FALSE)
