data(cars)
str(cars)
m <- lm(dist~speed, data=cars)
summary(m)
#Residuals 확인
#median이 0에 가깝게 안나오면 잔차에 대하여 정규성을 안따를 가능성이 높다
#계수값, 각 변수가 유의미한지 t검정으로 확인 
#(오차에 대한 평균검정) H0: beta1=0 차이가 없다, H0: beta0=0
#집단비교를 위한 F검정 평균적인 오차에 대한 차이가 있는지 알기 위해 검정
#H0: 회귀식이 유의하지 않다.y=beta0 vs y=beta0+beta1*x
#설명력은 결정계수로 판단, 변수가 여러개 있으면 조정된결정계수로 판단

coef(m)
fitted(m)[1:4]
residuals(m)[1:4]
par(mfrow=c(2,2), mar=c(3,3,3,3))
plot(m)
#main = "'fg' : axes, ticks and box in gray",fg = gray(0.7), bty = "7"

par(mfrow=c(1,1))
plot(cars$speed,cars$dist)
abline(coef(m))

head(iris)
m2 <- lm(Sepal.Length ~ .-Species, data=iris)
summary(m2)
#sepla.length = 1.856 + sepla.width0.6 + petal.lenth0.7 - petal.width0.5
#medianS
#변수 t검정
#경정계수
# 잔차
# F검정
# 데이터가 굉장히 선형적이다. 
m3 <- lm(Sepal.Length ~ ., data=iris)
summary(m3)
table(iris$Species)

#오후
library(mlbench)
data("BostonHousing")
str(BostonHousing)

m<-lm(medv ~ .,data=BostonHousing)
summary(m)
m2<-step(m, direction="both")
