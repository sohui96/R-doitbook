#실전분석문제풀이
#적용알고리즘1 다중선형회귀분석
#성적과 IQ간의 관계 분석 그리고 IQ가 125일 경우 예상 점수 예측

#데이터수집 및 저장
score <- read.csv("./score.txt", header=T, sep=",")
score
attach(score);

#lm()
lm1 <- lm(성적~IQ) #단순
summary(lm1)

coef(lm1)
fitted(lm1)[1:4]
residuals(lm1)[1:4]
par(mfrow=c(2,2), mar=c(3,3,3,3))
plot(lm1)

##Residuals 확인
# Residuals 잔차의 분포를 확인해 보면 medians이 0에 가까워 보인다. 이상치가 조금 보이는 것 같다.
# 정규성도 만족해보인다.
## 각 변수가 유의미한지 t검정으로 확인 
# 유의수준 5%에서 IQ 변수는 p-value가 0.05보다 작다. 따라서 유의미한 변수이다.
# 결정계수는 0.8285로 1에 가깝다고 판단, 설명력이 있어보인다.
## 회귀식: y = -5.2918 + 0.6714*IQ (H0: y = -5.2918)
# F검정, 유의수준 5%에서 p-value가 0.05보다 작다. 따라서 유의미한 모형이다.


#회귀식
x = 125
y = coef(lm1)[[2]] * x + coef(lm1)[[1]] # IQ가 125일 경우 예상 점수 계산
y

plot(IQ,성적, pch=20, col="red")
abline(lm1,col="blue")

#예측하기 predict()
coef(lm1)
predict(lm1, newdata=data.frame(rep(125,10)))
#각 학생별로 회귀식을 적용해서 IQ가 125일 경우 받을 예상 점수를 의미

lm2 <- lm(성적 ~ 다니는학원수) #단순
summary(lm2)
##Residuals 확인
# Residuals 잔차의 분포를 확인해 보면 medians이 0에 가깝고 고른것 같다.
## 각 변수가 유의미한지 t검정으로 확인 
# 유의수준 5%에서 다니는학원수 변수는 p-value가 0.05보다 작다. 따라서 유의미한 변수이다.
# 결정계수는 0.6147로 IQ변수만 있는 모형보다는 상대적으로 설명력이 약해보인다.
## 회귀식: y = 69.488 + 4.953*다니는학원수 (H0: y = 69.488)
# F검정, 유의수준 5%에서 p-value가 0.05보다 작다. 유의미한 모형이다.

lm3 <- lm(성적 ~ .-이름, data=score) #선형회귀분석
summary(lm3)
##Residuals 확인
# Residuals 잔차의 분포를 확인해 보면 고른것 같다.
## 각 변수가 유의미한지 t검정으로 확인 
# 유의수준 5%에서 IQ 변수는 p-value가 0.05보다 작다. 따라서 유의미한 변수이다. 이 외 변수는 모두 유의하지 않았다.
# 조정된결정계수로 보면 0.9295로 1에 가깝다, 설명력이 강하다.
## 회귀식: y = -5.2918 + 0.6714*IQ (H0: y = -5.2918)
# F검정, 유의수준 5%에서 p-value가 0.05보다 작다. 따라서 유의미한 모형이다.

###
step(lm3)
lm4<-lm(성적 ~ IQ+게임하는시간+TV시청시간, data=score)
summary(lm4)
lm5<-lm(성적 ~ IQ+TV시청시간, data=score)
summary(lm5)
par(mfrow=c(2,2), mar=c(3,3,3,3))
plot(lm5)
###

detach(score)
