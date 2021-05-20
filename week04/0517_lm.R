#오전 - 진도수업(머신러닝)

#오후
#2시간 - 과제풀이
#2시간 - 조별과제체크

## 코스피 지수 수익률과 개별주식의 수익률 상관관계_ lm
k_index <- read.csv("./MLData/K_index.csv", header=T, stringsAsFactors=F)
s_stock <- read.csv("./MLData/S_stock.csv", header=T, stringsAsFactors=F)
h_stock <- read.csv("./MLData/H_stock.csv", header=T, stringsAsFactors=F)
all_data <- merge(merge(k_index, s_stock), h_stock)
head(all_data)
str(all_data)

all_data$idx <- 1:249
attach(all_data)
plot(idx[2:100], k_rate[2:100], type="I", xlab="date", ylab="rate", xlim=c(0,100))
lines(all_data$idx[2:100], s_rate[2:100], col="blue")
abline(h=mean(k_rate[2:100]),lty=2, col="black")
abline(h=mean(s_rate[2:100]),lty=2, col="blue")
plot(k_rate, s_rate)

s_lm <- lm(s_rate ~ k_rate, data=all_data) # 삼성전자지수=w*코스피지수
h_lm <- lm(h_rate ~ k_rate, data=all_data)

summary(s_lm)
summary(h_lm)

detach(all_data)


## 지각모델_ glm 범주형
drink <- read.csv("./MLData/drink.csv", header=T,stringsAsFactors=F)
drink
str(drink)
drink$결혼여부 <- as.factor(drink$결혼여부)
drink$자녀여부 <- as.factor(drink$자녀여부)
drink$체력 <- as.factor(drink$체력)
drink$주량 <- as.factor(drink$주량)
drink$직급 <- as.factor(drink$직급)
drink$성별 <- as.factor(drink$성별)
drink$지각여부 <- as.factor(drink$지각여부)

attach(drink)
library(class)

m <- glm(지각여부~., family=binomial(link=logit), data=drink)
m
# Coefficients해석
# 기준컬럼을 대상으로 승산비율에 대한 log값 출력
# +: 기준 컬럼에 비해 해당 컬럼이 승산비율이 높다
# -: 기준 컬럼에 비해 해당 컬럼이 승산비율이 낮다

# 오즈비해석
predict(m, drink, type="response")
predict(m, drink, type="response") >= 0.5
table(drink$지각여부, predict(m, drink, type="response") > 0.5)
summary(m)

detach(drink)