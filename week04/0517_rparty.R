data("USArrests")
# statistics about violent crime rates by us state.
# Murder: Murder arrests (per 100,000) :살읶
# Assault: Assault arrests (per 100,000) : 폭행
# UrbanPop: Percent urban population # target으로 사용
# Rape: Rape arrests (per 100,000) : 강갂
head(USArrests)
nrow(USArrests) # 50건
# install.packages("rpart")
library(rpart) #load the rpart package #CART 알고리즘적용
t1 <- rpart(UrbanPop ~ ., data = USArrests)
# Anova, Poisson, Exponential 등을 split function으로 선택해 사용 가능
t1
hist(USArrests$UrbanPop) # 타겟의 값 분포를 확읶
plot(sort(USArrests$UrbanPop))
plot(t1, margin=.2)
text(t1, cex=0.8)

# 트리 플롯 그리기
#install.packages("rattle")
#install.packages("rpart.plot")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
rpart.plot(t1) # 트리 플롯 그리기 기본형
fancyRpartPlot(t1)

# 살읶에 비해 강갂비율이 높은 주가 읶구 맋음
plot(USArrests$Rape, USArrests$Murder, 
     col=ifelse(USArrests$UrbanPop > median(USArrests$UrbanPop),
                "blue","lightblue"),pch=19)
#66%확률로 assault, murder 분류
#rape가 17.55보다 작은 경우가 58%확률로 50건 중 21건, 나머지 확률 오분류
#rape < 17.55이면서 auuault가 94보다 작은 경우가 50%확률로 9건, 큰 경우가 12건이다.

#rape가 17.55보다 큰 경우가 50건 중 29건 
#rape >= 17.55이면서 murderdl 12.45 이상인 경우가 29건중 7건, 미만인 경우가 22건이다.
#rape >= 17.55, murder < 12.45 이면서 rape가 21.2보다 작은 경우가 22건 중 7건, 이상인 경우가 15건이다.
