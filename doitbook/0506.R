x<-c(3,5,8,11,13)
y<-1:5
cor(x,y)

head(iris)
str(iris)
help(par)
par(mfrow=c(2,2),mar=c(1,1,1,1))
plot(iris[,-5])
#install.packages("corrplot")
library(corrplot)
iris_cor<-cor(iris[,-5])
corrplot(iris_cor, method="circle")
corrplot(iris_cor, method="ellipse")

corrplot(iris_cor, method="shade", # 색 입힌 사각형
         addshade="all", # 상관관계 방향선 제시
         # shade.col=NA, # 상관관계 방향선 미제시
         tl.col="red", # 라벨 색 지정
         tl.srt=30, # 위쪽 라벨 회젂 각도
         diag=FALSE, # 대각선 값 미제시
         addCoef.col="black", # 상관계수 숫자 색
         order="FPC" # "FPC": First Principle Component 
         # "hclust" : hierarchical clustering 
         # "AOE" : Angular Order of Eigenvectors
         )

library(MASS)
data(survey)
table(survey$Smoke)
smoke<-table(survey$Smoke)
pie(smoke)
barplot(smoke)
table(survey$Sex, survey$Smoke)
hist(mtcars$mpg)
stem(mtcars$hp)

library(ggplot2)
ggplot(BOD, aes(x=BOD$Time, y=BOD$demand)) + geom_line()
ggplot(mtcars, aes(x=mtcars$hp, y=mtcars$wt)) + geom_point()


# 실습 209page
install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare15 <- read.spss("./Data/Koweps_hpc10_2015_beta1.sav", to.data.frame=T)
welfare15 <- raw_welfare15

head(welfare15)
tail(welfare15)
View(welfare15)
dim(welfare15)
str(welfare15)
summary(welfare15)

welfare15 <- rename(welfare15, 
                  sex=h10_g3,
                  birth=h10_g4,
                  marriage=h10_g10,
                  religion=h10_g11,
                  income=p1002_8aq1,
                  code_job=h10_eco9,
                  code_region=h10_reg7)

# 성별과 월급의 관계
## 변수검토 및 전처리
attach(welfare15)
class(sex)

### 이상치 확인
table(sex)

### 이상치 결측 처리
welfare15$sex <- ifelse(welfare15$sex==9, NA, welfare15$sex)
### 결측치 확인
table(is.na(sex))

welfare15$sex <- ifelse(welfare15$sex==1, "male","female")
table(sex)
qplot(sex)

class(income)
summary(income)
qplot(income) + xlim(0,1000)
# 0~250만 원 사이에 가장 많은 사람이 분포하고, 그 뒤로 점차 빈도가 감소



### 이상치 확인
summary(income)
### 이상치 결측 처리
welfare15$income <- ifelse(welfare15$income %in% c(0,9999), NA, welfare15$income)
### 결측치 확인
table(is.na(income))

## 변수 간 관계 분석
# 성별에 따른 월급
sex_income15 <- welfare15 %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income=mean(income))
sex_income15

ggplot(sex_income15, aes(x=sex,y=mean_income))+geom_col()
# 남성의 월급이 여성의 두 배 가까이 될 정도로 많다.

# 나이와 월급의 관계
## 변수검토 및 전처리

class(birth)
### 이상치 확인
summary(birth)
qplot(birth)
### 결측치 확인
table(is.na(birth))

### 이상치 결측 처리
welfare15$birth <- ifelse(welfare15$birth==9999, NA, welfare15$birth)
table(is.na(birth))

### 파생변수 만들기 - 나이
welfare15$age <- 2015-welfare15$birth+1
summary(welfare15$age)
qplot(welfare15$age)

detach(welfare15)

## 변수 간 관계 분석
### 나이에 따른 월급 평균표
age_income15 <- welfare15 %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income=mean(income))
head(age_income15)

ggplot(age_income15, aes(x=age, y=mean_income))+geom_line()
# 20대 초반에 100만 원가량의 월급, 이후 증가 추세
# 50대 무렵 300만 원 초반대로 가장 많은 월급을 받고,
# 그 이후 감소추세, 70대 이후에는 20대보다 낮은 월급

ggplot(sex_income15, aes(x=sex,y=mean_income))+geom_col()
ggplot(age_income15, aes(x=age, y=mean_income))+geom_line()
ggplot(sex_income20, aes(x=sex,y=mean_income))+geom_col()
ggplot(age_income20, aes(x=age, y=mean_income))+geom_line()

#---
which(age_income15$mean_income==max(age_income15))
age_income15[34,]

which(age_income20$mean_income==max(age_income20))
age_income20[26,]
#---

# 연령대에 따른 월급 차이
## 연령대 파생변수
welfare15 <- welfare15 %>% 
  mutate(ageg=ifelse(age<30,"young",
                     ifelse(age<=59, "middle","old")))
table(welfare15$ageg)
qplot(welfare15$ageg)

## 연령대별 월급 평균표
ageg_income15 <- welfare15 %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income15=mean(income))
ageg_income15

ggplot(ageg_income15, aes(x=ageg,y=mean_income15))+
  geom_col()+
  scale_x_discrete(limits=c("young","middle","old"))

# 성별 월급 차이는 연령대별로 다른지
## 연령대 및 성별 월급 평균표
sex_income15 <- welfare15 %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg,sex) %>% 
  summarise(mean_income15=mean(income))

ggplot(sex_income15, aes(x=ageg, y=mean_income15, fill=sex))+
  geom_col(position="dodge")+
  scale_x_discrete(limits=c("young","middle","old"))

# 나이 및 성별 월급 차이
sex_age15 <- welfare15 %>% 
  filter(!is.na(income)) %>% 
  group_by(age,sex) %>% 
  summarise(mean_income15=mean(income))
head(sex_age15)

ggplot(sex_age15,aes(x=age,y=mean_income15,col=sex)) + geom_line()

# 233page