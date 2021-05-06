# 실습 209page
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_welfare20 <- read.spss("./Data/Koweps_hpda15_2020_beta1.1.sav", to.data.frame=T)
welfare20 <- raw_welfare20

head(welfare20)
tail(welfare20)
View(welfare20)
dim(welfare20)
str(welfare20)
summary(welfare20)

welfare20 <- rename(welfare20, 
                  sex=h15_g3,
                  birth=h15_g4,
                  marriage=h15_g10,
                  #religion=h15_g11,
                  income=p1502_8aq1,
                  #code_job=h15_eco9,
                  code_region=h15_reg7)


# 성별과 월급의 관계
## 변수검토 및 전처리
attach(welfare20)
class(sex)

### 이상치 확인
table(sex)

### 이상치 결측 처리
welfare20$sex <- ifelse(sex==9, NA, sex)
### 결측치 확인
table(is.na(sex))

welfare20$sex <- ifelse(sex==1, "male","female")
table(sex)
#qplot(sex)

class(income)
summary(income)
qplot(income) + xlim(0,1000)
# 0~250만 원 사이에 가장 많은 사람이 분포하고, 그 뒤로 점차 빈도가 감소

### 이상치 확인
summary(income)
### 이상치 결측 처리
welfare20$income <- ifelse(income %in% c(0,9999), NA, income)
### 결측치 확인
table(is.na(income))

## 변수 간 관계 분석
# 성별에 따른 월급
sex_income20 <- welfare20 %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income=mean(income))
sex_income20

ggplot(sex_income20, aes(x=sex,y=mean_income))+geom_col()
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
birth <- ifelse(birth==9999, NA, birth)
table(is.na(birth))

### 파생변수 만들기 - 나이
welfare20$age <- 2020-birth+1
summary(welfare20$age)
qplot(welfare20$age)

detach(welfare20)

## 변수 간 관계 분석
### 나이에 따른 월급 평균표
age_income20 <- welfare20 %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income=mean(income))
head(age_income20)

ggplot(age_income20, aes(x=age, y=mean_income))+geom_line()
# 20대 초반에 100만 원가량의 월급, 이후 증가 추세
# 50대 무렵 300만 원 초반대로 가장 많은 월급을 받고,
# 그 이후 감소추세, 70대 이후에는 20대보다 낮은 월급 