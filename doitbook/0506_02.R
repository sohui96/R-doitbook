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
welfare20$sex <- ifelse(welfare20$sex==9, NA, welfare20$sex)
### 결측치 확인
table(is.na(sex))

welfare20$sex <- ifelse(welfare20$sex==1, "male","female")
table(sex)
#qplot(sex)

class(income)
summary(welfare20$income)
qplot(welfare20$income) + xlim(0,1000)
# 0~250만 원 사이에 가장 많은 사람이 분포하고, 그 뒤로 점차 빈도가 감소

### 이상치 확인
summary(income)
### 이상치 결측 처리
welfare20$income <- ifelse(welfare20$income %in% c(0,9999), NA, welfare20$income)
### 결측치 확인
table(is.na(welfare20$income))

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
welfare20$birth <- ifelse(welfare20$birth==9999, NA, welfare20$birth)
table(is.na(birth))

### 파생변수 만들기 - 나이
welfare20$age <- 2020-welfare20$birth+1
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








# 연령대에 따른 월급 차이
## 연령대 파생변수
welfare20 <- welfare20 %>% 
  mutate(ageg=ifelse(age<30,"young",
                     ifelse(age<=59, "middle","old")))
table(welfare20$ageg)
qplot(welfare20$ageg)

## 연령대별 월급 평균표
ageg_income20 <- welfare20 %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income20=mean(income))
ageg_income20

ggplot(ageg_income20, aes(x=ageg,y=mean_income20))+
  geom_col()+
  scale_x_discrete(limits=c("young","middle","old"))

# 성별 월급 차이는 연령대별로 다른지
## 연령대 및 성별 월급 평균표
sex_income20 <- welfare20 %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg,sex) %>% 
  summarise(mean_income20=mean(income))

ggplot(sex_income20, aes(x=ageg, y=mean_income20, fill=sex))+
  geom_col(position="dodge")+
  scale_x_discrete(limits=c("young","middle","old"))

# 나이 및 성별 월급 차이
sex_age20 <- welfare20 %>% 
  filter(!is.na(income)) %>% 
  group_by(age,sex) %>% 
  summarise(mean_income20=mean(income))
head(sex_age20)

ggplot(sex_age20,aes(x=age,y=mean_income20,col=sex)) + geom_line()

# 233page