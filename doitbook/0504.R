library(ggplot2)
library(dplyr)
load("examdata.RData")
ls()

exam <- df_csv_exam
str(exam)
rm(df_exam, df_csv_exam)

# 변수 추출
exam %>% select(math)
exam %>% select(class, math, english)
exam %>% select(-math) %>% head
exam %>% filter(class==1) %>% select(english)

# 실습 138page
new_mpg <- as.data.frame(mpg)
new_mpg <- mpg %>% select(class, cty)
ls(new_mpg)

suv <- new_mpg %>% filter(class=="suv") %>% select(cty)
compact <- new_mpg %>% filter(class=="compact") %>% select(cty)
mean(suv$cty); mean(compact$cty)

# 데이터 정렬 141page
new_mpg <- as.data.frame(mpg)
new_mpg %>% 
  filter(manufacturer=="audi") %>% 
  arrange(desc(hwy)) %>% 
  head(5)

# 파생변수
exam %>% mutate(test=ifelse(science>=60,"pass","fail")) %>% head
exam %>% mutate(total=math+english+science) %>% arrange(total) %>% head

# 144page
df<-as.data.frame(mpg)
df<-df %>% 
  mutate(total=cty+hwy, mean=total/2) %>% 
  arrange(desc(mean)) %>% 
  head(3)

# 집단별로 요약하기
exam %>% group_by(class) %>% 
  summarise(mean_math=mean(math),
            sum_math=sum(math),
            median_math=media(math),
            n=n()) #n()빈도

df<-as.data.frame(mpg)
str(df)
df %>% 
  group_by(manufacturer) %>% 
  filter(class=="suv") %>% 
  mutate(total=(cty+hwy)/2) %>%
  summarise(mean_tot=mean(total)) %>% 
  arrange(desc(mean_tot)) %>% 
  head(5)

# 150page
df %>% 
  group_by(class) %>% 
  summarise(mean_cty=mean(cty)) %>% 
  arrange(desc(mean_cty))

df %>% 
  group_by(manufacturer) %>% 
  summarise(mean_hwy=mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% 
  head(3)

df %>% 
  filter(class=="compact") %>% 
  group_by(manufacturer) %>% 
  summarise(n_manu=n()) %>% 
  arrange(desc(n_manu))

# 데이터합치기
test1 <- data.frame(id=1:5,
                    midterm=c(60,80,70,90,85))
test2 <- data.frame(id=1:5,
                    midterm=c(70,83,65,95,80))
test1
test2

# 열 기준 합치기
total <- left_join(test1,test2,by="id")
total

name <- data.frame(class=1:5, teacher=c("kim","lee","park","choi","jung"))
name
exam
exam_new <- left_join(exam, name, by="class")
exam_new

# 행 합치기
group_a <- data.frame(id=1:5,test=c(60,80,70,90,85))
group_b <- data.frame(id=6:10,test=c(70,83,65,95,80))
group_all <- bind_rows(group_a, group_b)

# 156page
df <- as.data.frame(mpg)
#fl <- unique(df$fl)
fuel <- data.frame(fl = c("c","d","e","p","r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
df <- left_join(df,fuel,by="fl")
df %>% select(model, fl, price_fl) %>% head(5)

# 과제2 160page
library(ggplot2)
library(dplyr)

df <- as.data.frame(midwest)
str(df)

#1
df <- df %>% mutate(ratio=(poptotal-popadults)/poptotal*100)

#2
df %>% arrange(desc(ratio)) %>% select(county, ratio) %>%  head(5) 

#3
df <- df %>% mutate(class=ifelse(ratio>=40,"large",
                    ifelse(ratio>=30,"middle","small")))

table(df$class)

df %>% 
  group_by(class) %>% 
  summarise(n_class = n())

#4
df2 <- df %>% 
  mutate(asian_ratio=(popasian/poptotal)*100) %>% 
  arrange(asian_ratio) %>% 
  select(state,county,asian_ratio) %>% 
  head(10)

df2

# 결측치 정제하기
df <- data.frame(sex = c("M","F", NA, "M","F"),
                 score = c(5,4,3,4,NA))
df
table(is.na(df))
table(is.na(df$sex))
table(is.na(df$score))

df %>% filter(is.na(score))
df_nomiss <- df %>% filter(!is.na(score))
mean(df_nomiss$score)
sum(df_nomiss$score)

df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss
na.omit(df)

mean(df$score, na.rm=T)
sum(df$score, na.rm=T)

exam[c(3,8,15), "math"] <- NA
exam

exam %>% summarise(mean_math=mean(math))
exam %>% summarise(mean_math=mean(math, na.rm=T))

exam$math <- ifelse(is.na(exam$math),55,exam$math)
table(is.na(exam$math))
mean(exam$math)

# 170page
mpg <- as.data.frame(mpg)
mpg[c(65,124,131,153,212), "hwy"] <- NA

table(is.na(mpg$drv))
table(is.na(mpg$hwy)) # 결측치가 5개 있다.

mpg_nomiss <- mpg %>% filter(!is.na(hwy)) 
mpg_nomiss %>% group_by(drv) %>% summarise(mean_hwy=mean(hwy))

# 대표값(평균, 최빈값, 중앙값 등)
# 예측값 추정 대체, 통계분석기법 적용

# 이상치 정제하기
outlier <- data.frame(sex=c(1,2,1,3,2,1),
                      score=c(5,4,3,4,2,6))
outlier
table(is.na(outlier))
table(outlier$sex)
table(outlier$score)

outlier$sex <- ifelse(outlier$sex==3,NA,outlier$sex)
outlier$score <- ifelse(outlier$score>5,NA,outlier$score)

outlier %>% 
  filter(!is.na(sex)&!is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(mean_score=mean(score))

# mpg
boxplot(mpg$hwy)
boxplot(mpg$hwy)$stats
mpg$hwy<-ifelse(mpg$hwy<12|mpg$hwy>37,NA,mpg$hwy)
table(is.na(mpg$hwy))
mpg %>% group_by(drv) %>% 
  summarise(mean_hwy=mean(hwy,na.rm=T))

# 178page
# 구동방식별로 도시연비가 다른지
mpg <- as.data.frame(mpg)
mpg[c(10,14,58,93), "drv"] <- 'k'
mpg[c(29,43,129,203), "cty"] <- c(3,4,39,42)

table((mpg$drv))
mpg$drv <- ifelse(mpg$drv %in% c('4','f','r'),mpg$drv,NA)
table((mpg$drv))

boxplot(mpg$cty)$stats
mpg$cty <- ifelse(mpg$cty<9 | mpg$cty>26,NA,mpg$cty)
boxplot(mpg$cty)$stats

mpg %>% filter(!is.na(drv) & !is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(mean_cty=mean(cty))
