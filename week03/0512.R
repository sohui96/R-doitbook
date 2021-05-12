setwd("./가설검정_book/Part-III")
data <- read.csv("cleanDescriptive.csv", header=T)
head(data)
x<-data$level2
y<-data$pass2
result <- data.frame(Level=x, Pass=y)
dim(result)

#교차분석
table(result)

install.packages("gmodels")
library(gmodels)
library(ggplot2)

#CrossTable(x=diamonds$color, y=diamonds$cut)
CrossTable(x,y)
CrossTable(x,y,chisq=T)



#--214page----------------------------------------------------------------------
data <- read.csv("three_sample.csv", header=T)
head(data)

#결측치, 이상치 제거
data<-subset(data, !is.na(score), c(method,score))
head(data)

#데이터 분포 확인
plot(data$score)
barplot(data$score)
mean(data$score)

#데이터 정제
length(data$score)
data2 <- subset(data,score<=14)
length(data2$score)

#확인
x<-data2$score
boxplot(x)

#세 집단
data2$method2[data2$method==1] <- "방법1"
data2$method2[data2$method==2] <- "방법2"
data2$method2[data2$method==3] <- "방법3"

x<-table(data2$method2)
y<-tapply(data2$score, data2$method2, mean)
y

df<-data.frame(교육방법=x, 성적=y)
df

#세 집단 간 동질성 검정
bartlett.test(score ~ method, data=data2)

#분산분석 세 집단 간 평균 차이검정
result <- aov(score ~ method2, data=data2)
names(result)
summary(result)

#사후검정
TukeyHSD(result)
plot(TukeyHSD(result))

#--219page----------------------------------------------------------------------
data4 <- read.csv("twomethod.csv", header=T)
head(data4)

#결측치 제거
data4 <- subset(data4, !is.na(score), c(method,score))
head(data4)

#데이터 분포 확인
plot(data4$score)
barplot(data4$score)
mean(data4$score)

data4$method2[data4$method==1] <- "방법1"
data4$method2[data4$method==2] <- "방법2"

x<-table(data4$method2)
y<-tapply(data4$score, data4$method2, mean)
y

df4<-data.frame(교육방법=x, 성적=y)
df4

#집단 간 동질성 검정
bartlett.test(score ~ method, data=data4)


#분산분석 세 집단 간 평균 차이검정
result <- aov(score ~ method2, data=data4)
names(result)
summary(result)

#사후검정
TukeyHSD(result)
plot(TukeyHSD(result))
