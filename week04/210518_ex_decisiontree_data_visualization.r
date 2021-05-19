### 과제1 의사결정트리 실습문제

df_skin <- read.csv('./skin.csv', header = T)
str(df_skin)
# 'data.frame':	30 obs. of  7 variables:
# $ 고객번호    : int  1 2 3 4 5 6 7 8 9 10 ...
# $ 성별        : chr  "남" "여" "여" "여" ...
# $ 나이        : chr  "30대" "20대" "20대" "40대" ...
# $ 직장여부    : chr  "NO" "YES" "YES" "NO" ...
# $ 결혼여부    : chr  "YES" "YES" "YES" "NO" ...
# $ 차량보유여부: chr  "NO" "YES" "NO" "NO" ...
# $ 쿠폰반응여부: chr  "NO" "NO" "NO" "NO" ...

df_skin <- df_skin[-1] # 1번째 idx 빼고 저장 (고객번호)
head(df_skin)
# 성별 나이 직장여부 결혼여부 차량보유여부 쿠폰반응여부
# 1   남 30대       NO      YES           NO           NO
# 2   여 20대      YES      YES          YES           NO
# 3   여 20대      YES      YES           NO           NO
# 4   여 40대       NO       NO           NO           NO
# 5   여 30대       NO      YES           NO           NO
# 6   여 30대       NO       NO          YES           NO

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

tree1 <- rpart(쿠폰반응여부 ~ ., data=df_skin, control=rpart.control(minsplit = 2))
plot(tree1, compress = T, uniform = T, margin=0.1)
text(tree1, use.n = T, col = "blue")
tree1

# 오류가 존재하지 않을 때 까지 가지 생성

# n= 30 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 30 12 NO (0.6000000 0.4000000)  
# 2) 결혼여부=NO 10  0 NO (1.0000000 0.0000000) *
#   3) 결혼여부=YES 20  8 YES (0.4000000 0.6000000)  
#   6) 나이=20대,30대 16  8 NO (0.5000000 0.5000000)  
#   12) 성별=남 5  1 NO (0.8000000 0.2000000)  
#   24) 직장여부=NO 3  0 NO (1.0000000 0.0000000) *
#     25) 직장여부=YES 2  1 NO (0.5000000 0.5000000)  
#     50) 나이=20대 1  0 NO (1.0000000 0.0000000) *
#       51) 나이=30대 1  0 YES (0.0000000 1.0000000) *
#         13) 성별=여 11  4 YES (0.3636364 0.6363636)  
#         26) 나이=20대 5  2 NO (0.6000000 0.4000000)  
#         52) 직장여부=YES 2  0 NO (1.0000000 0.0000000) *
#           53) 직장여부=NO 3  1 YES (0.3333333 0.6666667)  
#           106) 차량보유여부=NO 1  0 NO (1.0000000 0.0000000) *
#             107) 차량보유여부=YES 2  0 YES (0.0000000 1.0000000) *
#               27) 나이=30대 6  1 YES (0.1666667 0.8333333)  
#               54) 직장여부=NO 2  1 NO (0.5000000 0.5000000)  
#               108) 차량보유여부=NO 1  0 NO (1.0000000 0.0000000) *
#                 109) 차량보유여부=YES 1  0 YES (0.0000000 1.0000000) *
#                   55) 직장여부=YES 4  0 YES (0.0000000 1.0000000) *
#                     7) 나이=40대 4  0 YES (0.0000000 1.0000000) *
# 

xtabs(~결혼여부 + 쿠폰반응여부, data=df_skin)

# 쿠폰반응여부
# 결혼여부 NO YES
# NO  10   0
# YES  8  12

chisq.test(xtabs(~결혼여부+쿠폰반응여부, data=df_skin))
# 카이제곱 검정 결과 p-value > 0.05, 귀무가설 기각
# 즉, 카이제곱을 따르지 않는다.
# 변량 간의 영향이 있다. (독립이 아님)


# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  xtabs(~결혼여부 + 쿠폰반응여부, data = df_skin)
# X-squared = 7.6562, df = 1, p-value = 0.005658
# 
fancyRpartPlot(tree1)

### 과제2 Doit R 시각화

data("USArrests")
library(rpart)
#install.packages("mapproj")
#install.packages("ggiraphExtra")
library(ggiraphExtra)

str(USArrests)
head(USArrests)
library(tibble) # 행이름을 변수로 바꿔 새 데이터 프레임 만들기
crime <- rownames_to_column(USArrests, var = "state")
crime$state <- tolower(crime$state)
str(crime)

# 지도 데이터 준비
#install.packages("maps")
library(ggplot2)
states_map <- map_data("state")
str("states_map")

#단계구분도
?ggChoropletth
ggChoropleth(data = crime, aes(fill = Murder, map_id = state), map = states_map)
ggChoropleth(data = crime, aes(fill = Murder, map_id = state), map = states_map, interactive = T)
