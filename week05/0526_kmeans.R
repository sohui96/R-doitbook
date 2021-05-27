#계층형 군집
library(cluster)
academy <- read.csv("./MLData/academy.csv")
glimpse(academy)

hcl <- hclust(dist(academy)^2, method="single")
plot(hcl, hang=-1, xlab="strudent", ylab="distance")

#K-means군집
library(graphics)
kms <- kmeans(academy,5)
kms
#cluster means 5개 군집의 중심, plot에서 보였던 뭉쳐있는 부분 5개
#clustering vector 각 학생의 소속 군집 번호
#within cluster sum of squares of cluster 5개 변수 군집 중심과 소속점 들 간의 거리 제곱의 합

#kms$withinss 최적의 군집의 개수를 구하는 방법
#elbow point 구하기
wss <- 0

#k를 1~10까지 변화시키면서 각 withinss 값을 wss에 저장
for(i in 1:10) wss[i] <- sum(kmeans(academy, centers=i)$withinss)
wss

#기울기가 급격하게 완만해지는 부분이 보통 elbow point
plot(1:10, wss, type='b', main="Withinss", xlab="numer of clusters", ylab="within group sum of squares", col=2)
abline(v=6)

#군집분석 시각화 5개 변수 중 2개의 조합이니 경우의 수 20가지
plot(academy, col=kms$cluster)


