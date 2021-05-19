# 210518_PCA_Principal_component_analysis
# vector가 갖고있는 기초통계 분산 값이 클 수록 해당 데이터 설명을 잘하는 성분으로 간주
# 위 경우 w 가중치 값을 크게 만든다.
# ex) 위 자동차 변량(핸들모양, 차의 무게) 중 차의 무게 분산 값이 가장 크다면
# 차 무게의 가중치 값을 크게 하여
# 차종에 대한(차종,차의 무게) 2차원 정보를 -> 1차원 직선으로 표기한다?

# [05_주성분분석.pdf] p3
# 행렬식 표기 Xw = T
# Var(X)가 최대가 되는 w 찾기
# x11w1 + x12w2+ .... + x1pwp = t1

# 연습
x <- data.frame(c(1,2),c(3,4))
x
w <- c(2,2)
x*w


# [05_주성분분석.pdf] p11
x1<-c(26,46,57,36,57,26,58,37,36,56,78,95,88,90,52,56)
x2<-c(35,74,73,73,62,22,67,34,22,42,65,88,90,85,46,66)
x3<-c(35,76,38,69,25,25,87,79,36,26,22,36,58,36,25,44)
x4<-c(45,89,54,55,33,45,67,89,47,36,40,56,68,45,37,56)


score <- cbind(x1,x2,x3,x4)
score
#      x1 x2 x3 x4
# [1,] 26 35 35 45
# [2,] 46 74 76 89
# [3,] 57 73 38 54
# [4,] 36 73 69 55
# [5,] 57 62 25 33
# [6,] 26 22 25 45
# [7,] 58 67 87 67
# [8,] 37 34 79 89
# [9,] 36 22 36 47
# [10,] 56 42 26 36
# [11,] 78 65 22 40
# [12,] 95 88 36 56
# [13,] 88 90 58 68
# [14,] 90 85 36 45
# [15,] 52 46 25 37
# [16,] 56 66 44 56

colnames(score) <-c("국어","영어","수학","과학")
rownames(score)<-1:16
head(score)


result <- prcomp(score)
result

# Standard deviations (1, .., p=4):
#   [1] 30.122748 27.052808  9.076140  6.152386
# 
# Rotation (n x k) = (4 x 4): 가중치 w1, w2, w3, w4
#   PC1         PC2        PC3
# 국어 0.6093268 -0.39286407 -0.6126773
# 영어 0.7185749 -0.09337973  0.6200124
# 수학 0.2624323  0.73573272  0.1052861
# 과학 0.2085672  0.54372366 -0.4786711
# PC4
# 국어 -0.3146508
# 영어  0.3008572
# 수학 -0.6154198
# 과학  0.6570680

?prcomp
# Principal Components Analysis
# Performs a principal components analysis on the given data matrix and returns the results as an object of class prcomp.

summary(result)
# Importance of components:
# PC1 
# Standard deviation     30.1227  # 표준편차 
# Proportion of Variance  0.5157  # 분산 비율
# Cumulative Proportion   0.5157  # 설명력, 기여도, 약 52% 설명
# PC2
# Standard deviation     27.0528
# Proportion of Variance  0.4159
# Cumulative Proportion   0.9317  # 누적된 결과이므로 PC1과 PC2의 설명력
# PC3
# Standard deviation     9.07614
# Proportion of Variance 0.04682
# Cumulative Proportion  0.97849
# PC4
# Standard deviation     6.15239
# Proportion of Variance 0.02151
# Cumulative Proportion  1.00000



biplot(result)
?biplot
screeplot(result, npcs=4, type="lines", main="score")
