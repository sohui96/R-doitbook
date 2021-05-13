data(iris)
str(iris)
result <- lm(Petal.Length~Petal.Width, data=iris)
result
summary(result)

# 추정회귀식 Petal.Length = 2.22994*Petal.Width + 1.08356
# 변수 유의성 확인
# H0: 변수가 유의하지 않다. H1: not H0
# 유의수준 0.05에서, Petal.Width의 p-value가 유의수준 0.05보다 작기 때문에 귀무가설 기각. 이 변수는 유의하다.
# 잔차: 0.4782
# 결정계수: 92.71%, 1에 가까울수록 변수 설명력이 강하다.
# H0: 위 모형(회귀식) 유의하지 않다. H1: 모형유의하다.
# F검정통계량: 1882, df=148
# 유의수준 0.05에서, p-value가 유의수준 0.05보다 작기 때문에 귀무가설 기각. 모형 유의하다. 
