# [과제14][본인이름] ggplot2를 이용한 iris 데이터 상세 시각화해보기

data(iris)

ggplot(data=iris, mapping=aes(x=Sepal.Length, y = Sepal.Width)) +
  geom_point(colour=c("red", "blue", "hotpink")[iris$Species], pch=19, size=2) +
  annotate(geom="text", x=iris$Sepal.Length, y=iris$Sepal.Width, 
           label=rownames(iris), colour="black", alpha=0.8, size=2, hjust=0.5, vjust= -1)

ggplot(iris) +
  geom_point(aes(x=Sepal.Length, y = Sepal.Width, col = Species, size = 2, shape = Species))+
  annotate(geom="text", x=iris$Sepal.Length, y=iris$Sepal.Width, 
           label=rownames(iris), colour="black", alpha=0.8, size=3, hjust=0.5, vjust= -1)+
  theme_bw()

# 연관성 규칙
library(arules)
library(arulesViz)
data("Groceries")
str(Groceries)

inspect(Groceries[1:10],)
summary(Groceries)

sort(itemFrequency(Groceries, type="absolute"), decreasing = T)
itemFrequencyPlot(Groceries, topN=10, type="absolute")
itemFrequencyPlot(Groceries, topN=10, type="absolute")

result_rules = apriori(Groceries,
                       parameter=list(support=0.005, confidence=0.5,minlen))