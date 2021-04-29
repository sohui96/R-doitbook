y <- c(1,2,3,4,5)
x <- c("1", 2, "3")

names(x) <- c("kim","seo","park")
x[-1]

length(x)
x[c(1,3)]
x[1:2]
nrow(x)
NROW(x)

identical(x,y)
union(x,y)
intersect(x,y)
setdiff(x,y)
setequal(x,y[-1])

x<-c(1:5)
y<-c(1:5)

5 %in% x
x + 1

identical(c(1,2,3),c(1,2,3))
identical(c(1,2,3),c(1,2,100))
"a" %in% c("a","b","c")
"d" %in% c("a","b","c")  

c(1,2,3) == c(1,2,100)

union(c("a","b","c"),c("a","d"))
intersect(c("a","b","c"),c("a","d"))
setdiff(c("a","b","c"),c("a","d"))
setequal(c("a","b","c"),c("a","d"))

seq(1,5,1)
seq_along(along.with=2)

rep(1:2,times=5)
rep(1:2,each=5)
rep(1:2,each=5,times=2)

x <- list(name="foo", height=70)
x


x <- matrix(1:9, nrow=3)
matrix(1:9, nrow=3,byrow=TRUE)
matrix(1:9, nrow=3,
       dimnames=list(c("r1","r2","r3"),c("c1","c2","c3")))

x * 2
x - x
x %*% x 
x <- matrix(1:4, ncol=2)
x %*% solve(x)
dim(x)

array(1:12, dim=c(3,4))
x <- array(1:12, dim=c(2, 2, 3))
x[,,3]
dim(x)

# 데이터프레임
grade <- data.frame("성명"=c("홍길동", "김길동", "박길동"),
                    "국어"=c(80,97,85),
                    "영어"=c(94,100,97))

d <- data.frame(x=c(1, 2, 3, 4, 5), 
                y=c(2, 4, 6, 8, 10),
                z=c('M', 'F', 'M', 'F', 'M'),
                stringsAsFactors = T)
d$colname <- y
d$w <- c("A", "B", "C", "D", "E")

d <- data.frame(a=1:3, b=4:6, c=7:9)
d[, names(d) %in% c("b", "c")]
d[,1]
d[,1,drop=F]

# 유틸리티
d <- data.frame(x=1:1000)
head(d)
tail(d)
View(d)

# r > 데이터분석 > 통계 > 선형대수