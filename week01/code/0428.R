Sys.Date()
Sys.time()
date()

x <- '2021-04-26'
class(x)
y <- as.Date(x)
class(y)

x+10 # 문자열+숫자 x
y+18 # 날짜+숫자 o

#------------------------# 
val1 <- 30
val2 <- 40
if(val1>val2){
  cat("큰 수는=", val1)
}else{
  cat("큰 수는=", val2)
}

ifelse(val1 > val2, val1, val2)
# if > else if > else

#------------------------# 
emp_name = scan(what=character())
switch(emp_name, 
       "홍길동"=30,
       "김길동"=40)
# 길이 1 벡터, 그 이상은 확인안됨?

name = c("홍길동","강감찬","김길동")
which(name == "강감찬")

for(n in name){
  print(n)
}

#------------------------# 
a<-10; b<-20

myMax <- function(max,b){
  
  if(max < b){
    max = b
  }
  cat("max =", max)
  return(max)
}

result=myMax(a,b)

#------------------------# 
mySum = function(...){
  
  return(sum(...))
}

mySum(10,20,30,40,50)

#------------------------# 
myFunc <- function(name, age, dept){
 
  cat("이름:",name,"\n")
  cat("나이:",age,"\n")
  cat("학과:",dept)
}

myFunc("홍길동", 35,"컴퓨터")

#------------------------# 
?c

vec1 <- c(1:5)
length(vec1)
vec1 <- c(vec1,7)
vec1

vec1[9] <- 9
vec1

append(vec1, 10, after=3)
#------------------------# 

a <- as.Date("2015-01-01")
b <- as.Date("2015-01-31")
date4 <- seq(a,b,1)
date4

vec1 <- c("사과","배","감","버섯","고구마")
vec1[-3]
