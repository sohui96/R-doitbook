#data
child <- c(520,498, 481, 512, 515, 542, 520, 518, 527, 526)

#모평균에 대한 95% 신뢰구간을 구하는 사용자 정의 함수
#표본수가 적으니 t분포 사용
#child: 표본으로부터 관찰된 자료들의 벡터
#alpha:유의수준(default=0.05)

#this!
f1 <- function(data, alpha){
  
  mu <- mean(data)
  n <- length(data)
  t <- c(1,-1)*qt(alpha/2, df=(n-1))
  
  interval <- mu + t*(sd(data)/sqrt(n))
  
  cat(" 모평균에 대한 95% 신뢰구간:", interval,
      "\n","유의수준:" ,alpha)
}


f2 <- function(data, alpha){
  
  n <- length(data)
  mu <- mean(data)
  #sigma <- sqrt(sum((data-mu)^2)/n)
  sigma <- sd(data)/sqrt(n)
  
  t <- c(1,-1)*qt(alpha/2, df=(n-1))
  interval <- mu + t*sigma 
  
  return(interval)
}

#thinking
f3 <- function(data, alpha){
  
  n <- length(data)
  mu <- mean(data)
  sigma <- sqrt(sum((data-mu)^2)/n)
  
  t <- c(1,-1)*qt(alpha/2, df=(n-1))
  interval <- mu + t*sigma/sqrt(n) 
  
  return(interval)
}
