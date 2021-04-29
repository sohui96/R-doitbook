rm(list=ls())
# 텍스트 분석 실습

data1 <-readLines("./data/seoul_new.txt")
data1
class(data1)  #[1] "character" 문자열 벡터구조인 것을 확인
length(data1) #[1] 305
data1[1]

# JAVA 설치 후 환경변수 설정 그 후에 인스톨
install.packages("multilinguer")
library(multilinguer)
## 왜하냐면 r은 또 java기반.. java를 설치해줘야한다. java, rJava 관련한 라이브러리
install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type = "binary")
devtools::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
#install.packages("KoNLP")
library(KoNLP)
extractNoun("아버지가 가방에 들어가신다")
