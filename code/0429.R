x <- '2021-01-01'
obj <- as.Date(x)
class(obj)

for(i in 0:30){
  print(obj+i)
}

# matrix
mat1 <- matrix(1:6,2,3,byrow=T)
mat1

mat4 <- matrix(1:9,3,byrow=T)
mat4

rbind(mat1,mat4)
rbind(mat4,11:13)
cbind(mat4,11:13)

seasons <- matrix(c("봄","'여름","가을","겨울"),2,2,byrow=T)
seasons[,2]

#list
list1 <- list(name="sohee",
              address="Earth",
              tel="1004",
              pay=1000)
list1

list1$name # character 반환
list1[[1]] # character 반환
list1[1]   # list 반환

list1$tel <- NULL
list1

#dataframe
no <- c(1,2,3,4)
name <- c('Apple','Peach','Banana','Grape')
price <- c(500,200,100,50)
qty <- c(5,2,4,7)
sales <- data.frame(NO=no,NAME=name,PRICE=price,QTY=qty)
sales

sales[1:3,]
sales[1:2,2:3]

subset(sales,price==200)
subset(sales,qty<5)
subset(sales,name=='Banana')
