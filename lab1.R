setwd("C:/Users/User/Desktop/szkola/3r2sem/si_lab")

getwd()

temp <-"36.6"

temp

#"36.6"+ 0.1

class(temp)

temp <-as.numeric(temp)
temp

temp <-as.integer(temp)
temp

class("36")
class(36)
class(36L)
class(36+0i)
class(TRUE)

v1 <-3:0
v1

v1[1]
v1[length(v1)]
v1[c(T,F,T,F)]
v1[3:1]
v1[-2]
v1[0:3]

v2<-c(0/0, 1/0, 1/Inf, TRUE, as.numeric("abc"))
v2
class(v2)

v3<-as.logical(c("T", "False", "abc"))
v3
class(v3)

is.na(NaN)
is.nan(NA)

v4 <-vector("numeric", length=9)
v4[1:4] <-1:4
v4[5] <-"null"
v4[6:9] <-6:9
class(v4)
v4<-as.numeric(v4)
bad <-is.na(v4)
bad
v4<-v4[!bad]
v4

v5 <-1:5
v6 <-6:10
v5 + v6
v5 -v6
v5 * v6
v5 / v6
v5 == v6
v5 >= 3

m1 <-cbind(v5,v6)
m2 <-rbind(v5,v6)

m1
m2
m1[1,2]
m1[1,2, drop=FALSE]

m1[2:3,]

m3 <-matrix(1:10, nrow=2, ncol=5)
m3

m4 <-1:10
dim(m4) <-c(5,2)
attributes(m4)
m4

m5 <-matrix(rep(1,4),nrow=2,ncol=2)
m6 <-matrix(rep(2,4),nrow=2,ncol=2)
m5
m6
m5 * m6
m5 %*% m6

l1 <-list(id=1L, name="Kowalski", temp=36.6)
class(l1)
class(l1[[1]])
class(l1[[2]])

l1$name
l1["temp"]
arg <-"id"
l1$arg
l1[arg]
l1["arg"]
l1$i
l1[["i"]]
l1[["i", exact=FALSE]]

df <-data.frame(id=1:5, temp=c(36.6, NA, 37.2, 37.1, 36.8))
attributes(df)
nrow(df)
ncol(df)
df

good <-complete.cases(df)
good
df <-df[good,]

m <-data.matrix(df)

f <-factor(c("male","female","female","male"), level=c("male","female"))
table(f)
v <-unclass(f)
v

v <-c("Kowalski", "Jan")
names(v)<-c("nazwisko", "imie")
l <-list(nazwisko="Kowalski", imie="Jan")
m <-matrix(nrow=2, ncol=2)
m[1,1] <-"Kowalski"
m[1,2] <-"Jan"
m[2,1] <-"Nowak"
m[2,2] <-"Adam"
dimnames(m) <-list(c("nr1", "nr2"), c("nazwisko", "imie"))
m

v <-c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")

for(i in v){
  print(i)
}

for(i in 1:10){
  print(v[i])
}

for(i in seq_along(v)){
  print(v[i])
}


i <-1
while(i <= 10){
  print(i)
  i <-i + 1
}


i <-1
repeat{
  i<-i+ 1
  if(i== 11){
    break;
  } 
  else if(i== 5){
    next;
  }
  else{
  print(i)
  }
}


y<-function(x, a=1, b=0){
  a*x + b
}
y

#y()

y(1)

y(1,2,3)
y(b=3, a=2, x=1)
y(b=3, 1, a=2)

y <-function(x, a=1, b=0, c){
  a*x + b
}

y(1)


s <-function(...){
  arg <-c(...)
  sum <-0
  for(i in arg){
    sum <-sum + i
  }
  sum
}

s

s(1,2,3,4,5)


search()
library(dplyr)
search()

sum(1:5)
sum <-function(){}
#sum(1:5)
base::sum(1:5)

rm(sum)
sum(1:5)


y <-function(x){
  a*x + b
}
a <-1
b <-0
y(1)


y <-function(x){
  a <-2
  b <-1
  f <-function(){
    a*x + b
  }
f()
}
a <-1
b <-0
y(1)


a <-1
b <-2
m <-function(x){
  a <-3
  b <-4
  b*s(x)
}
s <-function(x){
  a+x
}
s(5)


l <-list(1:10, 2^(1:5))
lapply(l, mean)
l
sapply(l, mean)


v <-1:10
f <-function(x,n){
  x^n
}
sapply(v, f, 5)

v <-c(15,73,64,25,67,32,67,23,56,67)
g <-c(rep(1:2,length(v)/2))
tapply(v,g,mean)

rep(1:2)
g



m <-matrix(1:10, nrow=5, ncol=2)
apply(m,1,mean)
apply(m,2,mean)


v1 <-1:5
v2 <-5:1
mapply(f,v1,v2)
