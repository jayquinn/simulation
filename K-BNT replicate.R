####K-BNT replicate
#사람만들기
set.seed(3333)
id<-1:1953
age<-rep(1:9,each=217) # 9, 217
edu<-rep(1:7,each=31, times=9) # 7, 279
mean<-40.35-0.23*(age^2)+8.25*log(edu)+0.05*(age^2)*log(edu)
variance<-108.9*exp(-0.01*age-0.43*edu+0.04*age*edu)
sd<-sqrt(variance)
sample<-data.frame(id,age,edu,mean,sd)
sample<-round(sample, digits = 2)

#그림그리기
datainf<-sample[seq(from = 1, to = 1923, by = 31),]
library(ggplot2)
ggplot(data=datainf,mapping=aes(x=age,y=mean,group=edu,color=edu))+geom_line()+xlim(1,9)+ylim(10,60)
ggplot(data=datainf,mapping=aes(x=age,y=sd,group=edu,color=edu))+geom_line()+xlim(1,7)+ylim(0,18)

#총점 생성
n<-31
normals<-vector("list",length(datainf$mean))
for(i in seq_along(normals)){
  normals[[i]]<-round(rnorm(n,mean = datainf$mean[i], sd = datainf$sd[i]),0)
}
sample[,"ctt"]<-unlist(normals)
sample[,"ctt"]<-ifelse(sample$ctt>=60,60,sample$ctt)

#망한 것 같은데? (1) 문항에 대한 정보도 없고 (2) 문항 반응 패턴도 못 만든다.
#문항 정보를 알고 있으면 simdata 함수 이용
#문항 패턴을 알고 있으면 그건 그냥 추정하면 된다.
#따라서 할 일은 문항 패턴 만들기!
#60 문항에 대해서 앞쪽부터 맞춘 갯수 생각하기
# 1953 x 60짜리 행렬에 대해서
library(tidyverse)
f <-list(1,0)
to <- list(1,0)
plz<-list(from = f,to = to,length.out = lo)
pattern<-unlist(plz %>% pmap(seq))


pattern<-vector("list",nrow(sample))
for(i in seq_along(sample$ctt)){
  f <-list(1,0)
  to <- list(1,0)
  lo <- list(sample$ctt[i], 60-sample$ctt[i])
  plz<-list(from = f,to = to,length.out = lo)
  pattern[[i]]<-unlist(plz %>% pmap(seq))
}
unlist(pattern)
head(as.matrix(unlist(pattern),nrow = 60), byrow=T)
pattern<-as.data.frame(matrix(unlist(pattern), nrow = nrow(sample), ncol = 60, byrow=T))

#문항 모수 추정
library(mirt)
mod1<-mirt(pattern,1)
