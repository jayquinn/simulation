####K-BNT replicate
#자비스 사람만들기
start_time<-Sys.time()
end_time<-Sys.time()
end_time - start_time
set.seed(3333)
id<-1:1953
age<-rep(1:9,each=217) # 9, 217
edu<-rep(1:7,279) # 7, 279
mean<-40.35-0.23*(age^2)+8.25*log(edu)+0.05*(age^2)*log(edu)
variance<-108.9*exp(-0.01*age-0.43*edu+0.04*age*edu)
sd<-sqrt(variance)
sample<-data.frame(id,age,edu,mean,sd)
sample<-round(sample, digits = 2)
edit(sample)

datainf<-sample[c(1:7,218:224,435:441,652:658,869:874,1086:1092,1303:1309,1520:1526,1737:1743),]
datainf[1,]
edit(datainf)

library(ggplot2)
ggplot(data=datainf,mapping=aes(x=age,y=mean,group=edu,color=edu))+geom_line()+xlim(1,9)+ylim(10,60)
ggplot(data=datainf,mapping=aes(x=age,y=sd,group=edu,color=edu))+geom_line()+xlim(1,7)+ylim(0,18)

#### raw data 생성 ####
n<-31
normals<-vector("list",length(datainf$mean))
for(i in seq_along(normals)){
  normals[[i]]<-rnorm(n,mean = datainf$mean[i], sd = datainf$sd[i])
}
