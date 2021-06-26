####K-BNT replicate
#자비스 사람만들기
set.seed(3333)
id<-1:1953
age<-rep(1:9,each=217) # 9, 217
edu<-rep(1:7,279) # 7, 279
mean<-40.35-0.23*(age^2)+8.25*log(edu)+0.05*(age^2)*log(edu)
variance<-108.9*exp(-0.01*age-0.43*edu+0.04*age*edu)
sd<-sqrt(variance)
sample<-data.frame(id,age,edu,mean,sd)
edit(sample)