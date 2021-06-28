####K-BNT replicate
#library(tidyverse);library(ggplot2); library(mirt); library(lavaan)
#사람만들기
set.seed(3333)
id<-1:1953
age<-rep(1:9,each=217) # 9, 217
edu<-rep(1:7,each=31, times=9) # 7, 279
grp<-rep(1:63,each=31)
mean<-40.35-0.23*(age^2)+8.25*log(edu)+0.05*(age^2)*log(edu)
variance<-108.9*exp(-0.01*age-0.43*edu+0.04*age*edu)
sd<-sqrt(variance)
sample<-data.frame(id,age,edu,grp,mean,sd)
sample<-round(sample, digits = 2)

#그림그리기
#datainf<-sample[seq(from = 1, to = 1923, by = 31),]
#library(ggplot2)
#ggplot(data=datainf,mapping=aes(x=age,y=mean,group=edu,color=edu))+geom_line()+xlim(1,9)+ylim(10,60)
#ggplot(data=datainf,mapping=aes(x=age,y=sd,group=edu,color=edu))+geom_line()+xlim(1,7)+ylim(0,18)

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

#library(tidyverse)
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

#흐트러놓기 - 홀수 문항에 대해서
#for(i in seq(1,nrow(pattern),by=2)){
#  pattern[i,]<-sample(pattern[i,],60,replace = F)
#}
#흐트러놓기 - 전체 문항에 대해서
for(i in seq(1,nrow(pattern),by=1)){
  pattern[i,]<-sample(pattern[i,],60,replace = F)
}
#문항 모수 추정 rasch
#library(mirt)
model.rasch <- 'F1 = 1-60' 
results.rasch <- mirt(data = pattern, model=model.rasch, itemtype="Rasch", SE=TRUE, verbose=FALSE)
score.rasch<-fscores(results.rasch,method = 'EAP')# EAP(default) MAP ML WLE EAPsum
#coef.rasch <- coef(results.rasch, IRTpars=TRUE, simplify=TRUE)
#items.rasch <- as.data.frame(coef.rasch$items)
#print(coef.rasch)
#print(items.rasch)
#summary(results.rasch)
#plot(results.rasch, type = 'trace', which.items = c(1:60))
#plot(results.rasch, type = 'infotrace', which.items = c(1:60))
#plot(results.rasch, type = 'info', theta_lim = c(-4,4), lwd=2)
#plot(results.rasch, type = 'SE', theta_lim = c(-4,4), lwd=2)
#plot(results.rasch, type = 'score', theta_lim = c(-4,4), lwd=2)
#plot(results.rasch, type = 'itemscore', theta_lim = c(-4,4), lwd=2)
#plot(results.rasch, type = 'rxx', theta_lim = c(-4,4), lwd=2)
#hist(score.rasch)
#hist(sample$ctt)

#문항 모수 추정 2pl
#library(mirt)
model.tpl <- 'F1 = 1-60' 
results.tpl <- mirt(data=pattern, model=model.tpl, itemtype="2PL", SE=TRUE, verbose=FALSE)
score.tpl<-fscores(results.tpl,method = 'EAP')# EAP(default) MAP ML WLE EAPsum
#coef.tpl <- coef(results.tpl, IRTpars=TRUE, simplify=TRUE)
#items.tpl <- as.data.frame(coef.tpl$items)
#print(coef.tpl)
#print(items.tpl)
#summary(results.tpl)
#plot(results.tpl, type = 'trace', which.items = c(1:60))
#plot(results.tpl, type = 'infotrace', which.items = c(1:60))
#plot(results.tpl, type = 'info', theta_lim = c(-4,4), lwd=2)
#plot(results.tpl, type = 'SE', theta_lim = c(-4,4), lwd=2)
#plot(results.tpl, type = 'score', theta_lim = c(-4,4), lwd=2)
#plot(results.tpl, type = 'itemscore', theta_lim = c(-4,4), lwd=2)
#plot(results.tpl, type = 'rxx', theta_lim = c(-4,4), lwd=2)
#hist(score.tpl)

#문항 모수 추정 FA
#library(lavaan)
model.cfa<-'F1=~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10
                +V11+V12+V13+V14+V15+V16+V17+V18+V19+V20
                +V21+V22+V23+V24+V25+V26+V27+V28+V29+V30
                +V31+V32+V33+V34+V35+V36+V37+V38+V39+V40
                +V41+V42+V43+V44+V45+V46+V47+V48+V49+V50
                +V51+V52+V53+V54+V55+V56+V57+V58+V59+V60'
results.cfa<-cfa(model=model.cfa,data = pattern,ordered = F) #ordered T로 잡으면 threshold 추정됨. F면 연속형으로 가정하는듯?
score.cfa<-lavPredict(results.cfa)
#summary(results.cfa)
#hist(score.cfa)

#자료 통합
sample<-cbind(sample,score.rasch,score.tpl,score.cfa)
colnames(sample)<-c("id","age","edu","grp","mean","sd","sums","rasch","tpl","cfa")

#상관 그림
#plot(sample[,7:10])

#상관계수 
#attach(sample);cleft<-list(sums,sums,sums,rasch,rasch,cfa);cright<-list(rasch,tpl,cfa,tpl,cfa,rasch);detach(sample)
#totalcor<-as.data.frame(totalcor<-rbind(map2_dbl(cleft,cright,cor),map2_dbl(cleft,cright,cor, method = "spearman")));colnames(totalcor)<-c("sums-rasch","sums-tpl","sums-cfa","rasch-tpl","rasch-cfa","cfa-rasch");rownames(totalcor)<-c("pearson","spearman")
#print(totalcor)

