miner2<-read.table('h:/Windows7/Desktop/miner2.data', h=T)

Freq<-matrix(0, nrow=8, ncol=3)
Freq[,1]<-miner2$Freq[miner2$status=="normal"]
Freq[,2]<-miner2$Freq[miner2$status=="mild"]
Freq[,3]<-miner2$Freq[miner2$status=="severe"]

Freq_n<-matrix(0, nrow=8, ncol=3)
for(i in 1:nrow(Freq)){
  Freq_n[i,]<-Freq[i,]/sum(Freq[i,])
}

plot(miner2$year, miner2$Freq, col=miner2$status)

plot(miner2$year[miner2$status=="normal"], Freq_n[,1], type='l', col=1)
lines(miner2$year[miner2$status=="mild"], Freq_n[,2], col=2)
lines(miner2$year[miner2$status=="severe"], Freq_n[,3], col=3)

miner2$d<-ifelse(miner2$status=="normal", 1, 0)
miner2$e<-ifelse(miner2$status=="normal", 0, 1)

Freq<-matrix(0, nrow=8, ncol=2)
Freq[,1]<-miner2$Freq[miner2$status=="normal"]
Freq[,2]<-miner2$Freq[miner2$status=="mild"]+miner2$Freq[miner2$status=="severe"]
Freq_n<-matrix(0, nrow=8, ncol=2)
for(i in 1:nrow(Freq)){
  Freq_n[i,]<-Freq[i,]/sum(Freq[i,])
}
miner3<-miner2[miner2$status=="normal",]
miner3<-rbind(as.matrix(miner3), cbind(miner2$year[miner2$status=="mild"], miner2$status[miner2$status=="mild"],miner2$Freq[miner2$status=="mild"]+miner2$Freq[miner2$status=="severe"]))




miner2<-read.table('h:/Windows7/Desktop/miner2.data', h=T)
miner22<-read.table('h:/Windows7/Desktop/miner2.data', h=T)
miner3<-miner2[miner2$status!="severe",]
miner22$Freq[miner2$status=="mild"]<-miner2$Freq[miner2$status=="mild"]+miner2$Freq[miner2$status=="severe"]

model<-glm(miner22$status~miner22$year+)

library(faraway)
data(nes96)
head(nes96)
wiek, poziom wyksztaacenia i zarobki w badanej
grupie respondentów
table(nes96$PID)
nes96$PID2<-ifelse(as.character(nes96$PID)=='strDem' | as.character(nes96$PID)=='weakDem', 'Democrats', as.character(nes96$PID2))

nes96$PID2<-ifelse(as.character(nes96$PID)=='indDem' | as.character(nes96$PID)=='indind' | as.character(nes96$PID)=='indRep', 'Independent', as.character(nes96$PID2))


nes96$PID2<-ifelse(as.character(nes96$PID)=='strRep' | as.character(nes96$PID)=='weakRep', 'Republican', as.character(nes96$PID2))

table(as.character(nes96$PID2))

table(nes96$income)
inca <- c(1.5,4,6,8,9.5,10.5,11.5,12.5,13.5,14.5,16,18.5,21,
          23.5,27.5,32.5,37.5,42.5,47.5,55,67.5,82.5,97.5,115)


cc<-unclass(nes96$income)




nes96$nincome=0
for(i in 1:length(inca)){
  nes96$nincome[unclass(nes96$income)==i]<-inca[i]
}

summary(nes96$nincome)
boxplot(nes96$nincome)

tab<-table(nes96$educ,nes96$PID2)
tab<-tab/rowSums(tab)
matplot(tab, type='l')

?cut

cutinc<-cut(nes96$nincome, 7)
levels(cutinc)

il <- c(8,26,42,58,74,90,107)
levels(cutinc)<-il

tab2<-table(cutinc, nes96$PID2)
tab2<-tab2/rowSums(tab2)
matplot(tab2, type='l')

cutage<-cut(nes96$age, 7)
al <- c(24,34,44,54,65,75,85)
levels(cutage)<-al

tab3<-table(cutage, nes96$PID2)
tab3<-tab3/rowSums(tab3)
matplot(tab3, type='l')

library(nnet)
mmod <- multinom(PID2 ~ age + educ + nincome, data=nes96)

mmodi <- step(mmod)

model1<- multinom(PID2 ~ age + nincome, data=nes96)

pchisq(deviance(model1) - deviance(mmod), mmod$edf - model1$edf, lower=F)
table(nes96$educ)/sum(table(nes96$educ))
#powinno byc mniej wiecej 1/7, jest duzo mniej
predict(mmodi, newdata=(nincome=il), type="probs")

s <- summary(mmodi)
exp(c(0, s$coefficients[,1]))/sum(exp(c(0, s$coefficients[,1])))

p <- predict(mmodi, data.frame(nincome=c(0,1)), type="probs")
log(p[1,1]*p[2,2]/(p[2,1]*p[1,2]))
log(p[1,1]*p[2,3]/(p[2,1]*p[1,3]))
nes96$PID2 <- as.factor(nes96$PID2)
library(MASS)
pp <- polr(PID2 ~ age + nincome + educ, data = nes96)

pp_step <- step(pp)

pp$deviance
mmod$deviance


ss<- table(nes96$nincome, nes96$PID2)/rowSums(table(nes96$nincome, nes96$PID2))


