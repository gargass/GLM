malaria<-read.table('h:/Windows7/Desktop/malaria.data', h=T)
y<-cbind(malaria$Spositive, malaria$Number-malaria$Spositive)
model<-glm(y~malaria$Age, family="binomial")
summary(model)
logit<-function(x){
  log(x/(1-x))
}
proporcja<-malaria$Spositive/malaria$Number
wagi<-malaria$Number*proporcja*(1-proporcja) ##????
model_b<-lm(logit(proporcja)~malaria$Age, weights = wagi)

ilogit<-function(x){
  exp(x)/(1+exp(x))
}
attach(malaria)
(logit(1/4)-model$coefficients[1])/model$coefficients[2]

predict(model, newdata = data.frame(Age=20), se.fit = T)
# to sa logity pstwa

model<-glm(y~Age, family="binomial")
pr<-predict(model, newdata = data.frame(Age=20), se.fit = T)
ilogit(pr$fit)

ilogit(pr$fit+1.96*sqrt(pr$se.fit/9))

plot(proporcja~Age)
lines(Age,ilogit(model$coefficients[1]+model$coefficients[2]*Age))

finance<-read.table('C:/Users/gargass/Downloads/finance.data', h=T)
finance$y<-ifelse(finance$Outcome=='Bankrupt', 1, 0)

model<-glm(finance$Outcome~finance$x1+finance$x2+finance$x3+finance$x4, family="binomial")
summary(model)

model$null.deviance
model_0<-glm(finance$Outcome~1, family="binomial")

1-model$deviance/model$null.deviance

drop1(model, test="Chisq")
model1<-glm(finance$Outcome~finance$x1+finance$x2+finance$x3, family="binomial")
drop1(model1, test="Chisq")
model2<-glm(finance$Outcome~finance$x2+finance$x3, family="binomial")
drop1(model2, test="Chisq")
model2$deviance-model$deviance
#niewielka zmiana
step(model)
1-model2$deviance/model2$null.deviance

install.packages("faraway")
library("faraway")
halfnorm(rstandard(model2))
finance2<-finance[-c(16,34),]
model3<-glm(finance2$Outcome~finance2$x2+finance2$x3, family="binomial")
model3$iter
plot(finance2$x3~finance2$x2, pch=finance2$y+1)
HL<-read.csv('C:/Users/gargass/Downloads/HosLemData.csv')
model<-glm(HL$y~HL$x, family="binomial")
summary(model)
#nie ma sensu
cut
p<-model$fitted.values
p
1-p
ps<-sort(p)
lab<-cut(p, breaks=c(0, quantile(p, probs = seq(0.1, 0.9, 0.1)), 1), labels=F)
etykiety<-cbind(p, lab)
Ob<-as.matrix(0, nrow=10, ncol=2)
E<-as.matrix(0, nrow=10, ncol=2)
Ob[1,1]<-length(etykiety[,lab=1])
