model_logit<-glm(y~bliss$conc, family="binomial")
model_probit<-glm(y~bliss$conc, family=binomial(link="probit"))
model_clog<-glm(y~bliss$conc, family=binomial(link="cloglog"))

summary(model_logit)
model_logit$fitted.values
model_probit$fitted.values
model_clog$fitted.values
sum((model_logit$fitted.values-model_probit$fitted.values)^2)
sum((model_clog$fitted.values-model_probit$fitted.values)^2)

ilogit<-function(x){
  exp(x)/(1+exp(x))
}
plot(0:4, model_logit$fitted.values, xlim=c(-2,8), ylim=c(0,1))
curve(ilogit(model_logit$coefficients[1]+model_logit$coefficients[2]*x), add=T)
curve(pnorm(model_probit$coefficients[1]+model_probit$coefficients[2]*x), add=T, col="red")
curve(1-exp(-exp(model_clog$coefficients[1]+model_clog$coefficients[2]*x)), add=T, col="blue")
#punkty powiiny byc na³o¿óne na krzywe

#2
#probit i logit

curve(ilogit(model_logit$coefficients[1]+model_logit$coefficients[2]*x)/pnorm(model_probit$coefficients[1]+model_probit$coefficients[2]*x), xlim=c(-2,8))
curve((1-ilogit(model_logit$coefficients[1]+model_logit$coefficients[2]*x))/(1-pnorm(model_probit$coefficients[1]+model_probit$coefficients[2]*x)), xlim=c(-2,8), add=T, col="red")

curve((1-exp(-exp(model_clog$coefficients[1]+model_clog$coefficients[2]*x)))/pnorm(model_probit$coefficients[1]+model_probit$coefficients[2]*x), xlim=c(-2,8))
curve((exp(-exp(model_clog$coefficients[1]+model_clog$coefficients[2]*x))/(1-pnorm(model_probit$coefficients[1]+model_probit$coefficients[2]*x)), xlim=c(-2,8), add=T, col="red")

      #Itd
      
  data("discoveries")
plot(discoveries)

time(discoveries)

model_d<-glm(discoveries~1, family=poisson)
summary(model_d)


pchisq(deviance(model_d), df.residual(model_d), lower=F)

model_d2<-glm(discoveries~time(discoveries)+I(time(discoveries)^2), family=poisson)

summary(model_d2)

pchisq(deviance(model_d2), df.residual(model_d2), lower=F)

drop1(model_d2, test="Chisq")

gala<-read.table('h:/Windows7/Desktop/gala_data.txt')

model_l<-lm(Species~.-Endemics, data=gala)
summary(model_l)
plot(model_l$fitted.values, model_l$residuals)

library(MASS)
boxcox(model_l, lambda = seq(0,1,by=0.05), plotit=T)

model_l2<-lm(I(Species^(0.5))~.-Endemics, data=gala)
summary(model_l2)
plot(model_l2$fitted.values, model_l2$residuals)

model_l3<-glm(Species~.-Endemics, data=gala, family="poisson")
summary(model_l3)
pchisq(deviance(model_l3), df.residual(model_l3), lower=F)
model_l4<-glm(I(floor(Species^(0.5)))~.-Endemics, data=gala, family="poisson")
summary(model_l4)
pchisq(deviance(model_l3), df.residual(model_l3), lower=F)

1-model_l3$deviance/model_l3$null.deviance

qqnorm(residuals(model_l3))
library(faraway)
halfnorm(residuals(model_l3))

plot(log(model_l3$fitted.values), log((gala$Species-model_l3$fitted.values)^2))
abline(0,1)


























































































