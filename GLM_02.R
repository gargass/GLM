
conc<-c(0,1,2,3,4)
dead<-c(2,8,15,23,27)
number<-c(30,30,30,30,30)
alive<-number-dead
bliss<-cbind(conc, dead, number, alive)
bliss<-as.data.frame(bliss)

y<-cbind(bliss$dead, bliss$alive)

model<-glm(y~bliss$conc, family="binomial")
summary(model)
#residuals(model)
residuals(model) #Deviance Residuals
model$residuals #working residuals <-nie maja zastosowania\
#(robocze)
residuals(model, type="working")
residuals(model, type="pearson") #podobne do dewiancji

d<-residuals(model)
d%*%d #to samo co residuals deviance
#suma kwadratów

#std pearson
resid(model, type="pearson")/sqrt(1- hatvalues(model))
#std dew. 
rstandard(model)
d/sqrt(1- hatvalues(model))

residuals(model, type="response")
dead/number-fitted(model)
#dopasowane pi <- dead/number
#

plot(conc,dead/number)
x<-seq(0,4, length=100)
lines(x, ilogit(model$coefficients[1]+model$coefficients[2]*x), col="red")

#b)
ilogit(model$coefficients[1]+model$coefficients[2]*5)
#Zastanowic sie, skad liczy null deviance i residuals deviance
#w summary

deviance(model)
#residuals deviance
model$null.deviance
df.residual(model)
model$df.null

pchisq(deviance(model), df.residual(model), lower=F)

model0<-glm(y~1, family="binomial")
pchisq(deviance(model0), df.residual(model0), lower=F)

#chi^2 z d st. swobody, EX=d
# sqrt(VarX)=sqrt(2d)

pchisq(deviance(model0)-deviance(model), df.residual(model0)-df.residual(model), lower=F)
anova(model, test="Chi")
summary(model)

