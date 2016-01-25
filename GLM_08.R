library(faraway)
data(gala)
gala<-gala
model<-glm(gala$Species~.-gala$Endemics, family="poisson", data=gala)
summary(model)
par(mfrow=c(2,1))
plot(residuals(model)~predict(model, type="response")) #u^
plot(residuals(model)~predict(model, type="link")) #eta^

plot(gala$Species-model$fitted.values)
plot(gala$Species~gala$Area)
plot(gala$Species~log(gala$Area))

z<-predict(model, type="link")+(gala$Species-predict(model, type="response"))/predict(model, type="response")
plot(z~log(gala$Area))

modpl<-glm(gala$Species ~ log(gala$Area) + log(gala$Elevation) + log(gala$Nearest) + log(gala$Scruz+0.1)+log(gala$Adjacent), family=poisson)
deviance(modpl)
deviance(model)
summary(modpl)

pchisq(deviance(model), df.residual(model), lower=F)
pchisq(deviance(modpl), df.residual(modpl), lower=F)

e<-gala$Species-predict(modpl, type="response")
logArea<-log(gala$Area)

logArea*modpl$coefficients[2]+e
plot(log(gala$Area), log(gala$Area)*modpl$coefficients[2]+e/predict(modpl, type="response"))

plot(log(gala$Area), residuals(modpl, type="partial")[,1])

plot(log(gala$Elevation), residuals(modpl, type="partial")[,2])

p<-ncol( residuals(modpl, type="partial"))
for(i in 1:p){
  plot(log(gala[,2+i]), residuals(modpl, type="partial")[,i], main=colnames(residuals(modpl, type="partial"))[i])
}

plot(gala$Species, log(gala$Elevation))
?rstudent

plot(rstudent(modpl))

halfnorm(rstudent(modpl))
gala[c(22,13),]
#Powy¿ej 6
halfnorm(influence(modpl)$hat)
gala[c(16,25),]
#scruz - odleglosc od santacruz

halfnorm(cooks.distance(modpl))
gala[c(27,25),]

plot(influence(modpl)$coef[,5])
modpl2<-glm(Species ~ log(Area) + log(Elevation) + log(Nearest) + log(Scruz+0.1)+log(Adjacent), family=poisson, data=gala[-25,])
modpl2$coefficients
modpl$coefficients
summary(modpl2)

drop1(modpl2)
modpl3<-glm(Species ~ log(Area) +  log(Nearest) + log(Scruz+0.1)+log(Adjacent), family=poisson, data=gala[-25,])
drop1(modpl3)
modpl4<-glm(Species ~ log(Area) +  log(Nearest) + log(Adjacent), family=poisson, data=gala[-25,])
drop1(modpl4)
modpl5<-glm(Species ~ log(Area) +   log(Adjacent), family=poisson, data=gala[-25,])
