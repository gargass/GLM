#Zadanie 1
beetle<-read.table('h:/Windows7/Desktop/beetle.txt', h=T)

#a)
y<-cbind(beetle$affected, beetle$exposed-beetle$affected)
model<-glm(y~log(beetle$conc), family="binomial")
summary(model)

#b)
plot(log(beetle$conc), beetle$affected/beetle$exposed) #wykres pi
ilogit<-function(x){
  exp(x)/(1+exp(x))
}

lines(log(beetle$conc), ilogit(model$coefficients[1]+model$coefficients[2]*log(beetle$conc)))
curve(ilogit(model$coefficients[1]+model$coefficients[2]*x), add=T)
#powinien przypominac prost¹

library("faraway")
halfnorm(rstandard(model))
#w sumie to nie ma obs. odstaj¹cych
n=nrow(beetle)
p=ncol(beetle)
dp<-sum(resid(model, type="pearson")^2)/(n-p+1)

summary(model, dispersion = dp)
summary(model)
#Error std ró¿ni¹ siê o sqrt

sqrt(dp)*summary(model)$coefficients[,2]
summary(model, dispersion = dp)$coefficients[,2]
#to samo

#Test F
drop1(model, scale = dp, test="F")

model$null.deviance
model$deviance
#Powinno wyjœæ 22.29


############
#Zad 2
lips<-read.table('h:/Windows7/Desktop/lips.dat', h=T)
lip<-glm(lips$obs~lips$affpct+offset(log(lips$exp)), family = "poisson")
summary(lip)
plot(lips$affpct, lips$obs/lips$exp)

halfnorm(rstandard(lip))
pchisq(deviance(lip), df.residual(lip), lower=F)
lip0<-glm(lips$obs~1+offset(log(lips$exp)), family = "poisson")
pchisq(deviance(lip0)-deviance(lip), df.residual(lip0)-df.residual(lip), lower=F)
#Zmienna jest istotna

#Parametr rozproszenia
n=nrow(lips)
p=ncol(lips)
dp<-sum(resid(lip, type="pearson")^2)/(n-p+1)

summary(lip, dispersion = dp)
summary(lip)
#Error std ró¿ni¹ siê o sqrt

sqrt(dp)*summary(lip)$coefficients[,2]
summary(lip, dispersion = dp)$coefficients[,2]

lip2<-glm(lips$obs~lips$affpct+offset(log(lips$exp)), family=quasi(link="log", variance="mu"))
summary(lip2)

#### Zadanie 3
solder<-read.table('h:/Windows7/Desktop/solder.txt', h=T)

sol<-glm(skips~., family="poisson", data=solder)
summary(sol)
#le dopasowany
halfnorm(rstandard(sol))
library(MASS)
sol<-glm.nb(skips~., data=solder)
summary(sol)
#k<-Dispersion parameter for Negative Binomial(4.3972)
solk<-glm.nb(skips~., family=negative.binomial(2), data=solder)
