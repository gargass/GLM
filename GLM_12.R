uzywki <- read.table('file:///h:/Windows7/Desktop/uzywki.txt', h=T)

model.A.C.M <- glm(y ~ alcohol + cigarettes + marijuana, data = uzywki, family = poisson)
model.AC.M <- glm(y ~ alcohol * cigarettes + marijuana, data = uzywki, family = poisson)
model.AM.CM <- glm(y ~ alcohol * marijuana + alcohol * cigarettes, data = uzywki, family = poisson)
model.AM.CM.AC <- glm(y ~ alcohol * marijuana + alcohol * cigarettes + cigarettes * marijuana, data = uzywki, family = poisson)
model.ACM <- glm(y ~ alcohol * marijuana * cigarettes, data = uzywki, family = poisson)

dane_y <- cbind(uzywki$y, model.A.C.M$fitted.values, model.AC.M$fitted.values, model.AM.CM$fitted.values, model.AM.CM.AC$fitted.values, model.ACM$fitted.values)
colnames(dane_y) <- c("y", "A.C.M", "AC.M", "AM.CM", "AM.CM.AC", "ACM")

#stosunki szans
#warunkowe
u000 <- mean(model.AM.CM.AC$fitted.values[uzywki$alcohol == 0 & uzywki$cigarettes == 0 & uzywki$marijuana == 0])
u110 <- mean(model.AM.CM.AC$fitted.values[uzywki$alcohol == 1 & uzywki$cigarettes == 1 & uzywki$marijuana == 0])
u010 <- mean(model.AM.CM.AC$fitted.values[uzywki$alcohol == 0 & uzywki$cigarettes == 1 & uzywki$marijuana == 0])
u100 <- mean(model.AM.CM.AC$fitted.values[uzywki$alcohol == 1 & uzywki$cigarettes == 0 & uzywki$marijuana == 0])

teta_ac0 <- u000*u110/(u010 * u100)

u001 <- mean(model.AM.CM.AC$fitted.values[uzywki$alcohol == 0 & uzywki$cigarettes == 0 & uzywki$marijuana == 1])
u111 <- mean(model.AM.CM.AC$fitted.values[uzywki$alcohol == 1 & uzywki$cigarettes == 1 & uzywki$marijuana == 1])
u011 <- mean(model.AM.CM.AC$fitted.values[uzywki$alcohol == 0 & uzywki$cigarettes == 1 & uzywki$marijuana == 1])
u101 <- mean(model.AM.CM.AC$fitted.values[uzywki$alcohol == 1 & uzywki$cigarettes == 0 & uzywki$marijuana == 1])
teta_ac1 <- u001*u111/(u011 * u101)

#am
u000 <- mean(model.AM.CM.AC$fitted.values[uzywki$alcohol == 0 & uzywki$cigarettes == 0 & uzywki$marijuana == 0])
u001 <- mean(model.AM.CM.AC$fitted.values[uzywki$alcohol == 0 & uzywki$cigarettes == 0 & uzywki$marijuana == 1])
u100 <- mean(model.AM.CM.AC$fitted.values[uzywki$alcohol == 1 & uzywki$cigarettes == 0 & uzywki$marijuana == 0])
u101 <- mean(model.AM.CM.AC$fitted.values[uzywki$alcohol == 1 & uzywki$cigarettes == 0 & uzywki$marijuana == 1])

teta_am0 <- u000*u101/(u001 * u100)

# równowœæ teta_ack wynika z modelumodel.AM.CM.AC

# stosunki szans warunkowe sa takie same dla ka¿dego k

# oprocz modelu trzeciego rzedu

u000 <- mean(model.ACM$fitted.values[uzywki$alcohol == 0 & uzywki$cigarettes == 0 & uzywki$marijuana == 0])
u110 <- mean(model.ACM$fitted.values[uzywki$alcohol == 1 & uzywki$cigarettes == 1 & uzywki$marijuana == 0])
u010 <- mean(model.ACM$fitted.values[uzywki$alcohol == 0 & uzywki$cigarettes == 1 & uzywki$marijuana == 0])
u100 <- mean(model.ACM$fitted.values[uzywki$alcohol == 1 & uzywki$cigarettes == 0 & uzywki$marijuana == 0])

teta_ac0 <- u000*u110/(u010 * u100)

u001 <- mean(model.ACM$fitted.values[uzywki$alcohol == 0 & uzywki$cigarettes == 0 & uzywki$marijuana == 1])
u111 <- mean(model.ACM$fitted.values[uzywki$alcohol == 1 & uzywki$cigarettes == 1 & uzywki$marijuana == 1])
u011 <- mean(model.ACM$fitted.values[uzywki$alcohol == 0 & uzywki$cigarettes == 1 & uzywki$marijuana == 1])
u101 <- mean(model.ACM$fitted.values[uzywki$alcohol == 1 & uzywki$cigarettes == 0 & uzywki$marijuana == 1])
teta_ac1 <- u001*u111/(u011 * u101)

teta_ac0
teta_ac1

u00. <- sum(model.AM.CM.AC$fitted.values[uzywki$alcohol == 0 & uzywki$cigarettes == 0])
u11. <- sum(model.AM.CM.AC$fitted.values[uzywki$alcohol == 1 & uzywki$cigarettes == 1])
u01. <- sum(model.AM.CM.AC$fitted.values[uzywki$alcohol == 0 & uzywki$cigarettes == 1 ])
u10. <- sum(model.AM.CM.AC$fitted.values[uzywki$alcohol == 1 & uzywki$cigarettes == 0 ])

teta_ac <- (u00.*u11.)/(u01.*u10.)

u0.0 <- sum(model.AM.CM.AC$fitted.values[uzywki$alcohol == 0 & uzywki$marijuana == 0])
u1.1 <- sum(model.AM.CM.AC$fitted.values[uzywki$alcohol == 1 & uzywki$marijuana == 1])
u0.1 <- sum(model.AM.CM.AC$fitted.values[uzywki$alcohol == 0 & uzywki$marijuana == 1 ])
u1.0 <- sum(model.AM.CM.AC$fitted.values[uzywki$alcohol == 1 & uzywki$marijuana == 0 ])

teta_am <- (u0.0*u1.1)/(u0.1*u1.0)

u.00 <- sum(model.AM.CM.AC$fitted.values[uzywki$cigarettes == 0 & uzywki$marijuana == 0])
u.11 <- sum(model.AM.CM.AC$fitted.values[uzywki$cigarettes == 1 & uzywki$marijuana == 1])
u.01 <- sum(model.AM.CM.AC$fitted.values[uzywki$cigarettes == 0 & uzywki$marijuana == 1 ])
u.10 <- sum(model.AM.CM.AC$fitted.values[uzywki$cigarettes == 1 & uzywki$marijuana == 0 ])

teta_cm <- (u.00*u.11)/(u.01*u.10)

#przy modelu a.c.m warunkowe szanse = 1

model.AM.CM.AC$deviance

#statystyka chi^2 pearsona
sum((model.AM.CM.AC$fitted.values - uzywki$y)^2/model.AM.CM.AC$fitted.values)

1-pchisq(- model.AM.CM.AC$deviance + model.AM.CM.AC$null.deviance, model.AM.CM.AC$df.null- model.AM.CM.AC$df.residual)

anova(model.AM.CM.AC, model.ACM)

anova(model.AM.CM, model.AM.CM.AC)

model.AM.CM$deviance - model.AM.CM.AC$deviance

##########
data("HairEyeColor")
HairEyeColor

dane <- HairEyeColor[,,1] + HairEyeColor[,,2]

summary(dane)

x <- apply(HairEyeColor, c(1, 2), sum)
summary(x)
class(x)
chisq.test(x) #jest zale¿noœæ

dotchart(x)
mosaicplot(x)
x

dane <- as.data.frame(HairEyeColor)

dane <- 
model <- glm(Freq ~ Eye + Hair, data = dane, family = poisson)
summary(model)
dane_2 <- NULL
for(e in 1:4){
  for(h in 1:4){
    dane_2 <- rbind(dane_2, c())
  }
}

res <- residuals(model, type="pearson")
install.packages('sqldf')
library(sqldf)
x1 <- sqldf("select Hair, Eye, sum(Freq) as freq from dane group by Hair, Eye")
svd(res)
model <- glm(Freq ~ Eye + Hair, data = dane, family = poisson)
res <- residuals(model, type="pearson")

x1 <- data.frame(x)
x1
model <- glm(freq ~ Eye + Hair, data = x1, family = poisson)
res <- residuals(model, type="pearson")
x1

m <- matrix(res, ncol=4, nrow=4)
colnames(m) <- colnames(x)
rownames(m) <- rownames(x)
A <- svd(m)

 X = t(apply(A$u, 1, "*", sqrt(A$d)))
 Y = t(apply(A$v, 1, "*", sqrt(A$d)))

 
 plot(X,Y)
 text(X,Y, labels=paste(rownames(X,Y))

