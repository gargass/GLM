gator<-read.table('file:///h:/Windows7/Desktop/gator.data', h=T)
gator$food<-factor(gator$food, levels=1:5, labels=c("ryby", "bezkregowce", "gady", "ptaki", "inne"))
gator$size<-factor(gator$size, levels=1:2, labels=c("<2.3", ">=2.3"))
gator$gender<-factor(gator$gender, levels=1:2, labels=c("M", "F"))
gator$lake<-factor(gator$lake, levels=1:4, labels=c("Hancock", "Oklawaha", "Trafford", "George"))

gator_sub <- cbind(gator$food, gator$lake, gator$count)
colnames(gator_sub) <- c('food', 'lake', 'x')
gator_sub <- data.frame(gator_sub)

gator_sub$food<-factor(gator_sub$food, levels=1:5, labels=c("ryby", "bezkregowce", "gady", "ptaki", "inne"))
gator_sub$lake<-factor(gator_sub$lake, levels=1:4, labels=c("Hancock", "Oklawaha", "Trafford", "George"))

table(gator_sub$food, gator$lake)
levels(gator_sub$food)
g <- matrix(0, ncol=3, nrow=20)
k<-1
for(f in levels(gator_sub$food)){
  for(l in levels(gator_sub$lake)){
    g[k,1]<-f
    g[k,2]<-l
    g[k,3]<-sum(gator_sub$x[gator_sub$food == f & gator_sub$lake == l])
    k=k+1
  }
}

colnames(g)<-colnames(gator_sub)
g<-data.frame(gator_sub)

glm <- glm(x ~ lake + food, family = poisson, data= g)

glm$null.deviance-glm$deviance

dane_c <- matrix(0, ncol=3)
for(l in levels(g$lake)){
  fi=1
  for(f in levels(g$food)){
    test<-matrix(c(l, f), ncol=2)
    colnames(test)<-c('lake', 'food')
    ppr[fi] <- predict(glm, newdata=data.frame(test), type='response')/219
    fi=fi+1
  }
  
  pp <- exp(glm$coefficients)
  dane_c <- rbind(dane_c, c(l,sum(g$x[g$lake ==l])/sum(g$x), sum(ppr)))
}
dane_c <- data.frame(dane_c)
dane_c[,2] <- as.numeric(as.character(dane_c[,2]))

ppr <- 1:5
fi=1
for(l in levels(g$lake)){

  
  dane_c <- rbind(dane_c, c(l,sum(pp))
  fi=0
  }

library(faraway)
data(femsmoke)

fem <- femsmoke

tab <- matrix(0, nrow=2, ncol=2)
for(d in 1:length(levels(fem$dead))){
  for(s in 1:length(levels(fem$smoker))){
    tab[d,s]<-sum(
      fem$y[fem$smoker == levels(fem$smoker)[s] & fem$dead == levels(fem$dead)[d]])
  }
}

rownames(tab)<-c('dead_yes', 'dead_no')
colnames(tab)<-c('smoker_yes', 'smoker_no')

tab2 <- matrix(0, nrow=2, ncol=2)
for(d in 1:length(levels(fem$dead))){
  for(s in 1:length(levels(fem$smoker))){
    tab2[d,s]<-sum(
      fem$y[fem$smoker == levels(fem$smoker)[s] & fem$dead == levels(fem$dead)[d]])
  }
}

plot(prop.table(xtabs( y ~ smoker + age, fem),2)
)
fem
model_fem <- glm(y ~ smoker + dead + age, data = fem, family=poisson)
model_fem
1 - pchisq(model_fem$null.deviance - model_fem$deviance, model_fem$df.null - model_fem$df.residual)



model_fem.1 <- glm(y ~ smoker * dead + age, data = fem, family=poisson)

1 - pchisq(model_fem.1$null.deviance - model_fem.1$deviance, model_fem.1$df.null - model_fem.1$df.residual)
anova(model_fem.1, test="Chi")
1 - model_fem.1$deviance/model_fem.1$null.deviance
pchisq(deviance(model_fem.1), df.residual(model_fem.1), lower=F)

model_2<-glm(y ~ (smoker+age+dead)^2, data=fem, family=poisson)
xt <- xtabs(fitted(model_2) ~ smoker + dead + age, data=fem)

tmp <- xt[,,1]
tmp[1,1]*tmp[2,2]/(tmp[2,1]*tmp[1,2])

for(i in 1:7){

  tmp <- xt[,,i]
  print(tmp[1,1]*tmp[2,2]/(tmp[2,1]*tmp[1,2]))
}

for(i in 1:2){
  
  tmp <- xt[,i,]
  print(tmp[1,1]*tmp[2,2]/(tmp[2,1]*tmp[1,2]))
}

exp(model_2$coefficients)
#wspolczynnik jest taki sam jak szanse

model <- glm(y ~ smoker*age*dead, femsmoke, family=poisson)
drop1(model, test="Chi")
#moZnA USUN¥C TYLKO 3 RZEDU, inaczej nie byloby hierqarchi