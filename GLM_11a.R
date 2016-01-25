impair <- read.table('h:/Windows7/Desktop/impair.data', h=T)
summary(impair)

library(MASS)
g <- polr(mental ~ ses + events, data = impair)
#mental musi byæ czynnikiem

impair$mental <- ordered(impair$mental, levels = 1:4, labels = c('dobry', '³agodny', 'umiarkowany', 'zaburzony'))
g <- polr(mental ~ ses + events, data = impair)

# s¹ minusy
summary(g)

g_coef <- -g$coefficients

install.packages('VGAM')
library(VGAM)

vg <- vglm( mental ~ ses + events, family = cumulative(parallel = TRUE), data = impair)
summary(vg)

summary(g)

exp(-0.2819+g_coef%*%c(0, 4.275))/(1+exp(-0.2819+g_coef%*%c(0, 4.275)))

predict(g, newdata=test, type='probs')

fun <- function(x, ss){
  test_s<-matrix(c(ss, x), ncol=2)
  colnames(test)<-c('ses', 'events')
  pred <- predict(g, newdata=test_s, type='probs')
  #exp(-0.2819+g_coef%*%c(ss, x))/(1+exp(-0.2819+g_coef%*%c(ss, x)))
  pred[3:4]
}

ee <- seq(0,9, length=100)
ss <-c(0,1)

ppp <- matrix(0, nrow=length(ee), ncol=length(ss))
for(s in 1:length(ss)){
  for(e in 1:length(ee)){
    ppp[e,s] <- fun(ee[e], ss[s])
  }
}

exp(-0.2819+g_coef%*%c(0, 4.275))/(1+exp(-0.2819+g_coef%*%c(0, 4.275)))

#e

exp(-0.2819+4.275*(-0.3189)+1*1.1112)/(1+exp(-0.2819+4.275*(-0.3189)+1*1.1112))

#f
summary(dane$events)
#ses=1

exp(-0.2819+2*(-0.3189)+1*1.1112)/(1+exp(-0.2819+2*(-0.3189)+1*1.1112))
exp(-0.2819+6.25*(-0.3189)+1*1.1112)/(1+exp(-0.2819+6.25*(-0.3189)+1*1.1112))

#ses=0
exp(-0.2819+2*(-0.3189))/(1+exp(-0.2819+2*(-0.3189)))
exp(-0.2819+6.25*(-0.3189))/(1+exp(-0.2819+6.25*(-0.3189)))