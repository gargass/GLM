install.packages('PBImisc')
library(PBImisc)
data(milk)
milk <- as.data.frame(milk)

library("sqldf")
colnames(milk) <- c("cow", "amount")
tab <- sqldf('select cow, avg(amount) as avg from milk group by cow')
library(lattice)
dotplot(tab)

colnames(milk) <- c("cow", "amount")
)
library(lme4)
model1<-lmer(amount~(1|cow), data=milk)
summary(model1)

model2 <- lm(amount ~ cow, data=milk)

s <- cbind(as.matrix(ranef(model1)$cow) + fixef(model1), model2$coefficients)
colnames(s) <- c("ocena mieszany", "ocena lm")
