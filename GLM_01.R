x<-c(1,2,3,4,5,6)
y<-c(1,1,1,0,0,0)
model<-glm(y~x, family="binomial")
model
summary(model)
model$iter

n<-25
n<-c(25, 50)
y<-c(10, 20)

curve(x, x^y[1]*(1-x)^(n[1]-y[1])*x^y[2]*(1-x)^(n[2]-y[2]))

logL<-function(p, n, y){
  choose(n, y)+y*log(p)+(n-y)*log(1-p)
}

p<-seq(0,1,length=100)

logL(p, n[1], y[1])

plot(logL(p, n[1], y[1]))
lines(logL(p, n[2], y[2]))

curve(logL(x, n[2], y[2])-logL(y[2]/n[2],n[2], y[2]))
curve(logL(x, n[1], y[1])-logL(y[1]/n[1], n[1], y[1]), add=T, col="red")

?nlm

f<-function(x) -logL(x, n[1], y[1])
nlm<-nlm(f, p=0.2, hessian=T)

hessian<-nlm$hessian
1/hessian

n[1]*nlm$estimate*(1-nlm$estimate)

data(bliss)

conc<-c(0,1,2,3,4)
dead<-c(2,8,15,23,27)
number<-c(30,30,30,30,30)
alive<-number-dead
bliss<-cbind(conc, dead, number, alive)
bliss<-as.data.frame(bliss)

y<-cbind(bliss$dead, bliss$alive)

model<-glm(y~bliss$conc, family="binomial")
model
summary(model)


bliss2<-matrix(0,nrow=150, ncol=2)
bliss2[1:2,]<-c(0,1,0)
bliss2[3:30,]<-c(0,0,1)
bliss2[31:38,]<-c(1,1,0)
bliss2[39:60,]<-c(1,0,1)
bliss2[61:75,]<-c(2,1,0)
bliss2[76:90,]<-c(2,0,1)
bliss2[91:113,]<-c(3,1,0)
bliss2[114:120,]<-c(3,0,1)
bliss2[121:147,]<-c(4,1,0)
bliss2[148:150,]<-c(4,0,1)

bliss2<-matrix(0,nrow=150, ncol=2)
k<-1
for(i in 1:nrow(bliss)){
 
  bliss2[k:(30*(k+1)-1), 1]<-bliss[i,1]
  bliss2[k:(30*(k+1)-1), 2]<-0
  
  for(j in 1:bliss[i,2]){
   bliss2[k, 1]<-bliss[i,1]
   bliss2[k,2]<-1
   k<-k+1
 } 
  bliss2[k, 1]<-bliss[i,1]
  bliss2[k,2]<-0
  k<-k+1
  
}

b_old<-c(0,0)
pi<-bliss[,4]/30
pi
W<-diag(pi*(1-pi))
W

logit<-function(x){
  log(x/(1-x))
}

ilogit<-function(x){
  exp(x)/(1+exp(x))
}
logit(1/2)

X<-cbind(c(1,1,1,1,1), bliss$alive)
z<-logit(pi)+
z<-X%*%b_old+solve(W)*(y-pi)
lm(conc~z, weights = pi*(1-pi))
