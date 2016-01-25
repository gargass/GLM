lungcanc <- read.table('h:/Windows7/Desktop/lungcanc.dat', h=T)

#a)
lungcanc_a <- lungcanc
lungcanc_a$death[lungcanc_a$follow>1]<-0

#b)
cbind(lungcanc$row.names[lungcanc$death>=lungcanc$freq], lungcanc$death[lungcanc$death>=lungcanc$freq], lungcanc$freq[lungcanc$death>=lungcanc$freq])
table(lungcanc$cigscat[lungcanc$follow==1], lungcanc$age[lungcanc$follow==1])
table(lungcanc$cigscat, lungcanc$age)
table(lungcanc$cigscat, lungcanc$age)

par(mfrow=c(3,2))
for( i in 1:6)
{
plot(lungcanc$age[lungcanc$follow==i], lungcanc$freq[lungcanc$follow==i], col=lungcanc$death[lungcanc$follow==i])
}
max(lungcanc$follow)
table(lungcanc$death, lungcanc$follow)

lungcanc_a$death[lungcanc_a$death==2 & lungcanc_a$follow==1]=0

dane<-matrix(0, nrow=(max(lungcanc_a$age)-min(lungcanc_a$age)+1), ncol=3)
for( a in 1:nrow(dane)){
  for( c in 1:3){
    dane[a,c]<-sum(lungcanc_a$freq[lungcanc_a$death==1 & lungcanc_a$age==a-1+min(lungcanc_a$age) & lungcanc_a$cigscat==c-1])/sum(lungcanc_a$freq[lungcanc_a$age==a-1+min(lungcanc_a$age) & lungcanc_a$cigscat==c-1])
  }
}

plot(dane[,3])
m<-as.matrix(table(lungcanc$age, lungcanc$follow))
plot(m[,3])

<-glm(skips~., family="poisson", data=solder)
