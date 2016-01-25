gator<-read.table('file:///h:/Windows7/Desktop/gator.data', h=T)
library('nnet')
is.numeric(gator$food)
#ryby, bezkr , egowce, gady, ptaki, inne
gator$food<-factor(gator$food, levels=1:5, labels=c("ryby", "bezkregowce", "gady", "ptaki", "inne"))
gator$size<-factor(gator$size, levels=1:2, labels=c("<2.3", ">=2.3"))
gator$gender<-factor(gator$gender, levels=1:2, labels=c("M", "F"))
gator$lake<-factor(gator$lake, levels=1:4, labels=c("Hancock", "Oklawaha", "Trafford", "George"))

fitS<-multinom(food~lake*size*gender,data=...)
#FG, FS, FL
fitS_1<-multinom(gator$food~1, data=gator)
fitS_g<-multinom(gator$food~gator$gender, data=gator)
fitS_s<-multinom(gator$food~gator$size, data=gator)
fitS_l<-multinom(gator$food~gator$lake, data=gator)
#G,S,L
fitS_gs<-multinom(gator$food~gator$gender*gator$size, data=gator)
fitS_gl<-multinom(gator$food~gator$gender*gator$lake, data=gator)
fitS_ls<-multinom(gator$food~gator$lake*gator$size, data=gator)
#FG+S+L

fitS_gsl<-multinom(gator$food~gator$lake+gator$size+gator$gender, data=gator)
fitS_all<-multinom(food~lake*size*gender,data=gator)

deviance(fitS_1)-deviance(fitS_all)

gator2<-matrix(0, nrow=1, ncol=5)
gator_m<-as.matrix(gator)
for(i in 1:nrow(gator_m)){
  if(as.numeric(gator_m[i,5])!=0){
  for(j in 1:as.numeric(gator_m[i,5])){
    gator2<-rbind(gator2, gator_m[i,])
  }
  }
}
gator2<-gator2[-1, -5]

#FG, FS, FL
gator2<-data.frame(gator2)
fitS_1<-multinom(gator2$food~1, data=gator2)
fitS_g<-multinom(gator2$food~gator2$gender, data=gator2)
fitS_s<-multinom(gator2$food~gator2$size, data=gator2)
fitS_l<-multinom(gator2$food~gator2$lake, data=gator2)
#G,S,L
fitS_gs<-multinom(gator2$food~gator2$gender*gator2$size, data=gator2)
fitS_gl<-multinom(gator2$food~gator2$gender*gator2$lake, data=gator2)
fitS_ls<-multinom(gator2$food~gator2$lake*gator2$size, data=gator2)

fitS_gsglls<-multinom(gator2$food~gator2$lake*gator2$size+gator2$gender*gator2$lake+gator2$gender*gator2$size, data=gator2)


fitS_gsl<-multinom(gator2$food~gator2$lake+gator2$size+gator2$gender, data=gator2)
fitS_all<-multinom(food~lake*size*gender,data=gator2)
deviance(fitS_1)-deviance(fitS_all)
deviance(fitS_gsglls)-deviance(fitS_all)

fitS_l_s<-multinom(gator2$food~gator2$lake+gator2$size, data=gator2)

fitS_g<-multinom(gator2$food~gator2$gender, data=gator2)
summary(fitS_g)
summary(fitS_l_s)

tab<-table(gator2$size, gator2$lake)

tab_ho<-table(gator2$size[gator2$lake=='Hancock'], gator2$food[gator2$lake=='Hancock'])
tab_oo<-table(gator2$size[gator2$lake=='Oklawaha'], gator2$food[gator2$lake=='Oklawaha'])
tab_go<-table(gator2$size[gator2$lake=='George'], gator2$food[gator2$lake=='George'])
tab_to<-table(gator2$size[gator2$lake=='Trafford'], gator2$food[gator2$lake=='Trafford'])

tab_he<-tab_ho
tab_oe<-tab_oo
tab_ge<-tab_go
tab_te<-tab_to

for(i in 1:2){
 
    tmp<-fitS_l_s$fitted.values[which(gator2$lake=='Hancock' & gator2$size==rownames(tab_ho)[i]),]
    tab_he[i,]<-tmp[1,]*(tab_ho[i,])
    
    tmp<-fitS_l_s$fitted.values[which(gator2$lake=='Oklawaha' & gator2$size==rownames(tab_ho)[i]),]
    tab_oe[i,]<-tmp[1,]*(tab_oo[i,])
    
    tmp<-fitS_l_s$fitted.values[which(gator2$lake=='George' & gator2$size==rownames(tab_ho)[i]),]
    tab_ge[i,]<-tmp[1,]*(tab_go[i,])
    
    tmp<-fitS_l_s$fitted.values[which(gator2$lake=='Trafford' & gator2$size==rownames(tab_ho)[i]),]
    tab_te[i,]<-tmp[1,]*(tab_to[i,])
  
}
fitS_l_s$fitted.values[1,]

fitS_l_s$fitted.values[which(gator2$size=='<2.3' & gator2$lake=='Hancock'),]*39
fitS_l_s$fitted.values[which(gator2$size=='>=2.3' & gator2$lake=='Hancock'),]*16
tab
rownames(tab)[1]

table(data2$size, data2$lake)


x1  = fitS_l_s$fitted.values[which(gator2$lake=="Hancock" & gator2$size=="<2.3"),]*39
x1[1,]
x2 = fitS_l_s$fitted.values[which(gator2$lake=="Hancock" & gator2$size==">=2.3"),]*16
x2[1,]
x3  = fitS_l_s$fitted.values[which(gator2$lake=="Oklawaha" & gator2$size=="<2.3"),]*20
x3[1,]
x4 = fitS_l_s$fitted.values[which(gator2$lake=="Oklawaha" & gator2$size==">=2.3"),]*28
x4[1,]
x5  = fitS_l_s$fitted.values[which(gator2$lake=="George" & gator2$size=="<2.3"),]*41
x5[1,]
x6 = fitS_l_s$fitted.values[which(gator2$lake=="George" & gator2$size==">=2.3"),]*22
x6[1,]
x7  = fitS_l_s$fitted.values[which(gator2$lake=="Trafford" & gator2$size=="<2.3"),]*24
x7[1,]
x8 = fitS_l_s$fitted.values[which(gator2$lake=="Trafford" & gator2$size==">=2.3"),]*29
x8[1,]
X=rbind(x1[1,], x2[1,], x3[1,], x4[1,], x5[1,], x6[1,], x7[1,], x8[1,])
rownames(X)=c("Hancock<=2.3", "Hancock>2.3", "Oklawaha<=2.3", "Oklawaha>2.3", "George<=2.3", "George>2.3",
              "Trafford<=2.3", "Trafford>2.3")
X

tab_he<-X[1:2,]
tab_oe<-X[3:4,]
tab_ge<-X[5:6,]
tab_te<-X[7:8,]

chisq.test(tab_he, tab_ho)
chisq.test(tab_oe, tab_oo)
chisq.test(tab_ge, tab_go)
chisq.test(tab_te, tab_to)
sum((X-rbind(tab_ho, tab_oo, tab_go, tab_to))^2/X)
chisq.test(X, rbind(tab_ho, tab_oo, tab_go, tab_to))

gator3<-gator2
summary(fitS_l_s, corr=F)
gator3$size<-relevel(gator3$size, ref=">=2.3")
gator3$food<-relevel(gator3$food, ref="ryby")
fitS_l_s<-multinom(food~lake+size, data=gator3)
summary(fitS_l_s)

p=predict(fitS_l_s, newdata=data.frame(size=">=2.3", lake="Hancock"), type="probs")


exp(-1.55-1.66)/(1+
exp(-1.55+1.46*0-1.66*1+0.94*0+1.12*0)+
exp(-1.55+1.46*0-1.66*0+0.94*1+1.12*0)+
exp(-1.55+1.46*0-1.66*0+0.94*0+1.12*1)+
  exp(-1.55+1.46*1-1.66*1+0.94*0+1.12*0)+
  exp(-1.55+1.46*1-1.66*0+0.94*1+1.12*0)+
  exp(-1.55+1.46*1-1.66*0+0.94*0+1.12*1))

gator3$lake<-relevel(gator3$lake, ref='Hancock')
gator3$size<-relevel(gator3$size, ref='<2.3')
gator3$food<-relevel(gator3$food, ref="ryby")
fitS_l_s<-multinom(food~lake+size, data=gator3)
summary(fitS_l_s)

exp(-1.7491448-1.4579947)/(1+
exp(-2.4233052+0.3516687)+
exp(-0.7465718-0.3314015)+
exp( -2.028315+0.6305244))

gator3$lake<-relevel(gator3$lake, ref='Oklawaha')
gator3$size<-relevel(gator3$size, ref='>=2.3')
gator3$food<-relevel(gator3$food, ref="ryby")
fitS_l_s<-multinom(food~lake+size, data=gator3)
summary(fitS_l_s)

exp(-0.6117756-2.5956415)/(1+
                             exp(-0.6117756-2.5956415)+
                             exp(-0.8556164-1.2161443)+
                             exp(-1.8984732 +0.8203809)+
                            exp(-2.7462496  + 1.3482723))


