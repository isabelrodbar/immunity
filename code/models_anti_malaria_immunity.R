
library(mgcv)

dat<-read.csv("../data/data_PRISM.csv")
dat$log_parsdens<-log(dat$parasitedensity, 10)
dat$mal.epis<-ifelse(dat$parasitedensity>0 & dat$febrile==1, 1, 0)

#### Models Anti-malaria immunity

## Model 1
mod_mal_0<-gam(mal.epis~age+log(eir_geom3)+ s(uid_f, bs="re") + s(hhid_f, bs="re"),  family="binomial", data=dat)
save(mod_mal_0, file="output/malaria/mod_mal_0.Rdata")

## Model 2
mod_mal_1<-gam(mal.epis~s(age) + s(log(eir_geom3))+ s(uid_f, bs="re") + s(hhid_f, bs="re") , family="binomial", data=dat)
save(mod_mal_1, file="../output/malaria/mod_mal_1.Rdata")

## Model 3
mod_mal_2<-gam(mal.epis~te(age, log(eir_geom3), bs="cr", k=5)+ s(uid_f, bs="re") + s(hhid_f, bs="re"), family="binomial", data=dat)
save(mod_mal_2, file="../output/malaria/mod_mal_2.Rdata")

print(summary(mod_mal_0))
print(summary(mod_mal_1))
print(summary(mod_mal_2))

## compare AIC of these 3 models
print(AIC(mod_mal_0, mod_mal_1, mod_mal_2))


#### Generate some output from model 2
### Generate two matrices containing predicted values 

new.dat_s<-data.frame(age=rep(seq(.5, 11, length.out=40), 40),uid_f=rep("3357", 1600), hhid_f=rep("143009503", 1600), symp=rep(1, 1600), eir_geom3=c(matrix(rep(10^(seq(log(1.4, 10), log(350, 10), length.out=40)), 40), ncol=40, byrow=T)))
# 
pred.mod<-predict(mod_mal_2, newdata=new.dat_s, se.fit=T)
pred.mat_s<-matrix(pred.mod[[1]], ncol=40)
pred.mat_s_se<-matrix(pred.mod[[2]], ncol=40)


save(pred.mat_s, file="../output/malaria/pred.mat_mal_mod2.Rdata")
save(pred.mat_s_se, file="../output/malaria/pred.mat_mal_mod2_se.Rdata") 
