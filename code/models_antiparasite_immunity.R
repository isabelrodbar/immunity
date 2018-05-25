
library(mgcv)


dat<-read.csv("data/data_PRISM.csv")

### The individual (uid_f) and household (hhid_f) variables need to be factors
dat$uid_f<-as.factor(dat$uid_f)
dat$hhid_f<-as.factor(dat$hhid_f)

#### Alternatively you can read the .Rdata file where "uid_f" and "hhid_f" are factors
###load("data/data_PRISM.Rdata")

dat$log_parsdens<-log(dat$parasitedensity, 10)

#### Models Anti-Parasite immunity

## Model 1
mod_pars_0<-gam(log_parsdens~age+log(eir_geom3) + s(uid_f, bs="re") + s(hhid_f, bs="re"), data=dat)
save(mod_pars_0, file="output/parasite/mod_pars_0.Rdata")

## Model 2
mod_pars_1<-gam(log_parsdens~s(age, log(eir_geom3)) + s(uid_f, bs="re") + s(hhid_f, bs="re"), data=dat)
save(mod_pars_1, file="output/parasite/mod_pars_1.Rdata")

## Model 3 (Linear interaction)
mod_pars_2a<-gam(log_parsdens~age*log(eir_geom3) + s(uid_f, bs="re") + s(hhid_f, bs="re"), data=dat)
save(mod_pars_2a, file="output/parasite/mod_pars_2a.Rdata")

## Model 4
mod_pars_2<-gam(log_parsdens~te(age, log(eir_geom3), bs="cr", k=5) + s(uid_f, bs="re") + s(hhid_f, bs="re"), data=dat)
save(mod_pars_2, file="output/parasite/mod_pars_2.Rdata")

print(summary(mod_pars_0))
print(summary(mod_pars_1))
print(summary(mod_pars_2a))
print(summary(mod_pars_2))


## compare AIC of these 3 models
print(AIC(mod_pars_0, mod_pars_1, mod_pars_2a, mod_pars_2))

#### Generate some output from model 2
### Generate two matrices containing predicted values 

new.dat_s<-data.frame(age=rep(seq(.5, 11, length.out=40), 40),uid_f=rep("3357", 1600), hhid_f=rep("143009503", 1600), symp=rep(1, 1600), eir_geom3=c(matrix(rep(10^(seq(log(1.4, 10), log(350, 10), length.out=40)), 40), ncol=40, byrow=T)))

pred.mod<-predict(mod_pars_2, newdata=new.dat_s ,se.fit=T)
pred.mat_s<-matrix(pred.mod[[1]], ncol=40)
pred.mat_s_se<-matrix(pred.mod[[2]], ncol=40)

save(pred.mat_s, file="output/parasite/pred.mat_s.Rdata")
save(pred.mat_s_se, file="output/parasite/pred.mat_pars_se.Rdata")
 
