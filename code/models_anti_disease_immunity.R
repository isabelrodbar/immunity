
library(mgcv)

dat<-read.csv("data/data_PRISM.csv")

### The individual (uid_f) and household (hhid_f) variables need to be factors
dat$uid_f<-as.factor(dat$uid_f)
dat$hhid_f<-as.factor(dat$hhid_f)

#### Alternatively you can read the .Rdata file where "uid_f" and "hhid_f" are factors
###load("data/data_PRISM.Rdata")

dat$log_parsdens<-log(dat$parasitedensity, 10)


#### Models Anti-disease immunity

## Model 1 
mod_fev_0<-gam(temperature~age + log(eir_geom3) + log_parsdens+ s(uid_f, bs="re") + s(hhid_f, bs="re"), data=dat)
save(mod_fev_0, file = "output/fever/mod_fev_0.Rdata")

## Model 2
mod_fev_1<-gam(temperature~s(age) + s(log(eir_geom3)) + s(log_parsdens) + s(uid_f, bs="re") + s(hhid_f, bs="re"), data=dat)
save(mod_fev_1, file = "output/fever/mod_fev_1.Rdata")

## Model 3
mod_fev_2a<-gam(temperature~age*log(eir_geom3)*log_parsdens + s(uid_f, bs="re") + s(hhid_f, bs="re"), data=dat)
save(mod_fev_2a, file = "output/fever/mod_fev_2a.Rdata")

## Model 4
mod_fev_2<-gam(temperature~te(age, log(eir_geom3), log_parsdens, bs="cr", k=5) + s(uid_f, bs="re") + s(hhid_f, bs="re"), data=dat)
save(mod_fev_2, file = "output/fever/mod_fev_2.Rdata")

print(summary(mod_fev_0))
print(summary(mod_fev_1))
print(summary(mod_fev_2a))
print(summary(mod_fev_2))


## compare AIC of these 3 models
print(AIC(mod_fev_0, mod_fev_1, mod_fev_2a, mod_fev_2))

#### Generate some output from best fitting model
### Generate two arrays containing predicted values 

age.seq<-seq(.5, 11, length.out=40)
arr.mat<-array(dim=c(40, 40, 40))
arr.var<-array(dim=c(40, 40, 40))
eir.seq<-10^seq(log(1.4,10), log(350,10), length.out=40)

for( i in 1:40) {
print(i)
new.dat<-data.frame(age=rep(seq(.5, 11, length.out=40), 40), log_parsdens=c(matrix(rep(seq(1,6.2, length.out=40), 40), ncol=40, byrow=T)),  eir_geom3=rep(eir.seq[i],1600), uid_f=rep("3357", 1600), hhid_f=rep("143009503", 1600))

pred.i <- predict(mod_fev_2, newdata=new.dat , se.fit=T)
arr.mat[,,i]<-pred.i[[1]]
arr.var[,,i]<-pred.i[[2]] ### Remember these are standard errors

}

save(arr.mat, file="output/fever/arr.mat_fever.Rdata")
save(arr.var, file="output/fever/arr.var.Rdata")


