
library(RColorBrewer)
library(akima)
library(fields)

##### Load output from models

load("../output/parasite/pred.mat_s.Rdata") #Anti-parasite

load("../output/fever/arr.mat_fever.Rdata") #Anti-disease

breaks3=seq(2.5, 6.2, length.out=55)
age.seq<-seq(.5, 11, .1)
eir.seq<-10^seq(log(1.4,10), log(350,10), length.out=40)

#### generate interpolated matrices to find smooth fever threshold

x.int<-rep(seq(.5, 11, length.out=40), 40)
y.int<- c(matrix(rep(seq(1,6.2, length.out=40), 40) ,ncol=40, byrow=T))
z.int<-c(matrix(rep(10^seq(log(1.4,10), log(350,10), length.out=40), 40) ,ncol=40, byrow=T))
x.0<-seq(.5, 11, length.out=100)
y.0<-seq(1,6.2, length.out=100)
z.0<-10^seq(log(1.4,10), log(350,10), length.out=100)

mat_age_eir<-matrix(NA, nrow=100, ncol=40)

for (e in 1:40) {
  
  mat.temp<-matrix(interp(x.int, y.int, arr.mat[,,e], xo=x.0, yo=y.0)[[3]], ncol=100)
  for (a in 1:100) {
    mat_age_eir[a,e]<-seq(1, 6.2, length.out=100)[which(mat.temp[a,]>38)][1]
  }
}

mat_age_eir[which(is.na(mat_age_eir))]<-6.2

###############################################
#Figure anti-parasite and anti-disease immunity
#############################################
quartz(height=4, width=6)

### First anti-parasite
y.int<-c(matrix(rep(10^seq(log(1.4,10), log(350,10), length.out=40), 40), ncol=40,  byrow=T))
x.int<-rep(seq(.5, 11, length.out=40), 40)
x.0=seq(.5, 11, length.out=100)
y.0=10^seq(log(1.4,10), log(350,10), length.out=100)
par(mfrow=c(1,2))
par( mar=c(4, 4, 3, .5))
image(interp(x=x.int, y=y.int, z=pred.mat_s, xo=x.0, yo=y.0), xlab="Age (years)", ylab="aEIR", main="", breaks=breaks3, col=tim.colors(54), log="y", cex.axis=1.1, cex.lab=1.1, zlim=range(breaks3))
age.seq<-seq(.5, 11, .1)

### Then anti-disease
y.int<-c(matrix(rep(10^seq(log(1.4,10), log(350,10), length.out=40), 100), ncol=40,  byrow=T))
x.int<-rep(seq(.5, 11, length.out=100), 40)
x.0=seq(.5, 11, length.out=100)
y.0=10^seq(log(1.4,10), log(350,10), length.out=100)

par( mar=c(4, .5, 3, 5))
image(interp(x=x.int, y=y.int, z=mat_age_eir, xo=x.0, yo=y.0), xlab="Age (years)", ylab="", yaxt="n", main="", breaks=breaks3, col=tim.colors(54), log="y", cex.axis=1.1, cex.lab=1.1, zlim=range(breaks3))
image.plot(interp(x=x.int, y=y.int, z=mat_age_eir, xo=x.0, yo=y.0), xlab="Age (years)", ylab="aEIR", main="", breaks=breaks3, col=tim.colors(54), log="y", cex.axis=1.2, cex.lab=1.5, zlim=range(breaks3), legend.only=T)

