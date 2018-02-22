
library(RColorBrewer)
library(akima)
library(fields)

##### Load output from models

print(load("../output/malaria/pred.mat_mal_mod2.Rdata"))

breaks3=seq(0.3, 1 , length.out=55)
spec.pal<-colorRampPalette(brewer.pal(8, "Spectral"))

y.int<-c(matrix(rep(10^seq(log(1.4,10), log(350,10), length.out=40), 40), ncol=40,  byrow=T))
x.int<-rep(seq(.5, 11, length.out=40), 40)
x.0=seq(.5, 11, length.out=100)
y.0=10^seq(log(1.4,10), log(350,10), length.out=100)

quartz(height=4.5, width=4)
par( mar=c(4.5, 4.5, 3, 4))
image.plot(interp(x=x.int, y=y.int, z=exp(pred.mat_s)/(1+exp(pred.mat_s)), xo=x.0, yo=y.0), xlab="Age (years)", ylab="aEIR", main="", breaks=breaks3, col=tim.colors(59)[1:54], log="y", cex.axis=1.1, cex.lab=1.1, zlim=range(breaks3), ylim=c(1.4, 200))




