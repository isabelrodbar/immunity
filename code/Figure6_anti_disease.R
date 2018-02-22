
library(RColorBrewer)
library(akima)
library(fields)

##### Load output from models

print(load("../output/fever/arr.mat_fever.Rdata") #Anti-disease)

breaks3=seq(2.5, 6.2, length.out=55)
age.seq<-seq(.5, 11, .1)
eir.seq<-10^seq(log(1.4,10), log(350,10), length.out=40)

###############################################
#Figure anti-disease immunity
#############################################
col.pal<-colorRampPalette(brewer.pal(8, "RdYlBu"))

x.int<-rep(seq(.5, 11, length.out=40), 40)
y.int<- c(matrix(rep(seq(1,6.2, length.out=40), 40) ,ncol=40, byrow=T))
x.0<-seq(.5, 11, length.out=50)
y.0<-seq(1,6.2, length.out=50)


quartz(height=4, width=8)
breaks2<-seq(36.5,40, length.out=56)
par(mfrow=c(1,4))
par(oma=c(1,4,1,4))
par(mar=c(4, .5, 3, .5))
image(matrix(interp(x.int, y.int,  z=matrix(arr.mat[,,1], nrow=40), xo=x.0, yo=y.0)[[3]], ncol=50), x=x.0, y=10^y.0,  main="", breaks=breaks2, col=rev(col.pal(55)), zlim=range(breaks2), cex.lab=1.5, cex.axis=1.2, log="y")
mtext("Parasite Density ", side=2, line =3)
contour(matrix(interp(x.int, y.int,  z=matrix(arr.mat[,,1], nrow=40), xo=x.0, yo=y.0)[[3]], ncol=50), x=x.0, y=10^y.0,  main="Mean aEIR = 2", breaks=breaks2, col=tim.colors(55), zlim=range(breaks2), add=T, level=c(38, 39), log="y", drawlabels=FALSE)

par(mar=c(4, .5, 3, .5))
image(matrix(interp(x.int, y.int, arr.mat[,,11], xo=x.0, yo=y.0)[[3]], ncol=50), x=seq(.5, 11, length.out=50), y=10^seq(1,6.2, length.out=50), xlab="Age (years)", ylab="", yaxt="n",  main="", breaks=breaks2, col=rev(col.pal(55)), zlim=range(breaks2), cex.lab=1.5, cex.axis=1.2, log="y")
contour(matrix(interp(x.int, y.int, arr.mat[,,11], xo=x.0, yo=y.0)[[3]], ncol=50), x=seq(.5, 11, length.out=50), y=10^seq(1,6.2, length.out=50), xlab="Age (years)", ylab="", yaxt="n",  main="Mean aEIR = 10", breaks=breaks2, col=tim.colors(55), zlim=range(breaks2), add=T, level=c(38, 39), log="y", drawlabels=FALSE)


par(mar=c(4, .5, 3, .5))
image(matrix(interp(x.int, y.int, arr.mat[,,28], xo=x.0, yo=y.0)[[3]], ncol=50), x=seq(.5, 11, length.out=50), y=10^seq(1,6.2, length.out=50), xlab="Age (years)",  ylab="", yaxt="n", main="", breaks=breaks2, col=rev(col.pal(55)), zlim=range(breaks2), cex.lab=1.5, cex.axis=1.2, log="y")
contour(matrix(interp(x.int, y.int,  z=matrix(arr.mat[,,28], nrow=40), xo=x.0, yo=y.0)[[3]], ncol=50), x=x.0, y=10^y.0,  main="Mean aEIR ~ 2", breaks=breaks2, col=tim.colors(55), zlim=range(breaks2), add=T, level=c(38, 39), log="y", drawlabels=FALSE)


par(mar=c(4, .3, 3, 1))
image.plot(matrix(interp(x.int, y.int, arr.mat[,,37], xo=x.0, yo=y.0)[[3]], ncol=50), x=seq(.5, 11, length.out=50), y=10^seq(1,6.2, length.out=50), xlab="Age (years)",  ylab="", yaxt="n", main="", breaks=breaks2, col=rev(col.pal(55)), zlim=range(breaks2), smallplot=c(.95,1,0.15,.85), cex.lab=1.5, cex.axis=1.2, log="y")
contour(matrix(interp(x.int, y.int,  z=matrix(arr.mat[,,37], nrow=40), xo=x.0, yo=y.0)[[3]], ncol=50), x=x.0, y=10^y.0,  main="Mean aEIR ~ 2", breaks=breaks2, col=tim.colors(55), zlim=range(breaks2), add=T, level=c(38,39), log="y", drawlabels=FALSE)
