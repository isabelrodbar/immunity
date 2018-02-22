
library(doBy)

dat<-read.csv("../data/data_PRISM.csv")

# Get unique values for individuals
unique_id<-splitBy(~uid_f, data=dat)
first.visit<-do.call("rbind", lapply(unique_id, function(x) x[1,]))

# get unique values for households
unique.hh<-splitBy(~hhid_f, expanded_2)
eir.g<-do.call(c, lapply(unique.hh, function(x) x$eir_geom3[1]))
siteid.g<- unlist(lapply(unique.hh, function(x) x$siteid[1]))


# Set colors for this plot
cols.rgb.p<-adjustcolor(c("orangered", "royalblue", "forestgreen"), alpha=.5)

col.plot<-NA
col.plot[which(first.visit$siteid=="Tororo")]<-cols.rgb.p[2]
col.plot[which(first.visit$siteid=="Kanungu")]<-cols.rgb.p[1]
col.plot[which(first.visit$siteid=="Jinja")]<-cols.rgb.p[3]


##################################################
#Figure 2 - panel a and b
##################################################

quartz(height=4.5, width=8)
par(mfrow=c(1,2))
hist(log(eir.g[which(siteid.g=="Tororo")],10), na.rm=T, breaks=20, ylim=c(0,20),  border=cols.rgb.p[2], col=cols.rgb.p[2], main="", xlab="aEIR", xlim=c(.1,3.2), xaxt="n", cex.lab=1.4, cex.axis=1.2)
hist(log(eir.g[which(siteid.g=="Kanungu")],10), na.rm=T, xlim=c(1, 6.4), add=T,  border=cols.rgb.p[1], col=cols.rgb.p[1], breaks=10)
hist(log(eir.g[which(siteid.g=="Jinja")],10), na.rm=T, xlim=c(1, 1600), add=T,  border=cols.rgb.p[3], col=cols.rgb.p[3], breaks=10)
legend("topright", legend=c("Nagongera", "Kihihi", "Walukuba"), col=cols.rgb.p[c(2, 1, 3)], pch=15, bty="n", cex=1.3)
axis(1, at=c(log(.5, 10), log(10, 10), log(50, 10), log(200,10), log(500,10), log(1000,10)), labels=c(2, 10, 50, 200, 500, 1000))

plot(log(first.visit$eir_geom3,10), first.visit$haz_all, log="y", pch=19 ,  col=col.plot, cex=.7, ylab="Daily hazard", xlab="aEIR", xaxt="n", cex.axis=1.2, cex.lab=1.4)
axis(1, at=c(log(.5, 10), log(10, 10), log(50, 10), log(200,10), log(500,10), log(1000,10)), labels=c(2, 10, 50, 200, 500, 1000))
legend("bottomright", legend=c("Nagongera", "Kihihi", "Walukuba"), col=cols.rgb.p[c(2, 1, 3)], pch=19, bty="n", cex=1.3)


##################################################
#Explore correlation between these metrics of exposure
##################################################
summary(lm(log(first.visit$haz_all)~log(first.visit$eir_geom3, 10)))
summary(lm(log(first.visit$haz_all[which(first.visit$siteid=="Tororo")])~log(first.visit$eir_geom3[which(first.visit$siteid=="Tororo")], 10)))
summary(lm(log(first.visit$haz_all[which(first.visit$siteid=="Kanungu")])~log(first.visit$eir_geom3[which(first.visit$siteid=="Kanungu")], 10)))
summary(lm(log(first.visit$haz_all[which(first.visit$siteid=="Jinja")])~log(first.visit$eir_geom3[which(first.visit$siteid=="Jinja")], 10)))


