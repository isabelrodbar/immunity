
library(doBy)
library(plyr)

dat<-read.csv("../data/data_PRISM.csv")

expanded<-dat
expanded$log_parsdens<-log(expanded$parasitedensity, 10)
expanded$cut_ages<-cut(expanded$age, seq(.5, 11, .5))
expanded$cut_eir<-cut(log(expanded$eir_geom3, 10), seq(0.1699477, 2.7648223, .1))

mid.ages<-seq(.5, 11, .5)[1:21]+diff(seq(.5, 11, .5))/2
mid.eir<-seq(0.1699477, 2.7648223, .1)[1:25]+diff(seq(0.1699477, 2.7648223, .1))/2

cols.rgb2<-adjustcolor(c("forestgreen", "orangered", "royalblue"), .5)


##################################################
#Figure 3 - panel a
##################################################
all<-expanded[which( expanded$parasitedensity>0),]

par_wind_a<-ddply(all, .(cut_ages, uid_f), summarize, meanpars=mean(log_parsdens), siteid=as.numeric(siteid[1]))
par_winds_a<-ddply(par_wind_a, .(cut_ages), summarize, median=median(meanpars), upper=quantile(meanpars, .75), lower=quantile(meanpars, .25) )

par_wind_a$age_group<-mid.ages[as.numeric(par_wind_a[,1])]
par_winds_a$age_group<-mid.ages[as.numeric(par_winds_a[,1])]

quartz(height=4.5, width=6)
par(mfrow=c(1,2), mar=c(4,4,1,.5))
plot(jitter(par_wind_a$age_group , 1), jitter(10^par_wind_a[,3], .01), pch=19, cex=.4, col=cols.rgb2[par_wind_a$siteid], ylab="Parasite density (parasites/uL)", xlab="Age (years)", cex.lab=1.5, cex.axis=1.5, main="", log="y", ylim=c(50, 5000000))

points(par_winds_a$age_group, 10^par_winds_a$median, cex=1.1, col="grey30", pch=15, )

for(i in 1:nrow(par_winds_a)) {
  segments(x0=par_winds_a$age_group[i], y0=10^par_winds_a$lower[i], y1=10^par_winds_a$upper[i], col="grey30")
}
legend("topright", legend=c("Walakuba", "Kihihi", "Nagongera"), col=c("forestgreen", "orangered", "royalblue"), pch=19, bty="n", cex=1)

## Now with aEIR
par_wind_a<-ddply(all, .(cut_eir, uid_f), summarize, meanpars=mean(log_parsdens), siteid=as.numeric(siteid[1]))
par_winds_a<-ddply(par_wind_a, .(cut_eir), summarize, median=median(meanpars), upper=quantile(meanpars, .75), lower=quantile(meanpars, .25) )

par_wind_a$eir_group<-mid.eir[as.numeric(par_wind_a[,1])]
par_winds_a$eir_group<-mid.eir[as.numeric(par_winds_a[,1])]
par(mar=c(4,.5,1,4))

plot(jitter(par_wind_a$eir_group , 1), jitter(10^par_wind_a[,3], .01), pch=19, cex=.4, col=cols.rgb2[par_wind_a$siteid], xlab="aEIR", cex.lab=1.5, cex.axis=1.5, main="", log="y", ylim=c(50, 5000000),yaxt="n", xaxt="n")
axis(1, at=log(c(2, 5, 10, 20, 50, 100, 200), 10), labels=c(2, 5, 10, 20, 50, 100, 200), cex.lab=1.5, cex.axis=1.5)

# lines(seq(.5, 11, .1), predict(loess(10^meanpars ~ age_group, data=par_wind_a[which(par_wind_a$siteid==3),],), newdata=data.frame(age_group=seq(.5, 11, .1))), col="royalblue", lwd=2)
# lines(seq(.5, 11, .1), predict(loess(10^meanpars ~ age_group, data=par_wind_a[which(par_wind_a$siteid==2),], span=1), newdata=data.frame(age_group=seq(.5, 11, .1))), col="orangered", lwd=2)
# lines(seq(.5, 11, .1), predict(loess(10^meanpars ~ age_group, data=par_wind_a[which(par_wind_a$siteid==1),], span=1), newdata=data.frame(age_group=seq(.5, 11, .1))), col="forestgreen", lwd=2)


points(par_winds_a$eir_group, 10^par_winds_a$median, cex=1.1, col="grey30", pch=15, )

for(i in 1:nrow(par_winds_a)) {
  segments(x0=par_winds_a$eir_group[i], y0=10^par_winds_a$lower[i], y1=10^par_winds_a$upper[i], col="grey30")
}
legend("topright", legend=c("Walakuba", "Kihihi", "Nagongera"), col=c("forestgreen", "orangered", "royalblue"), pch=19, bty="n", cex=1)





##################################################
#Figure 3 - panel b
##################################################

### with age
all<-expanded[which( expanded$parasitedensity>50000 & expanded$parasitedensity<200000),]
all<-expanded[which( expanded$parasitedensity>0),]

par_wind_a<-ddply(all, .(cut_ages, uid_f), summarize, meanpars=mean(temperature), meanpars2=mean(log_parsdens), siteid=as.numeric(siteid[1]))
par_winds_a<-ddply(par_wind_a, .(cut_ages), summarize, median=median(meanpars),  median2=median(meanpars2), upper=quantile(meanpars, .75), lower=quantile(meanpars, .25) )
par_wind_a$age_group<-mid.ages[as.numeric(par_wind_a[,1])]
par_winds_a$age_group<-mid.ages[as.numeric(par_winds_a[,1])]

quartz(height=4.5, width=6)
par(mfrow=c(1,2), mar=c(4,4,1,.5))
plot(jitter(par_wind_a$age_group , 1), jitter(par_wind_a[,3], .01), pch=19, cex=.4, col=cols.rgb2[par_wind_a$siteid], ylab="Temperature ( C)", xlab="Age (years)", cex.lab=1.5, cex.axis=1.5, main="", ylim=c(36, 41))

points(par_winds_a$age_group, par_winds_a$median, cex=1.1, col="grey30", pch=15, )

for(i in 1:nrow(par_winds_a)) {
  segments(x0=par_winds_a$age_group[i], y0=par_winds_a$lower[i], y1=par_winds_a$upper[i], col="grey30")
}
legend("topright", legend=c("Walakuba", "Kihihi", "Nagongera"), col=c("forestgreen", "orangered", "royalblue"), pch=19, bty="n", cex=1)


### Now with eir
par_wind_a<-ddply(all, .(cut_eir, uid_f), summarize, meanpars=mean(temperature), meanpars2=mean(log_parsdens), siteid=as.numeric(siteid[1]))
par_winds_a<-ddply(par_wind_a, .(cut_eir), summarize, median=median(meanpars), median2=median(meanpars2), upper=quantile(meanpars, .75), lower=quantile(meanpars, .25) )

par_wind_a$eir_group<-mid.eir[as.numeric(par_wind_a[,1])]
par_winds_a$eir_group<-mid.eir[as.numeric(par_winds_a[,1])]

par(mar=c(4,.5,1,4))
plot(jitter(par_wind_a$eir_group , 1), jitter(par_wind_a[,3], .01), pch=19, cex=.4, col=cols.rgb2[par_wind_a$siteid], ylab="", xlab="aEIR", cex.lab=1.5, cex.axis=1.5, main="",  xaxt="n", ylim=c(36, 41), yaxt="n")
axis(1, at=log(c(2, 5, 10, 20, 50, 100, 200), 10), labels=c(2, 5, 10, 20, 50, 100, 200), cex.lab=1.5, cex.axis=1.5)
points(par_winds_a$eir_group, par_winds_a$median, cex=1.1, col="grey30", pch=15)

for(i in 1:nrow(par_winds_a)) {
  segments(x0=par_winds_a$eir_group[i], y0=par_winds_a$lower[i], y1=par_winds_a$upper[i], col="grey30")
}
legend("topright", legend=c("Walakuba", "Kihihi", "Nagongera"), col=c("forestgreen", "orangered", "royalblue"), pch=19, bty="n", cex=1)




