
library(RColorBrewer)

expit.fun<-function(x) {exp(x)/(1+exp(x))}

##### Load output from models

###### Anti-disease

print(load("output/fever/arr.mat_fever.Rdata"))
print(load("output/fever/arr.var_fever.Rdata"))

## Confidence bounds
temp_pars_upper<-arr.mat+1.96*arr.var
temp_pars_lower<-arr.mat-1.96*arr.var

###### Anti-malaria

print(load("output/malaria/pred.mat_mal_mod2.Rdata"))
print(load("output/malaria/pred.mat_mal_mod2_se.Rdata"))

prob_mal<-exp(pred.mat_s)/(1+exp(pred.mat_s))
lower_mal<-expit.fun(pred.mat_s-1.96*pred.mat_s_se)
upper_mal<-expit.fun(pred.mat_s+1.96*pred.mat_s_se)

###### Anti-parasite 
print(load("../output/parasite/pred.mat_s.Rdata"))
print(load("../output/parasite/pred.mat_pars_se.Rdata"))

## Confidence bounds
lower<-pred.mat_s-1.96*pred.mat_s_se
upper<-pred.mat_s+1.96*pred.mat_s_se


###### Indices to plot specific ages and aEIR (Age is in rows, eir is in columns)
ind.plot.eir<-c(4, 15, 26, 36)
ind.plot.age<-c(3, 18, 36)
indices.plot=outer(c(1,2,3), c(-.2, -.1, 0, .1), FUN = "+")

cols.plot<-brewer.pal(4, "Set1")


quartz(height=4, width=8)
par(mfrow=c(1, 3))

plot(1,1, ylim=10^c(range(lower, upper)), xlim=c(0.5, 3.5), pch=NA, xaxt="n", log="y", ylab="Parasite density (parasites/uL)", xlab="Age (years)", cex.lab=1.5, cex.axis=1.5)
for( i in 1:length(ind.plot.age)) {
  for (j in 1:length(ind.plot.eir)) {
    points(indices.plot[i, j], 10^pred.mat_s[ind.plot.age[i], ind.plot.eir[j]], pch=15, col=cols.plot[j])
    segments(x0=indices.plot[i, j], x1=indices.plot[i, j], y0=10^lower[ind.plot.age[i], ind.plot.eir[j]], y1=10^upper[ind.plot.age[i], ind.plot.eir[j]], col=cols.plot[j])
  }
}
axis(1, at=c(.95, 1.95, 2.95), labels=c(1, 5, 10 ), cex.lab=1.5, cex.axis=1.5)
legend("bottomleft", legend=c("aEIR", 2, 10, "",  50, 200 ), col=c(NA, cols.plot[1:2] , NA, cols.plot[3:4]), pch=c(NA, 15, 15, NA, 15, 15), bty="n", ncol=2, cex=1.5)

plot(1,1, ylim=c(36.6, 39.4), xlim=c(0.5, 3.5), pch=NA, xaxt="n", ylab="Temperature | Parasite density", xlab="Age (years)", cex.lab=1.5, cex.axis=1.5)
for( i in 1:length(ind.plot.age)) {
  for (j in 1:length(ind.plot.eir)) {
    points(indices.plot[i, j], arr.mat[ind.plot.age[i], 28,  ind.plot.eir[j]], pch=15, col=cols.plot[j])
    segments(x0=indices.plot[i, j], x1=indices.plot[i, j], y0=temp_pars_upper[ind.plot.age[i], 28, ind.plot.eir[j]], y1=temp_pars_lower[ind.plot.age[i], 28, ind.plot.eir[j]], col=cols.plot[j])
  }
}
axis(1, at=c(.95, 1.95, 2.95), labels=c(1, 5, 10 ), cex.lab=1.5, cex.axis=1.5)
legend("bottomleft", legend=c("aEIR", 2, 10, "",  50, 200 ), col=c(NA, cols.plot[1:2] , NA, cols.plot[3:4]), pch=c(NA, 15, 15, NA, 15, 15), bty="n", ncol=2, cex=1.5)



plot(1,1, ylim= c(0,1), xlim=c(0.5, 3.5), pch=NA, xaxt="n", ylab="Prob. symptomatic malaria | infection", xlab="Age (years)", cex.lab=1.5, cex.axis=1.5)
for( i in 1:length(ind.plot.age)) {
  for (j in 1:length(ind.plot.eir)) {
    points(indices.plot[i, j], prob_mal[ind.plot.age[i], ind.plot.eir[j]], pch=15, col=cols.plot[j])
    segments(x0=indices.plot[i, j], x1=indices.plot[i, j], y0=lower_mal[ind.plot.age[i], ind.plot.eir[j]], y1=upper_mal[ind.plot.age[i], ind.plot.eir[j]], col=cols.plot[j])
  }
}
axis(1, at=c(.95, 1.95, 2.95), labels=c(1, 5, 10 ), cex.lab=1.5, cex.axis=1.5)
legend("bottomleft", legend=c("aEIR", 2, 10, "",  50, 200 ), col=c(NA, cols.plot[1:2] , NA, cols.plot[3:4]), pch=c(NA, 15, 15, NA, 15, 15), bty="n", ncol=2, cex=1.5)

