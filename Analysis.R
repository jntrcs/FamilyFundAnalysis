load("Simulation.RData")
start_date<-as.Date("2017-08-01")


###We never transition no goes out of uneligible, so they never become payers
months<-seq(start_date, by = paste (1, "months"), length = 216)

net_parents<- income_parents-expenditures
med<-apply(net_parents,2,quantile, .5)
lb<-apply(net_parents,2,quantile, .025)
ub<-apply(net_parents, 2, quantile,.975)

net_all<- income_total-expenditures
med_all<-apply(net_all,2,quantile, .5)
lb_all<-apply(net_all,2,quantile, .025)
ub_all<-apply(net_all, 2, quantile,.975)

plot(months[1:20], med[1:20], type='l', lwd=3, ylim=c(-1000,5000),main="20 Month Projections", ylab="$ in bank", xlab="Year")
lines(months[1:20],lb[1:20], lty=2)
lines(months[1:20], ub[1:20], lty=2)
lines(months[1:20], med_all[1:20], col="blue", lwd=3)
lines(months[1:20],lb_all[1:20], lty=2, col="blue")
lines(months[1:20], ub_all[1:20], lty=2, col="blue")
abline(h=0, col="Red")
legend("topleft", col=c("blue","black", "black"),legend=c("With Grandkid Support", "Just parents", "95% Confidence Intervals"), lwd=c(3,3,1), lty=c(1,1,2))

plot(months, med, type='l', lwd=3,ylim=c(-2000,65000), main="18 Year Projections", ylab="$ in bank", xlab="Year")
lines(months,lb, lty=2)
lines(months, ub, lty=2)
lines(months, med_all, col="blue", lwd=3)
lines(months,lb_all, lty=2, col="blue")
lines(months, ub_all, lty=2, col="blue")
abline(h=0, col="red")
legend("topleft", col=c("blue","black", "black"),legend=c("With Grandkid Support", "Just parents", "95% Confidence Intervals"), lwd=c(3,3,1), lty=c(1,1,2))

