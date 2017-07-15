#load("Simulation.RData")

numSims<-400
income_parents<-matrix(NA, nrow=numSims, ncol=18*12)
income_total<-matrix(NA, nrow=numSims, ncol=18*12)
expenditures<-matrix(NA, nrow=numSims, ncol=18*12)

for (i in 1:numSims){
  load(paste0("Simulations2/Sim",i))
  expenditures[i,]<-full$calendar$total_expenditure
  income_parents[i,]<-full$calendar$money_from_parents
  income_total[i,]<-full$calendar$total_incoming
}

start_date<-as.Date("2017-08-01")
months<-seq(start_date, by = paste (1, "months"), length = 216)

net_parents<- income_parents-expenditures
med<-apply(net_parents,2,quantile, .5)
lb<-apply(net_parents,2,quantile, .025)
ub<-apply(net_parents, 2, quantile,.975)

net_all<- income_total-expenditures
med_all<-apply(net_all,2,quantile, .5)
lb_all<-apply(net_all,2,quantile, .025)
ub_all<-apply(net_all, 2, quantile,.975)

plot(months[1:60], med[1:60], type='l', lwd=3, ylim=c(-2000,12000),main="5 Year Projections", ylab="$ in bank", xlab="Year")
lines(months[1:60],lb[1:60], lty=2)
lines(months[1:60], ub[1:60], lty=2)
lines(months[1:60], med_all[1:60], col="blue", lwd=3)
lines(months[1:60],lb_all[1:60], lty=2, col="blue")
lines(months[1:60], ub_all[1:60], lty=2, col="blue")
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

plot(months[1:60], med_all[1:60], col="Blue", type='l', lwd=3, ylim=c(-2000,12000),main="5 Year Projections--Proposal", ylab="$ in bank", xlab="Year")
lines(months[1:60], ub_all[1:60], lty=2, col="blue")
lines(months[1:60],lb_all[1:60], lty=2, col="blue")
abline(h=0, col="red")
abline(v=months[30], col="green")
legend("topleft", col=c("blue","blue", "green"),legend=c("Median with Grandkid Support", "95% Confidence Intervals", "Pay Change"), lwd=c(3,1,1), lty=c(1,2,1))


plot(months, med_all, type='l', col="Blue", lwd=3,ylim=c(-2000,55000), main="18 Year Projections--Proposal", ylab="$ in bank", xlab="Year")
lines(months,lb_all, lty=2, col="blue")
lines(months, ub_all, lty=2, col="blue")
abline(h=0, col="red")
abline(v=months[30], col="green")
legend("topleft", col=c("blue","blue", "green"),legend=c("Median with Grandkid Support", "95% Confidence Intervals", "Pay Change"), lwd=c(3,1,1), lty=c(1,2,1))

