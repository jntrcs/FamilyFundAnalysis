##Things unaccounted for
#Come home and go back out



family<-read.csv("family.csv")
#names(dat)<-c("name", "sex", "birth", "months_served")
#dat
#sum(dat$months_served>=18)
family$name<-as.character(family$name)
family$depart_date<-as.Date(family$depart_date,"%m/%d/%Y")
family$return_date<-as.Date(family$return_date,"%m/%d/%Y")
family$will_start_paying_on<-as.Date(family$will_start_paying_on)
family$payer<-F
#write.csv(dat,"family.csv")

#Probability of girl serving modeled by Beta(19,31)
#probability of boy serving modeled by Beta(48,11)
prob_when_go_boy<-c(90,6,2,1,1,0,0,0)
prob_when_go_girl<-c(0, 90, 4,3,2,.5,.3,.2)
pr_return_boy<-c(rep(2, 6), rep(8/17, 17), 80)
pr_return_girl<-c(rep(2.5, 6), rep(3/4, 11), 76.75, rep(0,6))
pr_what_month<-c(rep(50/9, 5), rep(50/3, 3), rep(50/9, 4))
pr_parent_pays<-.98
pr_kid_payer_pays<-.9
parent_contrib<-30
kid_contrib<-5
death_of_contrib_prob<-c(.5,.5)
start_date<-as.Date("2017-08-01")


get_return_date<-function(sex, left){
  home<-Sys.Date()-1
  if (sex=="M") probs=pr_return_boy else probs=pr_return_girl
  count=0
  while (home<Sys.Date())
  { count=count+1
    home<-add.months(left, sample(1:24, 1, prob=probs))
    if (count>100){
      print("youbrokeit")
    }
  }
  return(home)
}

calc_dep_date<-function(year_born, sex){
  
  if (sex=="M"){
    probs=prob_when_go_boy
  }else
    probs=prob_when_go_girl
  year<-year_born+sample(18:25, 1, prob=probs)
  month<-sample(1:12, 1, prob=pr_what_month)
  date<-as.Date(paste0(year,"-",month,"-","01"))
  return(date)
}

add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]

start_time_sim<-function(dat, death ){
  payers = 12
  
  current_date<-start_date
  cal<-data.frame(month=seq(start_date, by = paste (1, "months"), length = 216))
  cal$money_from_parents<-NA
  cal$money_from_kids<-NA
  cal$total_incoming<-NA
  cal$total_expenditure<-NA
  if (death){
    death_when<-sample(cal$month,1)
  }
  else death_when<-Sys.Date()-1000
  ###initial payers
  will_pay_RMs<-sample(c(T,F), sum(dat$status=="R"), replace=T, prob=c(pr_pay_if_rm, 1-pr_pay_if_rm))
  dat$payer[dat$status=="R"]<-will_pay_RMs
  will_pay_other<-sample(c(T,F), sum(dat$status=="N"|dat$status=="ER"), replace=T, prob=c(pr_pay_if_not,1-pr_pay_if_not))
  dat$payer[dat$status=="N"|dat$status=="ER"]<-will_pay_other
  dat$processed<-is.na(dat$return_date)
  for (month in 1:nrow(cal))
  {
    current_date<-cal$month[month]
    if (current_date==death_when) payers=11
    
    ###Make missionary adjustments
    for (per in 1:nrow(dat)){
      if (!is.na(dat$depart_date[per])){
        if (current_date==dat$depart_date[per]){
          dat$status[per]<-"S"
          
        }
        else if (current_date>dat$return_date[per] & !dat$processed[per]){
          if (add.months(dat$depart_date[per], ifelse(dat$sex[per]=="M", 24,18))==dat$return_date[per]){
            dat$status[per]<-"R"
            dat$payer[per]<-sample(c(T,F), 1, prob = c(pr_pay_if_rm,1-pr_pay_if_rm))
          }else{
            dat$status[per]<-"ER"
            dat$payer[per]<-sample(c(T,F), 1, prob = c(pr_pay_if_not, 1-pr_pay_if_not))
          }
          dat$processed[per]<-T
        }
      }
      if (!is.na(dat$will_start_paying_on[per])){
        if(current_date==dat$will_start_paying_on[per])
          dat$payer[per]<-sample(c(T,F), 1, prob = c(pr_pay_if_not, 1-pr_pay_if_not))
      }
    }
    cal$total_expenditure[month]<-100*sum(dat$status=="S", na.rm=T)+ifelse(month==1, 0, cal$total_expenditure[month-1])
    
    
    ###Adjust income
    if(month>1) {
      cal$money_from_parents[month]<-cal$money_from_parents[month-1]+rbinom(1,payers,.98)*parent_contrib
      cal$money_from_kids[month]<-cal$money_from_kids[month-1]+rbinom(1, sum(dat$payer, na.rm=T), pr_kid_payer_pays)*kid_contrib
      
    }else{
      cal$money_from_parents[1]<-rbinom(1,payers,.98)*parent_contrib
      cal$money_from_kids<-rbinom(1, sum(dat$payer, na.rm=T), pr_kid_payer_pays)*kid_contrib
    }
    cal$total_incoming[month]<-cal$money_from_kids[month]+cal$money_from_parents[month]
    
  }
  
  return(list(fam=dat, calendar=cal))
}



nReps<-1000

income_parents<-matrix(NA, nrow=nReps, ncol=18*12)
income_total<-matrix(NA, nrow=nReps, ncol=18*12)
expenditures<-matrix(NA, nrow=nReps, ncol=18*12)

for (rep in 1:nReps)
{
  dat<-family
  pr_pay_if_rm<-rbeta(1,12,2)
  pr_pay_if_not<-rbeta(1,4,6)
  pr_boy_going<-rbeta(1,48,11)
  pr_girl_going<-rbeta(1,19,31)
  death<-sample(c(T,F), 1, prob=death_of_contrib_prob)
  #Modeling a Gio/Mandy remarry effect
  extreme_family_change<-sample(0:5,1, prob=c(.92, .04,.01,.01, .01,.01))
  if (extreme_family_change>0)
  {
    for (i in 1:extreme_family_change)
    {
      row<-nrow(dat)+1
      dat[row, "name"]<-"Adoptee"
      dat[row, "sex"]<-as.factor(sample(c("M","F"),1))
      dat[row, "status"]<-as.factor("E")
      dat[row, "birth"]<-sample(2000:2017,1)
    }
  }
  for (per in 1:nrow(dat))
  {
    go<-F
    if (dat[per, "status"]=="E")
    {
      go<-sample(c(T,F), 1, prob=ifelse(rep(dat[per,"sex"]=="M",2),
                                        c(pr_boy_going, 1-pr_boy_going),
                                        c(pr_girl_going, 1-pr_girl_going)))
      dat$will_go[per]<-go
    }
    if (go==T)
    {
      when_go<-calc_dep_date(dat$birth[per], dat$sex[per])
      #if (when_go<as.Date(cut(start_date, "year"))
      if (when_go<as.Date(cut(start_date, "year"))){
        dat[per, "will_go"]<-F
        dat[per, "depart_date"]<-NA
        dat[per, "status"]<-"N"
      }
      else if (when_go>=as.Date(cut(start_date, "year")) & when_go< start_date){
        when_go<-as.Date(paste0(2017, "-", sample(9:12, 1), "-01"))
      }
      dat[per, "depart_date"]<-when_go
    }
    if(!is.na(dat$will_go[per])){
      if (!dat$will_go[per])
      {
         dat$depart_date[per]<-NA
         dat$status[per]="N"
         dat$will_start_paying_on[per]<-max(start_date,as.Date(paste0(as.character(dat$birth[per]+22), "-01-01")))
      }
    }
    ###determine return dates
    if (!is.na(dat$depart_date[per]))
    {
      dat$return_date[per]<-get_return_date(dat$sex[per], dat$depart_date[per])
    }
  }
  
 full<-start_time_sim(dat,death)
 expenditures[rep,]<-full$calendar$total_expenditure
 income_parents[rep,]<-full$calendar$money_from_parents
 income_total[rep,]<-full$calendar$total_incoming
  save(full, file=paste0("Simulations/Sim",rep))
}            



