
rm(list=ls(all=TRUE))


library(dplyr)
library(lubridate)
library(tidyr)

TaggingMaster = read.csv("https://github.com/PrullMarcus/MastersAnalysis/raw/main/TaggingMasterSheet.csv")

TaggingMaster$Reward.Tag.. = as.numeric(TaggingMaster$Reward.Tag..)

TaggingMaster = TaggingMaster %>% 
  mutate(RewardLevel = case_when(
    Reward.Tag.. <= 1000 | Reward.Tag.. >=4000 & Reward.Tag..<=4499 ~ 1,
    Reward.Tag.. >=1001 & Reward.Tag..<=2000 | Reward.Tag.. >=5000 & Reward.Tag..<=5249 ~ 2,
    Reward.Tag.. >=2001 & Reward.Tag..<=2500 ~ 3
  ))
TaggingMaster


#We need tagged by period (Month/Year) separated by reward value and tagging status (single/double) 
#Currently the "Date" Column is not actually being stored as a data. Its being stored as a "chr". We are gonna use lubridate to make an actual date column. THEN, we need to make a "month/year which will set the basis of our matrix we want to create. 

TaggingMaster$Date = as.Date(TaggingMaster$Date, format = "%m/%d/%y")
TaggingMaster$MonthYear = format(TaggingMaster$Date, "%m-%Y")
unique(TaggingMaster$MonthYear)

#Creating the periods string (first tagging period till end of year 2023). 

Period_Seq = format(seq.Date(as.Date("2022-01-01"), as.Date("2023-12-01"), by = "month"),format = "%m-%Y")

test = TaggingMaster %>% group_by(MonthYear, RewardLevel, Num.tagged) %>% count(n())
#test = tidyr::pivot_wider(test,names_from = MonthYear, values_from=n)
test[is.na(test)]=0
test

MonthYearOrder = c("01-2022", "02-2022", "12-2022","01-2023","05-2023")
RewardLevOrder = c(1,2,3)
SingleDoubleOrder = c(1,2)


test$MonthYear = factor(test$MonthYear, levels = MonthYearOrder)
test = test[with(test,order(factor(test$MonthYear, levels = MonthYearOrder),factor(test$RewardLevel, levels = RewardLevOrder),factor(test$Num.tagged, levels = SingleDoubleOrder))),]
test

#Now we need to somehow figure out how to tell r where to assign all of our tag returns. We need to take the tag return data, assign every tag return to its proper release period, reward group, and capture month. Then have r take a tally of all the fish in each criteria/recap group. 

#This gets rid of the n() column which is just the total of all the tag types.  
test = test[,!(names(test) %in% "n()")]
test

#Firstly, I am going to take the tag recap matrix and make the capture date into a month/year format like we have in our matrix. Additionally, I am going to add its release period and TaggingGroup which we can figure out from the tagging data for that individual fish. 
tag_return = read.csv("https://github.com/PrullMarcus/MastersAnalysis/raw/main/TagReturnsMasterSheet.csv")

tourney_returns = filter(tag_return, If.released.it.was == "c")
tourney_returns

#Selecting only variables I think we need for this. 

tourney_returns = tourney_returns %>% dplyr::select(c("Tag.number","Second.Tag","Date.of.catch","Which.body.of.water",                                   "Access.to.the.lake","If.released.it.was"))
tourney_returns

#An easy thing to do first is to change the Date.of.catch part to MonthYear just like it is in the matrix we setup.

tourney_returns$Date.of.catch = as.Date(tourney_returns$Date.of.catch, format = "%m/%d/%y")
tourney_returns$RecapMonthYear = format(tourney_returns$Date, "%m-%Y")
unique(tourney_returns$RecapMonthYear)
tourney_returns

#Now have the release date in the same format as the matrix. Now we need to add whether the fish was single/double tagged intitially. Then we need to combine the assigned reward to the fish as well as its release month. Then we should be able to populate the matrix correctly.  
tmp1 <- merge(tourney_returns, TaggingMaster, by.x = "Tag.number", by.y = "Reward.Tag..", all.x = FALSE)
tmp1 = tmp1 %>% dplyr::select(c("Tag.number","Second.Tag","Which.body.of.water",
                                   "Access.to.the.lake", "MonthYear",
                                   "RecapMonthYear","Num.tagged", "RewardLevel"))

tmp2 <- merge(tourney_returns, TaggingMaster, by.x = "Tag.number", by.y = "X2nd.Reward.Tag..", all.x = FALSE)
tmp2 = tmp2 %>% dplyr::select(c("Tag.number","Second.Tag","Which.body.of.water",
                                   "Access.to.the.lake", "MonthYear",
                                  "RecapMonthYear","Num.tagged", "RewardLevel"))

tmp3 = rbind(tmp1,tmp2)
tmp3

test2 = tmp3 %>% group_by(MonthYear, RewardLevel,Num.tagged, RecapMonthYear) %>% count(n())
test2 = tidyr::pivot_wider(test2,names_from = RecapMonthYear, values_from=n)
test2[is.na(test2)]=0
test2

test2 = test2[,!(names(test2) %in% "n()")]
test2

non_date_cols = c("MonthYear", "RewardLevel","Num.tagged")
date_cols = setdiff(colnames(test2), non_date_cols)
missing_months = setdiff(Period_Seq, date_cols)
for(m in missing_months){
  
  test2[[m]]<-0
  
}

test2 = test2[,c(non_date_cols, Period_Seq)]
test2


#Now I need to sort them properly (tag types in proper order and release periods in order)

tourn_test2 = test2[with(test2,order(factor(test2$MonthYear, levels = MonthYearOrder),factor(test2$RewardLevel, levels = RewardLevOrder),factor(test2$Num.tagged, levels = SingleDoubleOrder))),]
tourn_test2

##############  non-tournament returns
nontourney_returns = filter(tag_return, If.released.it.was != "c"|is.na(If.released.it.was))
nrow(nontourney_returns)
nrow(tag_return)
nrow(tourney_returns)

#Selecting only variables I think we need for this. 
nontourney_returns = nontourney_returns %>% dplyr::select(c("Tag.number","Second.Tag","Date.of.catch","Which.body.of.water",                                   "Access.to.the.lake","If.released.it.was"))
nontourney_returns

#An easy thing to do first is to change the Date.of.catch part to MonthYear just like it is in the matrix we setup.
nontourney_returns$Date.of.catch = as.Date(nontourney_returns$Date.of.catch, format = "%m/%d/%y")
nontourney_returns$RecapMonthYear = format(nontourney_returns$Date, "%m-%Y")
unique(nontourney_returns$RecapMonthYear)

#nontourney_returns

#Now have the release date in the same format as the matrix. Now we need to add whether the fish was single/double tagged initially. Then we need to combine the assigned reward to the fish as well as its release month. Then we should be able to populate the matrix correctly.  
tmp1 <- merge(nontourney_returns, TaggingMaster, by.x = "Tag.number", by.y = "Reward.Tag..", all.x = FALSE)
tmp1 = tmp1 %>% dplyr::select(c("Tag.number","Second.Tag","Which.body.of.water",
                                   "Access.to.the.lake", "MonthYear",
                                   "RecapMonthYear","Num.tagged", "RewardLevel"))

tmp2 <- merge(nontourney_returns, TaggingMaster, by.x = "Tag.number", by.y = "X2nd.Reward.Tag..", all.x = FALSE)
tmp2 = tmp2 %>% dplyr::select(c("Tag.number","Second.Tag","Which.body.of.water",
                                   "Access.to.the.lake", "MonthYear",
                                  "RecapMonthYear","Num.tagged", "RewardLevel"))

tmp3 = rbind(tmp1,tmp2)
tmp3

test2 = tmp3 %>% group_by(MonthYear, RewardLevel,Num.tagged, RecapMonthYear) %>% count(n())
test2 = tidyr::pivot_wider(test2,names_from = RecapMonthYear, values_from=n)
test2[is.na(test2)]=0
test2

test2 = test2[,!(names(test2) %in% "n()")]
test2

#This ensures that all periods are included for the matrix even if there were 0 recaps for a particular month/year combination
non_date_cols = c("MonthYear", "RewardLevel","Num.tagged")
date_cols = setdiff(colnames(test2), non_date_cols)
missing_months = setdiff(Period_Seq, date_cols)
for(m in missing_months){
  
  test2[[m]]<-0
  
}

test2 = test2[,c(non_date_cols, Period_Seq)]
test2


#Now I need to sort them properly (tag types in proper order and release periods in order)

non_test2 = test2[with(test2,order(factor(test2$MonthYear, levels = MonthYearOrder),factor(test2$RewardLevel, levels = RewardLevOrder),factor(test2$Num.tagged, levels = SingleDoubleOrder))),]
non_test2

################################################


#I just simplified down the original test dataset to be merged with our recap matrix. 
tests_merged = test %>% left_join(tourn_test2, by = c("MonthYear", "RewardLevel","Num.tagged"))
tests_merged

tourn_tests_merged=tests_merged
names(tourn_tests_merged)[-c(1:4)]=paste("T-",names(tourn_tests_merged)[-c(1:4)],sep='')

#I just simplified down the original test dataset to be merged with our recap matrix. 
tests_merged = test %>% left_join(non_test2, by = c("MonthYear", "RewardLevel","Num.tagged"))
non_tests_merged=tests_merged[,-c(1:4)]

names(non_tests_merged)=paste("NT-",names(non_tests_merged),sep='')

tests_merged=cbind(tourn_tests_merged,non_tests_merged)

#Now I just need to replace the NAs with 0s 
tests_merged = tests_merged %>% mutate(across(where(is.numeric),~replace_na(.,0)))
tests_merged

############  estimation model  ###################

#HERE IS THE MODEL WITHOUT DIFFERENCIATING BETWEEN TAG TYPES... 

n_mon = 24#number of recap periods. 2 calendar years with a montly timestep = 24 months.
n_rel = nrow(tests_merged)#Do we need to change this to 5 if we aren't differenciating by tag-type within the model????
TL = 0.05#tagloss/shedding (max). 

tests_merged$NotSeen = tests_merged$n-apply(tests_merged[,-c(1:4)],1,sum)

my_released=my(tests_merged$MonthYear)#converts to date
release_period=interval(my("1-2022"),my_released) %/% months(1)+1

u_index = numeric()

month_numbers <- rep(1:12, 2)
winter_months <- c(12, 1, 2)
u_index <- ifelse(month_numbers %in% winter_months, 1, match(month_numbers, setdiff(1:12, winter_months)) + 1)

#winter_group = c(1,2,12,13,14,24)  #what n_mon periods coorespond to the winter months (dec, jan, feb). 
#u_counter = 2

#for(i in 1:24) {
 # if (i %in% winter_group) {
  #  u_index[i] = 1
  #} else {
   # u_index[i] = u_counter
    #u_counter = u_counter +1
  #}
#}

cm_index=1:24

#cm_index=ifelse(cm_index%in%c(1,2,9,10,11,12,13,14,21,22,23,24),1,ifelse(
#	cm_index%in%c(3,4,5,15,16,17),2,3))
cm_index=ifelse(cm_index%in%c(1,2,9,10,11,12,13,14,21,22,23,24),1,2)
cm_index=rep(1,24)

m_index=cm_index
f_index=u_index

nll=function(theta,recaps,TL,nTag,RewardLevel,release_period,f_index,m_index){
  n_m=length(unique(m_index))
  n_f=length(unique(f_index))
  n_lam=2
  m_param=exp(theta[1:n_m])
  
  lambda=plogis(theta[(n_m+1):(n_m+n_lam)])
  lambda=c(lambda,.70)####lambda param####
  
  ft_param=exp(theta[(n_m+n_lam+1):(n_m+n_lam+n_f)])#should be 19 of these (dec, jan, feb lumped into 1 u, separate for every other month)
  fnt_param=exp(theta[(n_m+n_lam+n_f+1):(n_m+n_lam+2*n_f)])#should be 19 of these (dec, jan, feb lumped into 1 u, separate for every other month)
  ft=numeric(n_mon)
  fnt=numeric(n_mon)
  f=numeric(n_mon)
  m=numeric(n_mon)
  Z=numeric(n_mon)
  ut=numeric(n_mon)
  unt=numeric(n_mon)

  S=matrix(NA,nrow=n_rel,ncol=n_mon)
  
  p_ret=matrix(0,nrow=n_rel,ncol=2*n_mon+1)
  nll=numeric(n_rel)

  for (r in 1:n_rel){
    for(y in release_period[r]:n_mon){
      
      ft[y] = ft_param[u_index[y]]#this selects the grouped or individual u (winter or not).
      fnt[y] = fnt_param[u_index[y]]#this selects the grouped or individual u (winter or not).
 
 	f[y]=ft[y]+fnt[y]
     
      m[y] = m_param[m_index[y]]/12#uses the season (theta value) that corresponds to the necessary season
      
	Z[y]= f[y]+m[y]

	ut[y]=ft[y]/Z[y]*(1-exp(-Z[y]))
	unt[y]=fnt[y]/Z[y]*(1-exp(-Z[y]))

      S[r,y]=exp(-Z[y])
      
      if(release_period[r]==y){
        
        p_ret[r,y]=ut[y]*(1-TL^nTag[r])*lambda[RewardLevel[r]]
        p_ret[r,n_mon+y]=unt[y]*(1-TL^nTag[r])*lambda[RewardLevel[r]]
        
      } else {
        
        p_ret[r,y]=ut[y]*(1-TL^nTag[r])*prod(S[r,release_period[r]:(y-1)])*lambda[RewardLevel[r]]
        p_ret[r,n_mon+y]=unt[y]*(1-TL^nTag[r])*prod(S[r,release_period[r]:(y-1)])*lambda[RewardLevel[r]]
        
      }  # close if else
      
    }  #close y loop
    
    p_ret[r,2*n_mon+1]=1-sum(p_ret[r,])
    nll[r]=-dmultinom(recaps[r,],prob=p_ret[r,],log=T)
    
  } #close r loop
#print(theta)
  sum(nll)
}  #close function

theta=c(m=log(rep(0.3,length(unique(m_index)))),lambda=qlogis(c(.55,.65)), ft = log(rep(0.1,length(unique(f_index)))),fnt = log(rep(0.1,length(unique(f_index)))))

nll(theta,TL=TL,recaps = tests_merged[,-c(1:4)],nTag = tests_merged$Num.tagged, RewardLevel = tests_merged$RewardLevel,release_period=release_period,m_index=m_index,f_index=f_index)

fit70=optim(theta,fn=nll,method='BFGS',hessian=T,TL=TL,nTag=tests_merged$Num.tagged, RewardLevel=tests_merged$RewardLevel,release_period=release_period,recaps=tests_merged[,-c(1:4)],
          control = list(trace=1,maxit = 200),m_index=cm_index,f_index=u_index)#needed a few more iterations to "converge"

plogis(fit$par)
plogis(theta)

n_m=length(unique(m_index))
n_f=length(unique(f_index))


se=sqrt(diag(solve(fit$hessian)))
u95ci_m=exp(fit$par[1:n_m]+1.96*se[1:n_m])
est_m=exp(fit$par[1:n_m])
l95ci_m=exp(fit$par[1:n_m]-1.96*se[1:n_m])

months=c('J','F','M','A','M','J','J','A','S','O','N','D')

u95ci_ft=exp(fit$par[(n_m+3):(n_m+2+n_f)]+1.96*se[(n_m+3):(n_m+2+n_f)])
est_ft=exp(fit$par[(n_m+3):(n_m+2+n_f)])
l95ci_ft=exp(fit$par[(n_m+3):(n_m+2+n_f)]-1.96*se[(n_m+3):(n_m+2+n_f)])
plot(est_ft[f_index],type='l',ylim=c(0,0.3),xaxt='n',xlab='Month',ylab=expression('Tournament Weigh-in Rate (month  '^-1*')'))
axis(1,at=1:24,labels=rep(months,2),cex.axis=0.7)
lines(u95ci_ft[f_index],lty=3)
lines(l95ci_ft[f_index],lty=3)

u95ci_fnt=exp(fit$par[(n_m+3+n_f):(n_m+2+2*n_f)]+1.96*se[(n_m+3+n_f):(n_m+2+2*n_f)])
est_fnt=exp(fit$par[(n_m+3+n_f):(n_m+2+2*n_f)])
l95ci_fnt=exp(fit$par[(n_m+3+n_f):(n_m+2+2*n_f)]-1.96*se[(n_m+3+n_f):(n_m+2+2*n_f)])
windows()
par(mgp=c(2.5,1,0))
plot(est_fnt[f_index],type='l',ylim=c(0,0.5),xaxt='n',xlab='Month',ylab=expression('On-water Angler Encounter Rate (month  '^-1*')'))
axis(1,at=1:24,labels=rep(months,2),cex.axis=0.7)
lines(u95ci_fnt[f_index],lty=3)
lines(l95ci_fnt[f_index],lty=3)

u95ci_lam=plogis(fit$par[(n_m+1):(n_m+2)]+1.96*se[(n_m+1):(n_m+2)])
est_lam=plogis(fit$par[(n_m+1):(n_m+2)])
l95ci_lam=plogis(fit$par[(n_m+1):(n_m+2)]-1.96*se[(n_m+1):(n_m+2)])

plogis(fit$par)













plogis(fit$par)
plogis(theta)

n_m=length(unique(m_index))
n_f=length(unique(f_index))

se70=sqrt(diag(solve(fit70$hessian)))
se85=sqrt(diag(solve(fit85$hessian)))
se100=sqrt(diag(solve(fit100$hessian)))

u95ci_m_70=exp(fit70$par[1:n_m]+1.96*se70[1:n_m])
u95ci_m_85=exp(fit85$par[1:n_m]+1.96*se85[1:n_m])
u95ci_m_100=exp(fit100$par[1:n_m]+1.96*se100[1:n_m])

est_m_70=exp(fit70$par[1:n_m])
est_m_85=exp(fit85$par[1:n_m])
est_m_100=exp(fit100$par[1:n_m])

l95ci_m_70=exp(fit70$par[1:n_m]-1.96*se70[1:n_m])
l95ci_m_85=exp(fit85$par[1:n_m]-1.96*se85[1:n_m])
l95ci_m_100=exp(fit100$par[1:n_m]-1.96*se100[1:n_m])

months=c('J','F','M','A','M','J','J','A','S','O','N','D')

u95ci_ft_70=exp(fit70$par[(n_m+3):(n_m+2+n_f)]+1.96*se70[(n_m+3):(n_m+2+n_f)])
u95ci_ft_85=exp(fit85$par[(n_m+3):(n_m+2+n_f)]+1.96*se85[(n_m+3):(n_m+2+n_f)])
u95ci_ft_100=exp(fit100$par[(n_m+3):(n_m+2+n_f)]+1.96*se100[(n_m+3):(n_m+2+n_f)])

est_ft_70=exp(fit70$par[(n_m+3):(n_m+2+n_f)])
est_ft_85=exp(fit85$par[(n_m+3):(n_m+2+n_f)])
est_ft_100=exp(fit100$par[(n_m+3):(n_m+2+n_f)])

l95ci_ft_70=exp(fit70$par[(n_m+3):(n_m+2+n_f)]-1.96*se70[(n_m+3):(n_m+2+n_f)])
l95ci_ft_85=exp(fit85$par[(n_m+3):(n_m+2+n_f)]-1.96*se85[(n_m+3):(n_m+2+n_f)])
l95ci_ft_100=exp(fit100$par[(n_m+3):(n_m+2+n_f)]-1.96*se100[(n_m+3):(n_m+2+n_f)])

par(mar = c(5, 5, 4, 2))
plot(est_ft_70[f_index][1:12],type='l',ylim=c(0,0.3),xaxt='n',xlab='Month',ylab=expression('Tournament Weigh-in Rate (month  '^-1*')'))
axis(1,at=1:12,labels=months,cex.axis=1)
lines(u95ci_ft_70[f_index][1:12],lty=3)
lines(l95ci_ft_70[f_index][1:12],lty=3)

par(mar = c(5, 5, 4, 2))
plot(est_ft_85[f_index][1:12],type='l',ylim=c(0,0.3),xaxt='n',xlab='Month',ylab=expression('Tournament Weigh-in Rate (month  '^-1*')'))
axis(1,at=1:12,labels=months,cex.axis=1)
lines(u95ci_ft_85[f_index][1:12],lty=3)
lines(l95ci_ft_85[f_index][1:12],lty=3)

par(mar = c(5, 5, 4, 2))
plot(est_ft_100[f_index][1:12],type='l',ylim=c(0,0.3),xaxt='n',xlab='Month',ylab=expression('Tournament Weigh-in Rate (month  '^-1*')'))
axis(1,at=1:12,labels=months,cex.axis=1)
lines(u95ci_ft_100[f_index][1:12],lty=3)
lines(l95ci_ft_100[f_index][1:12],lty=3)

par(mar = c(5, 5, 4, 2))
plot(est_ft_85[f_index][1:12],type='l',col="blue",ylim=c(0,0.3),xaxt='n',xlab='Month',ylab=expression('Tournament Weigh-in Rate (month  '^-1*')'))
axis(1,at=1:12,labels=months,cex.axis=1)
lines(est_ft_70[f_index][1:12],col="darkorange")
lines(est_ft_100[f_index][1:12],col="darkgreen")
legend("topright", legend = c(expression(lambda[300] == 0.70),
                              expression(lambda[300] == 0.85),
                              expression(lambda[300] == 1)),
                              col = c("darkorange","blue","darkgreen"),
                              lty = 1
                              )

m_bar = data.frame(
  lambda300 = c(0.7,0.85,1),
  estimate = c(est_m_70,est_m_85,est_m_100),
  l95 = c(l95ci_m_70,l95ci_m_85,l95ci_m_100),
  u95 = c(u95ci_m_70,u95ci_m_85,u95ci_m_100)
)

ggplot(m_bar, aes(x=factor(lambda300),y=estimate))+
  geom_col(width=0.6)+
  geom_errorbar(aes(ymin=l95,ymax=u95),width=0.2)+
  scale_y_continuous(expand=c(0,0))+
  labs(x=expression(lambda300))+
  theme_classic()

u95ci_fnt70=exp(fit70$par[(n_m+3+n_f):(n_m+2+2*n_f)]+1.96*se70[(n_m+3+n_f):(n_m+2+2*n_f)])
u95ci_fnt85=exp(fit85$par[(n_m+3+n_f):(n_m+2+2*n_f)]+1.96*se85[(n_m+3+n_f):(n_m+2+2*n_f)])
u95ci_fnt100=exp(fit100$par[(n_m+3+n_f):(n_m+2+2*n_f)]+1.96*se100[(n_m+3+n_f):(n_m+2+2*n_f)])

est_fnt70=exp(fit70$par[(n_m+3+n_f):(n_m+2+2*n_f)])
est_fnt85=exp(fit85$par[(n_m+3+n_f):(n_m+2+2*n_f)])
est_fnt100=exp(fit100$par[(n_m+3+n_f):(n_m+2+2*n_f)])

l95ci_fnt70=exp(fit70$par[(n_m+3+n_f):(n_m+2+2*n_f)]-1.96*se70[(n_m+3+n_f):(n_m+2+2*n_f)])
l95ci_fnt85=exp(fit85$par[(n_m+3+n_f):(n_m+2+2*n_f)]-1.96*se85[(n_m+3+n_f):(n_m+2+2*n_f)])
l95ci_fnt100=exp(fit100$par[(n_m+3+n_f):(n_m+2+2*n_f)]-1.96*se100[(n_m+3+n_f):(n_m+2+2*n_f)])

par(mar = c(5, 5, 4, 2))
plot(est_fnt70[f_index][1:12],type='l',ylim=c(0,0.5),xaxt='n',xlab='Month',ylab=expression('On-water Angler Encounter Rate (month  '^-1*')'))
axis(1,at=1:12,labels=months,cex.axis=1)
lines(u95ci_fnt70[f_index][1:12],lty=3)
lines(l95ci_fnt70[f_index][1:12],lty=3)

par(mar = c(5, 5, 4, 2))
plot(est_fnt85[f_index][1:12],type='l',ylim=c(0,0.5),xaxt='n',xlab='Month',ylab=expression('On-water Angler Encounter Rate (month  '^-1*')'))
axis(1,at=1:12,labels=months,cex.axis=1)
lines(u95ci_fnt85[f_index][1:12],lty=3)
lines(l95ci_fnt85[f_index][1:12],lty=3)

par(mar = c(5, 5, 4, 2))
plot(est_fnt100[f_index][1:12],type='l',ylim=c(0,0.5),xaxt='n',xlab='Month',ylab=expression('On-water Angler Encounter Rate (month  '^-1*')'))
axis(1,at=1:12,labels=months,cex.axis=1)
lines(u95ci_fnt100[f_index][1:12],lty=3)
lines(l95ci_fnt100[f_index][1:12],lty=3)

par(mar = c(5, 5, 4, 2))
plot(est_fnt85[f_index][1:12],type='l',col="blue",ylim=c(0,0.5),xaxt='n',xlab='Month',ylab=expression('On-water Angler Encounter Rate (month  '^-1*')'))
axis(1,at=1:12,labels=months,cex.axis=1)
lines(est_fnt70[f_index][1:12],col = "darkorange")
lines(est_fnt100[f_index][1:12],col = "darkgreen")
legend("topright",legend = c(expression(lambda[300] == 0.70), 
                             expression(lambda[300] == 0.85), 
                             expression(lambda[300] == 1)),
                  col = c("darkorange","blue","darkgreen"),
                  lty = 1)


u95ci_lam70=plogis(fit70$par[(n_m+1):(n_m+2)]+1.96*se70[(n_m+1):(n_m+2)])
u95ci_lam85=plogis(fit85$par[(n_m+1):(n_m+2)]+1.96*se85[(n_m+1):(n_m+2)])
u95ci_lam100=plogis(fit100$par[(n_m+1):(n_m+2)]+1.96*se100[(n_m+1):(n_m+2)])

est_lam70=plogis(fit70$par[(n_m+1):(n_m+2)])
est_lam85=plogis(fit85$par[(n_m+1):(n_m+2)])
est_lam100=plogis(fit100$par[(n_m+1):(n_m+2)])

l95ci_lam70=plogis(fit70$par[(n_m+1):(n_m+2)]-1.96*se70[(n_m+1):(n_m+2)])
l95ci_lam85=plogis(fit85$par[(n_m+1):(n_m+2)]-1.96*se85[(n_m+1):(n_m+2)])
l95ci_lam100=plogis(fit100$par[(n_m+1):(n_m+2)]-1.96*se100[(n_m+1):(n_m+2)])

lam_bar = data.frame(
  lambda300 = c(0.7,0.7,0.85,0.85,1,1),
  estimate = c(est_lam70,est_lam85,est_lam100),
  l95 = c(l95ci_lam70,l95ci_lam85,l95ci_lam100),
  u95 = c(u95ci_lam70,u95ci_lam85,u95ci_lam100),
  group = rep(c("lambda100","lambda200"),3)
)

ggplot(lam_bar, aes(x=factor(lambda300),y=estimate,fill=group))+
  geom_col(position=position_dodge(width=0.7), width=0.6)+
  geom_errorbar(aes(ymin=l95,ymax=u95),position = position_dodge(width=0.7),width=0.2)+
  scale_y_continuous(expand=c(0,0),limits=c(0,1))+
  labs(x="lambda300")+
  theme_classic()

#### GRAPHS FROM EXCEL SIMULATION FOR EXIT SEMINAR ####

load_est_70 = read.csv(file.choose())
load_est_85 = read.csv(file.choose())
load_est_100 = read.csv(file.choose())

par(mar = c(5, 5, 4, 2))
plot(load_est_70$prop.in.zone, type = "l",ylim=c(0,0.5))

par(mar = c(5, 5, 4, 2))
plot(load_est_85$prop.in.zone,type = "l",ylim=c(0,0.5))

par(mar = c(5, 5, 4, 2))
plot(load_est_100$prop.in.zone,type = "l",ylim=c(0,0.5))

par(mar = c(5, 5, 4, 2))
plot(load_est_70$prop.in.zone,type="l",ylim=c(0.1,0.5),xaxt='n',xlab='Year',col="darkorange",ylab = "Proportion of Black Bass in Release-Zone")
axis(1, at = c(seq(1,120, 12)),
        labels = c(1:10),
        cex.axis = 1)
lines(load_est_85$prop.in.zone,col="blue")
lines(load_est_100$prop.in.zone,col="darkgreen")
legend("topleft", legend = c(expression(lambda[300] == 0.7),
                             expression(lambda[300] == 0.85),
                             expression(lambda[300] == 1.0)),
                  col = c("darkorange","blue","darkgreen"),
                  lty= 1)



######################### Proximity to ramp

require(geosphere)#package for straight line distance calculations

Tracking = read.csv("https://github.com/PrullMarcus/MastersAnalysis/raw/main/TrackingDataMaster.csv")

T_Manuals_All = subset(Tracking, ManuallyTracked == "Y" & TournamentRelease == "Y")
T_Manuals_All = T_Manuals_All %>% group_by(RewardTag1) %>% filter(n()>1)
CL_Manuals = subset(T_Manuals_All, ReleaseRamp == "CL")
CL_Manuals$Date = make_date(year = CL_Manuals$Year, month = CL_Manuals$Month, day = CL_Manuals$Day)
CL_Manuals = subset(CL_Manuals, GroupMovement == "A")

tag_nums = unique(CL_Manuals$RewardTag1)
n_fish = length(tag_nums)# of individual fish making up dataset

CL_Manuals_SL = numeric()
CL_Manuals_weeks = numeric()

for(i in 1:n_fish){
  
  new_tag = tag_nums[i]
  new_dat = subset(CL_Manuals,RewardTag1 == new_tag)
  tmax = nrow(new_dat)
  CL_Manuals_Dist = numeric(tmax)
  weeks = numeric(tmax)
  
  for(t in 2:tmax){
    
    dist_m = geosphere::distm(c(new_dat$Long[t],new_dat$Lat[t]),c(new_dat$Long[1],new_dat$Lat[1]))
    CL_Manuals_Dist[t] = dist_m
    weeks[t] = as.numeric(new_dat$Date[t]-new_dat$Date[1])/7
  }
  CL_Manuals_SL = append(CL_Manuals_SL,CL_Manuals_Dist)
  CL_Manuals_weeks = append(CL_Manuals_weeks,weeks)
}

CL_Manuals$WeeksSinceRel = CL_Manuals_weeks
CL_Manuals$ProxToCL = CL_Manuals_SL
disp_threshold = 3000 #3000 m is equal to 3 km in either direction. 
CL_Manuals$Within3km <- ifelse(CL_Manuals$ProxToCL <= disp_threshold, 1, 0)
CL_Manuals = subset(CL_Manuals, WeeksSinceRel != 0)
CL_Manuals$RoundedWeek = floor(CL_Manuals$WeeksSinceRel) + 1


########### Proportion dispersed over time (>3 km away from ramp)

ProportionDispersed <- CL_Manuals %>%
filter(RoundedWeek <= 52) %>%
  group_by(RoundedWeek) %>%
  summarise(
    nFixes = n(),
    nFish = n_distinct(RewardTag1), #counts number of fish for each week period
    nDispersed = sum(Within3km == 0),  #counts number of fish dispersed
    propDispersed = nDispersed / nFish, #simple proportion calculation
    nAlive = sum(GroupMovement == "A"), #identifying number of alive fish used for each calculation
    nDead = sum(GroupMovement == "D") #identifies number of dead fish used for calculation
    )

ggplot(ProportionDispersed, aes(x = RoundedWeek, y = propDispersed)) +
  geom_line(color = "blue") +
  geom_point() +
  coord_cartesian(xlim = c(0, 52)) + #One calendar year
  labs(title = "Proportion of Fixes Considered Dispersed (Weeks Since Release)",
       x = "Weeks Since Release", y = "Proportion of Fixes Dispersed") +
  theme_minimal()



################ Proportion of fish dead over time 
library(dplyr)
library(tidyr)
library(ggplot2)

death_weeks <- CL_Manuals %>%
  filter(GroupMovement == "D", RoundedWeek <= 52) %>%
  group_by(RewardTag1) %>%
  summarise(DeathWeek = min(RoundedWeek), .groups = "drop")#finds first week a fish coded dead 

all_tags <- unique(CL_Manuals$RewardTag1)

all_weeks <- expand.grid(
  RewardTag1 = all_tags,
  RoundedWeek = 0:52
)

status_data <- all_weeks %>%
  left_join(death_weeks, by = "RewardTag1") %>%
  mutate(Status = case_when(
    !is.na(DeathWeek) & RoundedWeek >= DeathWeek ~ "D",  # dead for every successive period after it first codes dead
    TRUE ~ NA_character_                            # unknown status
  ))

#Identify weeks each fish was actually detected alive
alive_detections <- CL_Manuals %>%
  filter(GroupMovement == "A", RoundedWeek <= 52) %>%
  distinct(RewardTag1, RoundedWeek) %>%
  mutate(Status = "A")

#Merge alive detections into the status table.
status_combined <- status_data %>%
  left_join(alive_detections, by = c("RewardTag1", "RoundedWeek"), suffix = c("_dead", "_alive")) %>%
  mutate(FinalStatus = case_when(
    Status_alive == "A" ~ "A",
    Status_dead == "D" ~ "D",
    TRUE ~ NA_character_
  ))

#Summarizes proportion of fish dead for each week, fish that are dead remain dead/included indefinitely, fish that are alive and then not 
#found or detected during certain periods are not included in the proportion calculation because we are not sure about their mortality status for those 
#periods. 

proportion_dead <- status_combined %>%
  filter(!is.na(FinalStatus)) %>%
  group_by(RoundedWeek) %>%
  summarise(
    nFish = n(),
    nDead = sum(FinalStatus == "D"),
    nAlive = sum(FinalStatus == "A"),
    propDead = nDead / nFish
  )

#Plotting proportion over time. 
ggplot(proportion_dead, aes(x = RoundedWeek, y = propDead)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red") +
  labs(title = "",
       x = "Weeks Since Release",
       y = "Proportion of Observed Fish Dead") +
  theme_minimal()

#Fitting the plot with a logistic model
mod_logistic_model <- nls(propDead ~ (K / (1 + exp(-b * (RoundedWeek - x0)))) * (RoundedWeek / (RoundedWeek + c)),
                          data = proportion_dead,
                          start = list(K = 0.8, b = 0.1, x0 = 10, c = 1),
                          control = nls.control(maxiter = 200, warnOnly = TRUE))

# Predictions
new_data <- data.frame(RoundedWeek = 0:52)
new_data$predicted <- predict(mod_logistic_model, newdata = new_data)

# Plot
ggplot(proportion_dead, aes(x = RoundedWeek, y = propDead)) +
  geom_point(color = "red", size = 2) +
  geom_line(color = "red", linewidth = 1) +
  geom_line(data = new_data, aes(x = RoundedWeek, y = predicted),
            color = "blue", linetype = "solid", linewidth = 1.2) +
  labs(title = "",
       x = "Weeks Since Release",
       y = "Proportion Dead") +
  theme_minimal()






############### Model Evaluating Loading Around Ramp, (not just rates)

N_tot = 1 
Initial_Lake = .975 ####Maybe inform this based on tagging data????
Initial_Ramp_3km = N_tot-Initial_Lake ###Inform based on tagging data????

n_yrs = 4
n_period = n_yrs*12

disp_rate = 0.6 #assymptotes around here after 1 month. 
Z = 0.05 #mortality # ADDED
Survival = exp(Z)

load_rate_index = c(1,1,2,3,4,5,6,7,8,9,10,1)
load_rate_index = rep(load_rate_index,n_yrs)
load_rate = as.vector(est_ft[load_rate_index])

immig = numeric(length=n_period)
emig = numeric(length=n_period)
Lake = numeric(length=n_period)
Ramp3kmFromLake = numeric(length=n_period)
Total_In = numeric(length=n_period)
N = numeric(length=n_period)

#InitialValues
Lake[1] = Initial_Lake
Ramp3kmFromLake[1] = 0
immig[1] = load_rate[1]*Lake[1]
emig[1] = disp_rate*Ramp3kmFromLake[1]
Total_In[1] = Ramp3kmFromLake[1]+Initial_Ramp_3km
N[1] = Total_In[1]+Lake[1]

for (t in 2:n_period){
  
  Lake[t] = Lake[t-1] + emig[t-1]-immig[t-1]
  Ramp3kmFromLake[t] = Ramp3kmFromLake[t-1]+immig[t-1]-emig[t-1]
  
  immig[t] = load_rate[t]*Lake[t]
  emig[t] = disp_rate*immig[t-1]
  
  Total_In[t] = Initial_Ramp_3km + Ramp3kmFromLake[t]
  N[t] = Total_In[t]+Lake[t]
  
}

Dat = data.frame(month=1:n_period,
                 Lake = Lake,
                 Ramp3kmFromLake = Ramp3kmFromLake,
                 immig = immig,
                 emig = emig,
                 Total_In = Total_In,
                 N = N)

Dat


####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
#Attempt to add mortality. 


N_tot = 1 
Initial_Lake = .975 ####Maybe inform this based on tagging data????
Initial_Ramp_3km = N_tot-Initial_Lake ###Inform based on tagging data????

n_yrs = 4
n_period = n_yrs*12

disp_rate = 0.6 #assymptotes around here after 1 month. 
Z = 0.03 #mortality # ADDED
Survival = exp(-Z)

load_rate_index = c(1,1,2,3,4,5,6,7,8,9,10,1)
load_rate_index = rep(load_rate_index,n_yrs)
load_rate = as.vector(est_ft[load_rate_index])

immig = numeric(length=n_period)
emig = numeric(length=n_period)
Lake = numeric(length=n_period)
Ramp3kmFromLake = numeric(length=n_period)
Total_In = numeric(length=n_period)
N = numeric(length=n_period)

#InitialValues
Lake[1] = Initial_Lake
Ramp3kmFromLake[1] = 0
immig[1] = load_rate[1]*Lake[1]
emig[1] = disp_rate*Ramp3kmFromLake[1]
Total_In[1] = Ramp3kmFromLake[1]+Initial_Ramp_3km
N[1] = Total_In[1]+Lake[1]

for (t in 2:n_period){
  
  Lake[t] = (Lake[t-1] + emig[t-1]-immig[t-1])
  Ramp3kmFromLake[t] = (Ramp3kmFromLake[t-1]+immig[t-1]-emig[t-1])
  
  immig[t] = load_rate[t]*Lake[t]
  emig[t] = disp_rate*immig[t-1]
  
  Total_In[t] = Initial_Ramp_3km + Ramp3kmFromLake[t]
  N[t] = Total_In[t]+Lake[t]
  
}

Dat = data.frame(month=1:n_period,
                 Lake = Lake,
                 Ramp3kmFromLake = Ramp3kmFromLake,
                 immig = immig,
                 emig = emig,
                 Total_In = Total_In,
                 N = N)

Dat


###Proportion of non-weigh in encounters that result in harvest. 
tag_return_test = read.csv("https://github.com/PrullMarcus/MastersAnalysis/raw/main/TagReturnsMasterSheet.csv")
non_weighin = subset(tag_return_test, If.released.it.was != "c"| is.na(If.released.it.was))
Kept = subset(non_weighin, Kept.or.released == "K")
p_harv = nrow(Kept)/nrow(non_weighin)


### Proportion of fish tagged within 3 km of CL boat launch. ###
library(sf)
library(dplyr)
TaggingMaster = read.csv("https://github.com/PrullMarcus/MastersAnalysis/raw/main/TaggingMasterSheet.csv")
SitesXY = read.csv("https://github.com/PrullMarcus/MastersAnalysis/raw/main/TaggingSitesMaster.csv")

Joined = TaggingMaster %>% 
         left_join(SitesXY, by = c("Site" = "Site", "TaggingEvent" = "ReleaseEvent"))

Joined = Joined %>% filter(!is.na(LongDecimalDegrees),!is.na(LatDecimalDegrees))

CL = data.frame(
  lat = 34.010097,	
  lon = -85.997069)

fish_sf = st_as_sf(Joined, coords = c("LongDecimalDegrees", "LatDecimalDegrees"), crs = 4326) %>% st_transform(crs = 32616)
CL_sf = st_as_sf(CL, coords = c("lon","lat"),crs = 4326) %>% st_transform(crs = 32616)

Distances = as.numeric(st_distance(fish_sf, CL_sf[1,]))
prop_3km = mean(Distances <= 2200)#0.1669565 (inflated because including other locations such as black creek. In reality those fish are well over 3km away from the launch) probably close to 10% of all tags, looks like closer to .1

CPUE = TaggingMaster %>% group_by(TaggingEvent,Site,Date) %>% summarize(n=n())

sum(CPUE$n)/nrow(CPUE)










