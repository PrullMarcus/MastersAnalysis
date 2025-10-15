##### OBJECTIVE 1 ####
#In this section we want to obtain loading rates for our tournament caught black bass. Additonally, 
#we want to estimate other various parameters such as apparent M, angler encounter rates, tag reporting values, etc. 

#### Obj 1 Data Setup POOLED ####

#### Obj 1 TAG LOSS VALUE SUMMARY ####

#Double Taggers Tagging
Tagging = read.csv("https://github.com/PrullMarcus/MastersAnalysis/raw/main/TaggingMasterSheet.csv")
Tagging$Date = as.Date(Tagging$Date,format = "%m/%d/%y")
Tagging$ReleaseMonth = format(Tagging$Date,"%m-%Y")
Tagging = subset(Tagging, Num.tagged == 2)
unique(Tagging$ReleaseMonth)

#Double Taggers Returns
Returns = read.csv("https://github.com/PrullMarcus/MastersAnalysis/raw/main/TagReturnsMasterSheet.csv")
Returns$Date.of.catch = as.Date(Returns$Date.of.catch,format = "%m/%d/%Y")
Returns$ReturnMonth = format(Returns$Date.of.catch, "%m-%Y")
Returns = Returns %>% filter(substr(ReturnMonth,4,7) %in% c("2022","2023"))
unique(Returns$ReturnMonth)


merge1 = merge(Returns, Tagging, by.x = "Tag.number", by.y = "Reward.Tag..", all.x = FALSE)
merge1 = merge1 %>% dplyr::select(c(ReturnMonth,ReleaseMonth,Number.of.tags))

merge2 = merge(Returns, Tagging, by.x = "Tag.number", by.y = "X2nd.Reward.Tag..", all.x=FALSE)
merge2 = merge2 %>% dplyr::select(c(ReturnMonth,ReleaseMonth,Number.of.tags))

merge3=rbind(merge1,merge2) %>% dplyr::select(c(ReturnMonth,ReleaseMonth,Number.of.tags))
merge3 = merge3 %>% 
  mutate(
    ReleaseDate = as.Date(paste0(ReleaseMonth,"-01"),format = "%m-%Y-%d"),
    ReturnDate = as.Date(paste0(ReturnMonth,"-01"),format = "%m-%Y-%d"),
    MonthDiff = interval(ReleaseDate,ReturnDate) %/% months(1),
    NumTagsReturn = ifelse(Number.of.tags == 2, 2, 1)
  )

TagLossTable = merge3 %>% count(MonthDiff, NumTagsReturn)
TagLossTable = TagLossTable %>% tidyr::pivot_wider(names_from=MonthDiff, values_from=n,values_fill=0)


test2 = tmp3 %>% group_by(MonthYear, RewardLevel,Num.tagged, RecapMonthYear) %>% count(n())
test2 = tidyr::pivot_wider(test2,names_from = RecapMonthYear, values_from=n)
test2[is.na(test2)]=0
test2

#### ORGANIZING ESTIMATION MODEL DATA ####
#Necessary packages


#### Obj 1 Eslibrary(dplyr)
library(lubridate)
library(tidyr)

#Reading in datasheet
TaggingMaster = read.csv("https://github.com/PrullMarcus/MastersAnalysis/raw/main/TaggingMasterSheet.csv")

#Turning Reward Tag into a numeric value
TaggingMaster$Reward.Tag.. = as.numeric(TaggingMaster$Reward.Tag..)

TaggingMaster = TaggingMaster %>% 
  mutate(RewardLevel = case_when(
    Reward.Tag.. <= 1000 | Reward.Tag.. >=4000 & Reward.Tag..<=4499 ~ 1,
    Reward.Tag.. >=1001 & Reward.Tag..<=2000 | Reward.Tag.. >=5000 & Reward.Tag..<=5249 ~ 2,
    Reward.Tag.. >=2001 & Reward.Tag..<=2500 ~ 3
  ))
TaggingMaster


#We need tagged by period (Month/Year) separated by reward value and tagging status (single/double) 
#Currently the "Date" Column is not actually being stored as a data. Its being stored as a "chr". 
#We are gonna use lubridate to make an actual date column. THEN, we need to make a "month/year which will set
#the basis of our matrix we want to create. 

TaggingMaster$Date = as.Date(TaggingMaster$Date, format = "%m/%d/%y")
TaggingMaster$MonthYear = format(TaggingMaster$Date, "%m-%Y")
unique(TaggingMaster$MonthYear)

#Creating the periods string (first tagging period till end of year 2023). 
Period_Seq = format(seq.Date(as.Date("2022-01-01"), as.Date("2023-12-01"), by = "month"),format = "%m-%Y")

test = TaggingMaster %>% group_by(MonthYear, RewardLevel, Num.tagged) %>% count(n())
test[is.na(test)]=0
test

MonthYearOrder = c("01-2022", "02-2022", "12-2022","01-2023","05-2023")
RewardLevOrder = c(1,2,3)
SingleDoubleOrder = c(1,2)


test$MonthYear = factor(test$MonthYear, levels = MonthYearOrder)
test = test[with(test,order(factor(test$MonthYear, levels = MonthYearOrder),factor(test$RewardLevel, levels = RewardLevOrder),factor(test$Num.tagged, levels = SingleDoubleOrder))),]
test

#This gets rid of the n() column which is just the total of all the tag types.  
test = test[,!(names(test) %in% "n()")]
test

#Firstly, I am going to take the tag recap matrix and make the capture date into a month/year format like we have in our matrix. 
#Additionally, I am going to add its release period and TaggingGroup which we can figure out from the tagging data for that individual fish. 

tag_return = read.csv("https://github.com/PrullMarcus/MastersAnalysis/raw/main/TagReturnsMasterSheet.csv")
tag_return = subset(tag_return, Which.body.of.water != "Logan Martin")

tourney_returns = filter(tag_return, If.released.it.was == "c")
tourney_returns

#Selecting only variables I think we need for this. 
tourney_returns = tourney_returns %>% dplyr::select(c("Tag.number","Second.Tag","Date.of.catch","Which.body.of.water",                                   "Access.to.the.lake","If.released.it.was"))
tourney_returns

#An easy thing to do first is to change the Date.of.catch part to MonthYear just like it is in the matrix we setup.

tourney_returns$Date.of.catch = as.Date(tourney_returns$Date.of.catch, format = "%m/%d/%Y")
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
nontourney_returns$Date.of.catch = as.Date(nontourney_returns$Date.of.catch, format = "%m/%d/%Y")
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

#### Estimation Model #### 

n_mon = 24#number of recap periods. 2 calendar years with a montly timestep = 24 months.
n_rel = nrow(tests_merged)#Do we need to change this to 5 if we aren't differenciating by tag-type within the model????

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

TL_init=0.028#about 3% in first month of tagging 
TL_chr=0.019#about 2% per month

lambda_fixed = c(0.8, 0.9, 1)
lambda_start = list(
  c(qlogis(0.7),qlogis(0.75)),
  c(qlogis(0.8),qlogis(0.85)),
  c(qlogis(0.9),qlogis(0.95))
)

fit_results = list()

for (i in seq_along(lambda_fixed)) {
  lambda_val = lambda_fixed[i]
  lambda_init = lambda_start[[i]]
  
  n_m = length(unique(m_index))
  n_f = length(unique(f_index))
  
  theta = c(
    m=log(rep(0.3, n_m)),
    lambda=lambda_init, 
    ft=log(rep(0.1, n_f)),
    fnt=log(rep(0.1, n_f))
  )
  
  nll=function(theta,recaps,TL_init,TL_chr,nTag,RewardLevel,release_period,f_index,m_index){
    n_m=length(unique(m_index))
    n_f=length(unique(f_index))
    n_lam=2
    m_param=exp(theta[1:n_m])
    
    lambda=plogis(theta[(n_m+1):(n_m+n_lam)])
    lambda=c(lambda,lambda_val)####lambda param####
    
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
    #Q=matrix(NA,nrow=n_rel,ncol=n_mon)
    #p_tag=matrix(NA,nrow=n_rel,ncol=n_mon)
    
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
          Q=(1-TL_init)*exp(-TL_chr*(y-release_period[r]))
          p_tag=Q^2*(nTag[r]-1)+Q*(2*(1-Q))^(nTag[r]-1)
          p_ret[r,y]=ut[y]*p_tag*lambda[RewardLevel[r]]
          p_ret[r,n_mon+y]=unt[y]*p_tag*lambda[RewardLevel[r]]
          #p_ret[r,y]=ut[y]*(1-TL_init^nTag[r])*lambda[RewardLevel[r]]
          #p_ret[r,n_mon+y]=unt[y]*(1-TL_init^nTag[r])*lambda[RewardLevel[r]]
          
        } else {
          Q=(1-TL_init)*exp(-TL_chr*(y-release_period[r]))
          p_tag=Q^2*(nTag[r]-1)+Q*(2*(1-Q))^(nTag[r]-1)
          p_ret[r,y]=ut[y]*p_tag*prod(S[r,release_period[r]:(y-1)])*lambda[RewardLevel[r]]
          p_ret[r,n_mon+y]=unt[y]*p_tag*prod(S[r,release_period[r]:(y-1)])*lambda[RewardLevel[r]]
          #p_ret[r,y]=ut[y]*(1-TL^nTag[r])*prod(S[r,release_period[r]:(y-1)])*lambda[RewardLevel[r]]
          #p_ret[r,n_mon+y]=unt[y]*(1-TL^nTag[r])*prod(S[r,release_period[r]:(y-1)])*lambda[RewardLevel[r]]
          
        }  # close if else
        
      }  #close y loop
      
      p_ret[r,2*n_mon+1]=1-sum(p_ret[r,])#probability of not being captured
      nll[r]=-dmultinom(recaps[r,],prob=p_ret[r,],log=T)
      
    } #close r loop
    #print(theta)
    sum(nll)
  }  #close function
  
  
  #nll(theta,recaps=,TL_init,TL_chr,nTag,RewardLevel,release_period,f_index,m_index)
  nll(theta,TL_init=TL_init,TL_chr=TL_chr,nTag=tests_merged$Num.tagged, RewardLevel=tests_merged$RewardLevel,
      release_period=release_period,recaps=tests_merged[,-c(1:4)],m_index=cm_index,f_index=u_index)
  
  fit=optim(theta,fn=nll,method='BFGS',hessian=T,TL_init=TL_init,TL_chr=TL_chr,nTag=tests_merged$Num.tagged, RewardLevel=tests_merged$RewardLevel,release_period=release_period,recaps=tests_merged[,-c(1:4)],
            control = list(trace=1,maxit = 200),m_index=cm_index,f_index=u_index)#needed a few more iterations to "converge"
  fit_results[[paste0("lambda_", lambda_val)]] = list(
    fit=fit,
    lambda_fixed=lambda_val,
    lambda_start=lambda_init
  )
  
}

n_m=length(unique(m_index))
n_f=length(unique(f_index))

all_estimates <- lapply(names(fit_results), function(name) {
  fit <- fit_results[[name]]$fit
  names(fit$par)=names(theta)
  lambda_fixed <- fit_results[[name]]$lambda_fixed
  
  hessian_inv <- solve(fit$hessian)
  se <- sqrt(diag(hessian_inv))
  
  est_z <- exp(fit$par[substr(names(fit$par),1,2)=='ft'])*0.73 +
    exp(fit$par[substr(names(fit$par),1,2)=='fn']) +
    exp(fit$par[substr(names(fit$par),1,1)=='m'])
  
  n_m <- length(unique(m_index))
  n_f <- length(unique(f_index))
  
  list(
    lambda_fixed = lambda_fixed,
    
    est_m = exp(fit$par[substr(names(fit$par),1,1)=='m']),
    l95ci_m = exp(fit$par[substr(names(fit$par),1,1)=='m'] - 1.96 * se[substr(names(fit$par),1,1)=='m']),
    u95ci_m = exp(fit$par[substr(names(fit$par),1,1)=='m'] + 1.96 * se[substr(names(fit$par),1,1)=='m']),
    
    est_ft = exp(fit$par[substr(names(fit$par),1,2)=='ft']),
    l95ci_ft = exp(fit$par[substr(names(fit$par),1,2)=='ft'] - 1.96 * se[substr(names(fit$par),1,2)=='ft']),
    u95ci_ft = exp(fit$par[substr(names(fit$par),1,2)=='ft'] + 1.96 * se[substr(names(fit$par),1,2)=='ft']),
    
    est_fnt = exp(fit$par[substr(names(fit$par),1,2)=='fn']),
    l95ci_fnt = exp(fit$par[substr(names(fit$par),1,2)=='fn'] - 1.96 * se[substr(names(fit$par),1,2)=='fn']),
    u95ci_fnt = exp(fit$par[substr(names(fit$par),1,2)=='fn'] + 1.96 * se[substr(names(fit$par),1,2)=='fn']),
    
    est_lam = plogis(fit$par[substr(names(fit$par),1,2)=='la']),
    l95ci_lam=plogis(fit$par[substr(names(fit$par),1,2)=='la'] - 1.96 * se[substr(names(fit$par),1,2)=='la']),
    u95ci_lam=plogis(fit$par[substr(names(fit$par),1,2)=='la'] + 1.96 * se[substr(names(fit$par),1,2)=='la']),
    
    est_z= est_z,
    est_ut=(exp(fit$par[substr(names(fit$par),1,2)=='ft'])*0.73)/est_z*(1-exp(-est_z))
  )
}
)

n_m=length(unique(m_index))

n_f=length(unique(f_index))

months=c('J','F','M','A','M','J','J','A','S','O','N','D')



#CL_Contribution = subset(tag_return, If.released.it.was == "c")
#CL_T_Prop = nrow(subset(CL_Contribution, Access.to.the.lake == "Coosa Landing")) / 
#  nrow(subset(CL_Contribution, !(Access.to.the.lake %in% c("Riverside Landing","Lincoln Landing"))))
#0.7313433 of tourney weighed in fish on Neely Henry were released at CL



#### ft, lambda = 0.80, pooled ####
windows()

png("F T Vertical Stack.png", widt=2000,height=2500, res=300)
layout(matrix(1:3, nrow=3, byrow=TRUE), heights = c(1,1,1))
par(mar = c(2, 2, 2, 2), oma = c(4,4,2,0),xpd=NA)

plot(all_estimates[[1]]$est_ft[f_index][1:12],type='l', lwd = 3, 
     ylim=c(0,0.2),xaxt='n',xlab='',cex.lab = 1.5, cex.axis = 1.5,
     ylab='')
axis(1,at=1:12,labels=months,cex.axis=1.5)
lines(all_estimates[[1]]$u95ci_ft[f_index][1:12],lty=3,lwd=3)
lines(all_estimates[[1]]$l95ci_ft[f_index][1:12],lty=3,lwd=3)
legend("topright",
       legend = c("Estimate", "95% CI"),
       lty = c(1,3),
       lwd = c(3,3),
       bty="n",
       cex=1.5
)

legend("topleft",
       legend = expression(lambda[300]==0.80),
       cex=1.5,
       adj = 0,
       bty="n",
       inset=0)

#### ft, lambda = 0.90, pooled ####
#windows()
#par(mar = c(5, 6, 4, 2))
plot(all_estimates[[2]]$est_ft[f_index][1:12],type='l', lwd = 3, 
     ylim=c(0,0.2),xaxt='n',xlab='',cex.lab = 1.5, cex.axis = 1.5,
     ylab='')
axis(1,at=1:12,labels=months,cex.axis=1.5)
lines(all_estimates[[2]]$u95ci_ft[f_index][1:12],lty=3,lwd=3)
lines(all_estimates[[2]]$l95ci_ft[f_index][1:12],lty=3,lwd=3)
legend("topright",
       legend = c("Estimate", "95% CI"),
       lty = c(1,3),
       lwd = c(3,3),
       bty="n",
       cex=1.5
)

legend("topleft",
       legend = expression(lambda[300]==0.90),
       cex=1.5,
       adj = 0,
       bty="n",
       inset=0)

#### ft, lambda = 1, pooled ####
#windows()
#par(mar = c(5, 6, 4, 2))
plot(all_estimates[[3]]$est_ft[f_index][1:12],type='l', lwd = 3, 
     ylim=c(0,0.2),xaxt='n',xlab='',cex.lab = 1.5, cex.axis = 1.5,
     ylab='')
axis(1,at=1:12,labels=months,cex.axis=1.5)
lines(all_estimates[[3]]$u95ci_ft[f_index][1:12],lty=3,lwd=3)
lines(all_estimates[[3]]$l95ci_ft[f_index][1:12],lty=3,lwd=3)
legend("topright",
       legend = c("Estimate", "95% CI"),
       lty = c(1,3),
       lwd = c(3,3),
       bty="n",
       cex=1.5
)

legend("topleft",
       legend = expression(lambda[300]==1.00),
       cex=1.5,
       adj = 0,
       bty="n",
       inset=0)

mtext("Month", side =1, outer =TRUE, line=1.5, cex=1)
mtext(expression('Tournament Weigh-in Rate (month  '^-1*')'), 
      side = 2, outer=TRUE, line=1, cex = 1)

dev.off()

#### fnt, lambda = 0.80, pooled ####
windows()
#png("F NT Vertical Stack.png", widt=2000,height=2500, res=300)
layout(matrix(1:3, nrow=3, byrow=TRUE), heights = c(1,1,1))
par(mar = c(2, 2, 2, 2), oma = c(4,4,2,0),xpd=NA)

plot(all_estimates[[1]]$est_fnt[f_index][1:12],type='l', lwd = 3, 
     ylim=c(0,0.4),xaxt='n',xlab='',cex.lab = 1.5, cex.axis = 1.5,
     ylab='')
axis(1,at=1:12,labels=months,cex.axis=1.5)
lines(all_estimates[[1]]$u95ci_fnt[f_index][1:12],lty=3,lwd=3)
lines(all_estimates[[1]]$l95ci_fnt[f_index][1:12],lty=3,lwd=3)
legend("topright",
       legend = c("Estimate", "95% CI"),
       lty = c(1,3),
       lwd = c(3,3),
       bty="n",
       cex=1.5
)

legend("topleft",
       legend = expression(lambda[300]==0.80),
       cex=1.5,
       adj = 0,
       bty="n",
       inset=0)

#### fnt, lambda = 0.90, pooled ####

plot(all_estimates[[2]]$est_fnt[f_index][1:12],type='l', lwd = 3, 
     ylim=c(0,0.4),xaxt='n',xlab='',cex.lab = 1.5, cex.axis = 1.5,
     ylab='')
axis(1,at=1:12,labels=months,cex.axis=1.5)
lines(all_estimates[[2]]$u95ci_fnt[f_index][1:12],lty=3,lwd=3)
lines(all_estimates[[2]]$l95ci_fnt[f_index][1:12],lty=3,lwd=3)
legend("topright",
       legend = c("Estimate", "95% CI"),
       lty = c(1,3),
       lwd = c(3,3),
       bty="n",
       cex=1.5
)

legend("topleft",
       legend = expression(lambda[300]==0.90),
       cex=1.5,
       adj = 0,
       bty="n",
       inset=0)

#### fnt, lambda = 1, pooled ####

plot(all_estimates[[3]]$est_fnt[f_index][1:12],type='l', lwd = 3, 
     ylim=c(0,0.4),xaxt='n',xlab='',cex.lab = 1.5, cex.axis = 1.5,
     ylab='')
axis(1,at=1:12,labels=months,cex.axis=1.5)
lines(all_estimates[[3]]$u95ci_fnt[f_index][1:12],lty=3,lwd=3)
lines(all_estimates[[3]]$l95ci_fnt[f_index][1:12],lty=3,lwd=3)
legend("topright",
       legend = c("Estimate", "95% CI"),
       lty = c(1,3),
       lwd = c(3,3),
       bty="n",
       cex=1.5
)

legend("topleft",
       legend = expression(lambda[300]==1),
       cex=1.5,
       adj = 0,
       bty="n",
       inset=0)

mtext("Month", side =1, outer =TRUE, line=1.5, cex=1)
mtext(expression('Non-Weigh-in Rate (month  '^-1*')'), 
      side = 2, outer=TRUE, line=1, cex = 1)

dev.off()


#### Instaneous Apparent M -1 Graph ####

M_dat = data.frame(
  est_m = c(all_estimates[[1]]$est_m, all_estimates[[2]]$est_m, all_estimates[[3]]$est_m),
  u_ci_m = c(all_estimates[[1]]$u95ci_m, all_estimates[[2]]$u95ci_m, all_estimates[[3]]$u95ci_m),
  l_ci_m = c(all_estimates[[1]]$l95ci_m, all_estimates[[2]]$l95ci_m, all_estimates[[3]]$l95ci_m),
  fixed_lambda=c(0.8,0.9,1)
)

windows()

#png("M Barplot.png", widt=2500,height=2000, res=300)
par(mar = c(5, 3, 2, 2), oma = c(0, 4, 0, 0))  
M_plot = barplot(M_dat$est_m, names.arg = M_dat$fixed_lambda,
        ylim = c(0,1),
        xaxt = 'n', yaxt = 'n')
axis(1, at = M_plot, labels = M_dat$fixed_lambda, cex.axis = 1.5)
axis(2, at = seq(0,1.5, by = 0.25), cex.axis = 1.5)

mtext(expression('Apparent Mortality (month  '^-1*')'),
      side = 2, line = 3, cex = 1.5)
mtext(expression('Assumed $300 Tag Reporting Rate ('*lambda[300]*')'),
      side = 1, line = 3, cex = 1.5)

arrows(x0 = M_plot, y0 = M_dat$l_ci_m,
       x1 = M_plot, y1 = M_dat$u_ci_m,
       angle=90, code = 3, length = 0.1, lwd = 1)
#dev.off()


#### Lambda Barplot ####

Lambda_dat = data.frame(
  est_lambda = c(all_estimates[[1]]$est_lam, all_estimates[[2]]$est_lam, all_estimates[[3]]$est_lam),
  u_ci_lam = c(all_estimates[[1]]$u95ci_lam, all_estimates[[2]]$u95ci_lam, all_estimates[[3]]$u95ci_lam),
  l_ci_lam= c(all_estimates[[1]]$l95ci_lam, all_estimates[[2]]$l95ci_lam, all_estimates[[3]]$l95ci_lam),
  RewardLevel=c(100,200,100,200,100,200),
  fixed_lambda=c(0.8,0.8,0.9,0.9,1,1)
)

Lambda_mat = matrix(Lambda_dat$est_lambda,
                    nrow=2, byrow=FALSE)
windows()
#png("Lambda Barplot.png", width=2500,height=2000, res=300)
par(mar = c(5, 5, 2, 2), oma = c(0, 3, 0, 0))  
lambda_plot = barplot(Lambda_mat, beside=TRUE,
        names.arg = unique(Lambda_dat$fixed_lambda),
        col = c("white", "darkgray"),
        ylim = c(0,1),
        xlab = expression("Assumed $300 Tag Reporting Rate ("*lambda[300]*")"),
        ylab = "Estimated Reporting Rate",cex.lab=1.5,
        cex.axis=1.5,
        cex.names = 1.5)
legend("topleft", legend=c("$100", "$200"),
       fill=c("white","darkgray"),bty="n",title = "Reward Level",
       cex=1.5,xpd=TRUE, inset = c(0.02,0))

arrows(x0=lambda_plot, y0=Lambda_dat$l_ci_lam,
       x1=lambda_plot, y1=Lambda_dat$u_ci_lam,
       angle=90,code=3, length=0.1,lwd=1)
#dev.off()

#### OBJECTIVE 2 ####
#For objective 2 we want to look at the behavior aspect of tournament dispersal. We will use telemetry data to do this
#Specifically, we want to get a dispersal rate away from Coosa Landing. 

library(sf)
library(terra)
library(raster)
library(gdistance)
library(lubridate)
library(dplyr)

#Loading in necessary data and shp files
TrackMaster = read.csv("https://github.com/PrullMarcus/MastersAnalysis/raw/main/TrackingDataMaster.csv")
NeelySHP = st_read("/vsicurl/https://github.com/PrullMarcus/MastersAnalysis/raw/main/SHP%20Files/NeelySHP_Feb2024_NAD_1983_2011.shp")
ReleaseZone = st_read("/vsicurl/https://github.com/PrullMarcus/MastersAnalysis/raw/main/SHP%20Files/Dispersal3kmPoly.shp")
CL_SHP = st_read("/vsicurl/https://github.com/PrullMarcus/MastersAnalysis/raw/main/SHP%20Files/CL.shp")

#Making date column for TrackMaster datasheet
Date = make_date(year = TrackMaster$Year, month = TrackMaster$Month, day = TrackMaster$Day)
TrackMaster = cbind(Date,TrackMaster)#binding the vector to the main csv
colnames(TrackMaster)[1]<-"Date"#Naming the date column "Date"
TrackMaster

#Setting up least-cost surface for movement to be calculated on
NeelyVec = vect(NeelySHP)
NeelyBuffer = terra::buffer(NeelyVec, width = 150)
#NeelyR = rast(NeelyVec, res = 0.00008)#res of 8.8 m grid cells, value of 1 is 1 "degree". that is why we need to use a big decimal to get it to meters. tried 00004 but it took way too long. 
NeelyR = rast(NeelyBuffer, res = 0.00008)
NeelyRast = rasterize(NeelyVec, NeelyR, field = 1)
NeelyRast[is.na(NeelyRast)] = 100#Land/outside of shapefile has a value of 100
NeelyRast = terra::mask(NeelyRast,NeelyBuffer)#Makes everything outside of the buffer NAs
NeelyRast = terra::crop(NeelyRast, NeelyBuffer)#Crops down to just cells that are within the buffer
#NeelyRast = rasterize(NeelyVec,NeelyR)#value of 1 is Neely, NA value is land
plot(NeelyRast)

#Converting raster to format used by the least-cost distance function
NeelyRast = raster(NeelyRast)#Converts spatraster to "raster" which is a different form of raster that the transition function needs in order to work properly. 
NeelyTrans = transition(1/NeelyRast, transitionFunction = mean, directions = 8)
#Here we correct for corners
NeelyTrans = geoCorrection(NeelyTrans, type = 'c', multpl=F)
NeelyTransRast = raster(NeelyTrans)
plot(NeelyTransRast)

#Filtering down to the fish that we tracked
T_Manuals = subset(TrackMaster, TournamentRelease == "Y")
T_Manuals = subset(T_Manuals, ManuallyTracked == "Y")
#T_Manuals = subset(T_Manuals, GroupMovement == "A")
T_Manuals = T_Manuals %>% group_by(RewardTag1) %>% filter(n()>1)

#Converting our lat/longs to a spatial points object
T_Manuals = sf::st_as_sf(T_Manuals, coords = c("Long","Lat"),crs = crs(NeelySHP))#making spatial object out of lat/long columns
plot(NeelySHP$geometry)
plot(T_Manuals, add = T)

#### Obj 2 Ramp Proximity ####
Start = Sys.time()

tag_nums = unique(T_Manuals$RewardTag1)#unique tag numbers
n_fish = length(tag_nums)#number of fish

T_LC_Prox = numeric()
T_LC_Step = numeric()
T_Days_Step = numeric()
T_Days_Rel = numeric()

for(i in 1:n_fish){
  
  new_tag = tag_nums[i]
  new_dat = subset(T_Manuals,RewardTag1 == new_tag)
  tmax = nrow(new_dat)
  Dispersal_prox = numeric(tmax)
  Dispersal_step = numeric(tmax)
  Days_Step = numeric(tmax)
  Days_Rel = numeric(tmax)
  
  for(t in 2:tmax){
    
    prox_m = gdistance:::costDistance(NeelyTrans, fromCoords = sf::st_coordinates(new_dat[1,]),
                                      toCoords = sf::st_coordinates(new_dat[t,]))
    step_m = gdistance::costDistance(NeelyTrans, fromCoords = sf::st_coordinates(new_dat[(t-1),]),
                                     toCoords = sf::st_coordinates(new_dat[t,]))
    Dispersal_prox[t] = prox_m
    Dispersal_step[t] = step_m
    
    Days_Step[t] = as.numeric(new_dat$Date[t]-new_dat$Date[t-1])
    Days_Rel[t] = as.numeric(new_dat$Date[t]-new_dat$Date[1])
    
  }
  T_LC_Prox = append(T_LC_Prox,Dispersal_prox)
  T_LC_Step = append(T_LC_Step,Dispersal_step)
  
  T_Days_Step = append(T_Days_Step,Days_Step)
  T_Days_Rel = append(T_Days_Rel,Days_Rel)
}
T_LC_Prox
T_LC_Step
T_Days_Rel
T_Days_Step

End = Sys.time()

difftime(End,Start, units = "mins")

#Adding the results to our T_Manuals Dataframe to be used later. 

T_Manuals = cbind(T_LC_Prox, T_Manuals)
colnames(T_Manuals)[1]<-"LC_Prox_m"#least-cost meters proximity to release ramp

T_Manuals = cbind(T_LC_Step, T_Manuals)
colnames(T_Manuals)[1]<-"LC_Step_m"#least cost step length in meters

T_Manuals = cbind(T_Days_Rel, T_Manuals)
colnames(T_Manuals)[1]<-"Days_Since_Rel"#Days since fish was initial released

T_Manuals = cbind(T_Days_Step, T_Manuals)
colnames(T_Manuals)[1]<-"Days_Step"#Days between previous location and current location for which the step length was calculated

T_Manuals$LC_M_W = T_Manuals$LC_Step_m/(T_Manuals$Days_Step/7)

T_Manuals$Weeks_Since_Rel = T_Manuals$Days_Since_Rel/7

T_Manuals = subset(T_Manuals,Weeks_Since_Rel != 0)

T_Manuals$RoundedWeekSinceRel = floor(T_Manuals$Weeks_Since_Rel)+1

#### KAPLAN #### 
#1 = dispersed, 0 = died before dispersal or never observed dispersing

library(survival)
library(survminer)
library(cmprsk)

CL_Fish = subset(T_Manuals,ReleaseRamp == "CL")
CL_Fish$Dispersed = ifelse(CL_Fish$LC_Prox_m>3000,1,0)

dat = CL_Fish %>% 
  group_by(RewardTag1) %>%
  summarize(
    first_dispersal = if(any(Dispersed == 1)) min(RoundedWeekSinceRel[Dispersed == 1]) else NA,
    first_death     = if(any(GroupMovement == "D")) min(RoundedWeekSinceRel[GroupMovement == "D"]) else NA,
    last_seen       = max(RoundedWeekSinceRel),
    .groups = "drop"
  ) %>%
  mutate(
    time = pmin(first_dispersal, first_death, last_seen, na.rm = TRUE),
    event = ifelse(!is.na(first_dispersal) & 
                     (is.na(first_death) | first_dispersal <= first_death), 1, 0)
  )

Kaplan = survfit(Surv(time, event) ~ 1, data = dat)
summary(Kaplan)

windows()
#png("Kaplan Dispersal.png", widt=2500,height=2000, res=300)
ggsurvplot(Kaplan,fun="event", conf.int=TRUE,
           surv.median.line="hv",ylim=c(0,1),
           ylab = "Probability of Dispersal",
           xlab = "Weeks Since Release",
           legend = "none")
#dev.off()
disp_summary <- summary(Kaplan)
disp_df <- data.frame(
  week = disp_summary$time,
  n_risk = disp_summary$n.risk,
  n_event = disp_summary$n.event,
  cum_disp = 1 - disp_summary$surv,
  lower_CI = 1 - disp_summary$upper,
  upper_CI = 1 - disp_summary$lower
)
disp_df

time_points <- seq(0, max(Kaplan$time), by = 4)  
km_summary <- summary(Kaplan, times = time_points)

# cumulative probability of dispersal at each time point
cum_disp <- 1 - km_summary$surv  

# monthly increments (probability of dispersing within that month)
monthly_disp <- diff(cum_disp)

disp_df <- data.frame(
  month = 1:length(monthly_disp),
  cum_disp = cum_disp[-1],       # cumulative by end of each month
  monthly_disp = monthly_disp    # dispersal during that month
)

disp_df

Disp_Probs = as.vector(c(disp_df[1,3],disp_df[2,3]))

#### CUMULATIVE INCIDENCE (KAPLAN BUT MORE COMPLICATED) ####
dat2 <- dat %>% mutate(
  time = pmin(first_dispersal, first_death, last_seen, na.rm=TRUE),
  status = case_when(
    !is.na(first_dispersal) & (is.na(first_death) | first_dispersal < first_death) ~ 1, # dispersal
    !is.na(first_death) & (is.na(first_dispersal) | first_death < first_dispersal) ~ 2, # death
    TRUE ~ 0 # censored
  )
)

#Running actual model
CIF <- cuminc(ftime = dat2$time, fstatus = dat2$status)

disp <- data.frame(
  time = CIF$`1 1`$time,
  est  = CIF$`1 1`$est,
  var  = CIF$`1 1`$var,
  event = "Dispersal"
) %>%
  mutate(
    lower = pmax(0, est - 1.96*sqrt(var)),
    upper = pmin(1, est + 1.96*sqrt(var))
  )

mort <- data.frame(
  time = CIF$`1 2`$time,
  est  = CIF$`1 2`$est,
  var  = CIF$`1 2`$var,
  event = "Mortality"
) %>%
  mutate(
    lower = pmax(0, est - 1.96*sqrt(var)),
    upper = pmin(1, est + 1.96*sqrt(var))
  )

# Combine all for plotting
cif_df <- bind_rows(
  disp %>% select(time, est, lower, upper, event),
  mort %>% select(time, est, lower, upper, event)
  )

#Main Plot with CIs
windows()
ggplot(cif_df, aes(x = time, y = est, color = event, fill = event)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(x = "Weeks Since release",
       y = "Probability",
       title = NULL) +
  theme_classic(base_size = 14) +
  scale_color_manual(values = c("Dispersal"="blue", "Mortality"="red")) +
  scale_fill_manual(values = c("Dispersal"="blue", "Mortality"="red")) +
  scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0))

#Dispersal = Fish observed dispersing
#Mortality = Fish observed dead without successfully dispersing
# 1 - (D + M) = Fish that were never observed dispersing or never seen dead within the zone


#### Obj 2. Proportion within release zone over time (RAW) ####

weekly_summary <- CL_Fish %>%
  group_by(Week = RoundedWeekSinceRel) %>%
  summarise(
    TotalFish = n(),
    NumWithin3km = sum(LC_Prox_m  > 3000, na.rm = TRUE),
    PropWithin3km = NumWithin3km / TotalFish
  ) %>%
  arrange(Week)

logistic_model_1 <- nls(
  PropWithin3km ~ L / (1 + exp(-k * (Week - t0))),
  data = weekly_summary,
  start = list(L = 1, k = 0.2, t0 = 2),
  weights = TotalFish,  # give more weight to high-sample weeks
  algorithm = "port",   # allows constraints
  lower = c(L = 0.7, k = 0.001, t0 = 0),
  upper = c(L = 1.05, k = 10, t0 = 4))

# Add fitted line to your data
weekly_summary$fit <- predict(logistic_model_1)

summary(logistic_model_1)


Prop_Dispersed = ggplot(weekly_summary, aes(x = Week, y = PropWithin3km)) +
  #geom_line(color="blue",linewidth = 1) +
  geom_point(size=2)+
  geom_line(aes(y = fit), color = "red", linetype = "solid",linewidth=2,alpha=0.5) +
  labs(x = "Weeks Since Release", y = "Proportion Dispersed") +
  theme_classic(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0.01))+
  scale_x_continuous(limits = c(0,52))

#### Obj 2. Prop Dispersed But Considered Dispersed Indefinitely Once Leaving ####
CL_Fish2 <- CL_Fish %>%
  arrange(RewardTag1, RoundedWeekSinceRel) %>%
  group_by(RewardTag1) %>%
  mutate(
    Dispersed = cumsum(LC_Prox_m > 3000 & !is.na(LC_Prox_m)) > 0
  ) %>%
  ungroup()

# Summarize weekly
weekly_summary <- CL_Fish2 %>%
  group_by(Week = RoundedWeekSinceRel) %>%
  summarise(
    TotalFish = n(),
    NumDispersed = sum(Dispersed, na.rm = TRUE),
    PropDispersed = NumDispersed / TotalFish
  ) %>%
  arrange(Week)

logistic_model_weighted <- nls(
  PropDispersed ~ L / (1 + exp(-k * (Week - t0))),
  data = weekly_summary,
  start = list(L = 1, k = 0.2, t0 = 2),
  weights = TotalFish,  # give more weight to high-sample weeks
  algorithm = "port",   # allows constraints
  lower = c(L = 0.7, k = 0.001, t0 = 0),
  upper = c(L = 1.05, k = 10, t0 = 4)
)

# View model summary
summary(logistic_model_weighted)

# Add fitted line to your data
weekly_summary$fit <- predict(logistic_model_weighted)


# Plot
Prop_Dispersed2 <- ggplot(weekly_summary, aes(x = Week, y = PropDispersed)) +
  #geom_line(color = "blue",lwd=1) +
  geom_point() +
  geom_line(aes(y = fit), color = "red", linetype = "dashed",linewidth=2) +
  labs(x = "Weeks Since Release", y = "Proportion Dispersed (>3km)") +
  theme_classic(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0.01)) +
  scale_x_continuous(limits = c(0, 52))
#geom_text(aes(label = TotalFish), vjust = -1, size = 3.5, color = "black")

#ggsave(filename = "C:/Users/mrp0099/OneDrive - Auburn University/Desktop/ProposalGraphs/Prop_Dispersed.pdf",plot = Prop_Dispersed)

install.packages("patchwork")
require("patchwork")

windows()
both = Prop_Dispersed + Prop_Dispersed2 + plot_layout(ncol=2)
ggsave(filename = "C:/Users/mrp0099/OneDrive - Auburn University/Desktop/ProposalGraphs/both2.pdf",plot = both,width = 12, height = 5)

#### Obj. 2. Distance displaced by tournaments ####

DistanceTransloc = TrackMaster %>% 
  filter(RewardTag1 %in% RewardTag1[TournamentRelease=="Y"])

DistanceTransloc = DistanceTransloc %>% 
  group_by(RewardTag1) %>% 
  filter(TournamentRelease == "Y" | lead(TournamentRelease=="Y"))

DistanceTransloc = DistanceTransloc %>% 
  group_by(RewardTag1) %>%
  slice(1:2)

DistanceTransloc = sf::st_as_sf(DistanceTransloc, coords = c("Long","Lat"),crs = crs(NeelySHP))

tag_nums = unique(DistanceTransloc$RewardTag1)
n_fish = length(tag_nums)# of individual fish making up dataset
T_Transloc_Dist = numeric()
#Outside loop that selects tag-number 
for(i in 1:n_fish){
  
  new_tag = tag_nums[i]
  new_dat = subset(DistanceTransloc,RewardTag1 == new_tag)
  tmax = nrow(new_dat)
  Transloc_Dist = numeric(tmax)
  
  for(t in 2:tmax){
    
    dist_m = gdistance:::costDistance(NeelyTrans, fromCoords = sf::st_coordinates(new_dat[t,]),
                                      toCoords = sf::st_coordinates(new_dat[(t-1),]))
    Transloc_Dist[t] = dist_m
  }
  
  T_Transloc_Dist = append(T_Transloc_Dist,Transloc_Dist)
}
T_Transloc_Dist#this is in meters

T_Transloc_Dist = T_Transloc_Dist[T_Transloc_Dist!=0]

T_Transloc_Dist_KM = T_Transloc_Dist/1000

DistanceTransloc = DistanceTransloc %>% group_by(RewardTag1) %>% slice(2)

DistanceTransloc = cbind(DistanceTransloc,T_Transloc_Dist_KM)

install.packages("ggh4x")
require("ggh4x")

TranslocateDistPlot = ggplot(DistanceTransloc, aes(x = T_Transloc_Dist_KM, fill=Species)) +
  geom_histogram(binwidth = 2,boundary=0,color="black") +
  facet_wrap2(~Species,ncol=1, axes = "all")+
  labs(x = "Distance Translocated by Angler (km)", y = "# of Fish") +
  theme_classic(base_size = 16)+
  theme(legend.position = "none")

#### Obj. 2. Homing ####

Homing = TrackMaster %>% 
  group_by(RewardTag1) %>%
  filter(any(TournamentRelease == "Y")) %>%
  ungroup

Homing = Homing %>% 
  group_by(RewardTag1) %>% 
  filter(TournamentRelease == "Y" | lead(TournamentRelease=="Y"))

Homing = subset(Homing, GroupMovement == "A")

Homing = sf::st_as_sf(Homing, coords = c("Long","Lat"),crs = crs(NeelySHP))

tag_nums = unique(Homing$RewardTag1)
n_fish = length(tag_nums)# of individual fish making up dataset

WeeksSinceRel = numeric()
Homing_SL = numeric()

#Outside loop that selects tag-number 
for(i in 1:n_fish){
  
  new_tag = tag_nums[i]
  new_dat = subset(Homing,RewardTag1 == new_tag)
  tmax = nrow(new_dat)
  Homing_Dist = numeric(tmax)
  WeeksFromRel = numeric(tmax)
  
  for(t in 2:tmax){
    
    dist_m = gdistance:::costDistance(NeelyTrans, fromCoords = sf::st_coordinates(new_dat[t,]),
                                      toCoords = sf::st_coordinates(new_dat[1,]))
    Homing_Dist[t] = dist_m
    WeeksFromRel[t] = as.numeric(new_dat$Date[t]-new_dat$Date[2])/7
    
  }
  Homing_SL = append(Homing_SL,Homing_Dist)
  WeeksSinceRel = append(WeeksSinceRel, WeeksFromRel)
}

Homing = cbind(Homing,Homing_SL)

Homing = cbind(Homing,WeeksSinceRel)

Homing

min_dist = Homing %>% group_by(RewardTag1) %>% summarise(MinDistFromPrev = min(Homing_SL/1000))
merged = st_drop_geometry(DistanceTransloc) %>% left_join(st_drop_geometry(min_dist), by = "RewardTag1")

merged =  merged %>% mutate(Within1km = as.numeric(MinDistFromPrev) <=1)

merged <- merged %>%
  mutate(DisplacementBin = cut(T_Transloc_Dist_KM,
                               breaks = seq(0, max(T_Transloc_Dist_KM, na.rm = TRUE) + 2, by = 2),
                               right = FALSE, include.lowest = TRUE))

# Summarize counts by species and bin
summary_table <- merged %>%
  group_by(Species, DisplacementBin) %>%
  summarise(Count = n(),
            ReturnedWithin1km = sum(Within1km, na.rm = TRUE),
            .groups = 'drop')

# View the table
print(summary_table)


Homing = Homing %>% group_by(RewardTag1) %>% slice(2:n())#Gets rid of double zeroes and intial location

ggplot(data = Homing, aes(x = WeeksSinceRel, y = Homing_SL/1000, group = RewardTag1,
                          color = as.factor(RewardTag1)))+
  geom_line()+
  geom_point(shape = factor(Homing$GroupMovement))
#triangle = Dead.... Need to look at this panel by panel to look at potential homing.

ggplot(data = Homing, aes(x = WeeksSinceRel, y = Homing_SL/1000,
                          color = as.factor(RewardTag1)))+
  geom_line()+
  geom_point(shape = factor(Homing$GroupMovement))+
  facet_wrap(vars(RewardTag1))



unique_tags = unique(Homing$RewardTag1)

for(tag in unique_tags){
  dat = subset(Homing,RewardTag1 == tag)
  
  p <- ggplot(dat, aes(x = WeeksSinceRel, y = Homing_SL / 1000)) +
    geom_line(color = "steelblue") +
    geom_point(aes(shape = factor(GroupMovement)), size = 2) +
    expand_limits(y=0)
  labs(
    title = paste("Homing Plot — Tag:", tag),
    x = "Weeks Since Release",
    y = "Homing Distance (km)",
    shape = "Group Movement"
  ) +
    theme_classic(base_size = 12)
  
  print(p)
  
}

Homing

#### Obj 2. Pre/Post ####

library(dplyr)

#Date = make_date(year = TrackMaster$Year, month = TrackMaster$Month, day = TrackMaster$Day)
#TrackMaster = cbind(Date,TrackMaster)#binding the vector to the main csv
#colnames(TrackMaster)[1]<-"Date"#Naming the date column "Date"

TrackFilt = TrackMaster %>% group_by(RewardTag1) %>% filter(ManuallyTracked == "Y"& GroupMovement =="A") %>% slice(-1)

# Step 1: Get first tournament release date per RewardTag1
release_dates <- TrackFilt %>%
  filter(TournamentRelease == "Y") %>%
  group_by(RewardTag1) %>%
  summarise(release_date = min(Date), .groups = "drop")

# Step 2: Join back and label as Pre/Post
labeled <- TrackFilt %>%
  inner_join(release_dates, by = "RewardTag1") %>%
  filter(ManuallyTracked == "Y") %>%
  mutate(period = case_when(
    Date < release_date ~ "Pre",
    Date > release_date ~ "Post",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period))

# Step 3: Keep only RewardTag1 groups with BOTH Pre and Post
valid_tags <- labeled %>%
  group_by(RewardTag1) %>%
  summarise(
    has_pre = any(period == "Pre"),
    has_post = any(period == "Post"),
    .groups = "drop"
  ) %>%
  filter(has_pre & has_post)

# Step 4: Filter labeled data to only those groups
filtered_labeled <- labeled %>%
  group_by(RewardTag1) %>%
  summarise(
    pre_count = sum(period == "Pre"),
    post_count = sum(period == "Post"),
    .groups = "drop"
  ) %>%
  filter(pre_count >= 2, post_count >= 2) %>%
  inner_join(labeled, by = "RewardTag1")

tag_nums = unique(filtered_labeled$RewardTag1)
n_fish = length(tag_nums)# of individual fish making up dataset

WeeksBetween = numeric()
PrePost_LC = numeric()


filtered_labeled = sf::st_as_sf(filtered_labeled, coords = c("Long","Lat"),crs = crs(NeelySHP))


#Outside loop that selects tag-number 
for(i in 1:n_fish){
  
  new_tag = tag_nums[i]
  new_dat = subset(filtered_labeled,RewardTag1 == new_tag)
  tmax = nrow(new_dat)
  PrePost_Dist = numeric(tmax)
  Weeks = numeric(tmax)
  
  for(t in 2:tmax){
    
    dist_m = gdistance:::costDistance(NeelyTrans, fromCoords = sf::st_coordinates(new_dat[t,]),
                                      toCoords = sf::st_coordinates(new_dat[(t-1),]))
    PrePost_Dist[t] = dist_m
    Weeks[t] = as.numeric(new_dat$Date[t]-new_dat$Date[t-1])/7
    
  }
  PrePost_LC = append(PrePost_LC,PrePost_Dist)
  WeeksBetween = append(WeeksBetween, Weeks)
}

filtered_labeled = cbind(filtered_labeled,PrePost_LC)

filtered_labeled = cbind(filtered_labeled,WeeksBetween)

filtered_labeled2 = filtered_labeled %>% group_by(RewardTag1,period) %>% filter(row_number() !=1) %>% ungroup()
filtered_labeled2$m_w = filtered_labeled2$PrePost_LC/filtered_labeled2$WeeksBetween
filtered_labeled2$period = factor(filtered_labeled2$period, levels = c("Pre", "Post"))

ggplot(filtered_labeled2, aes(x = period, y = m_w, fill = period))+
  geom_boxplot(position = position_dodge(width = 0.8))+
  facet_wrap(~RewardTag1,scales = "free_y") +
  theme_classic()

PrePost_plot = ggplot(filtered_labeled2, aes(x = Date, y = m_w, group = RewardTag1, shape = period)) +
  geom_point(size = 2) + 
  geom_line() + 
  geom_vline(aes(xintercept = release_date),linetype="dashed", color = "red", inherit.aes = FALSE)+
  facet_wrap(~RewardTag1, scales = "free") + 
  theme_classic() + 
  ylab("Movement Speed (meters/week")+
  theme(axis.text.x = element_blank()) +
  labs(color=NULL, shape = NULL)



#### OBJECTIVE 3 ####
#Now we want to pair up the results from objective 1 and objective 2 into our simulation model which will help 
#us understand the long-term spatial implications of the weigh-in rates that are occurring on Neely Henry


#CONSTANTS
n_yrs = 30
n_period = n_yrs*12
r_period = seq(4,n_period,12)

N_tot = 1 

ReleaseZone_AREA = as.numeric(st_area(ReleaseZone))
Lake_AREA = as.numeric(st_area(NeelySHP))

Initial_Lake = 1-(ReleaseZone_AREA/Lake_AREA) ####Maybe inform this based on tagging data????
InitialReleaseZone= N_tot-Initial_Lake ###Inform based on tagging data????

disp_rate = 0.65 #assymptotes around here after 1 month (DISPERSAL ANALYSIS)
Dnt = 0.1 #post release mortality (LITERATURE)#FIND CITATIONS FOR THIS
Dt = 0.2 #Weigh-in release mortality (LITERATURE)#FIND CITATIONS FOR THIS
Pharv = nrow(subset(tag_return,Kept.or.released == "K"))/nrow(subset(tag_return,If.released.it.was != "c")) #Proportion of non-weigh-in captures that resulted in harvest (TAG RETURN DATA)

sim_results = list()

for(i in seq_along(all_estimates)){
  
  parameters = all_estimates[[i]]
  
  Apparent_M = parameters$est_m / 12 #mortality (ESIMATION MODEL)
  ft = parameters$est_ft
  fnt = parameters$est_fnt 
  ut = parameters$est_ut
  
  load_rate_index = c(1,1,2,3,4,5,6,7,8,9,10,1)#
  load_rate_index = rep(load_rate_index,n_yrs)
  
  load_rate = as.vector(ut[load_rate_index])#tournament weigh in rate
  encounter_rate = as.vector(fnt[load_rate_index])
  
  Mort = Apparent_M + Pharv*encounter_rate+encounter_rate*(1-Pharv)*Dnt
  
  #SETTING UP WHAT WE ARE GOING TO TRACK
  NewImmig = numeric(length=n_period)
  NewEmig = numeric(length=n_period)
  Lake = numeric(length=n_period)
  
  ReleaseZonePop=numeric(length=n_period)
  InsiderPop = numeric(length=n_period)
  OutsidersInZone = numeric(length=n_period)
  TourneyInsiders=numeric(length=n_period)
  N = numeric(length=n_period)
  Prop_Zone=numeric(length=n_period)
  
  #InitialValues
  Lake[1] = Initial_Lake
  NewImmig[1] = load_rate[1]*Lake[1]
  NewEmig[1] = 0
  
  InsiderPop[1] = InitialReleaseZone
  OutsidersInZone[1] = 0
  ReleaseZonePop[1] = InsiderPop[1]+OutsidersInZone[1]
  TourneyInsiders[1] = InsiderPop[1]*load_rate[1]
  N[1] = InsiderPop[1]+Lake[1]
  
  for (t in 2:n_period){
    
    Lake[t] = ifelse(t %in% r_period,(Lake[t-1] - NewImmig[t-1])* exp(-Mort[t-1]) + NewEmig[t-1]*exp(-Mort[t-1]) + R*Initial_Lake,
                     (Lake[t-1] - NewImmig[t-1]) * exp(-Mort[t-1]) + NewEmig[t-1]*exp(-Mort[t-1]))
    
    NewImmig[t] = load_rate[t]*Lake[t]
    NewEmig[t] = NewImmig[t-1]*exp(-Mort[t-1])*(1-Dt)*disp_rate
    
    OutsidersInZone[t] = (OutsidersInZone[t-1]-NewEmig[t-1])*exp(-Mort[t-1])+NewImmig[t-1]*exp(-Mort[t-1])*(1-Dt)
    TourneyInsiders[t] = InsiderPop[t] * load_rate[t]
    
    InsiderPop[t] = ifelse(t %in% r_period,(InsiderPop[t-1]-TourneyInsiders[t-1])*exp(-Mort[t-1])+TourneyInsiders[t-1]*exp(-Mort[t-1])*(1-Dt)+R*InitialReleaseZone,
                           (InsiderPop[t-1]-TourneyInsiders[t-1])*exp(-Mort[t-1])+TourneyInsiders[t-1]*exp(-Mort[t-1])*(1-Dt))
    
    ReleaseZonePop[t] = InsiderPop[t] + OutsidersInZone[t] 
    
    
    N[t] = Lake[t] + ReleaseZonePop[t]
    Prop_Zone[t] = ReleaseZonePop[t]/N[t]
    
    if(t %in% r_period){
      deficit = N_tot - (Lake[t] + ReleaseZonePop[t])
      R_lake = deficit * Initial_Lake
      R_Zone = deficit * InitialReleaseZone
      
      Lake[t] = Lake[t] + R_lake
      InsiderPop[t] = InsiderPop[t] + R_Zone
      
      ReleaseZonePop[t] = InsiderPop[t] + OutsidersInZone[t]
      N[t] = Lake[t] + ReleaseZonePop[t]
      Prop_Zone[t] = ReleaseZonePop[t]/N[t]
    }
    
  }
  
  sim_results[[paste0("lambda_", parameters$lambda_fixed)]] <- list(
    
    Lake = Lake,
    Mort = Mort,
    NewImmig = NewImmig,
    NewEmig = NewEmig,
    OutsidersInZone = OutsidersInZone,
    TourneyInsiders = TourneyInsiders,
    InsiderPop = InsiderPop,
    ReleaseZonePop = ReleaseZonePop,
    N = N,
    Prop_Zone = Prop_Zone
    
  )
  
}

plot(sim_results$lambda_0.7$N, type="l")
plot(sim_results$lambda_0.85$N, type="l")
plot(sim_results$lambda_1$N, type="l")

windows()
plot(sim_results$lambda_0.7$Prop_Zone, type="l",col="blue")
lines(sim_results$lambda_0.85$Prop_Zone,col="red")
lines(sim_results$lambda_1$Prop_Zone,col="black")



































#### TRYING TO REVAMP OBJ 3 ####
#### OBJECTIVE 3 ####
#Now we want to pair up the results from objective 1 and objective 2 into our simulation model which will help 
#us understand the long-term spatial implications of the weigh-in rates that are occurring on Neely Henry


#CONSTANTS
n_yrs = 40
n_period = n_yrs*12
r_period = seq(4,n_period,12)

N_tot = 1 

ReleaseZone_AREA = as.numeric(st_area(ReleaseZone))
Lake_AREA = as.numeric(st_area(NeelySHP))

Initial_Lake = 1-(ReleaseZone_AREA/Lake_AREA) ####Maybe inform this based on tagging data????
InitialReleaseZone= N_tot-Initial_Lake ###Inform based on tagging data????

disp_rate = Disp_Probs #assymptotes around here after 1 month (DISPERSAL ANALYSIS)
Dnt = 0.1 #post release mortality (LITERATURE)#FIND CITATIONS FOR THIS
Dt = 0.2 #Weigh-in release mortality (LITERATURE)#FIND CITATIONS FOR THIS
Pharv = nrow(subset(tag_return,Kept.or.released == "K"))/nrow(subset(tag_return,If.released.it.was != "c")) #Proportion of non-weigh-in captures that resulted in harvest (TAG RETURN DATA)

sim_results = list()

for(i in seq_along(all_estimates)){
  
  parameters = all_estimates[[i]]
  
  Apparent_M = parameters$est_m / 12 #mortality (ESIMATION MODEL)
  ft = parameters$est_ft
  fnt = parameters$est_fnt 
  ut = parameters$est_ut
  ut = ut*CL_T_Prop
  
  load_rate_index = c(1,1,2,3,4,5,6,7,8,9,10,1)#
  load_rate_index = rep(load_rate_index,n_yrs)
  
  load_rate = as.vector(ut[load_rate_index])#tournament weigh in rate
  encounter_rate = as.vector(fnt[load_rate_index])
  
  Mort = Apparent_M + Pharv*encounter_rate+encounter_rate*(1-Pharv)*Dnt
  
  #SETTING UP WHAT WE ARE GOING TO TRACK
  NewImmig = numeric(length=n_period)
  NewEmig = numeric(length=n_period)
  Lake = numeric(length=n_period)
  
  ReleaseZonePop=numeric(length=n_period)
  InsiderPop = numeric(length=n_period)
  OutsidersInZone = numeric(length=n_period)
  TourneyInsiders=numeric(length=n_period)
  N = numeric(length=n_period)
  Prop_Zone=numeric(length=n_period)
  Undispersed=numeric(length=n_period)
  Month1Emig=numeric(length=n_period)
  Month2Emig=numeric(length=n_period)
  
  #InitialValues
  Lake[1] = Initial_Lake
  NewImmig[1] = load_rate[1]*Lake[1]
  NewEmig[1] = 0
  
  InsiderPop[1] = InitialReleaseZone
  OutsidersInZone[1] = 0
  ReleaseZonePop[1] = InsiderPop[1]+OutsidersInZone[1]
  TourneyInsiders[1] = InsiderPop[1]*load_rate[1]
  N[1] = InsiderPop[1]+Lake[1]

  for (t in 2:n_period){
    
    Lake[t] = (Lake[t-1] - NewImmig[t-1])* exp(-Mort[t-1]) + NewEmig[t-1]*exp(-Mort[t-1])
    
    NewImmig[t] = load_rate[t]*Lake[t]#Fish From Lake being added to Zone
    
    #Accounting for fish emigrating in either month 1 post release 
    Month1Emig[t] = NewImmig[t-1]*exp(-Mort[t-1])*(1-Dt)*Disp_Probs[1]#Fish that disperse in the first month in the zone
    Undispersed[t] = NewImmig[t-1]*exp(-Mort[t-1])*(1-Dt)*(1 - Disp_Probs[1])#Fish that remain after month 1
    Month2Emig[t] = Undispersed[t-1]*exp(-Mort[t-1])*Disp_Probs[2]#Fish that disperse in month 2
    
    NewEmig[t] = Month1Emig[t] + Month2Emig[t]
    
    OutsidersInZone[t] = (OutsidersInZone[t-1]-NewEmig[t-1])*exp(-Mort[t-1])+NewImmig[t-1]*exp(-Mort[t-1])*(1-Dt)#Fish that started in lake that are now in zone
    TourneyInsiders[t] = InsiderPop[t] * load_rate[t]#Fish that started in the zone that are caught in tournaments
    
    InsiderPop[t] = (InsiderPop[t-1]-TourneyInsiders[t-1])*exp(-Mort[t-1])+TourneyInsiders[t-1]*exp(-Mort[t-1])*(1-Dt)#Tracking the population of fish that start and stay in zone forever
    
    ReleaseZonePop[t] = InsiderPop[t] + OutsidersInZone[t]#Total population in zone including fish moved there and fish that start there
    
    
    N[t] = Lake[t] + ReleaseZonePop[t]#Total population of the entire waterbody
    Prop_Zone[t] = ReleaseZonePop[t]/N[t]#Proportion of the population inside the zone at [t]
    
    #Adding recruits to the population
    if(t %in% r_period){
      deficit = N_tot - (Lake[t] + ReleaseZonePop[t])
      R_lake = deficit * Initial_Lake
      R_Zone = deficit * InitialReleaseZone
      
      Lake[t] = Lake[t] + R_lake
      InsiderPop[t] = InsiderPop[t] + R_Zone
      
      ReleaseZonePop[t] = InsiderPop[t] + OutsidersInZone[t]
      N[t] = Lake[t] + ReleaseZonePop[t]
      Prop_Zone[t] = ReleaseZonePop[t]/N[t]
    }
    
  }
  
  sim_results[[paste0("lambda_", parameters$lambda_fixed)]] <- list(
    
    Lake = Lake,
    Mort = Mort,
    NewImmig = NewImmig,
    NewEmig = NewEmig,
    OutsidersInZone = OutsidersInZone,
    TourneyInsiders = TourneyInsiders,
    InsiderPop = InsiderPop,
    ReleaseZonePop = ReleaseZonePop,
    N = N,
    Prop_Zone = Prop_Zone
    
  )
  
}

plot(sim_results$lambda_0.7$N, type="l")
plot(sim_results$lambda_0.85$N, type="l")
plot(sim_results$lambda_1$N, type="l")

plot(sim_results$lambda_0.7$Prop_Zone, type="l",col="blue")
lines(sim_results$lambda_0.85$Prop_Zone,col="red")
lines(sim_results$lambda_1$Prop_Zone,col="black")



plot(sim_results$lambda_0.85$Prop_Zone, type="l")
plot(sim_results$lambda_1$Prop_Zone, type="l")



plot(sim_results$lambda_0.7$Prop_Zone[325:348],type="l",ylim=c(0,0.6),col="blue")
lines(sim_results$lambda_0.85$Prop_Zone[325:348],type="l",col="red")
lines(sim_results$lambda_1$Prop_Zone[325:348],type="l",col="black")






#### TABLES ####

















