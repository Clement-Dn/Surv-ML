# Libraries
library(timereg)
library(tidyverse)

# Data loading & cleaning
data(melanoma)

df = melanoma %>%
  mutate(
    lthick = log(thick),
    lthick_c = lthick - mean(lthick),
    lthick_gender = lthick*sex,
    ulcnew = ifelse(days>1400,ulc,0)
  )


#no a numeric vector. Patient code.
#status a numeric vector code. Survival status. 1: dead from melanoma, 2: alive, 3: dead from other cause.
#days a numeric vector. Survival time.
#ulc a numeric vector code. Ulceration, 1: present, 0: absent.
#thick a numeric vector. Tumour thickness (1/100 mm).
#sex a numeric vector code. 0: female, 1: male.

# Analysis
## Basic
fit=coxph(Surv(days,status==1)~ factor(sex) + lthick_c, data=df)
Hazcum=basehaz(fit,centered=FALSE)
par(mfrow=c(1,2))
plot(Hazcum$time,Hazcum$hazard,type="s",xlab="Temps",ylab="Hasard cumulé")
plot(Hazcum$time,exp(-Hazcum$hazard),type="s",xlab="Temps",ylab="Survie estimée")

## Gender
fit_gender = coxph(Surv(days,status==1)~ factor(sex)+lthick + lthick_gender,data=df)

## Strates
fit = coxph(Surv(days,status==1)~ factor(sex) + lthick + strata(ulc), data=df)
fit.detail=coxph.detail(fit)
logcum1=log(cumsum(fit.detail$hazard[c(17:57)]))
logcum0=log(cumsum(fit.detail$hazard[1:16]))
par(mfrow=c(1,1))
plot(c(fit.detail$time[17:57],3700),c(logcum1,logcum1[41]),type='s',xlab="Temps")
lines(c(fit.detail$time[1:16],3700),c(logcum0,logcum0[16]),type='s',lty=2)

## Time
df1 = survSplit(df,cut=c(1400),end="days",start="start",event="status")
fit1=coxph(Surv(start,days,status==1)~ factor(sex)+lthick+factor(ulc)
           + factor(ulcnew), data=df1)

# Kaplan-Meier
