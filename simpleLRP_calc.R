# this file is meant to calculate LRPs for the simple approaches as outlined in the NAP SAR.

library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(purrr)
library(magrittr)

# for FSA
library(FSA)
library(car)
library(plotrix)
library(nlstools)
library(lsmeans)


#clear environment
rm(list=ls())


# Source files ----
source("simpleLRP_FUN.R")
source("simpleLRP_dat.R")


# Bmsy proxies Fx%SPR ----

# X% Rmax ----
## Some other common approaches to defining LRPs are based on the biomass at a predefined percentage (X%) of Rmax, the maximum predicted recruitment from a SRR, or other thresholds to impaired recruitment 


## Ricker model ----
# https://notendur.hi.is/gunnar/kennsla/fii/lorna/fish480.pdf  File: fish480.pdf
# https://www.fao.org/3/X8498E/x8498e0e.htm

### Ricker 1 ----
sr1 <- lm(log(R/biomass) ~ biomass, data = sr)
summary(sr1)

# join the dataframe "sr" with the predicted values from the lm "sr1"
sr <- left_join(tibble::rownames_to_column(sr), tibble::rownames_to_column(as.data.frame(predict(sr1))), by = c("rowname"))
sr <- rename(sr, predict = "predict(sr1)")

# plot biomass v R/S and overlay predicted values
plot(sr$biomass, log(sr$R/sr$biomass))
lines(sr$biomass, sr$predict)


# From H&W pg 269, 
plot(sr$biomass, sr$R)
Sp <- seq(0, max(sr$biomass, na.rm = T), 100)
a <- sr1$coefficients[[1]]  # get it off log scale
b <- -a/sr1$coefficients[[2]]   # this matches 
Rp <- Sp*exp(a*(1-(Sp/b)))
lines(Sp, Rp, col = "red")
max(Rp)
abline(h=max(Rp))
abline(h=0.5*max(Rp))
abline(h=0.4*max(Rp))
Bmsy = b*(0.5-0.07*a) # from Hilborn and Walters Table 7.2 Smsy
abline(v = b*(0.5-0.07*a))
abline(v = 0.4*(b*(0.5-0.07*a)), col = "red") # the standard in Canada
Rmax = a/b*exp(-1) # Barrett WP 2022- Table 3 - no idea what this means.



# Plot of R v biomass
p <- ggplot(data = sr, aes(x = biomass, y = R, text = paste(
  "R: ", age2*age2PerMat, "\n", 
  "iSSB: ", biomass, "\n",
  "Year: ", year, "\n",
  sep = ""
)))
p <- p + geom_point()
p
ggplotly(p, tooltip = "text")

# Plot of R v biomass with SR curve and Smsy
SRR_dat <- as.data.frame(cbind(Sp, Rp))

p <- ggplot(data = sr, aes(x = biomass, y = R))
p <- p + geom_point()
p <- p + geom_line(data = SRR_dat, aes(x=Sp, y = Rp))
p <- p + geom_vline(xintercept = 0.4*(b*(0.5-0.07*a))) # 0.4Bmsy
p
ggplotly(p)

### Ricker 2 ----
# OK, this works.  https://notendur.hi.is/gunnar/kennsla/fii/lorna/fish480.pdf  Not sure I understand the Ricker formulation or all the work below but it is identical to what I have above and this is how I figured out how to plot this. 
a <- exp(sr1$coefficients[1])  # get it off log scale
b <- -1/sr1$coefficients[2]   # this matches 
Sp <- seq(0, max(sr$biomass, na.rm = T), 100)
Rp <- a*Sp*exp(-(1/b)*Sp)

plot(sr$biomass, sr$R)
lines(Sp, Rp)

# from Fish480.pdf
# Fmed is the fishing mortality which corresponds to the median observed slopes from data in an S-R plot. From file:///C:/Users/lewiske/Downloads/fish5106stockrec-pdf%20(2).pdf
# I think that this is a bit dangerous.  We have to accept the index as a proxy for SSB. Then, our catch has to be considered a portion of that.  And we need to feel that we can fish to modify the SSB/R ratio??????
SSB <- sr$biomass
R <- sr$R

Fmed <- median(SSB/R, na.rm = T)
Fhigh <- quantile(SSB/R, probs = 0.9, na.rm = T)
Flow <- quantile(SSB/R, probs = 0.1, na.rm = T)

abline(0, 1/median(SSB/R, na.rm = T)) #Fmed
abline(0,1/quantile(SSB/R, probs=0.1, na.rm = T), col=2) #Fhigh
abline(0,1/quantile(SSB/R, probs=0.9, na.rm = T), col=4) #Flow

### FSA----
# trying to replicate the above but with FSA from Ogle book
svR <- srStarts(R~biomass,data=na.omit(sr),type="Ricker", na.rm=T)
srFuns("Ricker")
rckr <- srFuns("Ricker")
rckr(S=135, a = svR$a, svR$b)

# non-linear regression
srR <- nls(R ~ rckr(biomass, a, b), data = na.omit(sr), start=svR)

# get parameters estimates and confidence intervals
cbind(estimates=coef(srR), confint(srR))


coef_srR <- coef(srR)
rckr(S=135, a = coef(srR))
a_r <- coef_srR[[1]]
b_r <- coef_srR[[2]]
# my code
#plot(sr$biomass, log(sr$R), ylim=c(0,12))
plot(sr$biomass, sr$R)

# bootstrap from book
## note that this works for the figure!!!
xr <- seq(0, max(sr$biomass, na.rm = T), 1)
pR <- rckr(xr, a = coef(srR))
lines(pR~xr, col="red")
abline(h=max(pR))
abline(h=0.5*(max(pR)))
abline(v = 1/coef_srR[[2]]) # from H&W table 7.2 Smax and I derived this independently - see notes.
abline(v = 0.4*(1/coef_srR[[2]])) # so this would be 0.4 of Bmsy I think
#abline(v = b*(0.5-0.07*a))  # from H&W table 7.2 Smsy
#abline(v = 0.4*(b*(0.5-0.07*a)))  # from standard for Bmsy
abline(v = (log(a_r)/b)*(0.5-0.07*log(a_r)))    # from H&W table 7.2 Smsy
abline(v = 0.5*(log(a_r)/b_r)*(0.5-0.07*log(a_r)))  # from standard for %Rmax = 326

BmsyRkr <- 0.5*(log(a_r)/b_r)*(0.5-0.07*log(a_r))

# Visualizing model fit

bootR <- nlsBoot(srR)
cbind(estimates = coef(srR), confint(bootR))

LCI_r <- UCI_r <- numeric(length(xr))
for(i in 1:length(xr)){
  tmp <- apply(bootR$coefboot, MARGIN = 1, FUN = rckr, S = xr[i])
  LCI_r[i] <- quantile(tmp, 0.025)
  UCI_r[i] <- quantile(tmp, 0.975)
} 

ylmts_r <- range(c(pR, LCI_r, UCI_r, sr$R), na.rm=T)
xlmts_r <- range(c(xr, sr$biomass), na.rm = T)

plot(sr$biomass, sr$R, xlim=xlmts_r, ylim=ylmts_r, col="white", ylab="Recruits", xlab = "Biomass (ktonnes)")
polygon(c(xr, rev(xr)), c(LCI_r,rev(UCI_r)), col = "gray80", border=NA)
points(R~biomass, data = sr, pch =19, col=rgb(0,0,0,1/2))
lines(pR~xr, lwd=2)
abline(v = 0.4*(log(a_r)/b_r)*(0.5-0.07*log(a_r)))  # from standard for Bmsy = 261


# resids
tmp <- na.omit(sr) %>%
  dplyr::mutate(fits=fitted(srR),
                resids=resid(srR),
                sresids=nlstools::nlsResiduals(srR)$resi2[,"Standardized residuals"])
peek(tmp,n=8)

ggplot(data=tmp,mapping=aes(x=fits,y=resids)) +
  geom_point() +
  geom_hline(yintercept=0,linetype="dashed")


ggplot(data=tmp,mapping=aes(x=resids)) +
  geom_histogram(color="gray30") 



## B-H ----
# https://github.com/fishR-Core-Team/FSA/blob/master/R/srStarts.R
### BH1 ----
#### This doesn't work

# sr2 <- lm(I(1/(age2*age2PerMat)) ~ I(1/biomass), data = sr)
# summary(sr2)
# slope = sr2$coefficients[[1]]
# intercept = sr2$coefficients[[2]]/sr2$coefficients[[1]]
# 
# # make a new data set
# srBH <- sr[,1:5]
# 
# tmp <- nls(age2*age2PerMat ~ biomass*slope/(1 + biomass/intercept), data = srBH, start = list(slope = 5, intercept = 50))
# 
# 
# srBH <- left_join(srBH, tibble::rownames_to_column(as.data.frame(predict(sr2))), by = c("rowname"))
# srBH <- rename(srBH, predict = "predict(sr2)")
# 
# # plot biomass v R/S and overlay predicted values
# plot(srBH$biomass, log(srBH$age2*srBH$age2PerMat/srBH$biomass))
# lines(srBH$biomass, srBH$predict)
# 
# plot(srBH$biomass, srBH$age2*srBH$age2PerMat)
# Sp <- seq(0, max(sr$biomass, na.rm = T), 100)
# #a <- 1/sr2$coefficients[1]  # get it off log scale
# #b <- 1/(sr2$coefficients[1]/sr1$coefficients[2])   # this matches 
# a <- 1/sr2$coefficients[2]  # get it off log scale
# b <- sr2$coefficients[1]*a   # this matches 
# 
# Rp <- a/b
# #Rp <- 1/((1/a + Sp/(a*b))*Sp)
# #lines(Sp, Rp, col = "red")
# 
# 
# lines(srBH$biomass, 1/srBH$predict)
# srBH <- sr[,1:5]
# srBH <- left_join(srBH, tibble::rownames_to_column(as.data.frame(predict(sr2))), by = c("rowname"))
# srBH <- rename(srBH, predict = "predict(sr2)")
# 
# 
# # 
# Sp <- seq(0, max(srBH$biomass, na.rm = T), 100)
# a <- coef(tmp)[[1]]  # get it off log scale
# b <- -a/coef(tmp)[[2]]   # this matches 
# Rp <- (a*Sp)/(b+Sp)
# lines(Sp, Rp, col = "red")

### BH2----
# https://books.google.ca/books?id=9Aq5k0hZLykC&pg=PA106&lpg=PA106&dq=beverton--holt+estimate+parameter+nls&source=bl&ots=jbtI6VEkf6&sig=ACfU3U0k7HRVfcMqThBOIaXjwRSDK6q9VA&hl=en&sa=X&ved=2ahUKEwjG286-lfH5AhUnpIkEHcuBC6M4HhDoAXoECBUQAw#v=onepage&q=beverton--holt%20estimate%20parameter%20nls&f=false
# tmp <- nls(age2*age2PerMat ~ biomass*alpha/(1 + biomass/k), data = srBH, start = list(alpha = 5, k = 50))
# 
# 
# # Trying to get spawners v recruits
# plot(srBH$biomass, srBH$age2*srBH$age2PerMat)
# 
# 
# plot(srBH$biomass, log(srBH$age2*srBH$age2PerMat/srBH$biomass))
# lines(srBH$biomass, srBH$predict)
# 
# 
# srBH$R <- sr$age2*sr$age2PerMat
# 
# sr2 <- lm(I(1/(age2*age2PerMat)) ~ I(1/biomass), data = sr)
# summary(sr2)                 
# 
# # this is as close as I can get to something sensible
# sr3 <- lm(biomass/(age2*age2PerMat) ~ biomass, data = sr)            
# summary(sr3)     
# 
# 
 srBH <- sr[,1:5]
# srBH <- left_join(srBH, tibble::rownames_to_column(as.data.frame(predict(sr3))), by = c("rowname"))
# srBH <- rename(srBH, predict = "predict(sr3)")
# 
# # plot biomass v R/S and overlay predicted values
# plot(srBH$biomass, srBH$age2*srBH$age2PerMat/srBH$biomass)
# lines(srBH$biomass, 1/srBH$predict)
# 
# 
# plot(srBH$biomass, srBH$age2*srBH$age2PerMat)
# Sp <- seq(0, max(sr$biomass, na.rm = T), 100)
# a <- 1/sr3$coefficients[1]  # get it off log scale
# b <- sr3$coefficients[2]*a   # this matches 
# 
# Rp <- a*Sp/(1+b*Sp)
# #Rp <- 1/((1/a + Sp/(a*b))*Sp)
# lines(Sp, Rp, col = "red")                 
# 
#bh.fit = nls(srBH$age2*srBH$age2PerMat ~ a * srBH$biomass/(1 + (a/b) *srBH$biomass), start = c(a = 0.696, b = 9.79))
#bh.fit


### FSA ----
#### this seems to wrok
srBH$R <- srBH$age2*1000*srBH$age2PerMat*0.01
svBH <- srStarts(R ~ biomass, data = na.omit(srBH), type = "BevertonHolt", na.rm=T)


svBH <- srStarts(R~biomass,data=na.omit(srBH),type="BevertonHolt",2)
srFuns("BevertonHolt",2)
bhr <- srFuns("BevertonHolt" ,2)
bhr(S=135, a = svBH$a, svBH$Rp)

srBH1 <- nls(R ~ bhr(biomass, a, Rp), data = na.omit(sr), start=svBH)

cbind(estimates=coef(srBH1), confint(srBH1))
coef_srBH <- coef(srBH1)
bhr(S=135, a = coef(srBH1))
a <- coef_srBH[[1]]
b <- coef_srBH[[2]]

# my code
#plot(sr$biomass, log(sr$R), ylim=c(0,12))
plot(sr$biomass, sr$R)

# bootstrap from book
## note that this works for the figure!!!
x <- seq(0, max(srBH$biomass, na.rm = T), 1)
pBH <- bhr(x, a = coef(srBH1))
lines(pBH~x, col="red")
abline(h=max(pBH))

# from H&W table 7.2 Smax is infinite
abline(v = b*sqrt(1/a)-b/a)  # from H&W table 7.2 Smsy
abline(v = 0.4*(b*sqrt(1/a)-b/a))  # from standard for Bmsy
abline(v = max(pBH)/(a*(1-max(pBH)/b))) # I derived this equation myself - see notes but need confirmation that it is correct.

bootR <- nlsBoot(srBH1)
cbind(estimates = coef(srBH1), confint(bootR))

LCI <- UCI <- numeric(length(x))
for(i in 1:length(x)){
  tmp <- apply(bootR$coefboot, MARGIN = 1, FUN = bhr, S = x[i])
  LCI[i] <- quantile(tmp, 0.025)
  UCI[i] <- quantile(tmp, 0.975)
} 

ylmts <- range(c(pBH, LCI, UCI, sr$R), na.rm=T)
xlmts <- range(c(x, sr$biomass), na.rm = T)

plot(sr$biomass, sr$R, xlim=xlmts, ylim=ylmts, col="white", ylab="Recruits", xlab = "Biomass (ktonnes)")
polygon(c(x, rev(x)), c(LCI,rev(UCI)), col = "gray80", border=NA)
points(R~biomass, data = sr, pch =19, col=rgb(0,0,0,1/2))
lines(pBH~x, lwd=2)
abline(v = 0.4*(b*sqrt(1/a)-b/a))  # from standard for Bmsy = 118

BmsyBH <- 0.4*(b*sqrt(1/a)-b/a)

# compare models to an indpendent one, i.e., a regression
ind <- srFuns("independence")
svI <- srStarts(R~biomass,data=na.omit(sr),type="independence")
srI <- nls(R ~ ind(biomass, a), data = na.omit(sr), start=svI)
extraSS(srI, com=srR)
extraSS(srI, com=srBH1)


summary(srR, correlation = T)


# resids
tmp <- na.omit(sr) %>%
  dplyr::mutate(fits=fitted(srBH1),
                resids=resid(srBH1),
                sresids=nlstools::nlsResiduals(srBH1)$resi2[,"Standardized residuals"])


ggplot(data=tmp,mapping=aes(x=fits,y=resids)) +
  geom_point() +
  geom_hline(yintercept=0,linetype="dashed")


ggplot(data=tmp,mapping=aes(x=resids)) +
  geom_histogram(color="gray30") 

### FSA 2 ----
#### This is for calculations in the spreadsheet which use a different formulation of BH
svBH2 <- srStarts(R~biomass,data=na.omit(srBH),type="BevertonHolt", 1)
srFuns("BevertonHolt",1)
bhr2 <- srFuns("BevertonHolt" , 1)
bhr2(S=135, a = svBH2$a, svBH2$b)

srBH2 <- nls(R ~ bhr2(biomass, a, b), data = na.omit(srBH), start=svBH2)

cbind(estimates=coef(srBH2), confint(srBH2))

coef_srBH2 <- coef(srBH2)
a <- coef_srBH2[[1]]
(a <- coef_srBH2[[1]])
b <- coef_srBH2[[2]]
(b <- coef_srBH2[[2]])

# my code
#plot(sr$biomass, log(sr$R), ylim=c(0,12))
plot(srBH$biomass, srBH$R)

# bootstrap from book
## note that this works for the figure!!!
x <- seq(0, max(sr$biomass, na.rm = T), 1)
pBH <- bhr2(x, a = coef(srBH2))
lines(pBH~x, col="red")
abline(h=max(pBH))


### FSA 2-ln ----
#### This is for calculations in the spreadsheet which use a different formulation of BH
srBH$logR <- log(srBH$R)
svBH2 <- srStarts(R~biomass,data=na.omit(srBH),type="BevertonHolt", 1)
bhr2 <- srFuns("BevertonHolt" , 1)
bhr2(S=135, a = svBH2$a, svBH2$b)

# something wrong with this - perhaps because it is with log - not sure why
#srBH2 <- nls(logR ~ log(bhr2(biomass, a, b)), data = na.omit(srBH), start=svBH2)

cbind(estimates=coef(srBH2), confint(srBH2))

coef_srBH2 <- coef(srBH2)
a <- coef_srBH2[[1]]
(a <- coef_srBH2[[1]])
b <- coef_srBH2[[2]]
(b <- coef_srBH2[[2]])

# my code
#plot(sr$biomass, log(sr$R), ylim=c(0,12))
plot(srBH$biomass, srBH$R)

# bootstrap from book
## note that this works for the figure!!!
x <- seq(0, max(sr$biomass, na.rm = T), 1)
pBH <- bhr2(x, a = coef(srBH2))
lines(pBH~x, col="red")
abline(h=max(pBH))

## Hockey stick ----


# Brecover from models ----
## Bloss is the lowest observed biomass 


## Brecover is the lowest observed biomass which produced recruitment that lead to stock recovery 





## Bmin ----
### Bmin is the lowest observed biomass from which a recovery to average has been observed

## Other minimum biomass that produced “good” recruitment 



## ICES Type I ----
### Haddock type approach - all data
### Brecover is the lowest observed biomass which produced recruitment that lead to stock recovery 

# calculate anomalies - get mean and SD
cap <- anomaly(cap, "biomass_med_lead")


# get the quantile and extract the 90th for the hline
h90 <- quantile(cap$anomaly, c(0.1, 0.9), na.rm = T)[2]
h50 <- quantile(cap$anomaly, c(0.1, 0.9), na.rm = T)[2]
# confirm above value and check on plot
quantile(cap$anomaly, c(0.1, 0.9), na.rm = T)


# simple test plot
plot(cap$year, cap$anomaly)
abline(h = h90)


#get the value for the lowest index to generate large recruitment (or a large index)
biomass90_1 <- quantile(cap$biomass_med, c(0.1, 0.5, 0.9), na.rm = T)
y1 <- subset(cap, biomass_med_lead >= biomass90_1[3], na.rm = T)
v90 <- min(y1$biomass_med)

y2 <- subset(cap, biomass_med_lead >= biomass90_1[2], na.rm = T)
v50 <- min(y2$biomass_med, na.rm = T)

# simple test plot
plot(cap$biomass_med, cap$biomass_med_lead)
abline(v = v90)
abline(v = v50, lty=2)

Scatter2(df = cap, xaxis = biomass_med, yaxis = biomass_med_lead, c2 = "Biomass: ", c3 = "Recruitment: ", xlab = "Index (ktonnes)", ylab = "Recruitment (ktonnes)", vline1 = v90, vline2 = v50, filename = "figs/6-Biomass_postCollapse-index-recruit.pdf", save = "no")


## ICES Type I - post collapse----
### Haddock type approach
### Brecover is the lowest observed biomass which produced recruitment that lead to stock recovery 

#calculate anomalies - get mean and SD
cap_postCollapse <- anomaly(cap_postCollapse, "biomass_med_lead")


# get the quantile and extract the 90th for the hline
h90_post <- quantile(cap_postCollapse$anomaly, c(0.1, 0.9), na.rm = T)[2]
# confirm above value and check on plot
quantile(cap_postCollapse$anomaly, c(0.1, 0.9), na.rm = T)

plot(cap_postCollapse$year, cap_postCollapse$anomaly)
abline(h = h90_post)


#get the value for the lowest index to generate large recruitment (or a large index)
biomass90_1 <- quantile(cap_postCollapse$biomass_med, c(0.1, 0.5, 0.9), na.rm = T)
y1 <- subset(cap_postCollapse, biomass_med_lead >= biomass90_1[3], na.rm = T)
y2 <- subset(cap_postCollapse, biomass_med_lead >= biomass90_1[2], na.rm = T)
v90_post <- min(y1$biomass_med)
v50_post <- min(y2$biomass_med, na.rm = T)


# simple test plot
plot(cap_postCollapse$year, cap_postCollapse$biomass_med_lead)
plot(cap_postCollapse$biomass_med, cap_postCollapse$biomass_med_lead)

# simple test plot
plot(cap_postCollapse$biomass_med, cap_postCollapse$biomass_med_lead)
abline(v = v90_post)
abline(v = v50_post)

# Bmsy – historical proxies----
##A historical proxy for BMSY can be estimated as the mean or median value of an indicator over a historical time period when the indicator is high (and assumed recruitment is stable) and catches are high; or the mean or median value of an indicator over a productive period. 

##A historical proxy for B0 can be estimated as the mean/median indicator over a historical time period reflecting the beginning of exploitation, or the maximum value of the indicator if the stock has a history of exploitation. 

# this reflects the highest time period on record and could be a historical Bo although this doesn't reflect the begnining of exploitation it is the max value of the indicator.
str(cap)
cap$year[1:7]
ma1 <- mean(cap$abundance_med[1:7])
mb1 <- mean(cap$biomass_med[1:7])
mda1 <- median(cap$abundance_med[1:7])
mdb1 <- median(cap$biomass_med[1:7])

# Bmsy: this is the time period from 1999:2018.  It "captures" the post collapse period without the data gaps of the 1990s and has the high point of 2013-2015
# probelm that there is no productive period since 1991 except for 2013-2015
cap$year[15:34] 
ma2 <- mean(cap$abundance_med[15:34], na.rm = T)
mb2 <- mean(cap$biomass_med[15:34], na.rm = T)
mda2 <- median(cap$abundance_med[15:34], na.rm = T)
mdb2 <- median(cap$biomass_med[15:34], na.rm = T)

# Bmsy: this time period is 2011-2018.  It captures the high period of 2013-2015 without the low of 2010.
cap$year[27:34] 
ma3 <- mean(cap$abundance_med[27:34], na.rm = T)
mb3 <- mean(cap$biomass_med[27:34], na.rm = T)
mda3 <- median(cap$abundance_med[27:34], na.rm = T)
mdb3 <- median(cap$biomass_med[27:34], na.rm = T)

# Bmsy: this time period is 2013-2015.  It captures the high period of 2013-2015, i.e., the indicator is high.
cap$year[29:31] 
ma4 <- mean(cap$abundance_med[29:31], na.rm = T)
mb4 <- mean(cap$biomass_med[29:31], na.rm = T)
mda4 <- median(cap$abundance_med[29:31], na.rm = T)
mdb4 <- median(cap$biomass_med[29:31], na.rm = T)


histLRP <- as.data.frame(matrix(NA, 4, 6))
histLRP <- rename(histLRP, indicator = V1, mct = V2, "<=1991" = V3, "1999-2018" = V4, "2011-2018" = V5, "2012-2015" = V6)
histLRP$indicator <- sort(rep(c("abund", "biomass"), 2))
histLRP$mct <- rep(c("mean", "median"), 2)

# put all values in the table and multiple by 0.4
histLRP$`<=1991`[1] <- 0.4*ma1
histLRP$`<=1991`[2] <- 0.4*mda1
histLRP$`<=1991`[3] <- 0.4*mb1
histLRP$`<=1991`[4] <- 0.4*mdb1

histLRP$`1999-2018`[1] <- 0.4*ma2
histLRP$`1999-2018`[2] <- 0.4*mda2
histLRP$`1999-2018`[3] <- 0.4*mb2
histLRP$`1999-2018`[4] <- 0.4*mdb2

histLRP$`2011-2018`[1] <- 0.4*ma3
histLRP$`2011-2018`[2] <- 0.4*mda3
histLRP$`2011-2018`[3] <- 0.4*mb3
histLRP$`2011-2018`[4] <- 0.4*mdb3

histLRP$`2012-2015`[1] <- 0.4*ma4
histLRP$`2012-2015`[2] <- 0.4*mda4
histLRP$`2012-2015`[3] <- 0.4*mb4
histLRP$`2012-2015`[4] <- 0.4*mdb4

histLRP <- histLRP %>% mutate_if(is.numeric, round) 






# Empirical LRP ----
## Thresholds set from empirical indicators such as catch, catch per unit effort (CPUE), survey indices, etc.
## Often employed as proxies for other reference points such as BMSY, B0, or Brecover, although other thresholds to serious harm may be considered (e.g., agreed-upon undesirable states to avoid).


## Bloss is the lowest observed biomass 
Bloss_abund_all <- min(cap$abundance_med, na.rm = T)
Bloss_abund_post <- min(cap$abundance_med[15:34], na.rm = T)
Bloss_abund_recent <- min(cap$abundance_med[27:34], na.rm = T)

Bloss_bio_all <- min(cap$biomass_med, na.rm = T)
Bloss_bio_post <- min(cap$biomass_med[15:34], na.rm = T)
Bloss_bio_recent<- min(cap$biomass_med[27:34], na.rm = T)

Bloss_bio_all <- min(cap$biomass_med, na.rm = T)
Bloss_bio_post <- min(cap$biomass_med[15:34], na.rm = T)
Bloss_bio_recent<- min(cap$biomass_med[27:34], na.rm = T)

B0_abund_all <- max(cap$abundance_med, na.rm = T)
B0_abund_post <- max(cap$abundance_med[15:34], na.rm = T)
B0_abund_recent<- max(cap$abundance_med[27:34], na.rm = T)

B0_bio_all <- max(cap$biomass_med, na.rm = T)
B0_bio_post <- max(cap$biomass_med[15:34], na.rm = T)
B0_bio_recent<- max(cap$biomass_med[27:34], na.rm = T)

Bmin <- as.data.frame(matrix(NA, 4, 5))
Bmin <- rename(Bmin, indicator = V1, "mt" = V2, "all" = V3, "1999-2018" = V4, "2011-2018" = V5)
Bmin$indicator <- sort(rep(c("abund", "biomass"), 2))
Bmin$mt <- rep(c("min", "B0"), 2)


Bmin$all[1] <- Bloss_abund_all
Bmin$all[2] <- Bloss_bio_all

Bmin$`1999-2018`[1] <- Bloss_abund_post
Bmin$`1999-2018`[2] <- Bloss_bio_post

Bmin$`2011-2018`[1] <- Bloss_abund_recent
Bmin$`2011-2018`[2] <- Bloss_bio_recent


Bmin$all[3] <- 0.4*B0_abund_all
Bmin$all[4] <- 0.4*B0_bio_all

Bmin$`1999-2018`[3] <- 0.4*B0_abund_post
Bmin$`1999-2018`[4] <- 0.4*B0_bio_post

Bmin$`2011-2018`[3] <- 0.4*B0_abund_recent
Bmin$`2011-2018`[4] <- 0.4*B0_bio_recent


# Brecover - is the lowest observed biomass which produced recruitment that lead to stock recover.  1) this is a two step process - needs to produce recruitment that leads to recovery!!!! 2) Could debate if this stock has recovered.



# Bmin is the lowest observed biomass from which a recovery to average has been observed or other minimum biomass that produced “good” recruitment


# Multivariate ----
## see dashboard.  Need to come up with some sort of proposal here

