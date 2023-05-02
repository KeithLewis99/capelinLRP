# The purpose of this code is to perform Beverton-Holt (BHJ) and Ricker Stock Recruit Relationships (SRR), X%Rmax, Fx%SPR and determine other thresholds for the capelin LRP.  Code is largely from Tim Barrett - see folder Barett for functions. 

library(ggplot2)

source("C:/Users/lewiske/Documents/capelin_LRP/analyses/capelinLRP/Barrett/FTN_230116.R") # contains functions "survivorship_F" and "RPcalc"

# Data ----
## this data set has recruits_t+2 (age-2) and mature biomass_t.  See simpleLRP_dat.R
DF <- read.csv("C:/Users/lewiske/Documents/capelin_LRP/IPM/data/ssb_all.csv")

# format data for Tim's functions
names(DF)
names(DF)[4] <- "SSB"
names(DF)[5] <- "REC"

DF$REC = 1000*DF$REC #put recruitment in millions. Code assumes SSB is in kt, recruitment is in millions, waa is in kg
R <- DF$REC[!is.na(DF$SSB) & !is.na(DF$REC)]
S <- DF$SSB[!is.na(DF$SSB) & !is.na(DF$REC)]


## For Fx%SPR: weight, maturity, M, and selectivity at age.  See excel files in Barrett's folder
waa <- c(0.008, 0.18, 0.022, 0.0294, 0.0456)
mat <- c(0,0.35,1,1,1) # used 0.3 for age 2 from data_for_Tim.csv (year 2017)
sel <- mat
sel <- c(0.5, 0.95, 0.9, 0.9, 0.9) # based on Fran's best guess

# various mortality-at-age values
#Maa <- c(0.4,rep(0.2,4))  # based on nothing at all
#Maa <- c(0.9,rep(0.8,4)) # based on wanting higher values and see what happens
#Maa <- c(1.2, 0.9,0.8, 0.7, 0.6) # based on BS where M from 2-3 = 0.93 - I adjusted things from there.
#Maa <- c(1.2, 0.8,0.7, 0.6, 0.5) # simple sensitivity test

# from IPM_dat
Maa <- c(1.2, 0.9, 1.4, 3.0, 2.5)

# initial values
Rinf = 350000 # starting value for asymptotic recruitment = geometric mean of rec from highest 50% of SSB
S50 = 1000 # starting value for SSB at 50% of Rinf is 0.2 maximum SSB



# plot data ----
# plot of SSB v R
ggplot(DF) + geom_point(mapping=aes(y=REC,x=SSB,colour=year)) +
  theme_classic() + labs(x="SSB (kt)", y="Recruitment (millions)") + 
  scale_y_continuous(limits = c(0,400000), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0,2500), expand = c(0, 0))


# fit SRR ----
#fit BH
mle_BH <- optim(fn=BHnll,par=c(log(Rinf),log(S50),log(0.5)),hessian = T)
if(mle_BH$convergence!=0){print("Did not conv")}
MLE_Rinf <- exp(mle_BH$par[1]) # R and S are logged so need to get them to real numbers
MLE_S50 <- exp(mle_BH$par[2])
# assuming here that par 1 = a (max survival) and par2 = b(DD)

#fit rick
mle_rick <- optim(fn=ricknll,par=c(log(200000),log(700),log(0.5)),hessian = T)
if(mle_rick$convergence!=0){print("Did not conv")}
MLE_rk <- exp(mle_rick$par[1])
MLE_Sk<- exp(mle_rick$par[2]) # DD with overcompensation


# plot SRR ----
# plot SRR fits at scale of data
ggplot(DF) + geom_point(mapping=aes(y=REC,x=SSB,colour=year)) +
  theme_classic() + labs(x="SIB (kt)", y="Recruitment (millions)") + 
  geom_function(fun=function(x) (MLE_Rinf/ (1+MLE_S50/x)),colour="black",linetype=1) +
  geom_function(fun=function(x) (MLE_rk*x/MLE_Sk*exp(1-(x/MLE_Sk))),colour="purple",linetype=1) +
  scale_colour_continuous(type = "viridis") +
  theme(legend.key.size = unit(0.3, 'cm'))
ggsave("Barrett/SRR_SSB.png", width = 12, heigh = 8, units = "cm")

# Fx%SPR - calculations ----
## BH
RPcalcBH <- RPcalc(M=Maa,waa,mat,sel,SRR='BH_Rinf',SRRpars=c(MLE_Rinf,MLE_S50),Req=median(DF$REC[DF$year <= 1990]),Smax=15000)

RPcalcBH_geo <- RPcalc(M=Maa,waa,mat,sel,SRR='BH_Rinf',SRRpars=c(MLE_Rinf,MLE_S50),Req=exp(mean(log(DF$REC[DF$year <= 1990]))),Smax=15000)

RPcalcBH$SSBmsy
RPcalcBH$SSBmsy/RPcalcBH$SSB0
RPcalcBH$eq_SSB_f_spr_40 # this part I don't get - if we dont believe the SRR, how can we SRRpars?
RPcalcBH_geo$eq_SSB_f_spr_40 # this part I don't get - if we dont believe the SRR, how can we SRRpars?


#BH a, b parameterization
BHa <- MLE_Rinf/MLE_S50
BHb <- 1/MLE_S50

BHa/BHb # This is Rmax - aymptotic recruitment

## Ricker
RPcalcrick <- RPcalc(M=Maa,waa,mat,sel,SRR='SL',SRRpars=c(MLE_rk,MLE_Sk,1),Req=median(DF$REC[DF$year <= 1990]),Smax=7500)
RPcalcrick_geo <- RPcalc(M=Maa,waa,mat,sel,SRR='SL',SRRpars=c(MLE_rk,MLE_Sk,1),Req=exp(mean(log(DF$REC[DF$year <= 1990]))),Smax=7500)

RPcalcrick$SSBmsy
RPcalcrick$SSBmsy/RPcalcrick$SSB0
#RPcalcrick$eq_SSB_f_spr_40
#RPcalcrick_geo$eq_SSB_f_spr_40

# Unfished spawning biomass (or eggs) per recruit
## get the survivorship (lo_age) with function; then phi0 = lo_age*waa*mat
phi0 <- sum(survivorship_F(M=Maa,n_ages = length(Maa))*waa*mat) 
1/phi0


# Plot SRR fits and phi0
ggplot(DF) + geom_point(mapping=aes(y=REC,x=SSB,colour=year)) +
  theme_classic() + labs(x="SSB (kt)", y="Recruitment (millions)") + 
  scale_y_continuous(limits = c(0,400000), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0,15000), expand = c(0, 0)) +
  geom_function(fun=function(x) (MLE_Rinf/ (1+MLE_S50/x)),colour="black",linetype=1) +
  geom_function(fun=function(x) (MLE_rk*x/MLE_Sk*exp(1-(x/MLE_Sk))),colour="purple",linetype=1) +
  geom_function(fun=function(x) (1/phi0*x),colour="green",linetype=1) # what does this intersection tell us?

# Plot SRR fits with all lines over full range of Ricker curve
ggplot(DF) + geom_point(mapping=aes(y=REC,x=SSB,colour=year)) +
  theme_classic() + labs(x="SSB (kt)", y="Recruitment (millions)") + 
  scale_y_continuous(limits = c(0,400000), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0,15000), expand = c(0, 0)) +
  geom_function(fun=function(x) (MLE_Rinf/ (1+MLE_S50/x)),colour="black",linetype=1) +
  #geom_function(fun=function(x) (BHa*x / (1+BHb*x)),colour="red",linetype=1) + #QC check on BHa and BHb values
  geom_function(fun=function(x) (MLE_rk*x/MLE_Sk*exp(1-(x/MLE_Sk))),colour="purple",linetype=1) +
  geom_function(fun=function(x) (1/phi0*x),colour="green",linetype=1) + #phi0
  geom_vline(xintercept = RPcalcBH$SSBmsy, colour="black") + # BH - SSBmsy
  geom_vline(xintercept = RPcalcrick$SSBmsy, colour="purple") + # rick - SSBmsy
  geom_vline(xintercept = RPcalcBH$eq_SSB_f_spr_40, colour="orange") + #Fx%SPR
  geom_hline(yintercept = median(DF$REC[DF$year <= 1990]), colour="orange") + #Fx%SPR
  geom_vline(xintercept = RPcalcrick$eq_SSB_f_spr_40, colour="red3") + # 
  geom_hline(yintercept = exp(mean(log(DF$REC[DF$year <= 1990]))), colour="red3") 


# as above but plot SRR fits over range of observed biomass
ggplot(DF) + geom_point(mapping=aes(y=REC,x=SSB,colour=year)) +
  theme_classic() + labs(x="SSB (kt)", y="Recruitment (millions)") + 
  geom_function(fun=function(x) (MLE_Rinf/ (1+MLE_S50/x)),colour="black",linetype=1) +
  #geom_function(fun=function(x) (BHa*x / (1+BHb*x)),colour="red",linetype=1) + #QC check on BHa and BHb values
  geom_function(fun=function(x) (MLE_rk*x/MLE_Sk*exp(1-(x/MLE_Sk))),colour="purple",linetype=1) +
  geom_function(fun=function(x) (1/phi0*x),colour="green",linetype=1) +
  geom_vline(xintercept = RPcalcBH$SSBmsy, colour="black") + # Bmsy for BH
  geom_vline(xintercept = RPcalcrick$SSBmsy, colour="purple") + # Bmsy for Ricker
  geom_vline(xintercept = RPcalcBH$eq_SSB_f_spr_40, colour="orange") + # BH
  geom_hline(yintercept = median(DF$REC[DF$year <= 1990]), colour="orange") + # median recruitment
  geom_vline(xintercept = RPcalcrick$eq_SSB_f_spr_40, colour="red3") + # as above but for Ricker
  geom_hline(yintercept = exp(mean(log(DF$REC[DF$year <= 1990]))), colour="red3") # geometric mean recruitment

# X%Rmax -----
# Ricker
test <- function(x){MLE_rk*x/MLE_Sk*exp(1-(x/MLE_Sk))}
tmp <- test(1:2500)
max(tmp)
df1 <- as.data.frame(cbind(1:2500, tmp))
colnames(df1) <- c("x", "R")
plot(df1$x, df1$R)
abline(h=max(tmp))
abline(v=MLE_Sk)
abline(h = 0.5*MLE_rk)
abline(v = df1$x[df1$R < 0.5*MLE_rk+30 & df1$R > 0.5*MLE_rk-30])
df1$x[df1$R < 0.5*MLE_rk+30 & df1$R > 0.5*MLE_rk-30]

abline(h = 0.4*MLE_rk, col = "red")
df1$x[df1$R < 0.4*MLE_rk+30 & df1$R > 0.4*MLE_rk-30]
abline(v = df1$x[df1$R < 0.4*MLE_rk+30 & df1$R > 0.4*MLE_rk-30], col = "red")


abline(h = 0.6*MLE_rk, col = "orange")
df1$x[df1$R < 0.6*MLE_rk+30 & df1$R > 0.6*MLE_rk-30]
abline(v = df1$x[df1$R < 0.6*MLE_rk+30 & df1$R > 0.6*MLE_rk-30], col = "red")

#abline(v=0.5*MLE_Sk) - need 1/2 MLE_rk and then find associated biomass

# BH
test1 <- function(x){MLE_Rinf/ (1+MLE_S50/x)} # MLE_Rinf = Rmax
tmp1 <- test1(1:2500)
max(tmp1)
df2 <- as.data.frame(cbind(1:2500, tmp1))
colnames(df2) <- c("x", "R")
plot(df2$x, df2$R)
abline(h=max(tmp1))
abline(v=MLE_S50)
abline(h=0.5*MLE_Rinf)
abline(h=0.4*MLE_Rinf, col = "red")
abline(h=0.6*MLE_Rinf, col = "orange")

df2$x[df2$R < 0.4*MLE_Rinf+10 & df2$R > 0.4*MLE_Rinf-10]
abline(v = df2$x[df2$R < 0.4*MLE_Rinf+10 & df2$R > 0.4*MLE_Rinf-10], col = "red")

df2$x[df2$R < 0.6*MLE_Rinf+10 & df2$R > 0.6*MLE_Rinf-10]
abline(v = df2$x[df2$R < 0.6*MLE_Rinf+10 & df2$R > 0.6*MLE_Rinf-10], col = "orange")

# SRR plot with X%Rmax
ggplot(DF) + geom_point(mapping=aes(y=REC,x=SSB,colour=year)) +
  theme_classic() + labs(x="SSB (kt)", y="Recruitment (millions)") + 
  geom_function(fun=function(x) (MLE_Rinf/ (1+MLE_S50/x)),colour="black",linetype=1) +
  geom_function(fun=function(x) (MLE_rk*x/MLE_Sk*exp(1-(x/MLE_Sk))),colour="purple",linetype=1) + 
  geom_hline(yintercept = max(tmp)) +
  geom_vline(xintercept = MLE_Sk) +
  geom_vline(xintercept = 0.5*MLE_Sk) #+


# Thresholds - Hockey stick ----
## Iceland approach
# https://www.statology.org/piecewise-regression-in-r/
#  https://www.r-bloggers.com/2012/08/r-for-ecologists-putting-together-a-piecewise-regression/
# ICES may use FLR but I can't find the segmented regression associated with this; https://flr-project.org/

library(segmented)

plot(DF$SSB, DF$REC)

#fit simple linear regression model
fit <- lm(REC ~ SSB, data=DF)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~ SSB, psi=1000)

#view summary of segmented model
summary(segmented.fit)

#plot original data
plot(DF$SSB, DF$REC, pch=16, col='steelblue')

#add segmented regression model
plot(segmented.fit, add=T)
