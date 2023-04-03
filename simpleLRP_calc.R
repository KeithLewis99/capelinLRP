
# The purpose of this file is to calculate various Limit Reference Points for 2J3KL capelin as outlined in DFO 2023 (the SAR from the June 2022 developing guidance for LRPs, see ResDoc for ref). The order of analyses follows the same order as the tables in DFO 2023 specifically X%Rmax, Brecover, Bmsy - historical proxies, empirical LRPs etc (see RPcalcs_230223.R - Tim BArrett).  These analyses are fed into and simpleLRP_LRPdisplay.Rmd.  This file and others are supported by simpleLRP_FUN and simpleLRP_dat.R.

## Note that I have not been consistent in removing code from a truncated time series, e.g., the 1991-present.  But I have indicated where this code is not appropirate by indicating its DEPRECATED

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
#rm(list=ls())


# Source files ----
source("simpleLRP_FUN.R")
source("simpleLRP_dat.R")


# Bmsy: proportion-----
## At this point, I have no viable approach for calculating Bmsy or B0 with a model.



# Bmsy proxies Fx%SPR ----
## In spreadsheet C:\Users\lewiske\Documents\Pelagics\training\LRP_Dec2021\Capelin.xlsx and X.R and others.  Have gone over this with Tim Barrett.

# X% Rmax ----
## Some other common approaches to defining LRPs are based on the biomass at a predefined percentage (X%) of Rmax, the maximum predicted recruitment from a SRR, or other thresholds to impaired recruitment.  See RPcalcs_2302323.R and SRR_tracker.xlsx (C:\Users\lewiske\Documents\capelin_LRP\analyses\capelinLRP\Barrett)


## Thresholds - Hockey stick ----

## Iceland approach
# https://www.statology.org/piecewise-regression-in-r/
#  https://www.r-bloggers.com/2012/08/r-for-ecologists-putting-together-a-piecewise-regression/
# ICES may use FLR but I can't find the segmented regression associated with this; https://flr-project.org/


# DEPRECATED - see RPcalcs_230223.R for this code
# library(segmented)
# 
# # plot SRR
# plot(sr$biomass_tm2, sr$R)
# 
# #fit simple linear regression model
# fit <- lm(R ~ biomass_tm2, data=sr)
# 
# #fit piecewise regression model to original model, estimating a breakpoint at x=9
# segmented.fit <- segmented(fit, seg.Z = ~ biomass_tm2, psi=1000)
# 
# #view summary of segmented model
# summary(segmented.fit)
# 
# #plot original data
# plot(sr$biomass_tm2, sr$R, pch=16, col='steelblue')
# 
# #add segmented regression model
# plot(segmented.fit, add=T)

 
## DEPRECATED - NOT USED BECAUSE WE USE FULL TIME SERIES
# # post - collapse 
# plot(sr[9:37,]$biomass_tm2, sr[9:37,]$R)
# fit_post <- lm(R ~ biomass_tm2, data=sr[9:37,])
# 
# #fit piecewise regression model to original model, estimating a breakpoint at x=9
# segmented.fit_post <- segmented(fit_post, seg.Z = ~ biomass_tm2, psi=350)
# 
# #view summary of segmented model
# summary(segmented.fit_post)
# 
# #plot original data
# plot(sr[9:37,]$biomass_tm2, sr[9:37,]$R, pch=16, col='steelblue')
# 
# #add segmented regression model
# plot(segmented.fit_post, add=T)


# Historical LRP ----

## Bloss ----

## Bloss is the lowest observed abundance/biomass 
### Abundance: this is the time period from 1999:2018
Bloss_abund_all <- min(df_cap$abundance_med, na.rm = T)
# this is the time period from 1999:2018
Bloss_abund_post <- min(df_cap$abundance_med[7:35], na.rm = T) 
# this time period is 2011-2018 meant to exclude 2010 
Bloss_abund_recent <- min(df_cap$abundance_med[27:35], na.rm = T) 

# as above but biomass
Bloss_bio_all <- min(df_cap$biomass_med, na.rm = T)
Bloss_bio_post <- min(df_cap$biomass_med[7:35], na.rm = T)
Bloss_bio_recent <- min(df_cap$biomass_med[27:35], na.rm = T)

# as above but biomass w/o 2010
Bloss_abund_all_n2010 <- min(df_cap$abundance_med[c(1:25, 27:35)], na.rm = T)
Bloss_bio_all_n2010 <- min(df_cap$biomass_med[c(1:25, 27:35)], na.rm = T)
Bloss_bio_post_n2010 <- min(df_cap$biomass_med[c(7:25, 2735)], na.rm = T)


# create a dataframe to hold the min and max values
Bmin <- as.data.frame(matrix(NA, 3, 5))
Bmin <- rename(Bmin, indicator = V1, "LRP" = V2, "1985-2019" = V3, "1991-2019" = V4, "2011-2019" = V5)
Bmin$indicator <- c("abund", "biomass", "biomass_no2010")
Bmin$LRP <- rep(c("Bloss"), 3)

# put values from above into Bmin object
Bmin$`1985-2019`[1] <- Bloss_abund_all
Bmin$`1991-2019`[1] <- Bloss_abund_post
Bmin$`2011-2019`[1] <- Bloss_abund_recent

Bmin$`1985-2019`[2] <- Bloss_bio_all
Bmin$`1991-2019`[2] <- Bloss_bio_post
Bmin$`2011-2019`[2] <- Bloss_bio_recent

Bmin$`1985-2019`[3] <- Bloss_bio_all_n2010
Bmin$`1991-2019`[3] <- Bloss_bio_post_n2010
Bmin$`2011-2019`[3] <- Bloss_bio_recent

# table summarizing Bmin values 
Bmin

## Brecover ----
### Brecover is the lowest observed biomass which produced recruitment that lead to stock recovery 

#### FROM MODELS: These will only be viable IF we get the IPM up and running
#### see dashboard for the empirical version - this is just a simple graph and requires the 1982 data


## Bmin ----
### Bmin is the lowest observed biomass from which a recovery to average has been observed or the lowest observed biomass from which a recovery to average has been observed or other minimum biomass that produced “good” recruitment.  We use the latter definition.

###ICES Type I
### Haddock type approach - all data

# calculate anomalies - get mean and SD
str(sr, give.attr = FALSE)
sr <- anomaly(sr[3:38,], "R")

# get the quantile and extract the 90th for the hline
h90 <- quantile(sr$anomaly, c(0.9), na.rm = T)
h80 <- quantile(sr$anomaly, c(0.8), na.rm = T)
h50 <- quantile(sr$anomaly, c(0.5), na.rm = T)
# confirm above value and check on plot
quantile(sr$anomaly, c(0.1, 0.5, 0.9), na.rm = T)


# simple test plot
plot(sr$year, sr$anomaly)
abline(h = h90)
abline(h = h80)
abline(h = h50)


#get the value for the lowest index to generate large recruitment (or a large index) for percentiles from 0.1 - 0.9
R_quant_all <- quantile(sr$R, c(0.1, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm = T)
#y1 <- subset(sr[3:38, ], R >= R_quant_all[3], na.rm = T)
y1 <- subset(sr, R >= R_quant_all[6], na.rm = T)
v90 <- min(y1$biomass_tm2, na.rm = T)
#h90 <- y1[which.min(y1[,4]), 6]

y80 <- subset(sr, R >= R_quant_all[5], na.rm = T)
v80 <- min(y80$biomass_tm2, na.rm = T)


#y70 <- subset(sr, R >= R_quant_all[4], na.rm = T)
#v70 <- min(y70$biomass_tm2, na.rm = T)

# y2 <- subset(df_cap, biomass_med_lead >= biomass90_1[2], na.rm = T)
# v50 <- min(y2$biomass_med, na.rm = T)

y2 <- subset(sr, R >= R_quant_all[2], na.rm = T)
v50 <- min(y2$biomass_tm2, na.rm = T)
#h50 <- y2[which.min(y2[,4]), 6]


# simple test plot
plot(sr$biomass_tm2, sr$R)
abline(v = v90)
abline(v = v80)
abline(v = v50, lty=2)


## ICES Type I - post collapse----
## DEPRECATED - NOT USED BECAUSE WE USE FULL TIME SERIES
### Haddock type approach
### Brecover is the lowest observed biomass which produced recruitment that lead to stock recovery 

#calculate anomalies - get mean and SD
sr_post <- anomaly(sr[5:37,], "R")


# get the quantile and extract the 90th for the hline
h90_post <- quantile(sr_post$anomaly, c(0.1, 0.9), na.rm = T)[2]
h50_post <- quantile(sr_post$anomaly, c(0.1, 0.5, 0.9), na.rm = T)[2]
h50_post_alt <- quantile(sr_post$anomaly[-20], c(0.1, 0.5, 0.9), na.rm = T)[2]
# confirm above value and check on plot
quantile(sr_post$anomaly, c(0.1, 0.5, 0.9), na.rm = T)

plot(sr_post$year, sr_post$anomaly)
abline(h = h90_post)
abline(h = h50_post)
abline(h = h50_post_alt)


#get the value for the lowest index to generate large recruitment (or a large index)
R_quant_post <- quantile(sr_post$R, c(0.1, 0.5, 0.9), na.rm = T)
y1_post <- subset(sr_post, R >= R_quant_post[3], na.rm = T)
y2_post <- subset(sr_post, R >= R_quant_post[2], na.rm = T)
y3_post <- subset(sr_post[-22,], R >= R_quant_post[2], na.rm = T) # removes 2010
v90_post <- min(y1_post$biomass_tm2, na.rm = T)
v50_post <- min(y2_post$biomass_tm2, na.rm = T)
v50_post_alt <- min(y3_post$biomass_tm2, na.rm = T)
#h50_post <- y2_post[which.min(y2_post[,4]), 6]
#h50_post_alt <- y3_post[which.min(y3_post[,4]), 6]


# simple test plot
plot(sr_post$year, sr_post$biomass_tm2)

# simple test plot
plot(sr_post$biomass_tm2, sr_post$R)
abline(v = v90_post)
abline(v = v50_post)
abline(v = v50_post_alt)



# Hist proxies----
##A historical proxy for BMSY can be estimated as the mean or median value of an indicator over a historical time period when the indicator is high (and assumed recruitment is stable) and catches are high; or the mean or median value of an indicator over a productive period. 

##A historical proxy for B0 can be estimated as the mean/median indicator over a historical time period reflecting the beginning of exploitation, or the maximum value of the indicator if the stock has a history of exploitation. See Res Doc for details on why we chose B0 over Bmsy.

# this reflects the highest time period on record and could be a historical Bo although this doesn't reflect the begnining of exploitation it is the max value of the indicator.
str(df_cap)
df_cap$year[1:6]
ma1 <- mean(df_cap$abundance_med[1:6])
mb1 <- mean(df_cap$biomass_med[1:6])
sd(df_cap$biomass_med[1:6]) # used in Res Doc but not dashboard
mda1 <- median(df_cap$abundance_med[1:6])
mdb1 <- median(df_cap$biomass_med[1:6])
gmb1 <- exp(mean(log(df_cap$biomass_med[1:6])))

# DEPRECATED
# this is the time period from 1991:2018.  It "captures" the post collapse period without the data gaps of the 1990s and has the high point of 2013-2015
# probelm that there is no productive period since 1991 except for 2013-2015
df_cap$year[7:37] 
ma2 <- mean(df_cap$abundance_med[7:37], na.rm = T)
mb2 <- mean(df_cap$biomass_med[7:37], na.rm = T)
mda2 <- median(df_cap$abundance_med[7:37], na.rm = T)
mdb2 <- median(df_cap$biomass_med[7:37], na.rm = T)
gmb2 <- exp(mean(log(df_cap$biomass_med[7:37]), na.rm = T))

# DEPRECATED
# this time period is 2011-2018.  It captures the high period of 2013-2015 without the low of 2010.
df_cap$year[27:34] 
ma3 <- mean(df_cap$abundance_med[27:34], na.rm = T)
mb3 <- mean(df_cap$biomass_med[27:34], na.rm = T)
mda3 <- median(df_cap$abundance_med[27:34], na.rm = T)
mdb3 <- median(df_cap$biomass_med[27:34], na.rm = T)
gmb3 <- exp(mean(log(df_cap$biomass_med[27:34]), na.rm = T))

# DEPRECATED
# this time period is 2013-2015.  It captures the high period of 2013-2015, i.e., the indicator is high.
df_cap$year[29:31] 
ma4 <- mean(df_cap$abundance_med[29:31], na.rm = T)
mb4 <- mean(df_cap$biomass_med[29:31], na.rm = T)
mda4 <- median(df_cap$abundance_med[29:31], na.rm = T)
mdb4 <- median(df_cap$biomass_med[29:31], na.rm = T)
gmb4 <- exp(mean(log(df_cap$biomass_med[29:31])))

## This is # DEPRECATED but keeping just in case
# create a table to hold the above values
# histLRP <- as.data.frame(matrix(NA, 5, 6))
# histLRP <- rename(histLRP, indicator = V1, mct = V2, "<=1991" = V3, "1991-2022" = V4, "2011-2018" = V5, "2013-2015" = V6)
# histLRP$indicator <- c(sort(rep(c("abund", "biomass"), 2)), "biomass")
# histLRP$mct <- c(rep(c("mean", "median"), 2), "geometric mean")
# 
# # put values from above into histLRP object
# histLRP$`<=1991`[1] <- ma1
# histLRP$`<=1991`[2] <- mda1
# histLRP$`<=1991`[3] <- mb1
# histLRP$`<=1991`[4] <- mdb1
# histLRP$`<=1991`[5] <- gmb1
# 
# histLRP$`1991-2022`[1] <- ma2
# histLRP$`1991-2022`[2] <- mda2
# histLRP$`1991-2022`[3] <- mb2
# histLRP$`1991-2022`[4] <- mdb2
# histLRP$`1991-2022`[5] <- gmb2
# 
# histLRP$`2011-2018`[1] <- ma3
# histLRP$`2011-2018`[2] <- mda3
# histLRP$`2011-2018`[3] <- mb3
# histLRP$`2011-2018`[4] <- mdb3
# histLRP$`2011-2018`[5] <- gmb3
# 
# histLRP$`2013-2015`[1] <- ma4
# histLRP$`2013-2015`[2] <- mda4
# histLRP$`2013-2015`[3] <- mb4
# histLRP$`2013-2015`[4] <- mdb4
# histLRP$`2013-2015`[5] <- gmb4
# 
# # use standard multipliers for the Bmsy and B0
# multBmsy <- 0.4
# multB0 <- 0.2
# histBmsy <- histLRP
# histB0 <- histLRP
# 
# # multiply values in histLRP by multBmsy
# cols <- sapply(histBmsy, is.numeric)
# histBmsy[, cols] <- histBmsy[, cols]*multBmsy
# histBmsy1 <- histBmsy %>% mutate_if(is.numeric, round) 
# 
# # multiply values in histLRP by multB0
# cols <- sapply(histB0, is.numeric)
# histB0[, cols] <- histB0[, cols]*multB0
# histB01 <- histB0 %>% mutate_if(is.numeric, round)


# reformat to just B0 and Bmsy for the full time frame and have the full values as well
histLRP1 <- as.data.frame(matrix(NA, 5, 5))
histLRP1 <- rename(histLRP1, indicator = V1, mct = V2, "<=1991" = V3, "Bmsy<=1991" = V4, "B0<=1991" = V5)
histLRP1$indicator <- c(sort(rep(c("abund", "biomass"), 2)), "biomass")
histLRP1$mct <- c(rep(c("mean", "median"), 2), "geometric mean")

# put values from above into histLRP1 object
histLRP1$`<=1991`[1] <- ma1
histLRP1$`<=1991`[2] <- mda1
histLRP1$`<=1991`[3] <- mb1
histLRP1$`<=1991`[4] <- mdb1
histLRP1$`<=1991`[5] <- gmb1

histLRP1$`Bmsy<=1991`[1] <- ma1
histLRP1$`Bmsy<=1991`[2] <- mda1
histLRP1$`Bmsy<=1991`[3] <- mb1
histLRP1$`Bmsy<=1991`[4] <- mdb1
histLRP1$`Bmsy<=1991`[5] <- gmb1

histLRP1$`B0<=1991`[1] <- ma1
histLRP1$`B0<=1991`[2] <- mda1
histLRP1$`B0<=1991`[3] <- mb1
histLRP1$`B0<=1991`[4] <- mdb1
histLRP1$`B0<=1991`[5] <- gmb1

# use standard multipliers for the Bmsy and B0
multBmsy <- 0.4
multB0 <- 0.2

# multiply values in histLRP by multB0 and multBmsy
histLRP1[, 4] <- histLRP1[, 4]*multBmsy
histLRP1[, 5] <- histLRP1[, 5]*multB0
histLRP1 <- histLRP1 %>% mutate_if(is.numeric, round) 


### Deprecated
## B0: proportion-----
## Need to check with Tim if this approach is a) appropriate and b) belongs in this section.
# B0
# B0_abund_all <- max(df_cap$abundance_med, na.rm = T) # all
# B0_abund_post <- max(df_cap$abundance_med[7:35], na.rm = T) # post collapse
# B0_abund_recent <- max(df_cap$abundance_med[27:34], na.rm = T) # a productive period post collapse
# 
# B0_bio_all <- max(df_cap$biomass_med, na.rm = T)
# B0_bio_post <- max(df_cap$biomass_med[7:35], na.rm = T)
# B0_bio_recent <- max(df_cap$biomass_med[27:34], na.rm = T)
# 
# # create a dataframe to hold the min and max values
# B0 <- as.data.frame(matrix(NA, 2, 5))
# B0 <- rename(B0, indicator = V1, "LRP" = V2, "1985-2019" = V3, "1991-2019" = V4, "2011-2019" = V5)
# B0$indicator <- c("abund", "biomass")
# B0$LRP <- rep(c("B0"), 2)
# 
# # put values from above
# B0$`1985-2019`[1] <- multB0*B0_abund_all
# B0$`1991-2019`[1] <- multB0*B0_abund_post
# B0$`2011-2019`[1] <- multB0*B0_abund_recent
# 
# 
# B0$`1985-2019`[2] <- multB0*B0_bio_all
# B0$`1991-2019`[2] <- multB0*B0_bio_post
# B0$`2011-2019`[2] <- multB0*B0_bio_recent

