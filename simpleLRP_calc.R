# this file is meant to calculate LRPs for the simple approaches as outlined in the NAP SAR.


library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(purrr)

#clear environment
rm(list=ls())


# Source files
source("simpleLRP_FUN.R")
source("simpleLRP_dat.R")


# Bmsy proxies Fx%SPR ----

# X% Rmax ----
## Some other common approaches to defining LRPs are based on the biomass at a predefined percentage (X%) of Rmax, the maximum predicted recruitment from a SRR, or other thresholds to impaired recruitment 
## BH
## Ricker
## Hockey stick


# Brecover from models ----
## Bloss is the lowest observed biomass 
## Brecover is the lowest observed biomass which produced recruitment that lead to stock recovery 
## Bmin is the lowest observed biomass from which a recovery to average has been observed
## Other minimum biomass that produced “good” recruitment 


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
