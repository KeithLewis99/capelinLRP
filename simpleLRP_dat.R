# The purpose of this file is to import data and do some simple exploratory analyses in the effort to create simple LRPs, specifically X%Rmax, Brecover, Bmsy - historical proxies, empirical LRPs etc.  Some EDA is done after the import but others are done after the dataframes have been joined.  More complex analyses are done in simpleLRP_calc and RPcalcs_230223.R from Tim BArrett.  These analyses are fed into simple_LRP.Rmd and simpleLRP_LRPdisplay.Rmd.  It is supported by simpleLRP_FUN

# Set up a project - see the below link fir directions.
#https://happygitwithr.com/rstudio-git-github.html

# But basically:
# 1.	Set up a Git repo on GitHub.
# 2.	Create the project in R - New Project - VErsion Control - Git
# 3. type "git add -A" in the terminal
# 4.	Create a bunch of directories automatically (see below)
# 5. Copy git -ignore file

#Create a "name_dat.R" file
#put this file in the folder with the project and create the following subfolders
if(!dir.exists("archive"))dir.create("archive")
if(!dir.exists("data"))dir.create("data")
if(!dir.exists("figs"))dir.create("figs") #for publication quality only
if(!dir.exists("output"))dir.create("output") # for tables and figures
if(!dir.exists("ms"))dir.create("ms") # manuscript
if(!dir.exists("report"))dir.create("report") #for rmd report
if(!dir.exists("refs"))dir.create("refs") #for rmd report


# Start----
# shouldn't need the above after the first dayfff
#libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(purrr)

options(dplyr.print_max = 1e9)

# Source files
source("simpleLRP_FUN.R")
save <- "no"
disaggregated <- "1985-present"

# Data ----


## read larval density data----
# larval density but with error bars
## year range: 1985-2022
df_ld  <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/data/larvae2001_2022.csv")
str(df_ld)


# add extra years to start the time series
if(disaggregated == "1985-present") {
  df_tmp <- as.data.frame(matrix(NA, 16, 3))
  df_tmp[, 1] <- c(1985:2000)
  names(df_tmp) <- names(df_ld)
  df_ld <- rbind(df_tmp, df_ld)
} else {
  df_tmp <- df_ld[1:2,]
  df_tmp[, 1:3] <- NA
  df_tmp$SurveyYear[1:2] <- c(1999,2000)
  df_tmp
  df_ld <- rbind(df_tmp, df_ld)
} 

# rename columns
df_ld <- df_ld %>% rename(year = `Year`,
                          avg_density = `Larval densities_ind_m-3`,
                          se_auc = `SE_AUC`)

# get the natural log
df_ld$lnlarvae <- log(df_ld$avg_density)


## summary stats and figures
summary(df_ld)
quantile(df_ld$avg_density, c(0.1, 0.9), na.rm = TRUE)
quant <- quantile(df_ld$avg_density, c(0.1, 0.9), na.rm = TRUE)

m1 <- mean(df_ld$avg_density, na.rm = TRUE)
median(df_ld$avg_density, na.rm = TRUE)
sd1 <- sd(df_ld$avg_density, na.rm = TRUE)

## create a density plot of larval density
p <- ggplot(data = df_ld, aes(x = avg_density))
p <- p + geom_density()
p

## create a rank column
df_ld$rank <- rank(df_ld$avg_density, na.last = "keep")
arrange(df_ld, rank)
## create a lag column so that avg_density[t-2] corresponds to year[t]
df_ld$avg_density_tm2 <- lag(df_ld$avg_density, 2)

# round the values
df_ld <- df_ld %>%
 mutate(across(avg_density:avg_density_tm2, round, 0))

## basic plot of year v density
plot(df_ld$year, df_ld$avg_density)

## basic plot of rank v. density with 10, 50, and 90th percentiles
plot(df_ld$rank, df_ld$avg_density)
quantile(df_ld$rank, c(0.1, 0.5, 0.9), na.rm = TRUE)
# not quite sure where I got these values but they are close to the above
abline(v = quantile(df_ld$rank, c(0.1), na.rm = TRUE))
abline(v = quantile(df_ld$rank, c(0.5), na.rm = TRUE))
abline(v = quantile(df_ld$rank, c(0.9), na.rm = TRUE))

## plot year v density couloured by rank
Scatter1(df = df_ld, 
         xaxis = year, yaxis = avg_density, 
         colour = rank,
         c1 = "Rank: ", c2 = "Year: ", c3 = "Density: ", 
         xlab = "Year", ylab = "Larval Density (#/m^3)", 
         filename = "figs/2-Abundance-rank-year.pdf", save = "no", 
         errorbar = "yes", 
         ymin= se_auc, ymax = se_auc)


## read in capelin data----
###read and check data
#### replacing this with summations from the age-aggregated data
#  df_cap <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/data/capelin-2021.csv", col_types = cols(
#   year = col_integer()
# ))
##### age aggregated data for the two time periods, 1985-1998 & 1999-present are imported into the IPM project, where the column names are made the same and the frames bound together.
df_agg_abun <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/IPM/data/capelin_aggregated_abundance_1985-2022.csv")
str(df_agg_abun)
df_agg_bio <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/IPM/data/capelin_aggregated_biomass_1985-2022.csv")
str(df_agg_bio)

# merge aggregation and biomass dataframes
df_cap <- merge(df_agg_abun, df_agg_bio[4:40,], by = "year")
str(df_cap)

# round values
df_cap$abundance_med <- round(df_cap$abundance_med, 0)
df_cap$biomass_med <- round(df_cap$biomass_med, 0)

# summary stats
summary(df_cap)
quantile(df_cap$abundance_med, na.rm = T)

# create a rank column
df_cap$rankA <- rank(df_cap$abundance_med)
df_cap$rankB <- rank(df_cap$biomass_med)

#arrange(df_cap, rankB)
df_cap$abundance_med_tm2 <- lag(df_cap$abundance_med, 2)

#View(df_cap)

# plot biomass and abundance
plot(df_cap$abundance_med, df_cap$biomass_med)

# basic plot of year v capelin abundance
plot(df_cap$year, df_cap$abundance_med)

# basic plot of rank v capelin abundance
plot(df_cap$rankA, df_cap$abundance_med)
abline(v = 22.5) #everything to the right is pre1991



## read in ice data----
#read and check data
df_ice <- read_csv("data/capelin-m1-2021.csv", col_types = cols(
  year = col_integer()
))
str(df_ice)

#create a rank colum
df_ice$rank <- rank(df_ice$tice)



## read in condition data----
#read and check data
df_cond <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/data/fromAaron/condition_2JK_ag1_2_MF_2022.csv", col_types = cols(
  year = col_integer()
))
str(df_cond)

df_cond$meanCond <- round(df_cond$meanCond, 2)
df_cond$rank <- rank(df_cond$meanCond)
df_cond$cond_tm1 <- lag(df_cond$meanCond)



## read in maturity data----
#read and check data - note that these are percentages
## However, there are some errors/discrepancies between this and the biochar file (BIOCHAR FROM ACOUSTICS_revised to use Monte Carlo abundnace for 1988-1996 in annual page (003).xls; C:\Users\lewiske\Documents\capelin_LRP\IPM\data)  
### Therefore, bring in values from IPM - deprecated the area that has been commented out but keep derived variables

# df_mat <- read_csv("data/springAcoustics-percentMature.csv", col_types = cols(
#   year = col_integer()
# ))
# str(df_mat)
# df_mat <- df_mat %>%
#             rename(mat1 = age1, mat2 = age2, mat3 = age3, mat4 = age4, mat5 = age5, mat6=age6)
# 
# str(df_mat)


df_mat <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/IPM/data/capelin_perMat_1985-2022.csv", col_types = cols(
  year = col_integer()
))
str(df_mat, give.attr = F)


#make variables
df_mat$rank <- rank(df_mat$mat2)
df_mat$mat2_tm1 <- lag(df_mat$mat2, 1)



## #read in disaggregated data----
### as for age-aggregated data above, bring in data from IPM project

df_dis <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/IPM/data/capelin_abundance_1985-2022.csv")
str(df_dis) 

# join age disaggregated data with larval density
df_dis <- left_join(df_dis, df_ld, by = 'year')
df_dis <- left_join(df_dis, df_mat, by = 'year')
str(df_dis)

Scatter1(df = df_dis, xaxis = year, yaxis = I5, colour = NULL, 
         c2 = "Year: ", c3 = "Abundance: ", 
         xlab = "Year", ylab = "Capelin abundance (millions) - age-5",
         filename = "figs/2-cond-rank-year.pdf", save = "no")

## read in biomass- age disaggregated
df_bio <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/IPM/data/capelin_biomass_1985-2022.csv")
str(df_bio) 


# join all dataframes with lags----
# this is for the "Indices Lagged" tab in the dashboard.  It makes it easier to see the relations because all indices are put to the survey year, i.e., abundance and biomass are at time t, larval density is t-2 and condition is t-1.  Recruitment t-2 (abundance_med_tm2) is matched to biomass in a given year (biomass_med)

ls <- list(df_cap, df_ld, df_ice, df_cond, df_mat)
df_lag <- ls %>% reduce(left_join, by ="year") %>%
  select(year, 
         abundance_med, abundance_med_tm2, biomass_med, rankB, 
         avg_density_tm2, 
         tice, 
         cond_tm1, 
         mat2, mat2_tm1)
str(df_lag)
write.csv(df_lag, "data/lag.csv")

# derived variables ----
# This is the abundance at t-2 - useful for a crude S-R relationship
plot(df_lag$abundance_med, df_lag$abundance_med_tm2)

# this is the strength of the AGe 3 cohort; lagged three years so that it can be compared to the index for a S-R relationship
## I think the idea here is that abundance_med is dominated by age-2 fish and that the immature age2[t] fish will be mature at t+1 and their offspring will recruit at t+3
df_lag$SR_age3_tm3 <- lag(df_lag$abundance_med*(1-(df_lag$mat2*0.01)), 3)

# relationship between immature at age 2 that will influence recruits in 3 years and mature at age 2 that will influence recruits in 2 years
## Not sure what this is giving us its percent age2 v abundance
df_lag$immat_tm3 <- "NA"
df_lag$immat_tm3 <- lag((100-df_lag$mat2), 3)
plot(df_lag$immat_tm3[c(7:34)], df_lag$abundance_med[c(7:34)])
summary(lm(df_lag$abundance_med ~ df_lag$immat_tm3 + df_lag$mat2_tm1))

#this is the strength of the Age 2 cohort, lagged one year - fish that will be Age 3 the next year
## Not sure what this is giving us - its mature age -2
df_lag$SRage3_tm1 <- lag(df_lag$abundance_med*(df_lag$mat2*0.01), 1)


# explore lagged relationships between %Mat@AG2 and abundance----
# Influence of abundance (t) on Abundance of age 3 (t3) 
temp1a <- df_lag %>%
  filter(year > 1992) %>%
  ggplot(aes(abundance_med, SR_age3_tm3, text = paste("Year", year))) +            geom_point()
  
ggplotly(temp1a)


  
# Influence of the larvae (t-2) on Age3 (t3): this is a bit of a stretch.  Good correlation between ld and abundance_med but this is taking it three years beyond that  
temp1c <- df_lag %>%
    filter(year > 1992) %>%
    ggplot(aes(SR_age3_tm3, avg_density_tm2,  text = paste("Year", year))) +          geom_point()
ggplotly(temp1c)

  
# no idea why this is here  
temp1d <- df_lag %>%
    filter(year > 1992) %>%
    ggplot(aes(SR_age3_tm3, lag(avg_density_tm2, 1),  text = paste("Year", year))) + geom_point()
  ggplotly(temp1d)
  
# note on the dashboard the strong decline in larval density from 2005 onwards - this is to try to relate it to maturity at age 2 and tehre seems to be some relation
  ## Not sure what this is giving us -
plot(df_lag$mat2[21:34], df_lag$avg_density_tm2[21:34])
summary(lm(df_lag$avg_density_tm2[21:34] ~ df_lag$mat2[21:34]))
plot(df_lag$year[23:34], df_lag$avg_density_tm2[23:34])
summary(lm(df_lag$avg_density_tm2[23:34] ~ df_lag$year[23:34]))


# Brecover ---- 
## see simple LRP_calc.R

# S-R Approach----
### this is used in simpleLRP_calc and RPcalcs_230223.R

# the 2 year lead [t-2] of the mature capelin ages 2/3/4 and the abundance of mature capelin [t]. Note that the code is moving the mature age 2 back in time so that they correspond to the abundance at time t - probably easier to see this in JAGS
plot(lag(df_dis$I2, 2)*lag(df_dis$mat2*0.01, 2), # mature age-2 two years in the past
df_dis$I2*(1-df_dis$mat2*0.01)+df_dis$I3+df_dis$I4) # immature age-2 + age3+4


tmp <- lag(df_lag$biomass_med, 2) # my reasoning in lagging this is that leading throws away two values

# make a smaller dataframe of the relevant variables
sr <- as.data.frame(cbind(year = df_dis$year, 
                          age2 = df_dis$I2, 
                          age2PerMat = df_dis$mat2, 
                          biomass_tm2 = tmp))
str(sr)

# sr <- as.data.frame(cbind(year = df_dis$year, age2 = df_dis$age2, age2PerMat = df_dis$age2PerMat, biomass = df_lag$biomass_med[1:33]))

sr$R <- sr$age2 # this creates redundant columns but solves a short term problem
#sr$R <- sr$age2*1000 # the 1000 is to get this to billions so that resulting units are kt, the 0.01 is to get PerMat to a percentage
# sr$R <- sr$age2*1000*sr$age2PerMat*0.01
str(sr)

#write.csv(sr, "Barrett/data_for_Tim.csv")


#################################################################
# clean and commented to this point
#################################################################

##### Not lagging biomass bc its already lagged (or lead)
# exploratory - as per Hilborn and Walters on pg ~ 269, plot biomass v R
plot(sr$biomass_tm2, sr$R)

# from 269 - biomass v logaritm of S/R - spawners v recruits
plot(sr$biomass_tm2, log(sr$R))

# log biomass v log S/R (this may not be right)
plot(log(sr$biomass_tm2), log(sr$R))

# trying for Barents Sea plot

p <- ggplot(data = sr, aes(x = biomass_tm2, y = R, label = year))
p <- p + geom_point()
p <- p + geom_text(size = 3, hjust = 0, vjust = 0, nudge_x = 0.1, nudge_y = 0.1)
p


## "SSB" ----
### biomass with just the mature: calculated in IPM_dat.R, write to csv, and then plots in simpleLRP_calc
df_ssb <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/IPM/data/ssb_all.csv")
str(df_ssb)
plot(df_ssb$ssb, lead(df_ssb$abundance, 2))
plot(df_ssb$ssb, df_ssb$abundance_tp2)
plot(df_ssb$ssb[6:37], lead(df_ssb$abundance[6:37], 2))
plot(df_ssb$ssb[6:37], df_ssb$abundance_tp2[6:37])


## SRR without collapse years ----
sr_noCollapse <- sr[c(1:6, 9:35),]
plot(sr_noCollapse$biomass_tm2, sr_noCollapse$R)


## SRR with lead v lag ----
# this produces exactly the same plot as with lag because the NAs move too.  
tmp <- lag(df_lag$biomass_med, 2)
R_tp2 <- lead(sr$R[1:35] , 2) # my reasoning in lagging this is that leading throws away two values

# make a smaller dataframe of the relevant variables
sr_lead <- as.data.frame(cbind(year = df_dis$year[1:35], 
                          age2 = df_dis$I2[1:35], 
                          age2PerMat = df_dis$mat2[1:35], 
                          biomass_tm2 = tmp[1:35],
                          biomass_t = df_lag$biomass_med[1:35],
                          R_tp2 = R_tp2))
str(sr_lead)

plot(sr_lead$biomass_t, sr_lead$R_tp2) # this produces exactly the same plot as with lag because the NAs move too.  


# Z & M Barents Sea (BS) style----
## See IPM project
