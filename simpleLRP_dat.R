# The purpose of this file is to import data and do some simple exploratory analyses in the effort to create simple LRPs, specifically X%Rmax, Brecover, Bmsy - historical proxies, empirical LRPs etc.

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
# shouldn't need the above after the first day
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

# Data ----
## read larval density data----
#read and check data
# ld  <- read_csv("data/larvae.csv", col_types = cols(
#   year = col_integer(),
#   avg_density = col_double()
# ))
# str(ld)


# larval density but with error bars
## year range: 1985-2022
df_ld  <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/data/larvae2001_2022.csv")
str(df_ld)

disaggregated <- "1985-present"

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

# change column names
# df_ld <- df_ld %>% rename(year = SurveyYear,
#                           larvae = `Bellevue_larvae_m-3`,
#                           log_larvae = `log_Bellevue_larvae_m-3`) 
# df_ld$lnlarvae <- log(df_ld$larvae)

df_ld <- df_ld %>% rename(year = `Year`,
                          avg_density = `Larval densities_ind_m-3`,
                          se_auc = `SE_AUC`)

df_ld$lnlarvae <- log(df_ld$avg_density)


## summary stats and figures
summary(df_ld)
quantile(df_ld$avg_density, c(0.1, 0.9), na.rm = TRUE)
quant <- quantile(df_ld$avg_density, c(0.1, 0.9), na.rm = TRUE)

m1 <- mean(df_ld$avg_density, na.rm = TRUE)
median(df_ld$avg_density, na.rm = TRUE)
sd1 <- sd(df_ld$avg_density, na.rm = TRUE)
3*sd(df_ld$avg_density, na.rm = TRUE)
# 68-95-99.7

m1-3*sd1

## Brecover
p <- ggplot(data = df_ld, aes(x = avg_density))
p <- p + geom_density()
p

## create a rank column
df_ld$rank <- rank(df_ld$avg_density, na.last = "keep")
arrange(df_ld, rank)
## create a lag column so that avg_density[t-2] corresponds to year[t]
df_ld$avg_density_tm2 <- lag(df_ld$avg_density, 2)

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
#read and check data
#### replacing this with summations from the age-disaggregated data
#  df_cap <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/data/capelin-2021.csv", col_types = cols(
#   year = col_integer()
# ))
df_agg_abun <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/IPM/data/capelin_aggregated_abundance_1985-2022.csv")
str(df_agg_abun)
df_agg_bio <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/IPM/data/capelin_aggregated_biomass_1985-2022.csv")
str(df_agg_bio)

df_cap <- merge(df_agg_abun, df_agg_bio, by = "year")
str(df_cap)

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
# deleted the "Unknown" from row 105, col "age"
### This is only 2014-2019
# ageD <- read_csv("data/spring-acoustic-age-disaggregated.csv", col_types = cols(
#   year = col_integer(),
#   age = col_integer()
# ))
# str(ageD)

# year	=	Year
# stratum	=	 My stratum
# age	=	 Age
# n	=	 N (millions)
# proportion	=	 Proportion by age in that strata
# n_mat	=	 N mature (millions)
# prop_mat	=	 Proportion mature
# weight	=	 Weight (tonnes)
# mean_length	=	 Mean length (mm)
# mean_weight	=	 Mean weight (g)

#manipulate 

# get biomass and abundance by strata and year
## n_mat is a percentage
# temp1 <- ageD %>%
#   group_by(year, age) %>%
#   #select(n, weight, proportion) %>%
#   mutate(abun=sum(n), biomass=sum(weight*0.000001))

# # exploratory plot to look at prop mature at age by stratum and year
# p <- ggplot(ageD, aes(x = factor(year), y = prop_mat, colour = factor(age), text = paste(year, "Year")))
# p <- p + geom_point(position = "jitter")
# p
# ggplotly(p, tooltip = "text")
# 
# 
# # get total biomass and abundance by year
# temp2 <- temp1 %>%
#   group_by(year) %>%
#   summarize (abun = sum(abun), biomass = sum(biomass))
# 
# ageD %>% select(year, stratum, age, prop_mat) %>% filter(age ==1 & prop_mat > 0.1)
# 
# 
# 
# # prop mature
# ## n_mat is a percentage
# temp4 <- temp1 %>%
#   group_by(year)
# 
# # experiment with "spread" to produce a table of prop_mat
# temp3 <- temp1 %>%
#   select(year, stratum, age, prop_mat) %>%
#   pivot_wider(id_cols = c(year, stratum), names_from = age, values_from = prop_mat)



#make variables
#ageD$rank <- rank(ageD$age2)
#df_mat$mat2_lag1 <- lag(df_mat$age2, 1)

# bring in age disaggregated data
# df_dis_all <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/data/capelin_age_disaggregate_abundance.csv")
# str(df_dis_all)

#df_dis <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/IPM/capelin_abundance_1985-2021.csv")
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
# this is for the "Indices Lagged" tab in the dashboard.  It makes it easier to see the relations because all indices are put to the survey year, i.e., abundance and biomass are at time t, larval density is t-2 and condition is t-1.

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
# Create indicator (index) of abundance next year based on % of immature Age 2 (1-age 2) that are will mature the next year. This is probably not quite right as the AGe 1's are still in but it assumes that the Age 3+ add very little.
##### Note in Aug 2022  - the below was fine for when I first made this in early 2021.  But not needed given the age disaggregated data.
# df_lag$abundAge3_t <- "NA"
# 
# df_lag$abundAge3_t <- ((1-(df_lag$mat2_tm1*0.01))*lag(df_lag$abundance_med, 1))
# plot(df_lag$mat2_tm1, df_lag$abundance_med)
#m1 <- lm(df_lag$abundance_med ~ df_lag$mat2_tm1)
#left_join(tibble::rownames_to_column(df_lag), as.data.frame(m1$fitted.values), by = c("rowname" = "Symbol"))
#lines(df_lag$mat2_tm1, m1$fitted.values, col = 'red', na.rm = T)
# plot(df_lag$abundAge3_t, df_lag$abundance_med)
# 
# abund_age3_YEAR <- Scatter1(df = df_lag, xaxis = year, yaxis = abundAge3_t, colour = NULL, 
#                       c1 = "Rank: ", c2 = "Year: ", c3 = "Abund_Age_3: ", xlab = "Year", ylab = "Abundance Age 3",
#                       filename = "figs/2-abund3-year.pdf", save = save)


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

# reverse of the above graph - not sure if this has any value
#temp1b <- df_lag %>%
#    filter(year > 1992) %>%
 #   ggplot(aes(SR_age3_t3, abundance_med,  text = paste#("Year", year))) + geom_point()
  
 # ggplotly(temp1b)
  
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
## NOTE THAT THE COMMENTED OUT CODE IS DEPRECATED.  THE dp_cap and cap_postCollapse have been replaced by sr and sr_post
# ## the plots in this section are just exploratory as this is age disaggregated but the filtered data sets are used in simpleLRP_calc
# #all data
# df_cap$biomass_med_tm2 <- lag(df_cap$biomass_med, 2) # tm2 = t+2
# plot(df_cap$biomass_med_tm2, df_cap$biomass_med)
# 
# #Based on the above, it seems to make sense to divide this across the regmime change
# #pre collapse
# cap_preCollapse <- filter(df_cap, year < 1991)
# plot(cap_preCollapse$biomass_med_tm2, cap_preCollapse$biomass_med)
# 
# #post collapse with correlation between abundance and biomasss
# cap_postCollapse <- filter(df_cap, year >= 1991)
# plot(cap_postCollapse$biomass_med, cap_postCollapse$abundance_med)
# cor(cap_postCollapse$biomass_med, cap_postCollapse$abundance_med, use = "complete.obs", method = c("pearson"))
# 
# 
# # S-R relationship post collapse - biomasss
# plot(cap_postCollapse$biomass_med_tm2, cap_postCollapse$biomass_med)
# quantile(cap_postCollapse$biomass_med, c(0.1, 0.9), na.rm = T)
# quantile(cap_postCollapse$biomass_med_tm2, c(0.1, 0.9), na.rm = T)
# #assuiming that i've done this right, not much here - 

# NOTE THAT THE S-R RELATIONSHIPS ARE IN THE BELOW AND IN THE DASHBOARD


# S-R Approach----
### this is for the 

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



### Abundance by year and age.  Just plotting this to get a sense of the abundance by year but this is only from 2014-2019 - still, most of the immatures will be age 2 and the age 3/4/5 have only a fraction that are immature.

# ageYear <- ageD %>%
#   group_by(year, age) %>%
#   summarize(abund = sum(n)) %>%
#   ggplot(aes(x = year, y = abund, colour = as.factor(age))) + 
#   geom_point()
# ageYear
# 
# # the above plot by strata
# p <- ggplot(data = ageD, aes(x = year, y = n, colour = as.factor(age)))
# p <- p + geom_point()
# p
# 

# Time to do this right - need ####OK NOTE SURE WHAT i DID BELOW BUT ITS GARBAGE - BEST TO DELETE AND START OVER
# just rename df_dis so that the join goes more smoothly
# not sure if this makes sense at all given the Recovery issue.
## these are all percentages
# df_dis1 <- rename(df_dis, mat1 = age1, mat2 = age2, mat3 = age3, mat4 = age4, mat5 = age5)
# 
# tmp <- left_join(df_dis, df_dis1, by = "year")
# str(tmp)
# tmp <- tmp %>% mutate(R = age2*mat2)
# str(tmp)
# 
# # convert maturity to a proportion
# cbind(((100-tmp$mat2)/100)*tmp$age2, tmp$age3)
# 
# plot((100-mat2)/100*age2 ~ age3, data = tmp)
# abline(a=0, b=1)
# 

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


#  Aaron's file

# multivariate approach----  
# start with the relationship between LD (t-2) and capelin abundance

temp3 <- df_lag %>%
  filter(year > 1990) %>%
  ggplot(aes(avg_density_tm2, biomass_med, text = paste("Year", year))) + geom_point()

ggplotly(temp3)


# Then, based on the idea that only LD values > 2000 have led to good recruitment, lets see if any thresholds in tice: filter the data accordingly
temp4 <- df_lag %>%
  filter(year > 1990 & avg_density_tm2 > 2000) %>%
  ggplot(aes(tice, abundance_med, text = paste(
    "Year", year, "\n", "Density", avg_density_tm2, "\n",  
    sep = ""))) + geom_point()

ggplotly(temp4)

# BAsed on the logic that late tice > 80 days, and only <80 days has produced the "large" numbers
temp5 <- df_lag %>%
  filter(year > 1990 & avg_density_tm2 > 2000 & tice <85) %>%
  ggplot(aes(cond_tm1, abundance_med, text = paste(
    "Year", year, "\n", "Cond", cond_tm1, "\n",  
    sep = ""))) + geom_point()

ggplotly(temp5)

#Suggests that condition > 0.99 might lead to large abundance.

# the problem with this approach is the small sample size and uncertainty, i.e., driven by 2013-2015 and that some low abundance year had good condition and "good" tice.

# Z & M Barents Sea (BS) style----
## See IPM project

