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

# Source files
source("simpleLRP_FUN.R")
save <- "no"

# Data ----
## read larval density data----
#read and check data
ld  <- read_csv("data/larvae.csv", col_types = cols(
  year = col_integer(),
  avg_density = col_double()
))
str(ld)

#summary stats
summary(ld)
quantile(ld$avg_density, c(0.1, 0.9))
quant <- quantile(ld$avg_density, c(0.1, 0.9))

m1 <- mean(ld$avg_density)
median(ld$avg_density)
sd1 <- sd(ld$avg_density)
3*sd(ld$avg_density)
# 68-95-99.7

m1-3*sd1

#Brecover
p <- ggplot(data = ld, aes(x = avg_density))
p <- p + geom_density()
p

#create a rank column
ld$rank <- rank(ld$avg_density)
arrange(ld, rank)
ld$avg_densityt_2 <- lag(ld$avg_density, 2)

ld[10:15,]
mean(ld$avg_densityt_2[10:12])

#basic plot of year v density
plot(ld$year, ld$avg_density)

#basic plot of rank v. density with 10, 50, and 90th percentiles
plot(ld$rank, ld$avg_density)
abline(v=2.8)
abline(v = 18.5)
abline(v = 9.8)

# larval density but with error bars
df_ld  <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/data/larvae2001_2022.csv")
str(df_ld)

disaggregated == "1985-present"

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
                          larvae = `Larval densities_ind_m-3`,
                          se_auc = `SE_AUC`) 
df_ld$lnlarvae <- log(df_ld$larvae)

## read in capelin data----
#read and check data
cap <- read_csv("data/capelin-2019.csv", col_types = cols(
  year = col_integer()
))
str(cap)

#summary stats
summary(cap)
quantile(cap$abundance_med, na.rm = T)

#create a rank colum
cap$rankA <- rank(cap$abundance_med)
cap$rankB <- rank(cap$biomass_med)
arrange(cap, rankB)
cap$abundance_med_lag2 <- lag(cap$abundance_med, 2)
#View(cap)

#plot biomass and abundance
plot(cap$abundance_med, cap$biomass_med)

#basic plot of year v capelin abundance
plot(cap$year, cap$abundance_med)

#basic plot of rank v capelin abundance
plot(cap$rankA, cap$abundance_med)
abline(v = 22.5) #everything to the right is pre1991



## read in ice data----
#read and check data
ice <- read_csv("data/capelin-m1-2020.csv", col_types = cols(
  year = col_integer()
))
str(ice)

#create a rank colum
ice$rank <- rank(ice$tice)




## read in condition data----
#read and check data
cond <- read_csv("data/condition_ag1_2_MF_out.csv", col_types = cols(
  year = col_integer()
))
str(cond)

cond$rank <- rank(cond$meanCond)
cond$condt_1 <- lag(cond$meanCond)



## read in maturity data----
#read and check data
matA <- read_csv("data/springAcoustics-percentMature.csv", col_types = cols(
  year = col_integer()
))
str(matA)

#make variables
matA$rank <- rank(matA$age2)
matA$mat2t_1 <- lag(matA$age2, 1)




## #read in age disaggregated data----
# deleted the "Unknown" from row 105, col "age"
ageD <- read_csv("data/spring-acoustic-age-disaggregated.csv", col_types = cols(
  year = col_integer(),
  age = col_integer()
))
str(ageD)

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
temp1 <- ageD %>%
  group_by(year, age) %>%
  #select(n, weight, proportion) %>%
  mutate(abun=sum(n), biomass=sum(weight*0.000001))

# exploratory plot to look at prop mature at age by stratum and year
p <- ggplot(ageD, aes(x = factor(year), y = prop_mat, colour = factor(age), text = paste(year, "Year")))
p <- p + geom_point(position = "jitter")
p
ggplotly(p, tooltip = "text")


# get total biomass and abundance by year
temp2 <- temp1 %>%
  group_by(year) %>%
  summarize (abun = sum(abun), biomass = sum(biomass))

ageD %>% select(year, stratum, age, prop_mat) %>% filter(age ==1 & prop_mat > 0.1)



# prop mature

temp4 <- temp1 %>%
  group_by(year)

# experiment with "spread" to produce a table of prop_mat
temp3 <- temp1 %>%
  select(year, stratum, age, prop_mat) %>%
  pivot_wider(id_cols = c(year, stratum), names_from = age, values_from = prop_mat)



#make variables
#ageD$rank <- rank(ageD$age2)
#matA$mat2_lag1 <- lag(matA$age2, 1)


# join all dataframes with lags----
# this is for the "Indices Lagged" tab in the dashboard.  It makes it easier to see the relations because all indices are put to the survey year.

ls <- list(cap, ld, ice, cond, matA)
df_lag <- ls %>% reduce(left_join, by ="year") %>%
  select(year, abundance_med, biomass_med, rankB, avg_densityt_2, tice, condt_1, age2, mat2t_1)
str(df_lag)

# derived variables ----
# Create indicator (index) of abundance next year based on % of immature Age 2 (1-age 2) that are will mature the next year. This is probably not quite right as the AGe 1's are still in but it assumes that the Age 3+ add very little.
##### Note in Aug 2022  - the below was fine for when I first made this in early 2021.  But not needed given the age disaggregated data.
# df_lag$abundAge3_t <- "NA"
# 
# df_lag$abundAge3_t <- ((1-(df_lag$mat2t_1*0.01))*lag(df_lag$abundance_med, 1))
# plot(df_lag$mat2t_1, df_lag$abundance_med)
#m1 <- lm(df_lag$abundance_med ~ df_lag$mat2t_1)
#left_join(tibble::rownames_to_column(df_lag), as.data.frame(m1$fitted.values), by = c("rowname" = "Symbol"))
#lines(df_lag$mat2t_1, m1$fitted.values, col = 'red', na.rm = T)
# plot(df_lag$abundAge3_t, df_lag$abundance_med)
# 
# abund_age3_YEAR <- Scatter1(df = df_lag, xaxis = year, yaxis = abundAge3_t, colour = NULL, 
#                       c1 = "Rank: ", c2 = "Year: ", c3 = "Abund_Age_3: ", xlab = "Year", ylab = "Abundance Age 3",
#                       filename = "figs/2-abund3-year.pdf", save = save)


# This is the abundance at t-2 - useful for a crude S-R relationship
df_lag$abundance_med_t_2 <- lag(df_lag$abundance_med,2)

# this is the strength of the AGe 3 cohort; lagged three years so that it can be compared to the index for a S-R relationship
df_lag$SR_age3_t3 <- lag(df_lag$abundance_med*(1-(df_lag$age2*0.01)), 3)

# relationship between immature at age 2 that will influence recruits in 3 years and mature at age 2 that will influence recruits in 2 years
df_lag$immat_t3 <- "NA"
df_lag$immat_t3 <- lag((100-df_lag$age2), 3)
plot(df_lag$immat_t3[c(7:34)], df_lag$abundance_med[c(7:34)])
summary(lm(df_lag$abundance_med ~ df_lag$immat_t3 + df_lag$mat2t_1))

#this is the strength of the Age 2 cohort, lagged one year - fish that will be Age 3 the next year
df_lag$SRage3_t1 <- lag(df_lag$abundance_med*(df_lag$age2*0.01), 1)


# explore lagged relationships between %Mat@AG2 and abundance----
# Influence of abundance (t) on Abundance of age 3 (t3) 
temp1a <- df_lag %>%
  filter(year > 1992) %>%
  ggplot(aes(abundance_med, SR_age3_t3, text = paste("Year", year))) + geom_point()
  
  ggplotly(temp1a)

# reverse of the above graph - not sure if this has any value
#temp1b <- df_lag %>%
#    filter(year > 1992) %>%
 #   ggplot(aes(SR_age3_t3, abundance_med,  text = paste#("Year", year))) + geom_point()
  
 # ggplotly(temp1b)
  
# Influence of the larvae (t-2) on Age3 (t3): this is a bit of a stretch.  Good correlation between ld and abundance_med but this is taking it three years beyond that  
temp1c <- df_lag %>%
    filter(year > 1992) %>%
    ggplot(aes(SR_age3_t3, avg_densityt_2,  text = paste("Year", year))) + geom_point()
  ggplotly(temp1c)

  
# no idea why this is here  
temp1d <- df_lag %>%
    filter(year > 1992) %>%
    ggplot(aes(SR_age3_t3, lag(avg_densityt_2, 1),  text = paste("Year", year))) + geom_point()
  ggplotly(temp1d)
  
# note on the dashboard the strong decline in larval density from 2005 onwards - this is to try to relate it to maturity at age 2 and tehre seems to be some relation
plot(df_lag$age2[21:34], df_lag$avg_densityt_2[21:34])
summary(lm(df_lag$avg_densityt_2[21:34] ~ df_lag$age2[21:34]))
plot(df_lag$year[23:34], df_lag$avg_densityt_2[23:34])
summary(lm(df_lag$avg_densityt_2[23:34] ~ df_lag$year[23:34]))

# multivariate approach----  
# start with the relationship between LD (t-2) and capelin abundance

temp3 <- df_lag %>%
  filter(year > 1990) %>%
  ggplot(aes(avg_densityt_2, biomass_med, text = paste("Year", year))) + geom_point()

ggplotly(temp3)


# Then, based on the idea that only LD values > 2000 have led to good recruitment, lets see if any thresholds in tice: filter the data accordingly
temp4 <- df_lag %>%
  filter(year > 1990 & avg_densityt_2 > 2000) %>%
  ggplot(aes(tice, abundance_med, text = paste(
    "Year", year, "\n", "Density", avg_densityt_2, "\n",  
                                               sep = ""))) + geom_point()

ggplotly(temp4)

# BAsed on the logic that late tice > 80 days, and only <80 days has produced the "large" numbers
temp5 <- df_lag %>%
  filter(year > 1990 & avg_densityt_2 > 2000 & tice <85) %>%
  ggplot(aes(condt_1, abundance_med, text = paste(
    "Year", year, "\n", "Cond", condt_1, "\n",  
    sep = ""))) + geom_point()

ggplotly(temp5)

#Suggests that condition > 0.99 might lead to large abundance.

# the problem with this approach is the small sample size and uncertainty, i.e., driven by 2013-2015 and that some low abundance year had good condition and "good" tice.

# Brecover ---- 

#all data
cap$biomass_med_lead <- lead(cap$biomass_med, 2)
plot(cap$biomass_med, cap$biomass_med_lead)

#Based on the above, it seems to make sense to divide this across the regmime change
#pre collapse
cap_preCollapse <- filter(cap, year < 1991)
plot(cap_preCollapse$biomass_med, cap_preCollapse$biomass_med_lead)

#post collapse with correlation between abundance and biomasss
cap_postCollapse <- filter(cap, year >= 1991)
plot(cap_postCollapse$biomass_med, cap_postCollapse$abundance_med)
cor(cap_postCollapse$biomass_med, cap_postCollapse$abundance_med, use = "complete.obs", method = c("pearson"))


# S-R relationship post collapse - biomasss
plot(cap_postCollapse$biomass_med, cap_postCollapse$biomass_med_lead)
quantile(cap_postCollapse$biomass_med, c(0.1, 0.9), na.rm = T)
quantile(cap_postCollapse$biomass_med_lead, c(0.1, 0.9), na.rm = T)
#assuiming that i've done this right, not much here - 

# NOTE THAT THE S-R RELATIONSHIPS ARE IN THE BELOW AND IN THE DASHBOARD


# S-R Approach----
### this is for the 
# bring in age disaggregated data
df_dis_all <- read_csv("C:/Users/lewiske/Documents/capelin_LRP/data/capelin_age_disaggregate_abundance.csv")
str(df_dis_all)

# join age disaggregated data with larval density
df_mat <- left_join(df_dis_all, ld, by = 'year')
str(df_mat)

# the 2 year lead [t-2] of the mature capelin ages 2/3/4 and the abundance of mature capelin [t]. Note that the code is moving the mature age 2 back in time so that they correspond to the abundance at time t - probably easier to see this in JAGS
plot(lead(df_mat$age2, 2)*lead(df_mat$age2PerMat, 2), df_mat$age2*df_mat$age2PerMat+df_mat$age3+df_mat$age4)


# make a smaller dataframe of the relevant variables
sr <- as.data.frame(cbind(year = df_mat$year, age2 = df_mat$age2, age2PerMat = df_mat$age2PerMat, biomass = df_lag$biomass_med[1:33]))
str(sr)
sr$R <- sr$age2*1000*sr$age2PerMat*0.01 # the 1000 is to get this to billions so that resulting units are kt, the 0.01 is to get PerMat to a percentage
str(sr)

#write.csv(sr, "data_for_Tim.csv")

# exploratory - as per Hilborn and Walters on pg ~ 269, plot biomass v R
plot(sr$biomass, sr$R)
# from 269 - biomass v logaritm of S/R - spawners v recruits
plot(sr$biomass, log(sr$R))
# log biomass v log S/R (this may not be right)
plot(log(sr$biomass), log(sr$R))



### Abundance by year and age.  Just plotting this to get a sense of the abundance by year but this is only from 2014-2019 - still, most of the immatures will be age 2 and the age 3/4/5 have only a fraction that are immature.

ageYear <- ageD %>%
  group_by(year, age) %>%
  summarize(abund = sum(n)) %>%
  ggplot(aes(x = year, y = abund, colour = as.factor(age))) + 
  geom_point()
ageYear

# the above plot by strata
p <- ggplot(data = ageD, aes(x = year, y = n, colour = as.factor(age)))
p <- p + geom_point()
p


# Time to do this right - need 
# just rename matA so that the join goes more smoothly
# not sure if this makes sense at all given the Recovery issue.
matA1 <- rename(matA, mat1 = age1, mat2 = age2, mat3 = age3, mat4 = age4, mat5 = age5)

tmp <- left_join(df_dis_all, matA1, by = "year")
str(tmp)
tmp <- tmp %>% mutate(R = age2*mat2)
str(tmp)

cbind(((100-tmp$mat2)/100)*tmp$age2, tmp$age3)

plot((100-mat2)/100*age2 ~ age3, data = tmp)
abline(a=0, b=1)




                                                                                                                                