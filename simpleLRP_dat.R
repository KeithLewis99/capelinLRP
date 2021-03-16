
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
if(!dir.exists("data"))dir.create("data")
if(!dir.exists("figs"))dir.create("figs") #for publication quality only
if(!dir.exists("output"))dir.create("output") # for tables and figures
if(!dir.exists("ms"))dir.create("ms") # manuscript
if(!dir.exists("report"))dir.create("report") #for rmd report


## Start----
# shouldn't need the above after the first day
#libraries
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(purrr)

#clear environment
rm(list=ls())


# Source files
source("simpleLRP_FUN.R")
save <- "no"


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

#create a rank column
ld$rank <- rank(ld$avg_density)
arrange(ld, rank)
ld$avg_density_lag2 <- lag(ld$avg_density, 2)

#basic plot of year v density
plot(ld$year, ld$avg_density)

#basic plot of rank v. density with 10, 50, and 90th percentiles
plot(ld$rank, ld$avg_density)
abline(v=2.8)
abline(v = 18.5)
abline(v = 9.8)



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
cap$rank <- rank(cap$abundance_med)
arrange(cap, rank)
cap$abundance_med_lag2 <- lag(cap$abundance_med, 2)
#View(cap)

#plot biomass and abundance
plot(cap$abundance_med, cap$biomass_med)

#basic plot of year v capelin abundance
plot(cap$year, cap$abundance_med)

#basic plot of rank v capelin abundance
plot(cap$rank, cap$abundance_med)
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
cond$cond_lag1 <- lag(cond$meanCond)



## read in maturity data----
#read and check data
matA <- read_csv("data/springAcoustics-percentMature.csv", col_types = cols(
  year = col_integer()
))
str(matA)

#make variables
matA$rank <- rank(matA$age2)
matA$mat2_lag1 <- lag(matA$age2, 1)




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
# proportion	=	 Proportion
# n_mat	=	 N mature (millions)
# prop_mat	=	 Proportion mature
# weight	=	 Weight (tonnes)
# mean_length	=	 Mean length (mm)
# mean_weight	=	 Mean weight (g)

#manipulate 


#make variables
#ageD$rank <- rank(ageD$age2)
#matA$mat2_lag1 <- lag(matA$age2, 1)


## join all dataframes with lags----
# this is for the "Indices Lagged" tab in the dashboard.  It makes it easier to see the relations because all indices are put to the survey year.

ls <- list(cap, ld, ice, cond, matA)
df_lag <- ls %>% reduce(left_join, by ="year") %>%
  select(year, abundance_med, avg_density_lag2, tice, cond_lag1, age2, mat2_lag1)



# Create indicator (index) of abundance next year based on % of immature Age 2 (1-age 2) that are will mature the next year.
df_lag$abund_age3 <- "NA"

df_lag$abund_age3 <- ((1-(df_lag$mat2_lag1*0.01))*lag(df_lag$abundance_med, 1))
plot(df_lag$mat2_lag1, df_lag$abundance_med)
plot(df_lag$abund_age3, df_lag$abundance_med)

abund_age3_YEAR <- Scatter1(df = df_lag, xaxis = year, yaxis = abund_age3, colour = NULL, 
                      c1 = "Rank: ", c2 = "Year: ", c3 = "Abund_Age_3: ", xlab = "Year", ylab = "Abundance Age 3",
                      filename = "figs/2-abund3-year.pdf", save = save)

df_lag$abundance_med_t2 <- lag(df_lag$abundance_med,2)

# this is the strength of the AGe 3 cohort; lagged two years so that it can be compared to the index for a S-R relationship
df_lag$SRt3 <- lag(df_lag$abundance_med*(1-(df_lag$age2*0.01)), 3)

# relationship between immature at age 2 that will influence recruits in 3 years and mature at age 2 that will influence recruits in 2 years
df_lag$immat_lag3 <- "NA"
df_lag$immat_lag3 <- lag((100-df_lag$age2), 3)
plot(df_lag$immat_lag3[c(7:34)], df_lag$abundance_med[c(7:34)])
summary(lm(df_lag$abundance_med ~ df_lag$immat_lag3 + df_lag$mat2_lag1))

#this is the strength of the Age 2 cohort, lagged one year
df_lag$SRt2 <- lag(df_lag$abundance_med*(df_lag$age2*0.01), 2)

temp1a <- df_lag %>%
  filter(year > 1992) %>%
  ggplot(aes(abundance_med, SRt3, text = paste("Year", year))) + geom_point()
  
  ggplotly(temp1a)

# abundance med = recruitment here
temp1b <- df_lag %>%
    filter(year > 1992) %>%
    ggplot(aes(SRt2, abundance_med,  text = paste("Year", year))) + geom_point()
  
  ggplotly(temp1b)
  
  
temp1c <- df_lag %>%
    filter(year > 1992) %>%
    ggplot(aes(SRt2, avg_density_lag2,  text = paste("Year", year))) + geom_point()
  ggplotly(temp1c)

  
  
temp1d <- df_lag %>%
    filter(year > 1992) %>%
    ggplot(aes(SRt3, lag(avg_density_lag2, 1),  text = paste("Year", year))) + geom_point()
  ggplotly(temp1d)

  
  
    
# just the 3 "recovery years"
temp2 <- df_lag %>%
    filter(year >= 2011 & year <=2015)


# tiered approach  
temp3 <- df_lag %>%
  filter(year > 1990) %>%
  ggplot(aes(avg_density_lag2, abundance_med, text = paste("Year", year))) + geom_point()

ggplotly(temp3)



temp4 <- df_lag %>%
  filter(year > 1990 & avg_density_lag2 < 2000) %>%
  ggplot(aes(tice, abundance_med, text = paste(
    "Year", year, "\n", "Density", avg_density_lag2, "\n",  
                                               sep = ""))) + geom_point()

ggplotly(temp4)


temp5 <- df_lag %>%
  filter(year > 1990 & avg_density_lag2 < 2000) %>%
  ggplot(aes(cond_lag1, abundance_med, text = paste(
    "Year", year, "\n", "Cond", cond_lag1, "\n",  
    sep = ""))) + geom_point()

ggplotly(temp5)


## S-R relationship----
# NOTE THAT THE S-R RELATIONSHIPS ARE IN THE BELOW AND IN THE DASHBOARD

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




## Haddock type approach - all data----

# calculate anomalies - get mean and SD
cap <- anomaly(cap, "biomass_med_lead")


# get the quantile and extract the 90th for the hline
h90 <- quantile(cap$anomaly, c(0.1, 0.9), na.rm = T)[2]
# confirm above value and check on plot
quantile(cap$anomaly, c(0.1, 0.9), na.rm = T)


# simple test plot
plot(cap$year, cap$anomaly)
abline(h = h90)


#get the value for the lowest index to generate large recruitment (or a large index)
biomass90_1 <- quantile(cap$biomass_med, c(0.1, 0.9), na.rm = T)[2]
y1 <- subset(cap, biomass_med_lead >= biomass90_1, na.rm = T)
v90 <- min(y1$biomass_med)

# simple test plot
plot(cap$biomass_med, cap$biomass_med_lead)
abline(v = v90)




## Haddock type approach - post collapse----
#calculate anomalies - get mean and SD
cap_postCollapse <- anomaly(cap_postCollapse, "biomass_med_lead")


# get the quantile and extract the 90th for the hline
h90_post <- quantile(cap_postCollapse$anomaly, c(0.1, 0.9), na.rm = T)[2]
# confirm above value and check on plot
quantile(cap_postCollapse$anomaly, c(0.1, 0.9), na.rm = T)

plot(cap_postCollapse$year, cap_postCollapse$anomaly)
abline(h = h90_post)


#get the value for the lowest index to generate large recruitment (or a large index)
biomass90_1 <- quantile(cap_postCollapse$biomass_med, c(0.1, 0.9), na.rm = T)[2]
y1 <- subset(cap_postCollapse, biomass_med_lead >= biomass90_1, na.rm = T)
v90_post <- min(y1$biomass_med)


# simple test plot
plot(cap_postCollapse$year, cap_postCollapse$biomass_med_lead)
plot(cap_postCollapse$biomass_med, cap_postCollapse$biomass_med_lead)

# simple test plot
plot(cap_postCollapse$biomass_med, cap_postCollapse$biomass_med_lead)
abline(v = v90_post)
