
# Set up a project - see the below link fir directions.
#https://happygitwithr.com/rstudio-git-github.html

# 1.	Set up a Git repo on GitHub.
# 2.	Create the project in R - New Project - VErsion Control - Git
# 3.	Create a bunch of directories automatically (see below)


rm(list=ls())

#libraries
library(readr)
library(dplyr)
library(ggplot2)

#put this file in the folder where you want to create a subfolder containing the project
if(!dir.exists("data"))dir.create("data")
if(!dir.exists("figs"))dir.create("figs") #for publication quality only
if(!dir.exists("output"))dir.create("output") # for tables and figures
if(!dir.exists("ms"))dir.create("ms")
if(!dir.exists("report"))dir.create("report") #for rmd report

##read larval density data----
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

#basic plot of year v density
plot(ld$year, ld$avg_density)

#basic plot of rank v. density with 10, 50, and 90th percentiles
plot(ld$rank, ld$avg_density)
abline(v=2.8)
abline(v = 18.5)
abline(v = 9.8)

#Make above plot nice
p <- ggplot(ld, aes(x = rank, y = avg_density, colour = year))
p <- p + geom_point()
p <- p + scale_colour_continuous(type = "viridis")
#p <- p + scale_colour_gradient(low = "yellow", high = "darkgreen")
p <- p + theme_bw()
p

ggsave("figs/1-larvae-density-rank.pdf")

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
#View(cap)

#plot biomass and abundance
plot(cap$abundance_med, cap$biomass_med)

#basic plot of year v capelin abundance
plot(cap$year, cap$abundance_med)

#basic plot of rank v capelin abundance
plot(cap$rank, cap$abundance_med)
abline(v = 22.5) #everything to the right is pre1991

#pretty graph
p <- ggplot(cap, aes(x = rank, y = abundance_med, colour = year))
p <- p + geom_point()
p <- p + scale_colour_continuous(type = "viridis")
#p <- p + scale_colour_gradient(low = "yellow", high = "darkgreen")
p <- p + theme_bw()
p

ggsave("figs/2-Abundance-rank-year.pdf")

##First stab at a S-R relationship----
#View(cap)

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
#assuiming that i've done this right, not much here



##first stab at Haddock type approach----

#all data
#calculate anomalies - get mean and SD
capMean <- mean(cap$biomass_med_lead, na.rm = T)
capSD <- sd(cap$biomass_med_lead, na.rm = T)

#create variable "anomaly" and calculate
cap$anomaly <- "NA"
cap$anomaly <- (cap$biomass_med_lead - capMean)/capSD
quantile(cap$anomaly, c(0.1, 0.9), na.rm = T)
plot(cap$year, cap$anomaly)
abline(h = 1.56)

#post collapse
#calculate anomalies - get mean and SD
capMeanPost <- mean(cap_postCollapse$biomass_med_lead, na.rm = T)
capSDPost <- sd(cap_postCollapse$biomass_med_lead, na.rm = T)

#create variable "anomaly" and calculate
cap_postCollapse$anomaly <- "NA"
cap_postCollapse$anomaly <- (cap_postCollapse$biomass_med_lead - capMeanPost)/capSDPost
quantile(cap_postCollapse$anomaly, c(0.1, 0.9), na.rm = T)
plot(cap_postCollapse$year, cap_postCollapse$anomaly)
abline(h = 1.61)
#View(cap_postCollapse)

# Wheeland/Haddock type plot Fig. 7
p <- ggplot(cap_postCollapse, aes(x = year, y = anomaly))
p <- p + geom_bar(stat = "identity")
p <- p + geom_hline(yintercept = 1.61)
p <- p + xlab("Year") 
p <- p + ylab("Recruitment anomolies")
p <- p + theme_bw()
p

ggsave("figs/3-Biomass-year-anomaly.pdf")
plot(cap_postCollapse$year, cap_postCollapse$biomass_med_lead)
plot(cap_postCollapse$biomass_med, cap_postCollapse$biomass_med_lead)

# Wheeland/Haddock type plot Fig. 8
p <- ggplot(cap_postCollapse, aes(x = biomass_med, y = biomass_med_lead))
p <- p + geom_point()
p <- p + geom_vline(xintercept = 210)
p <- p + xlab("Index (ktonnes)") 
p <- p + ylab("Recruitment (ktonnes)")
p <- p + theme_bw()
p

ggsave("figs/4-Biomass-index-recruit.pdf")
test