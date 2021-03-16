




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
source("simpleLRP_dat.R")
save <- "no"



## Plots of larval density----

#pretty plot of rank v. density
LD_rank <- Scatter1(df = ld, xaxis = rank, yaxis = avg_density, colour = year, 
                    c1 = "Year: ", c2 = "Rank: ", c3 = "Density: ",                     
                    xlab = "Rank", ylab = "Larval Density (#/m^-3)", 
                    filename = "figs/1-larvae-density-rank.pdf", save = save)
LD_rank



ld_YEAR <- Scatter1(df = ld, xaxis = year, yaxis = avg_density, colour = rank, 
                    c1 = "Rank: ", c2 = "Year: ", c3 = "tice: ", xlab = "Year", 
                    ylab = "Larval Density (#/m^-3)",
                    filename = "figs/2-cond-year-rank.pdf", save = save)



## Plots of capelin abundance----

#pretty plot of rank v capelin abundance
cap_rank <- Scatter1(df = cap, xaxis = rank, yaxis = abundance_med, colour = year, 
                     c1 = "Year: ", c2 = "Rank: ", c3 = "Abundance: ", xlab = "Rank", 
                     ylab = "Capelin abundance (millions?)",
                     filename = "figs/2-Abundance-rank-year.pdf", save = save)
cap_rank

cap_Year <- Scatter1(df = cap, xaxis = year, yaxis = abundance_med, colour = rank, 
                     c1 = "Rank: ", c2 = "Year: ", c3 = "Abundance: ", 
                     xlab = "Year", ylab = "Capelin abundance (millions?)",
                     filename = "figs/2-cond-rank-year.pdf", save = save)




## Plots of tice----
tice_rank <- Scatter1(df = ice, xaxis = rank, yaxis = tice, colour = year, 
                      c1 = "Year: ", c2 = "Rank: ", c3 = "tice: ", 
                      xlab = "Rank", ylab = "Ice retreat (tice - DOY)",
                      filename = "figs/2-tice-rank-year.pdf", save = save)
tice_YEAR <- Scatter1(df = ice, xaxis = year, yaxis = tice, colour = rank, 
                      c1 = "Rank: ", c2 = "Year: ", c3 = "tice: ", 
                      xlab = "Year", ylab = "Ice retreat (tice - DOY)",
                      filename = "figs/2-cond-year-rank.pdf", save = save)



## Plots of condition----
cond_rank <- Scatter1(df = cond, xaxis = rank, yaxis = meanCond, colour = year, 
                      c1 = "Year: ", c2 = "Rank: ", c3 = "Cond: ", xlab = "Rank", ylab = "Condition ()",
                      filename = "figs/2-cond-rank-year.pdf", save = save)

cond_YEAR <- Scatter1(df = cond, xaxis = year, yaxis = meanCond, colour = rank, 
                      c1 = "Rank: ", c2 = "Year: ", c3 = "Cond: ", xlab = "Year", ylab = "Condition ()",
                      filename = "figs/2-cond-year-rank.pdf", save = save)



## plots of maturity---- 
mat2_rank <- Scatter1(df = matA, xaxis = rank, yaxis = age2, colour = year, 
                      c1 = "Year: ", c2 = "Rank: ", c3 = "%Age_2: ", xlab = "Rank", ylab = "Capelin Age 2 (%)",
                      filename = "figs/2-mat2-rank-year.pdf", save = save)

mat2_YEAR <- Scatter1(df = matA, xaxis = year, yaxis = age2, colour = rank, 
                      c1 = "Rank: ", c2 = "Year: ", c3 = "%Age_2: ", xlab = "Year", ylab = "Capelin Age 2 (%)",
                      filename = "figs/2-mat2-year-rank.pdf", save = save)


mat3_YEAR <- Scatter1(df = matA, xaxis = year, yaxis = age3, colour = rank, 
                      c1 = "Rank: ", c2 = "Year: ", c3 = "%Age_3: ", xlab = "Year", ylab = "Capelin Age 3 (%)",
                      filename = "figs/2-mat3-year-rank.pdf", save = save)








## Haddock type approach - all data----

# Wheeland/Haddock type plot Fig. 7
Anomaly_year_all <- Bar1(df = cap, xaxis = year, yaxis = anomaly, c2 = "Abundance: ", c3 = "Anomaly: ", xlab = "Year", ylab = "Recruitment anomolies", hline = h90, filename = "figs/3-Biomass_all-year-anomaly.pdf", save = save)
Anomaly_year_all



# Wheeland/Haddock type plot Fig. 8
SR_all <- Scatter2(df = cap, xaxis = biomass_med, yaxis = biomass_med_lead, c2 = "Biomass: ", c3 = "Recruitment: ", xlab = "Index (ktonnes)", ylab = "Recruitment (ktonnes)", vline = v90, filename = "figs/4-Biomass_all-index-recruit.pdf", save = save)
SR_all




## Haddock type approach - post collapse----

# Wheeland/Haddock type plot Fig. 7

Anomaly_year_post <- Bar1(df = cap_postCollapse, xaxis = year, yaxis = anomaly, c2 = "Abundance: ", c3 = "Anomaly: ", xlab = "Year", ylab = "Recruitment anomolies", hline = h90, filename = "figs/5-Biomass_postCollapse-year-anomaly.pdf", save = save)
Anomaly_year_post


# Wheeland/Haddock type plot Fig. 8
SR_all <- Scatter2(df = cap_postCollapse, xaxis = biomass_med, yaxis = biomass_med_lead, c2 = "Biomass: ", c3 = "Recruitment: ", xlab = "Index (ktonnes)", ylab = "Recruitment (ktonnes)", vline = v90, filename = "figs/6-Biomass_all-index-recruit.pdf", save = save)
SR_all

