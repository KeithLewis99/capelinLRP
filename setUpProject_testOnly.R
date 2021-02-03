
# Set up a project
#https://happygitwithr.com/rstudio-git-github.html

# 1.	Set up a Git repo
# 2.	Create the project in R
# 3.	Create a bunch of directories automatically


rm(list=ls())

#libraries
library(readr)
library(ggplot2)

#put this file in the folder where you want to create a subfolder containing the project
if(!dir.exists("data"))dir.create("data")
if(!dir.exists("figs"))dir.create("figs") #for publication quality only
if(!dir.exists("output"))dir.create("output") # for tables and figures
if(!dir.exists("ms"))dir.create("ms")
if(!dir.exists("report"))dir.create("report") #for rmd report

#load larval density data

ld <- read_csv("data/larvae.csv")
str(ld)

test