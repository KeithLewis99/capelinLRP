Column
-------------------------------------
  
  ### Capelin Index
  
  ```{r fig-2}
par(mar= c(5, 4, 4, 2) + 0.1)

Scatter1(df = cap, xaxis = rank, yaxis = abundance_med, colour = year, 
         c1 = "Year: ", c2 = "Rank: ", c3 = "Abundance: ", xlab = "Rank", ylab = "Capelin abundance (millions?)", filename = "figs/2-Abundance-rank-year.pdf", save = "no")
```


# Indices by year

Column
-------------------------------------
  
  ### Capelin Index
  
  ```{r fig-3}
par(mar= c(5, 4, 4, 2) + 0.1)

Scatter1(df = cap, xaxis = year, yaxis = abundance_med, colour = rank, 
         c1 = "Rank: ", c2 = "Year: ", c3 = "Abundance: ", 
         xlab = "Year", ylab = "Capelin abundance (millions?)",
         filename = "figs/2-cond-rank-year.pdf", save = "no")

```

### Larval Index

```{r fig-4}
par(mar= c(5, 4, 4, 2) + 0.1)

Scatter1(df = ld, xaxis = year, yaxis = avg_density, colour = rank, 
         c1 = "Rank: ", c2 = "Year: ", c3 = "tice: ", xlab = "Year", 
         ylab = "Larval Density (#/m^-3)",
         filename = "figs/2-cond-year-rank.pdf", save = "no")

```

Column
-------------------------------------
  
  ### ice Index
  
  ```{r fig-5}
par(mar= c(5, 4, 4, 2) + 0.1)

Scatter1(df = ice, xaxis = year, yaxis = tice, colour = rank, 
         c1 = "Rank: ", c2 = "Year: ", c3 = "tice: ", 
         xlab = "Year", ylab = "Ice retreat (tice - DOY)",
         filename = "figs/2-cond-year-rank.pdf", save = "no")
```

### condition Index

```{r fig-6}
par(mar= c(5, 4, 4, 2) + 0.1)
Scatter1(df = cond, xaxis = year, yaxis = meanCond, colour = rank, 
         c1 = "Rank: ", c2 = "Year: ", c3 = "Cond: ", xlab = "Year", ylab = "Condition ()",
         filename = "figs/2-cond-year-rank.pdf", save = "no")

```



# Indices Lagged

Column
-------------------------------------
  
  ### Capelin Index
  
  ```{r}
par(mar= c(5, 4, 4, 2) + 0.1)

Scatter1(df = df_lag, xaxis = year, yaxis = abundance_med, colour = NULL, 
         c1 = "Rank: ", c2 = "Year: ", c3 = "Abundance: ", 
         xlab = "Year", ylab = "Capelin abundance (millions?)",
         filename = "figs/2-cond-rank-year.pdf", save = "no")

```

Column {.tabset}
-------------------------------------
  
  ### Larval Index
  
  ```{r}
par(mar= c(5, 4, 4, 2) + 0.1)

Scatter1(df = df_lag, xaxis = year, yaxis = avg_density, colour = NULL, 
         c1 = "Rank: ", c2 = "Year: ", c3 = "tice: ", xlab = "Year", 
         ylab = "Larval Density (#/m^-3)",
         filename = "figs/2-cond-year-rank.pdf", save = "no")

```

### ice Index

```{r}
par(mar= c(5, 4, 4, 2) + 0.1)

Scatter1(df = df_lag, xaxis = year, yaxis = tice, colour = rank, 
         c1 = "Rank: ", c2 = "Year: ", c3 = "tice: ", 
         xlab = "Year", ylab = "Ice retreat (tice - DOY)",
         filename = "figs/2-cond-year-rank.pdf", save = "no")
```

### condition Index

```{r}
par(mar= c(5, 4, 4, 2) + 0.1)
Scatter1(df = df_lag, xaxis = year, yaxis = meanCond, colour = rank, 
         c1 = "Rank: ", c2 = "Year: ", c3 = "Cond: ", xlab = "Year", ylab = "Condition ()",
         filename = "figs/2-cond-year-rank.pdf", save = "no")

```

### Maturity (Age 2) Index

```{r fig-7}
par(mar= c(5, 4, 4, 2) + 0.1)
Scatter1(df = df_lag, xaxis = year, yaxis = mat2_lag1, colour = rank, 
         c1 = "Rank: ", c2 = "Year: ", c3 = "%Age_2: ", xlab = "Year", ylab = "Condition ()",
         filename = "figs/2-cond-year-rank.pdf", save = "no")

```


# Brecover - Haddock approach
Column {.tabset}
-------------------------------------
  
  Vertical and Horizontal lines are 90th percentiles

### All data: anomalies

```{r fig-8}
Bar1(df = cap, xaxis = year, yaxis = anomaly, c2 = "Abundance: ", c3 = "Anomaly: ", xlab = "Year", ylab = "Recruitment anomolies", hline = h90, filename = "figs/3-Biomass_all-year-anomaly.pdf", save = "no")
```

### All data: stock-recruit

```{r fig-9}
Scatter2(df = cap, xaxis = biomass_med, yaxis = biomass_med_lead, c2 = "Biomass: ", c3 = "Recruitment: ", xlab = "Index (ktonnes)", ylab = "Recruitment (ktonnes)", vline = v90, filename = "figs/4-Biomass_all-index-recruit.pdf", save = "no")
```

Column {.tabset}
-------------------------------------
  
  ### Post collapse: anomalies
  
  ```{r fig-10}
Bar1(df = cap_postCollapse, xaxis = year, yaxis = anomaly, c2 = "Abundance: ", c3 = "Anomaly: ", xlab = "Year", ylab = "Recruitment anomolies", hline = h90, filename = "figs/5-Biomass_postCollapse-year-anomaly.pdf", save = "no")
```

### Post collapse: stock-recruit 

```{r fig-11}
Scatter2(df = cap_postCollapse, xaxis = biomass_med, yaxis = biomass_med_lead, c2 = "Biomass: ", c3 = "Recruitment: ", xlab = "Index (ktonnes)", ylab = "Recruitment (ktonnes)", vline = v90_post, filename = "figs/6-Biomass_postCollapse-index-recruit.pdf", save = "no")
```
