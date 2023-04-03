# FUNCTION FILE FOR simpleLRP_dat.R and dashboards


#' Bar1 - the purpose of this function is to create interactive bar plots with a third variable in colour.  
#'
#' @param df - data frame
#' @param xaxis - variable used in xaxis and in plotly label
#' @param yaxis - variable used in yaxis and in plotly label
#' @param c2 - name used for the label in plotly - corresponds to xaxis
#' @param c3 - name used for the label in plotly - corresponds to yaxis
#' @param xlab - label for xaxis
#' @param ylab - label for yaxis
#' @param hline - 90th percentile of anomaly
#' @param filename - name of file to be produced - naming convention (folder/[figure # in order of data file-][xaxis][yaxis].file type )
#' @param save - save file as pdf or not. "Yes" in run of simpleLRP_dat, no in simpleLRP.Rmd
#'
#' @return - plotly figure
#' @export
#'
#' @examples 
#' Anomaly_year_all <- Bar1(df = cap, xaxis = year, yaxis = anomaly, c2 = "Abundance: ", c3 = "Anomaly: ", xlab = "Year", ylab = "Recruitment anomolies", hline = hline, filename = "figs/3-Biomass_all-year-anomaly.pdf", save = "yes")

Bar1 <- function(df = df, xaxis = xaxis, yaxis = yaxis, width = NULL, height = NULL,
                 c2 = c2, c3 = c3, 
                 xlab = xlab, ylab = ylab, 
                 hline = hline, hline2 = NULL, hline3 = NULL, hline4 = NULL,
                 filename = filename, save = save){
  #browser()
  p <- ggplot(df, aes(x = {{xaxis}}, y = {{yaxis}}, text = paste(
    "Year[t]: ", year, "\n",
    c2, biomass_tm2, "\n",
    c3, {{yaxis}}, "\n",  
    "R[t]: ", R,
    sep = ""
  )))
  p <- p + geom_bar(stat = "identity")
  p <- p + geom_hline(yintercept = hline)
  if(is.numeric(hline2)){
    p <- p + geom_hline(yintercept = hline2, colour = 'red')   
  }
  if(is.numeric(hline3)){
    p <- p + geom_hline(yintercept = hline3, linetype = "dashed")    
  }
  if(is.numeric(hline4)){
    p <- p + geom_hline(yintercept = hline4, colour = 'red', linetype = "dashed")
  }
  p <- p + xlab(xlab)
  p <- p + ylab(ylab)
  p <- p + theme_bw()
  
  
  if(!! save == "yes"){
    ggsave(paste(filename))
    return(ggplotly(p, tooltip = "text"))  
  } else {
    return(ggplotly(p, tooltip = "text")) 
  }
}


#' Anomaly - meant to calculate anomalies for different variables
#'
#' @param df - the data frame 
#' @param x - the variable of interest
#'
#' @return - a new column in the dataframe with the anomalies
#' @export
#'
#' @examples cap <- anomaly(cap, "biomass_med_lead")
anomaly <- function(df, x){
  #browser()
  
  #calculate anomalies - get mean and SD
  dfMean <- mean(df[[x]], na.rm = T)
  dfSD <- sd(df[[x]], na.rm = T)
  
  #create variable "anomaly" and calculate
  df$anomaly <- "NA"
  df$anomaly <- round((df[[x]] - dfMean)/dfSD, 2)
  return(df)
}


# Scatter Table ----
# Scatter errorbar vline  text  hline adjust_size colour
#   1        x
#   2                x
#   3                       x     x        x
#   4        x                             x         x
#   5                       x     x        1 
## 1 - Note that Scatter 5 has the resizing done in ggplotly - this is easier and simpler than doing it in a function, i.e., Scatter5 is essentially Scatter3 but with another way of resizing.


#' Scatter1 - the purpose of this function is to create interactive scatter plots with a third variable in colour.  
#'
#' @param df - data frame
#' @param xaxis - variable used in xaxis and in plotly label
#' @param yaxis - variable used in yaxis and in plotly label
#' @param colour - variable used for fill - usually year
#' @param c1 - variable used for the label in plotly - corresponds to colour
#' @param c2 - name used for the label in plotly - corresponds to xaxis
#' @param c3 - name used for the label in plotly - corresponds to yaxis
#' @param xlab - label for xaxis
#' @param ylab - label for yaxis
#' @param filename - name of file to be produced - naming convention (folder/[figure # in order of data file-][xaxis][yaxis].file type )
#' @param save - save file as pdf or not. "Yes" in run of simpleLRP_dat, no in simpleLRP.Rmd
#' @param errorbar - "yes" for graph to include an errorbar, else "no"
#' 
#' @return - plotly figure
#' @export
#'
#' @examples 
#' Scatter1(df = df_ld, xaxis = rank, yaxis = avg_density, colour = year, c1 = "Year: ", c2 = "Rank: ", c3 = "Density: ",                     xlab = "Rank", ylab = "Larval Density (#/m^-3)", filename = "figs/1-larvae-density-rank.pdf", save = "no")

Scatter1 <- function(df = df, xaxis = xaxis, yaxis = yaxis, colour = NULL, 
                     c1 = NULL, c2 = c2, c3 = c3, 
                     xlab = xlab, ylab = ylab,
                     #xlab = xlab, ylab = ylab,
                     filename = filename, save = save, 
                     errorbar = "no", ymin = NULL, ymax = NULL){
  #browser()
  p <- ggplot(df, aes(x = {{xaxis}}, y = {{yaxis}}, colour = {{colour}}, text = paste(
    c1, {{colour}}, "\n",
    c2, {{xaxis}}, "\n",
    c3, {{yaxis}}, "\n",  
    sep = ""
  )))
  p <- p + geom_point()
  p <- p + scale_colour_continuous(type = "viridis")
  if(!! errorbar == "se"){
    p <- p + geom_errorbar(aes(ymin = {{yaxis}}-{{ymin}}*1.96, ymax = {{yaxis}}+{{ymin}}*1.96))
  } else if(!! errorbar == "limit"){
    p <- p + geom_errorbar(aes(ymin = ymin, ymax = ymax))
  }
  p <- p + xlab(xlab)
  p <- p + ylab(ylab)
  p <- p + theme(axis.title = element_text(size = 20))
  p <- p + theme_bw()
  
  if(!! save == "yes"){  # the !! just unquotes the arguement
    ggsave(paste(filename))
    return(ggplotly(p, tooltip = "text"))  
  } else {
    return(ggplotly(p, tooltip = "text")) 
  }
}






#' Scatter2 - the purpose of this function is to create interactive scatter plots with a vline at the lowest biomass that gives large recruitment.  
#'
#' @param df - data frame
#' @param xaxis - variable used in xaxis and in plotly label
#' @param yaxis - variable used in yaxis and in plotly label
#' @param c2 - name used for the label in plotly - corresponds to xaxis
#' @param c3 - name used for the label in plotly - corresponds to yaxis
#' @param xlab - label for xaxis
#' @param ylab - label for yaxis
#' @param vline - 90th percentile of anomaly and others
#' @param filename - name of file to be produced - naming convention (folder/[figure # in order of data file-][xaxis][yaxis].file type )
#' @param save - save file as pdf or not. "Yes" in run of simpleLRP_dat, no in simpleLRP.Rmd
#'
#' @return - plotly figure
#' @export
#'
#' @examples 
#' SR_all <- Scatter2(df = cap, xaxis = biomass_med, yaxis = biomass_med_lead, c2 = "Biomass: ", c3 = "Recruitment: ", xlab = "Index (ktonnes)", ylab = "Recruitment (ktonnes)", vline = v90, filename = "figs/4-Biomass_all-index-recruit.pdf", save = "yes")

Scatter2 <- function(df = df, width = NULL, height = NULL, xaxis = xaxis, yaxis = yaxis, 
                     c2 = c2, c3 = c3, 
                     xlab = xlab, ylab = ylab, 
                     vline1 = vline1, vline2 = NULL, vline3 = NULL, vline4 = NULL, 
                     filename = filename, save = save){
  #browser()
  p <- ggplot(df, aes(x = {{xaxis}}, y = {{yaxis}}, text = paste(
    "Year[t]: ", year, "\n",
    c2, {{xaxis}}, "\n",
    c3, {{yaxis}}, "\n",  
    sep = ""
  )))
  p <- p + geom_point()
  #p <- p + scale_colour_continuous(type = "viridis")
  p <- p + xlab(xlab)
  p <- p + ylab(ylab)
  p <- p + geom_vline(xintercept = vline1)
  if(is.numeric(vline2)){
    p <- p + geom_vline(xintercept = vline2, colour = 'red')   
  }
  if(is.numeric(vline3)){
    p <- p + geom_vline(xintercept = vline3, linetype = "dashed")    
  }
  if(is.numeric(vline4)){
    p <- p + geom_vline(xintercept = vline4, colour = 'red', linetype = "dashed")
  }

    p <- p + theme_bw()
  
  
  if(save == "yes"){
    ggsave(paste(filename))
    return(ggplotly(p, tooltip = "text"))  
  } else {
    return(ggplotly(p, tooltip = "text")) 
  }
}

# as above but with hlines
Scatter3 <- function(df = df, xaxis = xaxis, yaxis = yaxis, 
                     c2 = c2, c3 = c3, 
                     xlab = xlab, ylab = ylab, 
                     hline1 = hline1, hline2 = NULL, hline3 = NULL, hline4 = NULL, 
                     filename = filename, save = save, 
                     text = NULL, xlabel = NULL, ylabel = NULL,
                     font = 20, size = 15, width = NULL){
  #browser()
  p <- ggplot(df, aes(x = {{xaxis}}, y = {{yaxis}}, text = paste(
    #"Year: ", year, "\n",
    c2, {{xaxis}}, "\n",
    c3, {{yaxis}}, "\n",  
    sep = ""
  )))
  p <- p + geom_point()
  #p <- p + scale_colour_continuous(type = "viridis")
  p <- p + xlab(xlab)
  p <- p + ylab(ylab)
  p <- p + geom_hline(yintercept = hline1)
  if(is.numeric(hline2)){
    p <- p + geom_hline(yintercept = hline2, colour = 'red')   
  }
  if(is.numeric(hline3)){
    p <- p + geom_hline(yintercept = hline3, linetype = "dashed")    
  }
  if(is.numeric(hline4)){
    p <- p + geom_hline(yintercept = hline4, colour = 'red', linetype = "dashed")
  }
  #p + geom_text(aes(x = xlabel, y = ylabel), label = text)
  p <- p + annotate("text", x = xlabel, y = ylabel, label = text, size = 4)
  p <- p + theme_bw()
  
  
  if(!! save == "yes"){  # the !! just unquotes the arguement
    ggsave(paste(filename))
    return(ggplotly(p, tooltip = "text"))  
  } else {
    return(ggplotly(p, tooltip = "text") %>%
             layout(width = width,
               xaxis=list(title=list(text = xlab, font = list(size = font)), 
                          tickfont = list(size = size)),
               yaxis=list(title=list(text = ylab, font = list(size = font)), 
                          tickfont = list(size = size))
             ))
  }
}


#' Scatter4 - the purpose of this function is to create interactive scatter plots with a third variable in colour.  This differs from 1 in that the font size of the axis labels can be adjusted
#'
#' @param df - data frame
#' @param xaxis - variable used in xaxis and in plotly label
#' @param yaxis - variable used in yaxis and in plotly label
#' @param colour - variable used for fill - usually year
#' @param c1 - variable used for the label in plotly - corresponds to colour
#' @param c2 - name used for the label in plotly - corresponds to xaxis
#' @param c3 - name used for the label in plotly - corresponds to yaxis
#' @param xlab - label for xaxis
#' @param ylab - label for yaxis
#' @param filename - name of file to be produced - naming convention (folder/[figure # in order of data file-][xaxis][yaxis].file type )
#' @param save - save file as pdf or not. "Yes" in run of simpleLRP_dat, no in simpleLRP.Rmd
#' @param errorbar - "yes" for graph to include an errorbar, else "no"
#' 
#' @return - plotly figure
#' @export
#'
#' @examples 
#' Scatter1(df = df_ld, xaxis = rank, yaxis = avg_density, colour = year, c1 = "Year: ", c2 = "Rank: ", c3 = "Density: ",                     xlab = "Rank", ylab = "Larval Density (#/m^-3)", filename = "figs/1-larvae-density-rank.pdf", save = "no")

Scatter4 <- function(df = df, xaxis = xaxis, yaxis = yaxis, colour = NULL, 
                     c1 = NULL, c2 = c2, c3 = c3, 
                     xlab = xlab, ylab = ylab,
                     #xlab = xlab, ylab = ylab,
                     filename = filename, save = save, 
                     errorbar = "no", ymin = NULL, ymax = NULL,
                     font = 20, size = 15){
  #browser()
  p <- ggplot(df, aes(x = {{xaxis}}, y = {{yaxis}}, colour = {{colour}}, text = paste(
    c1, {{colour}}, "\n",
    c2, {{xaxis}}, "\n",
    c3, {{yaxis}}, "\n",  
    sep = ""
  )))
  p <- p + geom_point()
  p <- p + scale_colour_continuous(type = "viridis")
  if(!! errorbar == "se"){
    p <- p + geom_errorbar(aes(ymin = {{yaxis}}-{{ymin}}*1.96, ymax = {{yaxis}}+{{ymin}}*1.96))
  } else if(!! errorbar == "limit"){
    p <- p + geom_errorbar(aes(ymin = ymin, ymax = ymax))
  }
  p <- p + xlab(NULL)
  p <- p + ylab(NULL)
  #p <- p + theme(axis.title = element_text(size = 20))
  p <- p + theme_bw()
  
  if(!! save == "yes"){  # the !! just unquotes the arguement
    ggsave(paste(filename), device = "png", width = 10, units = "cm")
    return(ggplotly(p, tooltip = "text"))  
  } else {
    return(ggplotly(p, tooltip = "text") %>%
             layout(
               xaxis=list(title=list(text = xlab, font = list(size = font)), 
                          tickfont = list(size = size)),
               yaxis=list(title=list(text = ylab, font = list(size = font)), 
                          tickfont = list(size = size))
    ))
  }
}



# as Scatter 3 but with potential to resize 
Scatter5 <- function(df = df, xaxis = xaxis, yaxis = yaxis, 
                     c2 = c2, c3 = c3, 
                     xlab = xlab, ylab = ylab, 
                     hline1 = hline1, hline2 = NULL, hline3 = NULL, hline4 = NULL, 
                     filename = filename, save = save, 
                     text = NULL, xlabel = NULL, ylabel = NULL,
                     font = 20, size = 15, width = NULL){
  #browser()
  p <- ggplot(df, aes(x = {{xaxis}}, y = {{yaxis}}, text = paste(
    #"Year: ", year, "\n",
    c2, {{xaxis}}, "\n",
    c3, {{yaxis}}, "\n",  
    sep = ""
  )))
  p <- p + geom_point()
  #p <- p + scale_colour_continuous(type = "viridis")
  p <- p + xlab(xlab)
  p <- p + ylab(ylab)
  p <- p + geom_hline(yintercept = hline1)
  if(is.numeric(hline2)){
    p <- p + geom_hline(yintercept = hline2, colour = 'red')   
  }
  if(is.numeric(hline3)){
    p <- p + geom_hline(yintercept = hline3, linetype = "dashed")    
  }
  if(is.numeric(hline4)){
    p <- p + geom_hline(yintercept = hline4, colour = 'red', linetype = "dashed")
  }
  #p + geom_text(aes(x = xlabel, y = ylabel), label = text)
  p <- p + annotate("text", x = xlabel, y = ylabel, label = text, size = 4)
  p <- p + theme_bw()
  return(p)
}  

