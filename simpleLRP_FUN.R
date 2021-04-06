# FUNCTION FILE FOR simpleLRP_dat.R

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
#' 
#' @return - plotly figure
#' @export
#'
#' @examples 
#' LD_rank <- Scatter1(df = ld, xaxis = rank, yaxis = abundance_med, colour = year, c2 = "Rank: ", c3 = "Abundance: ", xlab = "Rank", ylab = "Capelin abundance (millions?)", filename = "figs/2-Abundance-rank-year.pdf", save = "yes")

Scatter1 <- function(df = df, xaxis = xaxis, yaxis = yaxis, colour = NULL, c1 = c1, c2 = c2, c3 = c3, xlab = xlab, ylab = ylab, filename = filename, save = save){
  #browser()
  p <- ggplot(df, aes(x = {{xaxis}}, y = {{yaxis}}, colour = {{colour}}, text = paste(
    c1, {{colour}}, "\n",
    c2, {{xaxis}}, "\n",
    c3, {{yaxis}}, "\n",  
    sep = ""
  )))
  p <- p + geom_point()
  p <- p + scale_colour_continuous(type = "viridis")
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

Bar1 <- function(df = df, xaxis = xaxis, yaxis = yaxis, c2 = c2, c3 = c3, xlab = xlab, ylab = ylab, hline = hline, filename = filename, save = save){
  #browser()
  p <- ggplot(df, aes(x = {{xaxis}}, y = {{yaxis}}, text = paste(
    "Year: ", year, "\n",
    c2, abundance_med, "\n",
    c3, {{yaxis}}, "\n",  
    "Rank: ", rank,
    sep = ""
  )))
  p <- p + geom_bar(stat = "identity")
  p <- p + geom_hline(yintercept = hline)
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



#' Scatter2 - the purpose of this function is to create interactive scatter plots with a vline at the lowest biomass that gives large recruitment.  
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
#' SR_all <- Scatter2(df = cap, xaxis = biomass_med, yaxis = biomass_med_lead, c2 = "Biomass: ", c3 = "Recruitment: ", xlab = "Index (ktonnes)", ylab = "Recruitment (ktonnes)", vline = v90, filename = "figs/4-Biomass_all-index-recruit.pdf", save = "yes")

Scatter2 <- function(df = df, xaxis = xaxis, yaxis = yaxis, c2 = c2, c3 = c3, xlab = xlab, ylab = ylab, vline = vline, filename = filename, save = save){
  #browser()
  p <- ggplot(df, aes(x = {{xaxis}}, y = {{yaxis}}, text = paste(
    "Year: ", year, "\n",
    c2, {{xaxis}}, "\n",
    c3, {{yaxis}}, "\n",  
    sep = ""
  )))
  p <- p + geom_point()
  p <- p + scale_colour_continuous(type = "viridis")
  p <- p + xlab(xlab)
  p <- p + ylab(ylab)
  p <- p + geom_vline(xintercept = vline)
  p <- p + theme_bw()
  
  
  if(save == "yes"){
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