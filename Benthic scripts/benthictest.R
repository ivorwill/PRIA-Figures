### PRIMNM Report - Benthic Cover Time Series Comparison ###
# this script plots benthic cover time series using historical REA data and TDS data to compare differences in results
# written by: morgan winston 10/26/2017

### INITIALIZATION ###
# set working directory
setwd("T:/Benthic/Ecological Assessment/PRIA Monitoring Report/PRIA_historicalestimates")
# load required packages
library(ggplot2)
library(stats)
library(reshape2)
library(tidyr)
library(grid)
library(gridExtra)
library(plyr)
library(dplyr)
fmt <- function(){
  f <- function(x) as.character(round(x,2))
  f
}

##### TDS data:
TDS_Data <- read.csv("T:/Benthic/Ecological Assessment/PRIA Monitoring Report/TDS/Benthic cover/TDS_benthiccover_mid.csv")
  # calculate total algae column
  TDS_Data[is.na(TDS_Data)] <- 0
  TDS_Data$TOT_ALGAE_MID <- TDS_Data$ALGAE_MID + TDS_Data$MACROALGAE_MID
  TDS_Data$ALGAE_MID <- NULL
  TDS_Data$MACROALGAE_MID <- NULL
  # convert from wide to long format
  TDS_Data <-gather(TDS_Data, benthic_cat, perc_cover, CORAL_MID:TOT_ALGAE_MID)
  means <- aggregate(list(perc_cover = TDS_Data$perc_cover), by = list(islacode = TDS_Data$ISLAND, year = TDS_Data$OBS_YEAR,
            diveid = TDS_Data$DIVEID, benthic_cat = TDS_Data$benthic_cat), FUN = mean)
  means_2 <- aggregate(list(perc_cover = means$perc_cover), by = list(islacode = means$islacode, year = means$year,
              benthic_cat = means$benthic_cat), FUN = mean)

##### REA data:
REA_Data <- read.csv("T:/Benthic/Ecological Assessment/PRIA Monitoring Report/PRIA_historicalestimates/pria0512_benthiccover_estimates_FRFmid.csv")


plot_fun_timeseries_coldens <- function(dat){
  
  isllist = unique(dat$isl)
  for(l in c(1:length(isllist))){
  
  dat2 <- dat[ which(dat$isl == isllist[l]),]
  plot_line <- ggplot(dat2, aes(x = year, y = avdns, group = 1, fill = data, color = data)) +
          geom_point(size = 2, shape = 23, color = "black") +
          geom_line(size = 1) +
            theme_bw() +
            theme(
              plot.background = element_blank()
              ,panel.grid.major = element_blank()
              ,panel.grid.minor = element_blank()
              #,legend.position="bottom"
              ,legend.title = element_blank()
            ) + 
            #scale_x_discrete(expand = c(0.05, 0.05)) + scale_y_continuous(labels = fmt()) + 
            theme(text = element_text(size=10)) +
            geom_errorbar(aes(ymin=avdns - se_dns, ymax=avdns + se_dns),width=.15, position=position_dodge(.9), color = "black") + 
            ylab(expression(bold(Density~~"(no. colonies"*~m^"-2"*")"))) + xlab(expression(bold("Year"))) + 
            scale_color_manual(values=c("firebrick2", "dodgerblue4"), labels = c("Fixed Sites", "StRS")) +
            scale_fill_manual(values=c("firebrick2", "dodgerblue4"), labels = c("Fixed Sites", "StRS")) +
            scale_x_continuous( breaks = seq(min(dat$year), max(dat$year),2))
  
  plot_bar <- ggplot(dat2, aes(x = year, y = avdns, fill = data)) + 
    geom_bar(position=position_dodge(), stat="identity", color="black") + 
    theme_bw() +
    theme(
      plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
      #,legend.position="bottom"
      ,legend.title = element_blank()
    ) + 
    #scale_x_discrete(expand = c(0.05, 0.05)) + scale_y_continuous(labels = fmt()) + 
    theme(text = element_text(size=10)) +
    geom_errorbar(aes(ymin=avdns - se_dns, ymax=avdns + se_dns),width=.15, position=position_dodge(.9), color = "black") + 
    ylab(expression(bold(Density~~"(no. colonies"*~m^"-2"*")"))) + xlab(expression(bold("Year"))) + 
    scale_fill_manual(values=c("firebrick2", "dodgerblue4"), labels = c("Fixed Sites", "StRS")) +
    scale_x_continuous(breaks = seq(min(dat$year), max(dat$year),2)) +
    coord_cartesian(ylim = c(0, 1.05*(dat2$avdns+dat2$se_dns)), xlim = c(min(dat2$year-1),max(dat2$year)+1), expand = F)

    
  setwd(paste("T:/Benthic/Ecological Assessment/PRIA Monitoring Report/PRIA_historicalestimates/Coral Density Time Series Figures/", isllist[l], sep = ""))
  
  png(paste(isllist[l], "_coldens_timeseries_line.png", sep = "_"), width = 8, height = 8, units = "in", res = 300)
  print(plot_line)
  dev.off()
  
  png(paste(isllist[l], "_coldens_timeseries_bar.png", sep = "_"), width = 8, height = 8, units = "in", res = 300)
  print(plot_bar)
  dev.off()
 
  }
}

