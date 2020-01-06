rm(list = ls())

######################################################################
# Scripts plot extracted NASC values and further nice stuff
####################################################################

# required libraries for Area and NASC plots
library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(mapdata)
library(rgdal)
library(broom)
#library(marmap)

path <- 'C:/git/HERAS/'

try(setwd(path),silent=TRUE)

# define paths
mainPath        <- file.path(".")
strataDataPath  <- file.path(".","data",'strata')
rawDataPath     <- file.path(".",'data','raw_data')
functionPath    <- file.path(".","functions")
outPath         <- file.path(".","output")
figurePath      <- file.path(".","figures",'report')

surveyYearMat   <- c(2018,2019)
species         <- 'HER'

scaling_factor <- 1.5

for(idxYear in 1:length(surveyYearMat)){
  raw_data    <- load(file.path(rawDataPath,surveyYearMat[idxYear],paste0(surveyYearMat[idxYear],'_HERAS.Rdata')))
  # read in merged NASC
  NASC_HER    <- read.csv(file.path(outPath,paste0('HERAS_',surveyYearMat[idxYear],'_',species,'_table.csv')))
  #NASC_HER    <- read.csv(file.path(dataPath,'HERAS2018_HER_5nmi.csv'))
  #NASC_SPR    <- read.csv(file.path(dataPath,'HERAS2018_SPR_5nmi.csv'))
  #TRANSECTS   <- read.csv(file.path(dataPath,'mergedNASCS9_HER2018.csv'))
  
  if(surveyYearMat[idxYear] == 2019){
    levels(CruiseAll$CruiseLocalID) <- c(levels(CruiseAll$CruiseLocalID),"2019207")
    CruiseAll$CruiseLocalID[CruiseAll$CruiseCountry == 'NO'] <- "2019207" 
  } else if(surveyYearMat[idxYear] == 2018){
    levels(CruiseAll$CruiseLocalID) <- c(levels(CruiseAll$CruiseLocalID),"2018207")
    CruiseAll$CruiseLocalID[CruiseAll$CruiseCountry == 'NO'] <- "2018207"
  }
  
  uniqueCruise <- as.character(unique(NASC_HER$Cruise))
  
  for(idxCruise in uniqueCruise){
    NASC_HER$Country[NASC_HER$Cruise == idxCruise] <- as.character(CruiseAll$CruiseCountry[CruiseAll$CruiseLocalID == idxCruise])
  }
  
  # read in Strata-Shapefile
  HERAS_Strata    <-readOGR(strataDataPath,'HERAS2017_StoX_polygons') #set to wd where you stored your shp-files
  MSAS_Strata     <-readOGR(strataDataPath,'MSHAS2017_polygon') #set to wd where you stored your shp-files
  
  #convert shapefile into dataframe to be used by ggplot2
  HERAS_Strata_df <- tidy(HERAS_Strata)
  MSHAS_Strata_df <- tidy(MSAS_Strata)
  
  # split dataset
  
  #Remove zero values (empty intervals) from HER and SPR NASC dataset (enhances visual style for plotting empty EDSUs later)
  NASC_HERZRM <- filter(NASC_HER, SA>0)
  #NASC_SPRZRM <- filter(NASC_SPR, SPR>0)
  
  #Identify zero values (empty intervals) from HER and SPR NASC dataset
  NASC_HER0 <- filter(NASC_HER, SA==0)
  #NASC_SPR0 <- filter(NASC_SPR, SPR ==0)
  
  # Area Map
  Area <- map_data("worldHires", region =c("Norway", "Germany", "Denmark", "Netherlands", "Belgium", "Ireland", "Sweden", "UK"))
  
  # AREA AND TRANSECT LINES PLOT
  
  png(file.path(figurePath,paste0(surveyYearMat[idxYear],'_log_distance','.png')), width = 16*scaling_factor, height = 12*scaling_factor, units = "cm", res = 300, pointsize = 10)
  # this plots the position of EACH log distance sampled per participant
  p <- ggplot(NASC_HER, aes(longitude, latitude)) +
    theme_bw()+
    geom_polygon(data=Area, aes(long, lat, group=group))+
    geom_point(aes(colour=Country))+
    geom_polygon(data=HERAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red", fill=NA)+
    geom_polygon(data=MSHAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red", fill=NA)+
    coord_quickmap(xlim=c(-12.0,12.5), ylim=c(51.5,62))+
    scale_x_continuous(breaks=seq(-12,12,2))+
    scale_y_continuous(breaks=seq(51,62,1), sec.axis=dup_axis(name=NULL, labels=NULL))+
    labs(x = "Longitude °E", 
         y = "Latitude °N")+
    theme(legend.justification=c(0,1), legend.position=c(0,1))+
    theme(legend.background = element_rect(size=0.3, linetype="solid",colour ="black"))+
    theme (axis.title = element_text(size=16, face="bold"),
           axis.text = element_text(size=14, color="black", face="bold"),
           legend.title = element_text(size=14, face="bold"),
           legend.text = element_text(size=12, face="bold"))
  
  print(p)
  
  dev.off()
  
  # this plots intervals (5nmi) and strata from HERAS 2018 with colour codes for transect spacing
  # per stratum
  
  png(file.path(figurePath,paste0(surveyYearMat[idxYear],'_transect_spacing','.png')), width = 16*scaling_factor, height = 12*scaling_factor, units = "cm", res = 300, pointsize = 10)
  HERAS_Strata <- read.csv(file.path(strataDataPath,'HERAS_Strata2018.csv'))
  MSHAS_Strata <- read.csv(file.path(strataDataPath,'MSHAS_Strata2018.csv'))
  
  SpacingColors <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  p <- ggplot(NASC_HER, aes(longitude, latitude)) +
    theme_bw()+
    geom_polygon(data=Area, aes(long, lat, group=group))+
    geom_polygon(data=HERAS_Strata, aes(x = long, y = lat, group = group, fill=SpacingLevel), colour="black")+
    geom_polygon(data=MSHAS_Strata, aes(x = long, y = lat, group = group, fill=SpacingLevel), colour="black")+
    scale_fill_manual(values =SpacingColors, 
                      name="Transect\nSpacing\n(nmi)", 
                      labels=c("10", "13","15","17.5","23","25","30","ZigZag"))+
    geom_point(color="black", size=0.01)+
    coord_quickmap(xlim=c(-12.0,12.5), ylim=c(51.5,62))+
    scale_x_continuous(breaks=seq(-12,12,2))+
    scale_y_continuous(breaks=seq(51,62,1), sec.axis=dup_axis(name=NULL, labels=NULL))+
    labs(x = "Longitude °E", 
         y = "Latitude °N")+
    theme(legend.justification=c(0,1), legend.position=c(0,1))+
    theme(legend.background = element_rect(size=0.3, linetype="solid",colour ="black"))+
    theme (axis.title = element_text(size=16, face="bold"),
           axis.text = element_text(size=14, color="black", face="bold"),
           legend.title = element_text(size=14, face="bold"),
           legend.text = element_text(size=12, face="bold"))
  
  print(p)
  
  dev.off()
  
  # NASC BUBBLE PLOTS
  
  png(file.path(figurePath,paste0(surveyYearMat[idxYear],'_bubble_plot_',species,'.png')), width = 16*scaling_factor, height = 12*scaling_factor, units = "cm", res = 300, pointsize = 10)
  
  # Herring
  p <- ggplot(NASC_HERZRM, aes(longitude, latitude)) +
    theme_bw()+
    geom_polygon(data=Area, aes(long, lat, group=group))+
    geom_point(data=NASC_HER0, aes(longitude, latitude), size=0.1, shape=3, col="darkgrey")+
    geom_point(aes(size = SA),alpha = 0.5, colour = "darkgreen")+
    scale_size_area(max_size = 20, breaks = c(500, 1000, 2500, 5000, 10000), name=bquote('NASC'~(m^2~nm^-2)))+
    geom_path(data=HERAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red")+
    geom_path(data=MSHAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red")+
    coord_quickmap(xlim=c(-12.0,12.5), ylim=c(51.5,62))+
    scale_x_continuous(breaks=seq(-12,12,2))+
    scale_y_continuous(breaks=seq(51,62,1), sec.axis=dup_axis(name=NULL, labels=NULL))+
    labs(
      #title = "HERAS Herring mean NASC per 5 nmi EDSU", 
      x = "Longitude °E", 
      y = "Latitude °N")+
    theme(legend.justification=c(0,1), legend.position=c(0,1))+
    theme(legend.background = element_rect(size=0.3, linetype="solid",colour ="black"))+
    theme (axis.title = element_text(size=16, face="bold"),
           axis.text = element_text(size=14, color="black", face="bold"),
           legend.title = element_text(size=14, face="bold"),
           legend.text = element_text(size=12, face="bold"))
  
  print(p)
  
  dev.off()
  
  
  ### Abundance of Mature and Immature Herring as BUBBLE PLOTS (NASC as proxy)
  
  # split dataset
  #Select only Mature and Immature herring from the dataset (enhances visual style for plotting empty EDSUs later)
  MAT_HERZRM <- filter(NASC_HER, MAT>0)
  IMMAT_HERZRM <- filter(NASC_HER, IMM>0)
  
  #Identify zero values for both MAT and IMMAT Herring
  MAT0 <- filter(NASC_HER, MAT==0)
  IMMAT0 <- filter(NASC_HER, IMM ==0)
  
  # Abundance BUBBLE PLOTS
  
  png(file.path(figurePath,paste0(surveyYearMat[idxYear],'_mature_plot_',species,'.png')), width = 16*scaling_factor, height = 12*scaling_factor, units = "cm", res = 300, pointsize = 10)
  # MATURE Herring
  p <- ggplot(MAT_HERZRM, aes(longitude, latitude)) +
    theme_bw()+
    geom_polygon(data=Area, aes(long, lat, group=group))+
    geom_point(data=MAT0, aes(longitude, latitude), size=0.1, shape=3, col="darkgrey")+
    geom_point(aes(size = MAT),alpha = 0.5, colour = "darkred")+
    scale_size_area(max_size = 15, breaks = c(25, 50, 100, 250, 500), name=bquote('Number'~(millions~nm^-2)))+
    geom_path(data=HERAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red")+
    geom_path(data=MSHAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red")+
    coord_quickmap(xlim=c(-12.0,12.5), ylim=c(51.5,62))+
    scale_x_continuous(breaks=seq(-12,12,2))+
    scale_y_continuous(breaks=seq(51,62,1), sec.axis=dup_axis(name=NULL, labels=NULL))+
    labs(
      #title = "HERAS Herring mean NASC per 5 nmi EDSU", 
      x = "Longitude °E", 
      y = "Latitude °N")+
    theme(legend.justification=c(0,1), legend.position=c(0,1))+
    theme(legend.background = element_rect(size=0.3, linetype="solid",colour ="black"))+
    theme (axis.title = element_text(size=16, face="bold"),
           axis.text = element_text(size=14, color="black", face="bold"),
           legend.title = element_text(size=14, face="bold"),
           legend.text = element_text(size=12, face="bold"))
  
  print(p)
  
  dev.off()
  
  png(file.path(figurePath,paste0(surveyYearMat[idxYear],'_immature_plot_',species,'.png')), width = 16*scaling_factor, height = 12*scaling_factor, units = "cm", res = 300, pointsize = 10)
  
  # IMMATURE Herring
  p <- ggplot(IMMAT_HERZRM, aes(longitude, latitude)) +
    theme_bw()+
    geom_polygon(data=Area, aes(long, lat, group=group))+
    geom_point(data=IMMAT0, aes(longitude, latitude), size=0.1, shape=3, col="darkgrey")+
    geom_point(aes(size = IMM),alpha = 0.5, colour = "cyan4")+
    scale_size_area(max_size = 15, breaks = c(25, 50, 100, 250, 500), name=bquote('Number'~(millions~nm^-2)))+
    geom_path(data=HERAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red")+
    geom_path(data=MSHAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red")+
    coord_quickmap(xlim=c(-12.0,12.5), ylim=c(51.5,62))+
    scale_x_continuous(breaks=seq(-12,12,2))+
    scale_y_continuous(breaks=seq(51,62,1), sec.axis=dup_axis(name=NULL, labels=NULL))+
    labs(
      #title = "HERAS Herring mean NASC per 5 nmi EDSU", 
      x = "Longitude °E", 
      y = "Latitude °N")+
    theme(legend.justification=c(0,1), legend.position=c(0,1))+
    theme(legend.background = element_rect(size=0.3, linetype="solid",colour ="black"))+
    theme (axis.title = element_text(size=16, face="bold"),
           axis.text = element_text(size=14, color="black", face="bold"),
           legend.title = element_text(size=14, face="bold"),
           legend.text = element_text(size=12, face="bold"))  
  
  print(p)
  
  dev.off()

}

