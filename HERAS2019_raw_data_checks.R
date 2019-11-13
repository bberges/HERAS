rm(list = ls())

#### Packages ####
library(ggplot2)
library(mapplots) # draw.pie
data("coast") # coastlines
library(maps)
library(mapdata)
library(gdata)  # drop.levels
library(reshape) # melt and cast
library(gstat)
library(sp)

path <- 'C:/git/HERAS/'

try(setwd(path),silent=TRUE)

surveyYear    <- 2018
mainPath      <- file.path(".")
dataPath      <- file.path(".","data")
functionPath  <- file.path(".","functions")
reportPath    <- file.path(".","reports",surveyYear)
rawDataPath   <- file.path(".","data",surveyYear)
figurePath    <- file.path(".","figures",surveyYear)

source(file.path(functionPath,"load_ICESdB.R"))
source(file.path(functionPath,"plot_biotic.R"))
source(file.path(functionPath,"plot_acoustic.R"))
source(file.path(functionPath,"mk_report_ICESdB.R"))
source(file.path(functionPath,"mk_length_matrix.R"))

mkReport  <- TRUE
mkPlot    <- TRUE

# load species list
fileName <- 'species_codes_201911.csv'

speciesList <- read.csv(file.path(dataPath,fileName), fill = TRUE, header = TRUE)

# build directory list
dataDirs <- list.dirs(path = rawDataPath, full.names = TRUE, recursive = TRUE)
dataDirs <- dataDirs[2:length(dataDirs)]

# loop on directories (i.e. each country)
for(idxDir in dataDirs){
  fileMat <- list.files(path = idxDir,pattern = "\\.csv$")
  
  ### load data
  for(idxFile in fileMat){
    A <- load_ICESdb(idxDir,idxFile)
    
    header <- A[[1]]
    for(idxHeader in 2:length(A)){
      eval(parse(text = paste0(header[idxHeader-1],
                               '=A[[',
                               as.character(idxHeader),']]')))
    }
  }

  if(mkPlot){
    ### plot Biotic data
    plot_biotic(Cruise,
                Biology,
                Catch,
                Haul,
                speciesList,
                figurePath)
      
    ### plot Acoustic data
    plot_acoustic(Data,
                  Cruise,
                  Haul,
                  figurePath)
  }
  
  if(mkReport){
    # make report
    mk_report_ICESdB(Cruise,
                     Biology,
                     Catch,
                     Haul,
                     Data,
                     speciesList,
                     reportPath)
  }
}
