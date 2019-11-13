rm(list = ls())

#### Packages ####
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

mainPath      <- file.path(".")
rawDataPath   <- file.path(".","data/raw_data/")
dataPath      <- file.path(".","data/")
figurePath    <- file.path(".","figures/raw_data/")
reportPath    <- file.path(".","reports/")
functionPath  <- file.path(".","functions/")

source(file.path(functionPath,"load_ICESdB.R"))
source(file.path(functionPath,"plot_biotic.R"))
source(file.path(functionPath,"plot_acoustic.R"))
#source(file.path(functionPath,"forecastScenarios.r"))
#source(file.path(functionPath,"forecastFunctions.r"))

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

  ### plot Biotic data
  plot_biotic(Cruise,Biology,Catch,Haul,speciesList,figurePath)
  
  ### plot Acoustic data
  plot_acoustic(Data,Cruise,Haul,figurePath)
  
  # make report
}
