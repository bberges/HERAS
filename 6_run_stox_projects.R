# script that derives the HERAS index directly from the StoX projects.
# !!! note 1 !!! There is a small missmatch in the NO index due to rounding error
# !!! note 2 !!! The official EU index is calculated based on manually edited superInd table. More specifically, some stock
# fields are allocated  manually. Note sure how it is done, I reached to Susan but no awnser to date.
# Here, there is an attempt to automate this process using age length information in nearby length bins.

rm(list=ls())

library(Rstox)
library(FLFleet)
library(ggplotFL)
library(mgcv)

#Set up directories

#path <- 'J:/git/HERAS/'
path <- 'C:/git/HERAS/'

try(setwd(path),silent=TRUE)

mainPath      <- file.path(".")
dataPath      <- file.path(".","data")
dataBigPath   <- 'D:/HERAS/data'
outPath       <- file.path(".","output")
functionPath  <- file.path(".","functions")

# parameters analysis
#surveyYearMat    <- c(2016,2017,2018,2019)
#surveyYearMat    <- c(2017,2018,2019)
surveyYearMat    <- c(2019)

nboot <- 500

# loop on the years
for(idxYear in 1:length(surveyYearMat)){
  surveyYear <- surveyYearMat[idxYear]
  
  StoXDataPath  <- file.path(dataBigPath,'StoX_projects',surveyYear)
  
  dataDirs      <- c(file.path(StoXDataPath,paste0('HERAS_',surveyYear,'_HER_EU')),
                     file.path(StoXDataPath,paste0('HERAS_',surveyYear,'_HER_NO')))
  for(idxDataDir in 1:length(dataDirs)){
    # create outputs
    runBaseline(projectName = dataDirs[idxDataDir], save = TRUE,exportCSV = TRUE,modelType = c('baseline'))
    runBaseline(projectName = dataDirs[idxDataDir], save = TRUE,exportCSV = TRUE,modelType = c('baseline-report'))
    
    # StoX bootstraping
    runBootstrap(projectName=dataDirs[idxDataDir],
                 bootstrapMethod="AcousticTrawl",
                 acousticMethod="PSU~Stratum",
                 bioticMethod="PSU~Stratum",
                 startProcess="TotalLengthDist",
                 endProcess="SuperIndAbundance",
                 nboot=nboot,
                 seed=1, cores=4)
    
    saveProjectData(dataDirs[idxDataDir])
  }
}
