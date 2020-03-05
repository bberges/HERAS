rm(list=ls())

library(Rstox)

#Set up directories

#path <- 'J:/git/HERAS/'
path <- 'C:/git/HERAS/'

try(setwd(path),silent=TRUE)

mainPath      <- file.path(".")
#dataPath      <- file.path(".","data")
outPath       <- file.path(".","output")
functionPath  <- file.path(".","functions")

dataPath <- 'Z:/HERAS/data'

#surveyYearMat    <- c(2016,2017,2018,2019)
#surveyYearMat    <- c(2017,2018,2019)
surveyYearMat    <- c(2019)

nboot <- 10

for(idxYear in 1:length(surveyYearMat)){
  surveyYear <- surveyYearMat[idxYear]
  
  StoXDataPath  <- file.path(dataPath,'StoX_projects',surveyYear)
  
  dataDirs      <- c(file.path(StoXDataPath,paste0('HERAS_',surveyYear,'_HER_EU')),
                     file.path(StoXDataPath,paste0('HERAS_',surveyYear,'_HER_NO')))
  for(idxDataDir in 1:length(dataDirs)){
    runBaseline(projectName = dataDirs[idxDataDir], save = TRUE,exportCSV = TRUE,modelType = c('baseline'))
    runBaseline(projectName = dataDirs[idxDataDir], save = TRUE,exportCSV = TRUE,modelType = c('baseline-report'))
    currentBaseLine       <- getBaseline(dataDirs[idxDataDir],save = TRUE,exportCSV = TRUE,modelType = c('baseline'))
    currentBaseLineReport <- getBaseline(dataDirs[idxDataDir],save = TRUE,exportCSV = TRUE,modelType = c('baseline-report'))
    
    runBootstrap(projectName=dataDirs[idxDataDir],
                 bootstrapMethod="AcousticTrawl",
                 acousticMethod="PSU~Stratum",
                 bioticMethod="PSU~Stratum",
                 startProcess="TotalLengthDist",
                 endProcess="SuperIndAbundance",
                 nboot=nboot,
                 seed=1, cores=4)
    
    r <- getReports(dataDirs[idxDataDir])
    
    saveProjectData(dataDirs[idxDataDir])
  }
}
