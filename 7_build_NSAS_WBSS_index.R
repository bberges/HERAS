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

# read split proportions
split_prop  <- read.csv(file.path(dataPath,'HERAS_split_prop.csv'))

source(file.path(functionPath,"fill_missing_species.R"))

# parameters analysis
#surveyYearMat    <- c(2016,2017,2018,2019)
#surveyYearMat    <- c(2017,2018,2019)
surveyYearMat    <- c(2019)

nboot <- 10

# parameters HERAS index
minYear <- 1989
maxYear <- 2019
yearVec <- minYear:maxYear
nYears  <- length(yearVec)
minAge  <- 0
maxAge  <- 9
ageVec  <- minAge:maxAge
nAges   <- length(ageVec)
nIter   <- 1

# initialise FLIndex object
FLR_HERAS_EU_NO <- FLQuant( array(-1,dim = c(nAges,nYears,1,1,2,nIter)),
                            dimnames=list(age=as.character(ageVec),
                                          year=as.character(yearVec),
                                          unit=c("1e6"),
                                          season="all", 
                                          area=c('EU','NO')),
                            units='1e6')
FLR_HERAS_EU_NO <- FLIndex(name = 'HERAS_EU_NO', 
                           desc = 'HERAS index from EU and NO StoX projects', 
                           index=FLR_HERAS_EU_NO)

FLR_HERAS <- FLQuant( array(-1,dim = c(nAges,nYears,1,1,1,nIter)),
                      dimnames=list(age=as.character(ageVec),
                                    year=as.character(yearVec),
                                    unit="1e6",
                                    season="all", 
                                    area='all'),
                      units='1e6')
FLR_HERAS <- FLIndex( name = 'HERAS_all', 
                      desc = 'HERAS index (total)', 
                      index=FLR_HERAS)

HERAS.NSAS <- FLIndices(FLR_HERAS_EU_NO, FLR_HERAS)
HERAS.WBSS <- FLIndices(FLR_HERAS_EU_NO, FLR_HERAS)

# loop on the years
for(idxYear in 1:length(surveyYearMat)){
  surveyYear <- surveyYearMat[idxYear]
  
  StoXDataPath  <- file.path(dataBigPath,'StoX_projects',surveyYear)
  
  dataDirs      <- c(file.path(StoXDataPath,paste0('HERAS_',surveyYear,'_HER_EU')),
                     file.path(StoXDataPath,paste0('HERAS_',surveyYear,'_HER_NO')))
  for(idxDataDir in 1:length(dataDirs)){
    projectName       <- strsplit(dataDirs[idxDataDir],'/')
    projectName       <- projectName[[1]][length(projectName[[1]])]
    projectComponent  <- strsplit(projectName,'_')
    projectComponent  <- projectComponent[[1]][length(projectComponent[[1]])]
    # create outputs
    #runBaseline(projectName = dataDirs[idxDataDir], save = TRUE,exportCSV = TRUE,modelType = c('baseline'))
    #runBaseline(projectName = dataDirs[idxDataDir], save = TRUE,exportCSV = TRUE,modelType = c('baseline-report'))
    # read outputs
    #currentBaseLine       <- getBaseline(dataDirs[idxDataDir],save = FALSE,exportCSV = FALSE,modelType = c('baseline'))
    currentBaseLineReport <- getBaseline(dataDirs[idxDataDir],save = FALSE,exportCSV = FALSE,modelType = c('baseline-report'))
    
    endTab                <- currentBaseLineReport$outputData$FillMissingData
    endTab[is.na(endTab)] <- '-' # replace NA with '-'
    
    # if NO StoX project, we use the split table
    # if EU StoX project, we use the stage field
    
    #######################################
    ############# NO project ##############
    #######################################
    if(projectComponent == 'NO'){ 
    
      uniqueStrata <- unique(endTab$Stratum)
      
      # initialize array nStrata x nAges
      indexHERASComponent.NSAS <- array(0, dim=c(length(uniqueStrata),nAges))
      indexHERASComponent.WBSS <- array(0, dim=c(length(uniqueStrata),nAges))
      
      # split numbers at age per strata (WBSS/NSAS)
      for(idxStrata in 1:length(uniqueStrata)){
        strataCurrent <- uniqueStrata[idxStrata]
        
        # loop on all the index ages (1-9+) with 9 as plus group
        for(idxAges in 1:nAges){
          
          # select age to be computed, make sure we combine ages for the plus group
          if(idxAges == nAges){
            # select ages as plut grounp
            ageSel <- unique(endTab[endTab$Stratum == strataCurrent,]$age)[unique(endTab[endTab$Stratum == strataCurrent,]$age) >= ageVec[idxAges]]
          }else{
            # select current age
            ageSel <- ageVec[idxAges]
          }
          # filter split proportion table
          split_propFilt  <- split_prop[split_prop$year == surveyYear & split_prop$strata == strataCurrent,]
            
          indexTemp.NSAS <- array(0,dim=c(length(ageSel),1))
          indexTemp.WBSS <- array(0,dim=c(length(ageSel),1))
          # loop on all ages available in the StoX object
          for(idxAgeSel in 1:length(ageSel)){
            # if age range within the split data
            if(ageSel[idxAgeSel] <= max(split_propFilt$age) & ageSel[idxAgeSel] >= min(split_propFilt$age)){
              # filter superInd table
              endTabFilt      <- endTab[endTab$Stratum == strataCurrent & endTab$age %in% ageSel[idxAgeSel],]
              # set abundance to 0 if no number
              if(dim(endTabFilt)[1] == 0){
                abundance <- 0
              }else{
                abundance <- sum(endTabFilt$Abundance)
              }
              
              # calculate split numbers
              indexTemp.NSAS[idxAgeSel] <- sum(abundance)*split_propFilt$prop_NSAS[split_propFilt$age == ageSel[idxAgeSel]]
              indexTemp.WBSS[idxAgeSel] <- sum(abundance)*split_propFilt$prop_WBSS[split_propFilt$age == ageSel[idxAgeSel]]
            }else if(ageSel[idxAgeSel] > max(split_propFilt$age)){ # if age range greater than the split data we take the older age in the split table
              # calculate split numbers
              endTabFilt      <- endTab[endTab$Stratum == strataCurrent & endTab$age %in% ageSel[idxAgeSel],]
              # set abundance to 0 if no number
              if(dim(endTabFilt)[1] == 0){
                abundance <- 0
              }else{
                abundance <- sum(endTabFilt$Abundance)
              }
                
              indexTemp.NSAS[idxAgeSel] <- sum(abundance)*split_propFilt$prop_NSAS[split_propFilt$age == max(split_propFilt$age)]
              indexTemp.WBSS[idxAgeSel] <- sum(abundance)*split_propFilt$prop_WBSS[split_propFilt$age == max(split_propFilt$age)]
            }else if(ageSel[idxAgeSel] < min(split_propFilt$age)){
              # calculate split numbers
              endTabFilt      <- endTab[endTab$Stratum == strataCurrent & endTab$age %in% ageSel[idxAgeSel],]
              # set abundance to 0 if no number
              if(dim(endTabFilt)[1] == 0){
                abundance <- 0
              }else{
                abundance <- sum(endTabFilt$Abundance)
              }
              
              indexTemp.WBSS[idxAgeSel] <- sum(abundance)*split_propFilt$prop_NSAS[split_propFilt$age == min(split_propFilt$age)]
              indexTemp.NSAS[idxAgeSel] <- sum(abundance)*split_propFilt$prop_WBSS[split_propFilt$age == min(split_propFilt$age)]
            }
          }
          indexHERASComponent.NSAS[idxStrata,idxAges] <- sum(indexTemp.NSAS)
          indexHERASComponent.WBSS[idxStrata,idxAges] <- sum(indexTemp.WBSS)
        }
      }
      
      # fill in FLR object with index in millions
      HERAS.NSAS$HERAS_EU_NO@index[,ac(surveyYear),,,projectComponent] <- colSums(indexHERASComponent.NSAS)*1e-6
      HERAS.WBSS$HERAS_EU_NO@index[,ac(surveyYear),,,projectComponent] <- colSums(indexHERASComponent.WBSS)*1e-6
    }
    
    #######################################
    ############# EU project ##############
    #######################################
    if(projectComponent == 'EU'){ # if NO project
      # fill in missing stock fields
      endTab <- fill_missing_species(endTab)
      
      # initialize array nStrata x nAges
      indexHERASComponent.NSAS <- array(0, dim=c(nAges,1))
      indexHERASComponent.WBSS <- array(0, dim=c(nAges,1))
      
      # loop on all the index ages (1-9+) with 9 as plus group
      for(idxAges in 1:nAges){
        
        # select age to be computed, make sure we combine ages for the plus group
        if(idxAges == nAges){
          # select ages as plut grounp
          ageSel <- unique(endTab$age)[unique(endTab$age) >= ageVec[idxAges]]
        }else{
          # select current age
          ageSel <- ageVec[idxAges]
        }
        
        endTabFilt.NSAS  <- endTab[ endTab$age %in% ageSel &
                                    endTab$stage == 'her-47d3',]
        endTabFilt.WBSS  <- endTab[ endTab$age %in% ageSel &
                                    endTab$stage == 'her-3a22',]
        
        indexHERASComponent.NSAS[idxAges] <- sum(endTabFilt.NSAS$Abundance)
        indexHERASComponent.WBSS[idxAges] <- sum(endTabFilt.WBSS$Abundance)
      }
      HERAS.NSAS$HERAS_EU_NO@index[,ac(surveyYear),,,projectComponent] <- indexHERASComponent.NSAS*1e-6
      HERAS.WBSS$HERAS_EU_NO@index[,ac(surveyYear),,,projectComponent] <- indexHERASComponent.WBSS*1e-6
    }
    
    # read bootstraping data
    #r <- getReports(dataDirs[idxDataDir])
  }
}

# combine NO and EU components
HERAS.NSAS$HERAS_all@index[] <- areaSums(HERAS.NSAS$HERAS_EU_NO@index)
HERAS.WBSS$HERAS_all@index[] <- areaSums(HERAS.WBSS$HERAS_EU_NO@index)
