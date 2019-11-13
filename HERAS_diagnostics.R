rm(list = ls())

#### Packages ####
library(ggplot2)


path <- 'C:/git/HERAS/'

try(setwd(path),silent=TRUE)

surveyYear    <- 2017
mainPath      <- file.path(".")
dataPath      <- file.path(".","data")
functionPath  <- file.path(".","functions")
reportPath    <- file.path(".","reports",surveyYear)
rawDataPath   <- file.path(".","data",surveyYear)
figurePath    <- file.path(".","figures",surveyYear)