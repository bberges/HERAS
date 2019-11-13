plot_biotic <- function(Cruise,Biology,Catch,Haul,speciesList){
  
  idxMatch <- match(c('HER', 'SPR'),speciesList$SPECIESID)
  
  idxFiltBio <- as.numeric(as.character(Biology$CatchSpeciesCode)) %in% speciesList$WORMS[idxMatch]
  Biology <- Biology[idxFiltBio,]
  
  idxFiltCatch <- as.numeric(as.character(Catch$CatchSpeciesCode)) %in% speciesList$WORMS[idxMatch]
  Catch <- Catch[idxFiltCatch,]

  country <- as.character(Cruise$CruiseCountry)
  
  uniqueSpecies <- as.numeric(as.character(unique(Biology$CatchSpeciesCode)))
  nSpecies  <- length(uniqueSpecies)
  
  uniqueSpeciesName <- as.character(speciesList$SPECIESNAME[match(uniqueSpecies,speciesList$WORMS)])
  uniqueSpeciesIDStr <- as.character(speciesList$SPECIESID[match(uniqueSpecies,speciesList$WORMS)])
  
  #### Step 1.1 overall Length frequencies ####
  for(idxSpecies in 1:nSpecies){
    currentSpecies  <- uniqueSpecies[idxSpecies]
    BiologyTemp     <- Biology[Biology$CatchSpeciesCode == currentSpecies,]
    
    png(file.path(figurePath,paste0('biotic_',country,'_lengthfrequency_all_',uniqueSpeciesIDStr[idxSpecies],'.png')), width = 16, height = 12, units = "cm", res = 300, pointsize = 10)
    
    hist(as.numeric(as.character(BiologyTemp$BiologyLengthClass))*1e-1,
         breaks = 0:50,
         ylab = 'Count (#)',
         xlab = 'Length (cm)',
         main = uniqueSpeciesName[idxSpecies])
    
    dev.off()
  }
  
  #### Step 1.2 Length frequencies for each haul ####
  for(idxSpecies in 1:nSpecies){
    currentSpecies  <- uniqueSpecies[idxSpecies]
    BiologyTemp     <- Biology[Biology$CatchSpeciesCode == currentSpecies,]
    haulUnique      <- as.numeric(as.character(unique(BiologyTemp$HaulNumber)))
    haulUnique      <- haulUnique[haulUnique %in% as.numeric(as.character(Haul$HaulNumber))]
    haulUnique      <- haulUnique[order(haulUnique)]
    nHauls          <- length(haulUnique)
    
    png(file.path(figurePath,paste0('biotic_',country,'_lengthfrequency_haul_',
                                    uniqueSpeciesIDStr[idxSpecies],
                                    '.png')), 
        width = 16, 
        height = 12, 
        units = "cm", 
        res = 300, 
        pointsize = 10)
    
    opar <- par(mfrow = n2mfrow(nHauls), mar = c(2,2,1,1), oma = c(2,2,3,0))
    
    for(idxHaul in 1:nHauls){
      BiologyTempHaul <- BiologyTemp[BiologyTemp$HaulNumber == haulUnique[idxHaul],]
      BiologyTempHaul$BiologyLengthClass
      hist(as.numeric(as.character(BiologyTempHaul$BiologyLengthClass))*1e-1,
           breaks = 0:50,
           ylab = 'Count (#)',
           xlab = 'Length (cm)',
           main = paste0('Haul ', haulUnique[idxHaul]%%1000)) # haulUnique[idxHaul]-min(haulUnique)+1,'-',uniqueSpeciesIDStr[idxSpecies]
      legend("topleft", legend = paste0('N=',length(BiologyTempHaul$BiologyLengthClass)),bty = "n") 
    }
    dev.off()
  }
  
  uniqueSpecies <- as.numeric(as.character(unique(Biology$CatchSpeciesCode)))
  nSpecies  <- length(uniqueSpecies)
  
  uniqueSpeciesName <- as.character(speciesList$SPECIESNAME[match(uniqueSpecies,speciesList$WORMS)])
  uniqueSpeciesIDStr <- as.character(speciesList$SPECIESID[match(uniqueSpecies,speciesList$WORMS)])
  
  
  #### Step 1.3 - length-weight relationship ####
  for(idxSpecies in 1:nSpecies){
    currentSpecies  <- uniqueSpecies[idxSpecies]
    BiologyTemp     <- Biology[Biology$CatchSpeciesCode == currentSpecies,]
    
    lengthInd <- as.numeric(as.character(BiologyTemp$BiologyLengthClass))*1e-1
    weightInd <- as.numeric(as.character(BiologyTemp$BiologyIndividualWeight))
    
    lengthInd <- lengthInd[which(!is.na(weightInd))]
    weightInd <- weightInd[which(!is.na(weightInd))]
    
    if(length(weightInd) != 0 && length(lengthInd != 0)){
      png(file.path(figurePath,paste0('biotic_',country,'_weight_length_',uniqueSpeciesIDStr[idxSpecies],'.png')), width = 16, height = 12, units = "cm", res = 300, pointsize = 10)
      
      
      plot(x = lengthInd,
           y = weightInd,
           xlab = "length (cm)",
           ylab = "weight (gr)", 
           main = paste0("Length vs weight - ",uniqueSpeciesName[idxSpecies]))
      
      dev.off()
    }
  }
  
  #### Step 1.4 age distribution for each haul ####
  for(idxSpecies in 1:nSpecies){
    currentSpecies  <- uniqueSpecies[idxSpecies]
    BiologyTemp     <- Biology[Biology$CatchSpeciesCode == currentSpecies,]
    haulUnique      <- as.numeric(as.character(unique(BiologyTemp$HaulNumber)))
    haulUnique      <- haulUnique[order(haulUnique)]
    nHauls          <- length(haulUnique)
    
    png(file.path(figurePath,paste0('biotic_',country,'_age_distribution_haul_',uniqueSpeciesIDStr[idxSpecies],'.png')), width = 16, height = 12, units = "cm", res = 300, pointsize = 10)
    
    opar <- par(mfrow = n2mfrow(nHauls), mar = c(2,2,1,1), oma = c(2,2,3,0))
    
    for(idxHaul in 1:nHauls){
      BiologyTempHaul <- BiologyTemp[BiologyTemp$HaulNumber == haulUnique[idxHaul],]
      
      ageInd <- as.numeric(as.character(BiologyTempHaul$BiologyIndividualAge))
      
      ageInd    <- ageInd[which(!is.na(ageInd))]
      
      ageInd[ageInd > 10] <- 10
      
      hist(ageInd,
           breaks = (0-0.5):(10+0.5),
           ylab = 'Count (#)',
           xlab = 'age',
           main = paste0('Haul ', haulUnique[idxHaul]%%1000))# -min(haulUnique)+1,'-',uniqueSpeciesIDStr[idxSpecies]
      legend("topleft", legend = paste0('N=',length(BiologyTempHaul$BiologyLengthClass)),bty = "n") 
    }
    dev.off()
  }
  
  #### Step 1.5 - length - age relationship ####
  for(idxSpecies in 1:nSpecies){
    currentSpecies  <- uniqueSpecies[idxSpecies]
    BiologyTemp     <- Biology[Biology$CatchSpeciesCode == currentSpecies,]
    
    lengthInd <- as.numeric(as.character(BiologyTemp$BiologyLengthClass))*1e-1
    ageInd <- as.numeric(as.character(BiologyTemp$BiologyIndividualAge))
    
    lengthInd <- lengthInd[which(!is.na(ageInd))]
    ageInd    <- ageInd[which(!is.na(ageInd))]
    
    if(length(lengthInd) != 0 && length(ageInd != 0)){
      png(file.path(figurePath,paste0('biotic_',country,'_age_length_',uniqueSpeciesIDStr[idxSpecies],'.png')), width = 16, height = 12, units = "cm", res = 300, pointsize = 10)
      
      
      plot(x = lengthInd,
           y = ageInd,
           xlab = "length (cm)",
           ylab = "age (wr)", 
           main = paste0("age vs length -",uniqueSpeciesName[idxSpecies]))
      
      dev.off()
    }
  }
  
  #### Step 1.6 - Age distribution  ####
  for(idxSpecies in 1:nSpecies){
    currentSpecies  <- uniqueSpecies[idxSpecies]
    BiologyTemp     <- Biology[Biology$CatchSpeciesCode == currentSpecies,]
    
    ageInd    <- as.numeric(as.character(BiologyTemp$BiologyIndividualAge))
    ageInd    <- ageInd[which(!is.na(ageInd))]
    
    if(length(ageInd != 0)){
      png(file.path(figurePath,paste0('biotic_',country,'_age_all_',uniqueSpeciesIDStr[idxSpecies],'.png')), width = 16, height = 12, units = "cm", res = 300, pointsize = 10)
      
      hist(ageInd,
           breaks = (min(ageInd)-0.5):(max(ageInd)+0.5),
           ylab = 'Count (#)',
           xlab = 'age (wr)',
           main = uniqueSpeciesName[idxSpecies])
      
      dev.off()
    }
  }
  
  #### Step 1.7 - Catch proportions SPR and HER  ####
  haulNumbers <- as.numeric(as.character(Catch$HaulNumber))
  
  dfCatch <- data.frame(  station=character(length = dim(Catch)[1]), 
                          catch=double(length = dim(Catch)[1]), 
                          lon=double(length = dim(Catch)[1]),
                          lat=double(length = dim(Catch)[1]),
                          species=character(length = dim(Catch)[1]),
                          stringsAsFactors = FALSE)
  
  for(idxHaul in 1:dim(dfCatch)[1]){
    currentSpecies <- as.numeric(as.character(Catch$CatchSpeciesCode[idxHaul]))
    b <- speciesList$WORMS == currentSpecies
    b[is.na(b)] <- FALSE
    boolHaul <- Haul$HaulNumber == Catch$HaulNumber[idxHaul]
    
    dfCatch$station[idxHaul]  <- haulNumbers[idxHaul]-min(haulNumbers)+1
    dfCatch$catch[idxHaul]    <- as.numeric(as.character(Catch$CatchSpeciesCategoryWeight[idxHaul]))/1e-3
    dfCatch$lon[idxHaul]      <- as.numeric(as.character(Haul$HaulStartLongitude[boolHaul]))
    dfCatch$lat[idxHaul]      <- as.numeric(as.character(Haul$HaulStartLatitude[boolHaul]))
    dfCatch$species[idxHaul]  <- as.character(speciesList$SPECIESID[b])
  }
  
  #dfCatch <- drop.levels(dfCatch[dfCatch$species %in% c('HER', 'SPR'),])
  
  hh <- make.xyz(x = dfCatch$lon, y = dfCatch$lat, z = dfCatch$catch, group = dfCatch$species)
  
  #opar <- par(mfrow = c(1,1), mar = c(2,2,1,1), oma = c(2,2,3,0))
  png(file.path(figurePath,paste0('biotic_',country,'_catch_prop.png')), width = 16, height = 12, units = "cm", res = 300, pointsize = 10)
  
  map('worldHires', 
      col = "green", 
      fill = TRUE, 
      xlim = c(min(dfCatch$lon)-2, 
               max(dfCatch$lon)+2), 
      ylim = c(min(dfCatch$lat)-2, 
               max(dfCatch$lat)+2))
  box()
  axis(side = 2, las = 2)
  draw.pie(x = hh$x, y = hh$y, z = hh$z, col = rainbow(10),radius=0.3)
  legend.pie(min(hh$x)-1, max(hh$y)+.25, labels = dimnames(hh$z)[[2]], col = rainbow(10), radius = 0.2, bty = "n")
  
  dev.off()
}