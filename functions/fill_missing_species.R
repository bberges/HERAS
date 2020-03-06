fill_missing_species <- function(endTab){
  
  outTab <- endTab
  
  # find the cells to fill
  idxFillMat <- which(endTab$stage == '-')
  
  for(idxFill in idxFillMat){
    if(!(endTab[idxFill,]$specialstage %in% c('MAT','IMM'))){
      endTabFilt <- endTab[   endTab$Stratum == endTab[idxFill,]$Stratum &
                                endTab$age == endTab[idxFill,]$age &
                                endTab$LenGrp >= endTab[idxFill,]$LenGrp-2 &
                                endTab$LenGrp <= endTab[idxFill,]$LenGrp+2,]
      
      prob_NSAS <- sum(endTabFilt$Abundance[endTabFilt $stage == 'her-47d3'])/sum(endTabFilt$Abundance)
      prob_WBSS <- sum(endTabFilt$Abundance[endTabFilt $stage == 'her-3a22'])/sum(endTabFilt$Abundance)
    }else{
      endTabFilt <- endTab[   endTab$Stratum == endTab[idxFill,]$Stratum &
                              endTab$specialstage == endTab[idxFill,]$specialstage &
                              endTab$age == endTab[idxFill,]$age &
                              endTab$LenGrp >= endTab[idxFill,]$LenGrp-2 &
                              endTab$LenGrp <= endTab[idxFill,]$LenGrp+2,]
      
      prob_NSAS <- sum(endTabFilt$Abundance[endTabFilt $stage == 'her-47d3'])/sum(endTabFilt$Abundance)
      prob_WBSS <- sum(endTabFilt$Abundance[endTabFilt $stage == 'her-3a22'])/sum(endTabFilt$Abundance)
    }
    if(prob_NSAS > prob_WBSS){
      outTab$stage[idxFill] <- 'her-47d3'
    }else if(prob_WBSS > prob_NSAS){
      outTab$stage[idxFill] <- 'her-3a22'
    }else{
      outTab$stage[idxFill] <- 'her-47d3'
    }
  }
  
  return(outTab)
}