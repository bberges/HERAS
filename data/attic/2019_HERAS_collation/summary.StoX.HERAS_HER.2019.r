rm(list=ls())

#Set up directories

#directories to extract  data from 
#StoXDir<- "C:/Users/Lusseaus/workspace/stox/project/" #Directory path where StoX workspace sits
StoXDir<- "D:/HERAS/data/StoX_projects/2019/" #Directory path where StoX workspace sits

#ProjectName <-  "01 Post-cruise - HERAS_2018_HER"
#ProjectName <-  "02 HERAS_2018_HER"
ProjectName <-  "HERAS_2019_HER_EU"
#ProjectName <-  "6aSPAWN_2019_Pathway_HER"


# directory for results
#pathOut<-"C:/Users/Lusseaus/Documents/Acoustics/NorthSea Acoustic surveys/1 HERAS COORDINATION/HERAS Analysis"
pathOut<-"C:/git/HERAS/data/attic/2019_HERAS_collation/"

# Create WD name and set working directory to the project
StoXbaseline<-paste(StoXDir,ProjectName,"/output/baseline/report",sep="")

setwd(StoXbaseline)

#read in data from StoX outputs
num <- read.delim("1_FillMissingData_SuperIndividuals.txt",stringsAsFactors = FALSE)

#calculate biomass

#missing weight?
summary(num$weight)
idx <- which(num$weight=="-") #blank weigths
#if so proceed with care - and either remove or change to NA before proceeding:
num$weight[idx]<-"NA"

num$biomass<-as.numeric(as.character(num$Abundance))*as.numeric(as.character(num$weight))
idx<-which(is.na(num$biomass))
num[idx,] #all NA from where weight is NA. 

#Aggregating by stock, length, age and maturity:

#aggregate at chosen level.
abun<-aggregate(num$Abundance, list(num$Stratum, num$length ,num$age, num$specialstage, num$stage), sum)
names(abun)<-c("strata","length","age","maturity","stock","number")

#same for biomass
biom<-aggregate(num$biomass, list(num$Stratum, num$length ,num$age, num$specialstage, num$stage), sum)
names(biom)<-c("strata","length","age","maturity","stock","biomass_g")


#--------------------------------------------------------------------------------

#combine into one
all<-abun
all$biomass_g<- biom$biomass_g
all$meanW_g<-all$biomass/all$number


# Output to csv file
write.csv(all,paste(pathOut,"/",ProjectName,"2.csv",sep=""),row.names = FALSE)

################--  END  --##################################################

