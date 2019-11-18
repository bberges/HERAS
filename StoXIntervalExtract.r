
######################################################################
# Script to extract SA values from StoX project for summaries and plotting
# Extracts Interval lat and lon and SA for each interval in project
# Extracts transect assignment from XML StoX process file
# Combines the info 
####################################################################

# Load required libraries
library(plyr)
library("XML")

# directory for results
pathOut<-"C:/Users/Lusseaus/Documents/Acoustics/NorthSea Acoustic surveys/1 HERAS COORDINATION/HERAS Analysis/"

# Set up directories to extract  data from 
 StoXDir<- "C:/Users/Lusseaus/workspace/stox/project/" #Directory path where StoX workspace sits

# Project name - one at a time only can be extracted
# ProjectName<-   "CLYDAS_2017_HER"
ProjectName<-   "HERAS_2018_HER"

# Create WD names
StoXbaseline <- paste(StoXDir,ProjectName,"/output/baseline/data/",sep="")
StoXProcess <- paste(StoXDir,ProjectName,"/process",sep="")

#--------------------------------------------------

## Extract Transect allocations from StoX project process data XML file
setwd(StoXProcess)

xml.file <- "project.xml"
data <- xmlParse(xml.file, useInternalNodes = TRUE)
xml_data <- xmlToList(data)
edsu <- ldply(xml_data$processdata$edsupsu , data.frame)
colnames(edsu) <- c("Unit","Transect","SampleUnit")
edsu$SampleUnit<- as.character(edsu$SampleUnit)

log <- unlist(strsplit(as.character(edsu$SampleUnit),"/"))
logpos <- seq(2,length(log),4)
cruisepos <- seq(1,length(log),4)
logs <- as.numeric(log[logpos])
cruise <- log[cruisepos]
transect <- as.character(edsu$Transect)
edsu <- as.data.frame(cbind(cruise,logs,transect))
names(edsu) <- c("Cruise","LOG","Transect") 
edsu$Transect <-as.character (edsu$Transect)
edsu$ID <- paste(edsu$Cruise,"_",edsu$LOG,sep="")


### get interval data summed over depth channels
#enter the filenames to use
nascfile<-  "4_NASC_NASC.txt"
logfile<-"3_FilterAcoustic_AcousticData_DistanceFrequency.txt"

#setwd to project baseline
setwd(StoXbaseline)

# process the data for that survey
spp_data_int <- read.table(nascfile,header=TRUE)
spp_data_int <- spp_data_int[c("SampleUnit","NASC")]
log <- unlist(strsplit(as.character(spp_data_int$SampleUnit),"/"))
logpos <- seq(2,length(log),4)
cruisepos <- seq(1,length(log),4)
logs <- as.numeric(log[logpos])
cruise <- log[cruisepos]
nasc <- spp_data_int$NASC
nascinfo <- as.data.frame(cbind(cruise,logs,nasc))
names(nascinfo) <- c("Cruise","LOG","SA")
nascinfo$SA<-as.numeric(as.character(nascinfo$SA))
nascinfo$ID <- paste(nascinfo$Cruise,"_",nascinfo$LOG,sep="")
loginfo <- read.table(logfile,sep="\t",header=TRUE)
loginfo <- loginfo[c("cruise","log_start","lon_start","lat_start")]
names(loginfo) <- c("Cruise","LOG","ACLON","ACLAT")
loginfo <- loginfo[!duplicated(loginfo),]
loginfo$ID <- paste(loginfo$Cruise,"_",loginfo$LOG,sep="")
spp_data_int <- merge(loginfo, nascinfo, by="ID", all.x = TRUE)
spp_data_int <- merge(spp_data_int, edsu, by="ID", all.x = TRUE)
spp_data_int$SA[is.na(spp_data_int$SA)] <- 0
spp_data_int <- spp_data_int[order(spp_data_int$Cruise.x, as.numeric(spp_data_int$LOG.x)),]
spp_data_int<-spp_data_int[,c(2,3,4,5,8,11)]
names(spp_data_int) <- c("Cruise","LOG","ACLON","ACLAT","SA","Transect")

write.csv(spp_data_int,paste(pathOut,"mergedNASCS9_HER2018.csv",sep=""),row.names = F)

##### END #####




