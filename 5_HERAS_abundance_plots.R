rm(list = ls())

### ABUNDANCE AT AGE PER YEAR PLOTS FOR REPORT

# required libraries for AbundanceAge/Year plots
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

path <- 'C:/git/HERAS/'

try(setwd(path),silent=TRUE)

# define paths
mainPath        <- file.path(".")
abuDataPath     <- file.path(".","data",'abundance')
strataDataPath  <- file.path(".","data",'strata')
rawDataPath     <- file.path(".",'data','raw_data')
functionPath    <- file.path(".","functions")
outPath         <- file.path(".","output")
figurePath      <- file.path(".","figures",'report')

scaling_factor <- 1.5

#read in data from different stocks
# format: Excel.csv (for now): Columns: Year, Age, Mio
NSAS  <- read.csv(file.path(abuDataPath,'NSAS_AbundanceAge.csv'))
WBSS  <- read.csv(file.path(abuDataPath,'WBSS_AbundanceAge.csv'))
WSAS  <- read.csv(file.path(abuDataPath,'WSAS_AbundanceAge.csv'))
MS    <- read.csv(file.path(abuDataPath,'MS_AbundanceAge.csv'))
SPR4  <- read.csv(file.path(abuDataPath,'SPR4_AbundanceAge.csv'))
SPR3  <- read.csv(file.path(abuDataPath,'SPR3_AbundanceAge.csv'))


# adding a year class to the tables
NSASa <- mutate(NSAS, yc =Year-Age)
WBSSa <- mutate(WBSS, yc =Year-Age)
WSASa <- mutate(WSAS, yc =Year-Age)
MSa   <- mutate(MS, yc =Year-Age)
SPR4a <- mutate(SPR4, yc =Year-Age)
SPR3a <- mutate(SPR3, yc =Year-Age)

NSASAgeClass <- c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7", "8" = "8", "9" = "9+") 
WBSSAgeClass <- c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7", "8" = "8+")
SPRAgeClass <- c("0" = "0", "1" = "1", "2" = "2", "3" = "3+")


# from here on, the codes plot the Abundance per Year class and year for different stocks

### NSAS
nNSAS <- length(unique(NSASa$yc))
PAIRED <- rep(brewer.pal(12,"Paired"),100)

currentSurveyYear <- max(NSASa$Year)

png(file.path(figurePath,paste0(currentSurveyYear,'_abundance_NSAS','.png')), width = 16*scaling_factor, height = 12*scaling_factor, units = "cm", res = 300, pointsize = 10)
p <- ggplot(NSASa,aes(Year,Mio,fill=factor(yc))) +
  theme_bw() +
  geom_bar(stat="identity") + 
  facet_grid(Age ~ .,scale="free_y", labeller = labeller(Age = NSASAgeClass)) +
  scale_fill_manual(values=PAIRED[1:nNSAS])  + 
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(1986, 2018, 2)) +
  # scale_y_continuous(breaks=seq(0, 25000, 5000)) +
  labs(
    #title = "NSAS Survey indices by age and year class", 
    x = NULL, 
    y = "n (Millions)") +
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=12, color="black", face="bold"),
         axis.ticks = element_line(size=1),
         axis.ticks.length=unit(.25, "cm"),
         strip.text.y = element_text(size = 12, colour = "black", face="bold",  angle=0),
         panel.grid = element_blank())

print(p)

dev.off()

### WBSS
nWBSS <- length(unique(NSASa$yc))
PAIRED <- rep(brewer.pal(12,"Paired"),100)

currentSurveyYear <- max(WBSSa$Year)

png(file.path(figurePath,paste0(currentSurveyYear,'_abundance_WBSS','.png')), width = 16*scaling_factor, height = 12*scaling_factor, units = "cm", res = 300, pointsize = 10)

p <- ggplot(WBSSa,aes(Year,Mio,fill=factor(yc))) +
  theme_bw() +
  geom_bar(stat="identity") + 
  facet_grid(Age ~ .,scale="free_y", labeller = labeller(Age = WBSSAgeClass)) +
  scale_fill_manual(values=PAIRED[1:nNSAS])  + 
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(1992, currentSurveyYear, 2)) +
  # scale_y_continuous(breaks=seq(0, 25000, 5000)) +
  labs(
    #title = "WBSSH: HERAS survey indices by age and year class", 
    x = NULL, 
    y = "n (Millions)") +
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=12, color="black", face="bold"),
         axis.ticks = element_line(size=1),
         axis.ticks.length=unit(.25, "cm"),
         strip.text.y = element_text(size = 12, colour = "black", face="bold",  angle=0),
         panel.grid = element_blank())

print(p)

dev.off()

### WOS
nWSAS <- length(unique(WSASa$yc))
PAIRED <- rep(brewer.pal(12,"Paired"),100)

currentSurveyYear <- max(WSASa$Year)

png(file.path(figurePath,paste0(currentSurveyYear,'_abundance_WS','.png')), width = 16*scaling_factor, height = 12*scaling_factor, units = "cm", res = 300, pointsize = 10)

p <- ggplot(WSASa,aes(Year,Mio,fill=factor(yc))) +
  theme_bw() +
  geom_bar(stat="identity") + 
  facet_grid(Age ~ .,scale="free_y", labeller = labeller(Age = NSASAgeClass)) +
  scale_fill_manual(values=PAIRED[1:nWSAS])  + 
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(1994, currentSurveyYear, 2)) +
  # scale_y_continuous(breaks=seq(0, 25000, 5000)) +
  labs(
    #title = "WBSSH: HERAS survey indices by age and year class", 
    x = NULL, 
    y = "n (Millions)") +
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=12, color="black", face="bold"),
         axis.ticks = element_line(size=1),
         axis.ticks.length=unit(.25, "cm"),
         strip.text.y = element_text(size = 12, colour = "black", face="bold",  angle=0),
         panel.grid = element_blank())
# scale_y_continuous(NULL,NULL)

print(p)

dev.off()

### MS
nMS <- length(unique(MSa$yc))
PAIRED <- rep(brewer.pal(12,"Paired"),100)

currentSurveyYear <- max(MSa$Year)

png(file.path(figurePath,paste0(currentSurveyYear,'_abundance_MS','.png')), width = 16*scaling_factor, height = 12*scaling_factor, units = "cm", res = 300, pointsize = 10)

p <- ggplot(MSa,aes(Year,Mio,fill=factor(yc))) +
  theme_bw() +
  geom_bar(stat="identity") + 
  facet_grid(Age ~ .,scale="free_y", labeller = labeller(Age = NSASAgeClass)) +
  scale_fill_manual(values=PAIRED[1:nMS])  + 
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(2008, currentSurveyYear, 1)) +
  # scale_y_continuous(breaks=seq(0, 25000, 5000)) +
  labs(
    #title = "WBSSH: HERAS survey indices by age and year class", 
    x = NULL, 
    y = "n (Millions)") +
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=12, color="black", face="bold"),
         axis.ticks = element_line(size=1),
         axis.ticks.length=unit(.25, "cm"),
         strip.text.y = element_text(size = 12, colour = "black", face="bold",  angle=0),
         panel.grid = element_blank())
# scale_y_continuous(NULL,NULL)

print(p)

dev.off()

### SPR4
nSPR4 <- length(unique(SPR4a$yc))
PAIRED <- rep(brewer.pal(12,"Paired"),100)

currentSurveyYear <- max(SPR4a$Year)

png(file.path(figurePath,paste0(currentSurveyYear,'_abundance_SPR4','.png')), width = 16*scaling_factor, height = 12*scaling_factor, units = "cm", res = 300, pointsize = 10)

p <- ggplot(SPR4a,aes(Year,Mio,fill=factor(yc))) +
  theme_bw() +
  geom_bar(stat="identity") + 
  facet_grid(Age ~ .,scale="free_y", labeller = labeller(Age = SPRAgeClass)) +
  scale_fill_manual(values=PAIRED[1:nSPR4])  + 
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(2004, currentSurveyYear, 1)) +
  # scale_y_continuous(breaks=seq(0,70000, 20000)) +
  labs(
    #title = "WBSSH: HERAS survey indices by age and year class", 
    x = NULL, 
    y = "n (Millions)") +
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=12, color="black", face="bold"),
         axis.ticks = element_line(size=1),
         axis.ticks.length=unit(.25, "cm"),
         strip.text.y = element_text(size = 12, colour = "black", face="bold",  angle=0),
         panel.grid = element_blank())
# scale_y_continuous(NULL,NULL)

print(p)

dev.off()

### SPR3a
nSPR3 <- length(unique(SPR3a$yc))
PAIRED <- rep(brewer.pal(12,"Paired"),100)

currentSurveyYear <- max(SPR3a$Year)

png(file.path(figurePath,paste0(currentSurveyYear,'_abundance_SPR3a','.png')), width = 16*scaling_factor, height = 12*scaling_factor, units = "cm", res = 300, pointsize = 10)

p <- ggplot(SPR3a,aes(Year,Mio,fill=factor(yc))) +
  theme_bw() +
  geom_bar(stat="identity") + 
  facet_grid(Age ~ .,scale="free_y", labeller = labeller(Age = SPRAgeClass)) +
  scale_fill_manual(values=PAIRED[1:nSPR3])  + 
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(2006, currentSurveyYear, 1)) +
  #scale_y_continuous(breaks=seq(0, 6000, 1000)) +
  labs(
    #title = "WBSSH: HERAS survey indices by age and year class", 
    x = NULL, 
    y = "n (Millions)") +
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=12, color="black", face="bold"),
         axis.ticks = element_line(size=1),
         axis.ticks.length=unit(.25, "cm"),
         strip.text.y = element_text(size = 12, colour = "black", face="bold",  angle=0),
         panel.grid = element_blank())
# scale_y_continuous(NULL,NULL)

print(p)

dev.off()
