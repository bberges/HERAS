#HERRING age by country and stratum

rm(list = ls())

library(maps)
library(mapdata)
library(PBSmapping)
library(lattice)

# NB - make sure you have correct strata
strata <- read.table("d:/sild/iesns_postcruise/2017/r/stratum_iesns2017.txt", sep = ",", header = TRUE)
str(strata)
strata$PID <- as.numeric(substr(strata$stratum,8,8))
for(i in 1:5){ # Assumes strata 1-5 in Norwegian sea (i.e. excluding Barents Sea)
  strata$POS[strata$PID == i] <- seq(1:length(strata$PID[strata$PID == i]))
}

png("D:\\Sild\\iesns_postcruise\\2019\\r\\strata_eydna.png", width = 3600, height = 3600, res = 300, pointsize = 18)

map('worldHires', xlim = c(-22,25), ylim = c(60,77), mar = c(0,0,0,0))

for(i in 1:5){
  points(strata$X[strata$PID == i], strata$Y[strata$PID == i], type = "l", col = as.numeric(strata$PID[strata$PID == i]))
}

strata$EID <- seq(1,length(strata$X),1)
strata <- as.PolySet(strata)

her <- read.table("d:/sild/iesns_postcruise/2019/r/iesns2019_pgnapes_her_age_comparison.csv", sep = ",", header = TRUE)
names(her) <- tolower(names(her))
str(her)
names(her) <- c("country", "cruise", "X", "Y", "station", "recnr", "species", "length", "age")
str(her)

her$EID <- seq(1,length(her$X),1)
her <- as.EventData(her)

###### koplað stratum uppá einkultfiskar
print(findPolys(her, strata))
herstrata_pre <- findPolys(her, strata)
table(herstrata_pre$Bdry)  #no duplicates
herstrata <- merge(her,herstrata_pre, by = "EID", all.x = TRUE, all.y = FALSE, incomparables = NA)
str(herstrata)

for(i in 1:5){
  points(herstrata$X[herstrata$PID == i], herstrata$Y[herstrata$PID == i], type = "p", col = i)
}

dev.off()

table(paste(herstrata$country,herstrata$station), herstrata$PID)

####### Age composition comparison

table(herstrata$country[!is.na(herstrata$age)], herstrata$PID[!is.na(herstrata$age)])


write.table(herstrata, "d:/sild/iesns_postcruise/2019/r/herstrata.csv", row.names = F, col.names = T, sep = ",")

herstrata <- read.table("d:/sild/iesns_postcruise/2019/r/herstrata.csv", sep = ",", header = T) 
str(herstrata)

herstrata$country <- as.character(herstrata$country)
herstrata$country[herstrata$country == 'DK'] <- 'EU'
herstrata$country <- factor(herstrata$country)

tapply(herstrata$age, INDEX = list(herstrata$country, herstrata$age, herstrata$PID), FUN = length)


png("d:/sild/iesns_postcruise/2019/r/iesns2019_her_agedistribution_comparison.png", width = 20, height = 12, units = "cm", res = 300, pointsize = 10)

opar <- par(mfrow = c(4,5), mar = c(2,2,1,1), oma = c(2,2,2,0))
for(i in c('NO', 'EU', 'FO', 'IS')){
  for(y in 1:5){
    
dd <- herstrata[herstrata$country == i & herstrata$PID == y,]
(aldur <- as.matrix(0:20))

(abyti <- tapply(dd$age, dd$age, length))

(abyti2 <- merge(aldur,abyti, by.x = "V1", by.y = "row.names", all.x = TRUE))
(abyti3 <- as.data.frame(abyti2[,-1]))
(abyti3$aldur <- 0:20)
(names(abyti3) <- c("tal", "aldur"))

(abyti3$prosent <- abyti3$tal/sum(abyti3$tal, na.rm = TRUE)*100)
if(sum(abyti3$tal, na.rm = TRUE) < 10) {
  abyti3$prosent <- 0
}

mp <- barplot(abyti3$prosent, axes = FALSE, xlim = c(1,23), ylim = c(0,65), axisnames = FALSE)
axis(side = 1, labels = c(0,5,10,15), at = c(mp[1], mp[6], mp[11], mp[16]))
axis(side = 1, labels = FALSE, at = mp)
abline(v = c(mp[6],mp[11],mp[16]), lty = 2, col = "darkgrey")
#abline(v = mp[11], lty = 2, col = "darkgrey")
#abline(v = mp[16], lty = 2, col = "darkgrey")
box()
axis(side = 2, labels = c("20%", "40%", "60%"), at = c(20,40,60), las = 2)
legend(0,65, legend = paste(as.character(i), "Stratum", as.character(y)), bty = "n") 
legend(0,50, legend = paste("n = ", as.character(sum(abyti3$tal, na.rm = TRUE))), bty = "n") 
}
}
mtext(side = 3, outer = TRUE, text = "Age-distribution of herring IESNS 2019 - comparison by vessel and stratum")
dev.off()

### mean length at age comparison

litkoda <- cbind(c('NO', 'IS', 'EU', 'FO'), c("blue", "green", "red", "orange"))

png("D:\\Sild\\iesns_postcruise\\2019\\R\\iesns2019_her_length_at_age_comparison.png", width = 3600, height = 3600, res = 300, pointsize = 18)

opar <- par(mfrow = c(2,2), oma = c(1,1,2,1))

for(i in 1:4){
  plot(-1,-1, xlim = range(herstrata$age[herstrata$PID == i], na.rm = TRUE), ylim = range(herstrata$length[herstrata$PID == i], na.rm = TRUE), axes = FALSE, xlab = "", ylab = "")
  axis(side = 1, labels = seq(2,18,2), at = seq(2,18,2))
  axis(side = 2, labels = c(10, 20, 30), at = c(10,20,30), las = 2)
  box()

for(y in c('NO', 'IS', 'EU', 'FO')){
  agegroups <- sort(unique(herstrata$age[herstrata$country == y & herstrata$PID == i]))
  meanlength <- tapply(X = herstrata$length[herstrata$country == y & herstrata$PID == i], INDEX = herstrata$age[herstrata$country == y & herstrata$PID == i], FUN = mean, na.rm = TRUE)
  selength <- tapply(X = herstrata$length[herstrata$country == y & herstrata$PID == i], INDEX = herstrata$age[herstrata$country == y & herstrata$PID == i], FUN = sd, na.rm = TRUE)/
    sqrt(tapply(X = herstrata$length[herstrata$country == y & herstrata$PID == i], INDEX = herstrata$age[herstrata$country == y & herstrata$PID == i], FUN = length))
  
  points(agegroups, meanlength, type = "b", col = litkoda[,2][litkoda[,1] == y])
arrows(x0 = agegroups, y0 = meanlength - selength, x1 = agegroups, y1 = meanlength + selength, 
       angle = 90, length = 0.02, code = 3, col = litkoda[,2][litkoda[,1] == y])
}
mtext(side = 3, paste("Stratum", as.character(i)))
}
legend("bottomright", c('NO', 'IS', 'EU', 'FO'), text.col = c("blue", "green", "red", "orange"))
mtext(side = 3, outer = TRUE, text = "Herring length-at-age IESNS 2019")
par(opar)

dev.off()

