#combining traits with McLaughlin dataset

setwd("C:\\Users\\Stella\\Documents\\PostDoc_Susan\\McLaughlinForPostdoc")

library(reshape2)
library(plyr)

Cover <- read.csv("RawData\\Core_Community_Data2014.csv", header=T)
Cover <- Cover[,c("Year","Site","Quadrat","Species_Name","Cover")]
Abiotic <- read.csv("RawData\\Basic Abiotic Data.csv", header=T)[,c(2,3)]
colnames(Abiotic) <- c("Site", "Serpentine")
Cover <- merge(Cover, Abiotic, by="Site",all.x = T)
to.remove <- c("Rock","Bare","Quercus sp.","Cuscuta californica","Aegilops triuncialis", "Adenostoma fasciculatum",
               "Cercocarpus betuloides","Eriodictyon californicum","Holodiscus discolor","Pinus sabiniana","Quercus douglasii",
               "Quercus durata", "Quercus lobata","Salvia columbariae","Quercus sp.","Cuscuta californica")
Cover <- Cover[which(!Cover$Species_Name%in%to.remove),]#Take out abiotic and woody species and dodder

Cov.SiteMn <- ddply(Cover, .(Year,Site,Serpentine, Species_Name), summarize, MnCov = sum(Cover, na.rm=T)/5)
Cov.SiteMn[which(Cov.SiteMn$MnCov==0),]$MnCov <- NA
#for now, just interested in cover data

FuncT <- read.csv("RawData\\McL_80SitesSpeciesTraits_012615.csv",header=T)

TraitsS.1 <- FuncT[which(FuncT$MatchS=="Yes"),c(1:7)]
TraitsS.2 <- FuncT[which(FuncT$MatchS=="No"&FuncT$MatchNS=="Yes"),c(1:2,8:12)]
colnames(TraitsS.2) <- colnames(TraitsS.1)
TraitsS <- rbind(TraitsS.1, TraitsS.2)

TraitsN.1 <- FuncT[which(FuncT$MatchNS=="Yes"),c(1:2,8:12)]
TraitsN.2 <- FuncT[which(FuncT$MatchNS=="No"&FuncT$MatchS=="Yes"),c(1:2,3:7)]
colnames(TraitsN.2) <- colnames(TraitsN.1)
TraitsN <- rbind(TraitsN.1, TraitsN.2)

Cov.SiteMnS <- merge(Cov.SiteMn[which(Cov.SiteMn$Serpentine=="S"),],TraitsS,  by="Species_Name", all.x =T)
Cov.SiteMnNS <- merge(Cov.SiteMn[which(Cov.SiteMn$Serpentine=="N"),],TraitsN,  by="Species_Name", all.x =T)
colnames(Cov.SiteMnS) <- c(colnames(Cov.SiteMnS)[1:6], "Height", "SLA", "LWC", "CN", "PerN")
colnames(Cov.SiteMnNS) <- colnames(Cov.SiteMnS)
Cov.SiteMnTrait <- rbind(Cov.SiteMnS, Cov.SiteMnNS)
Cov.SiteMnTrait2 <- merge(Cov.SiteMnTrait, FuncT[,c("Species_Name","Genus","Species","Family","Native.Exotic",
                        "Grass.Forb.Shrub","Annual.Perennial","Monocot.Dicot")], by="Species_Name",all.x=T)
#write.table(Cov.SiteMnTrait2, "Analysis\\Datafiles\\McLCoverTraits_022415.csv", col.names=T, row.names=F, sep=",")

