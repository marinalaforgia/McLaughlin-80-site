#Functional dispersion McLaughlin dataset

setwd("C:\\Users\\Stella\\Documents\\PostDoc_Susan\\McLaughlinForPostdoc")

library(reshape2)
library(plyr)
library(FD)
library(ggplot2)

Cover <- subset(read.csv("RawData\\Core_Community_Data2014.csv", header=T), Year>2005)
Cover <- Cover[,c("Year","Site","Quadrat","Species_Name","Cover")]
Abiotic <- read.csv("RawData\\Basic Abiotic Data.csv", header=T)[,c(2,3)]
colnames(Abiotic) <- c("Site", "Serpentine")
Cover <- merge(Cover, Abiotic, by="Site",all.x = T)
to.remove <- c("Rock","Bare","Quercus sp.","Cuscuta californica","Aegilops triuncialis", "Adenostoma fasciculatum",
               "Cercocarpus betuloides","Eriodictyon californicum","Holodiscus discolor","Pinus sabiniana","Quercus douglasii",
               "Quercus durata", "Quercus lobata","Salvia columbariae","Quercus sp.","Cuscuta californica")
Cover <- Cover[which(!Cover$Species_Name%in%to.remove),]#Take out abiotic and woody species and dodder

Cov.SiteMn <- ddply(Cover, .(Year,Site,Serpentine, Species_Name), summarize, MnCov = sum(Cover, na.rm=T)/5)

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

SpWithTrait <- unique(FuncT[which(FuncT$MatchS=="Yes"|FuncT$MatchNS=="Yes"),]$Species_Name)
SpWithoutTrait <- unique(Cov.SiteMnTrait$Species_Name[!Cov.SiteMnTrait$Species_Name%in%SpWithTrait])

Cov.SiteMnTrait$TraitCov <- ifelse(Cov.SiteMnTrait$Species_Name%in%SpWithTrait, Cov.SiteMnTrait$MnCov, NA)

#calculate trait coverage
FTTrait <- ddply(Cov.SiteMnTrait, .(Site, Year, Serpentine), summarize, FTPer = sum(TraitCov, na.rm=T)/sum(MnCov))
subset(FTTrait, FTPer <.80)#NO PLOTS IN THIS CATEGORY!!

#drop species with no traits
Cov.SiteMnTrait <- Cov.SiteMnTrait[which(!Cov.SiteMnTrait$Species_Name%in%SpWithoutTrait),]
Cov.SiteMnTrait$Species_Name <- factor(Cov.SiteMnTrait$Species_Name)


#Divide traits by soil
TraitsS <- unique(Cov.SiteMnTrait[which(Cov.SiteMnTrait$Serpentine=="S"),c("Species_Name","Height","SLA","LWC","CN","PerN")])
rownames(TraitsS) <- make.names(TraitsS$Species_Name, unique=T)
TraitsS <- TraitsS[,c(2:6)]

TraitsN <- unique(Cov.SiteMnTrait[which(Cov.SiteMnTrait$Serpentine=="N"),c("Species_Name","Height","SLA","LWC","CN","PerN")])
rownames(TraitsN) <- make.names(TraitsN$Species_Name, unique=T)
TraitsN <- TraitsN[,c(2:6)]

#divide cover by soil
Cov.SiteMnTraitS <- Cov.SiteMnTrait[which(Cov.SiteMnTrait$Serpentine=="S"),]
Cov.SiteMnTraitS$Species_Name <- factor(Cov.SiteMnTraitS$Species_Name) #drop names for other soil type
CoverwideS <- dcast(Cov.SiteMnTraitS[,c("Year","Site","MnCov","Species_Name")],
Year+Site~Species_Name, value.var="MnCov", drop=FALSE) 
CoverwideS[is.na(CoverwideS)] <- 0
colnames(CoverwideS) <- c("Year","Site",rownames(TraitsS))

Cov.SiteMnTraitN <- Cov.SiteMnTrait[which(Cov.SiteMnTrait$Serpentine=="N"),]
Cov.SiteMnTraitN$Species_Name <- factor(Cov.SiteMnTraitN$Species_Name)
CoverwideN <- dcast(Cov.SiteMnTraitN[,c("Year","Site","MnCov","Species_Name")],
                    Year+Site~Species_Name, value.var="MnCov", drop=FALSE) 
CoverwideN[is.na(CoverwideN)] <- 0
colnames(CoverwideN) <- c("Year","Site",rownames(TraitsN))

#write.table(CoverwideN, "NonSerpCoverWideforTraits_McL_012815.csv", col.names=T, row.names=F, sep=",")
#write.table(CoverwideS, "SerpCoverWideforTraits_McL_012815.csv", col.names=T, row.names=F, sep=",")
#write.table(TraitsN, "NonSerpTraits_McL_012815.csv", col.names=T, row.names=T, sep=",")
#write.table(TraitsS, "SerpTraits_McL_012815.csv", col.names=T, row.names=T, sep=",")

CoverwideN <- read.csv("Analysis\\Datafiles\\NonSerpCoverWideforTraits_McL_012815.csv", header=T)
CoverwideS <- read.csv("Analysis\\Datafiles\\SerpCoverWideforTraits_McL_012815.csv", header=T)
TraitsN <- read.csv("Analysis\\Datafiles\\NonSerpTraits_McL_012815.csv", header=T)
TraitsS <- read.csv("Analysis\\Datafiles\\SerpTraits_McL_012815.csv", header=T)

#Need function to subset dataset for Cover by year to calculate FD
abund14 <- CoverwideN[which(CoverwideN$Year=="2014"),-c(1:2)]
abund14.sp <- colSums(CoverwideN[which(CoverwideN$Year=="2014"),-c(1:2)])
present <- names(abund14.sp[abund14.sp>0])
abund14.2 <- abund14[,present]
rownames(abund14.2) <- make.names(CoverwideN[which(CoverwideN$Year=="2014"),]$Site, unique=T)
Traits14 <- TraitsN[rownames(TraitsN)[rownames(TraitsN)%in%present],]
dbFD(Traits14,abund14.2, w.abun = TRUE, calc.FRic = F, calc.CWM = F,calc.FDiv = F, corr="lingoes")$RaoQ


RaoFuncN <- function(x) #x is the cover dataset
      {x2 <- x[,-c(1:2)];
       abund.sp <- colSums(x2); present <- names(abund.sp[abund.sp>0]); abund <- x2[,present]; 
       rownames(abund) <- make.names(x$Site, unique=T);
       Traits.yr <- TraitsN[rownames(TraitsN)[rownames(TraitsN)%in%present],];
  dbFD(Traits.yr,  abund, w.abun = TRUE, calc.FRic = F, calc.CWM = F,calc.FDiv = F, corr="lingoes")$RaoQ}

#test2 <- ddply(CoverwideN[which(CoverwideN$Year=="2006"|CoverwideN$Year=="2007"),], .(Year), 
#    function(x) RaoQ = RaoFuncN(x))
#also works with apply
#test3 <- by(CoverwideN[which(CoverwideN$Year=="2006"|CoverwideN$Year=="2007"),], 
#            CoverwideN[which(CoverwideN$Year=="2006"|CoverwideN$Year=="2007"),"Year"],function(x) RaoFuncN(x))

RaoN <- ddply(CoverwideN, .(Year), function(x) RaoQ = RaoFuncN(x))
colnames(RaoN) <- c("Year", substring(colnames(RaoN)[-1], 2, 3)) 


RaoFuncS <- function(x) #x is the cover dataset
{x2 <- x[,-c(1:2)];
 abund.sp <- colSums(x2); present <- names(abund.sp[abund.sp>0]); abund <- x2[,present]; 
 rownames(abund) <- make.names(x$Site, unique=T);
 Traits.yr <- TraitsS[rownames(TraitsS)[rownames(TraitsS)%in%present],];
 dbFD(Traits.yr,  abund, w.abun = TRUE, calc.FRic = F, calc.CWM = F,calc.FDiv = F, corr="lingoes")$RaoQ}

RaoS <- ddply(CoverwideS, .(Year), function(x) RaoQ = RaoFuncS(x))
colnames(RaoS) <- c("Year", substring(colnames(RaoS)[-1], 2, 3)) 

Rao.all.raw <- cbind(RaoN, RaoS[,-1])
#write.table(Rao.all.raw, "Analysis\\Datafiles\\RawRaoQWide_01282014.csv" ,col.names=T, row.names=F, sep=",")


RaoN$MnN <- rowMeans(RaoN[,-1])
RaoS$MnS <- rowMeans(RaoS[,-1])

RaoMns <- merge(RaoN[,c("Year","MnN")],RaoS[,c("Year","MnS")], by="Year" )
#write.table(RaoMns , "Analysis\\Datafiles\\RaoQYrMeans_01282014.csv" ,col.names=T, row.names=F, sep=",")

Raolong <- melt(Rao.all.raw, id.vars="Year", variable.name="Site", value.name="RaoQ")
Raolong <- merge(Raolong, Abiotic, by="Site")
library(lme4)
Rao.mod <- lmer(RaoQ~Year*Serpentine + (1|Site), data=Raolong )
summary(Rao.mod)
car::Anova(Rao.mod)
fixef(Rao.mod)

Rao.modS <- lmer(RaoQ~Year + (1|Site), data=Raolong[which(Raolong$Serpentine=="S"),] )
summary(Rao.modS)
car::Anova(Rao.modS)
fixef(Rao.modS)

Rao.modNS <- lmer(RaoQ~Year + (1|Site), data=Raolong[which(Raolong$Serpentine=="N"),] )
summary(Rao.modNS)
car::Anova(Rao.modNS)
fixef(Rao.modNS)

source('C:\\Users\\Stella\\Documents\\PhD\\KlamathSisk\\TrientalisExperiment\\FieldSeason2012\\MicroR\\summarySE.r')
data.se <- summarySE(Raolong, measurevar="RaoQ", groupvars=c("Year","Serpentine"))
Disp <- ggplot(data.se , aes(x=Year, y=RaoQ), group=Serpentine, colour=Serpentine)
Disp <- Disp + geom_errorbar(aes(ymin=RaoQ-se, ymax=RaoQ+se,colour=Serpentine),position="identity", width=.2)
Disp <- Disp + geom_point(aes(color=Serpentine),size=6,  position="identity") 
Disp <- Disp + theme_bw(base_size=20) +ggtitle("McLaughlin 80 Sites: Year x Functional Dispersion x Soil Type")
Disp <- Disp + geom_smooth(method=lm, se=FALSE, aes(linetype=Serpentine, colour=Serpentine))
Disp <- Disp +  scale_x_continuous(breaks=seq(2000, 2014, 1)) + ylab("Functional Dispersion (Rao Q)")
Disp <- Disp +  annotate("text", x = 2012.5, y = 0.0212, size=6,
        label = "LMM results (site as random):\nYear = - 0.0002, p < 0.001\nSerpS = 0.5637, p = N.S.,\nYear x Serp = - 0.0003, p = 0.03")
ggsave(Disp, filename="Analysis\\Plots\\McLDispersion_012815.png", width=14, height=12, dpi=300)
