#Seed bank dataset - analysis (accumulation index) and adding in seed weight
setwd("C:/Users/Stella/Documents/PostDoc_Susan/Seedbank/Analysis/Datafiles")
plots <- "C:/Users/Stella/Documents/PostDoc_Susan/Seedbank/Datafiles/Plots"

list.files()
library(reshape2)
library(plyr)
library(ggplot2)

sd.mass <- read.csv("KewSeedMass_Marko061914.csv", header=T)
sd.mass.mn <- ddply(sd.mass, .(id), summarize, sd.mass.mn = mean(seed_mass_g))

sb.count <- read.csv("SeedbankMcLaughlin_COUNT_20150126.csv", header=T)
sb.ra <- read.csv("SeedbankMcLaughlin_RelativeAbundance_20150126.csv", header=T)
comm.data <- read.csv("Core_Community_Data2014.csv", header=T)

sb.countlong <- melt(sb.count,id.vars="Site", variable.name="id", value.name="Count")
sb.countlong$Genus <- as.character(lapply(strsplit(as.character(sb.countlong$id), split="_"),"[", 1))
sb.countlong$SpeciesVar <- as.character(lapply(strsplit(as.character(sb.countlong$id), split="_"),"[", 2))                                  

periodnames <- unique(sb.countlong$SpeciesVar[grep("[.]", sb.countlong$SpeciesVar)])
sb.countlong[which(sb.countlong$SpeciesVar%in%periodnames),]$SpeciesVar <-
paste(as.character(lapply(strsplit(as.character(sb.countlong[which(sb.countlong$SpeciesVar%in%periodnames),]$SpeciesVar), split="[.]"),"[", 1)),
      as.character(lapply(strsplit(as.character(sb.countlong[which(sb.countlong$SpeciesVar%in%periodnames),]$SpeciesVar), split="[.]"),"[", 2)),sep="-")
sb.countlong[which(sb.countlong$id=="Astragalus_rattanii_var._jepsonianus"),]$SpeciesVar <- "rattanii var. jepsonianus"
sb.countlong[which(is.na(sb.countlong$SpeciesVar)),]$SpeciesVar <- "sp."
sb.countlong$Species_Name <- paste(sb.countlong$Genus, sb.countlong$SpeciesVar, sep=" ")

#check for name consistency
unique(sb.countlong$Species_Name[!sb.countlong$Species_Name%in%comm.data$Species_Name])
#all genus 
unique(comm.data$Species_Name[!comm.data$Species_Name%in%sb.countlong$Species_Name])
#Take a look at these
miss <- unique(comm.data$Species_Name[!comm.data$Species_Name%in%sb.countlong$Species_Name])
subset(comm.data, Species_Name%in%miss[c(1,4:7)])

#substitute Nemophila sp for Nemophila heterophylla
comm.data[which(comm.data$Species_Name=="Nemophila sp"),]$Species_Name <- "Nemophila heterophylla"

#take out abiotic
comm.data <- comm.data[which(!comm.data$Species_Name=="Bare"&!comm.data$Species_Name=="Rock"),]
#leaves only four new species: "Pinus sabiniana"  "Bromus japonicus" "Quercus sp."      "Centaurea sp." all post 2012

#reduce counts to presence...
sb.countlong2 <- sb.countlong[which(sb.countlong$Count>0),c("Site","id","Count","Species_Name")]

#calculate relative abundance in the seedbank = # of individuals / total seedlings
sb.countlong2 <- merge(sb.countlong2, ddply(sb.countlong2, .(Site), summarize, Tot.ab = sum(Count)), by="Site", all.x =T)
sb.countlong2$RA.sb <- sb.countlong2$Count/sb.countlong2$Tot.ab

#plot frequency in seedbank = # of plots species occurs in
plotfreq.SB <- ddply(sb.countlong2, .(Species_Name), summarize, plot.freqsb = length(unique(Site)))
#plot frequency in aboveground dataset by year
plotfreq.AB <- ddply(comm.data, .(Year, Species_Name), summarize, plot.freqab = length(unique(Site)))
plotfreq.AB.mn <- ddply(plotfreq.AB, .(Species_Name), summarize, plot.freqab.allyrmn = sum(plot.freqab)/15)
plotfreq.AB.mnpre2013 <- ddply(plotfreq.AB[which(plotfreq.AB$Year<2013),], .(Species_Name), 
                               summarize, plot.freqab.pre2013 = sum(plot.freqab)/13)
plotfreq.AB.mnpost2012 <- ddply(plotfreq.AB[which(plotfreq.AB$Year>2012),], .(Species_Name), 
                               summarize, plot.freqab.post2012 = sum(plot.freqab)/2)
AB1 <- merge(plotfreq.AB.mn, plotfreq.AB.mnpre2013, by="Species_Name", all=T)
AB2 <- merge(AB1, plotfreq.AB.mnpost2012, by="Species_Name", all=T)
AB2[is.na(AB2)] <- 0

unique(AB2$Species_Name[!AB2$Species_Name%in%plotfreq.SB$Species_Name])#lots of these
unique(plotfreq.SB$Species_Name[!plotfreq.SB$Species_Name%in%AB2$Species_Name]) #few of these
#maybe take out poorly identified species (low occurrence anyway...)
NotSp <- c("Unknown grass", "Vulpia sp.","Juncus sp.","Calochortus sp.","Bromus sp.")
plotfreq.SB[which(plotfreq.SB$Species_Name%in%NotSp),] # just four occurrences
NotSpData <- sb.countlong[which(sb.countlong$Species_Name%in%NotSp&sb.countlong$Count>0),] 
#Bromus in site 65 unclear, both B. diandrus and hordaceous noted in seedling count already
#comm.data[which(comm.data$Species_Name%in%comm.data$Species_Name[grep("Bromus", comm.data$Species_Name)]&
#                comm.data$Site==65),]
#sb.countlong[which(sb.countlong$Site==65&sb.countlong$Count>0),]
#probably Calochortus luteus
#comm.data[which(comm.data$Species_Name%in%comm.data$Species_Name[grep("Calochortus", comm.data$Species_Name)]&
#                comm.data$Site==3),]
#sb.countlong[which(sb.countlong$Site==3&sb.countlong$Count>0),]
sb.countlong2[which(sb.countlong2$Site==3&sb.countlong2$Count>0
                   &sb.countlong2$Species_Name=="Calochortus sp."),]$Species_Name <-"Calochortus luteus"
#comm.data[which(comm.data$Species_Name%in%comm.data$Species_Name[grep("Juncus", comm.data$Species_Name)]&
#                comm.data$Site==37),]#no juncus
#sort(as.character(unique(comm.data[which(comm.data$Site==37),]$Species_Name)))#nothing looks likely
#sb.countlong[which(sb.countlong$Site==37&sb.countlong$Count>0),]
#seems likely to be Vulpia microstachys
#comm.data[which(comm.data$Species_Name%in%comm.data$Species_Name[grep("Vulpia", comm.data$Species_Name)]&
#                comm.data$Site==46),]
#sb.countlong[which(sb.countlong$Site==46&sb.countlong$Count>0),] 
sb.countlong2[which(sb.countlong2$Site==46&sb.countlong2$Count>0
                  &sb.countlong2$Species_Name=="Vulpia sp."),]$Species_Name <-"Vulpia microstachys"
#searching for "Unknown grass in site 69"
#couldn't ID - could be various grasses
#commnames <- ddply(comm.data[which(comm.data$Site==69),], .(Species_Name),
#                             summarize, no.yrs = length(unique(Year)))
#sb.countlong[which(sb.countlong$Site==69&sb.countlong$Count>0),]
#merge(sb.countlong[which(sb.countlong$Site==69&sb.countlong$Count>0),], commnames, by="Species_Name",all=T)
NotSpFinal <- c("Unknown grass", "Vulpia sp.","Bromus sp.")
plotfreq.SB <- ddply(sb.countlong2, .(Species_Name), summarize, plot.freqsb = length(unique(Site)))
plotfreq.SB2 <- plotfreq.SB[which(!plotfreq.SB$Species_Name%in%NotSp),] 
plotfreq.all <- merge(plotfreq.SB2, AB2,  by="Species_Name", all.x = T)

#frequency index: (SBfreq / (SBfreq + ABfreq)) * 100
plotfreq.all$FreqIndex.all <- (plotfreq.all$plot.freqsb / (plotfreq.all$plot.freqsb + plotfreq.all$plot.freqab.allyrmn)) * 100
plotfreq.all$FreqIndex.predrought <- (plotfreq.all$plot.freqsb / (plotfreq.all$plot.freqsb + plotfreq.all$plot.freqab.pre2013)) * 100
plotfreq.all$FreqIndex.drought <- (plotfreq.all$plot.freqsb / (plotfreq.all$plot.freqsb + plotfreq.all$plot.freqab.post2012)) * 100

Freq.plot <- ggplot(plotfreq.all, aes(x = plot.freqsb, y = plot.freqab.pre2013))
Freq.plot <- Freq.plot + geom_point(color="green") + 
  geom_smooth(color="green", se=F) +theme_bw(base_size=14)
Freq.plot <- Freq.plot + geom_point(aes(x = plot.freqsb, y = plot.freqab.post2012),color="black") + 
  geom_smooth(aes(x = plot.freqsb, y = plot.freqab.post2012),color="black", se=F)
Freq.plot <- Freq.plot + xlab("Belowground Frequency (No. of plots)") + ylab("Aboveground Frequency (Mean no. of plots)")
Freq.plot <- Freq.plot + ggtitle("Above and Belowground Frequency\nGreen 2000 - 2012, Black 2013 - 2014")
ggsave(Freq.plot,path=plots, filename="ABvsSBFrequency.png", dpi=300, width=6, height = 6)
#all values more or less the same

FI.plot <- ggplot(plotfreq.all, aes(x=FreqIndex.predrought))
FI.plot <- FI.plot +  theme_bw(base_size=14)
FI.plot <- FI.plot + geom_density(alpha = 0.2,color="green",binwidth = 10,size=1)
FI.plot <- FI.plot + geom_density(aes(x=FreqIndex.drought), alpha = 0.2,color="black",binwidth = 10,size=1)
FI.plot <- FI.plot +  ggtitle("Frequency Index\nGreen 2000 - 2012, Black 2013 - 2014")
FI.plot <- FI.plot +  ylab("Density") + xlab("Frequency Index (SBfreq / (SBfreq + ABfreq) * 100)")
ggsave(FI.plot ,path=plots, filename="FrequencyIndexByPeriod.png", dpi=300, width=6, height = 6)

#do species with high FI (high presence in seedbank) have higher or lower presence in drought years?
plotfreq.all$DroughtDiff <- plotfreq.all$plot.freqab.pre2013 - plotfreq.all$plot.freqab.post2012
FIAB.plot <- ggplot(plotfreq.all, aes(x=FreqIndex.all, y = DroughtDiff)) + theme_bw(base_size=14)
FIAB.plot <- FIAB.plot + geom_point() + geom_smooth(method="lm",se=F,size=1)
FIAB.plot <- FIAB.plot + ggtitle("Frequency Index vs. Difference in Plot Frequency")
FIAB.plot <- FIAB.plot  +  ylab("Difference in Frequency\n(Mean Plot No. 2000-2012 - Mean No. 2013 - 2014") + 
  xlab("Frequency Index (SBfreq / (SBfreq + ABfreq) * 100)")
ggsave(FIAB.plot ,path=plots, filename="FrequencyIndexDifferenceWithDrought.png", dpi=300, width=6, height = 6)


#bring in seed mass data
sd.mass.mn$Species_Name <- 
  paste(as.character(lapply(strsplit(as.character(sd.mass.mn$id), split="_"),"[", 1)),
        as.character(lapply(strsplit(as.character(sd.mass.mn$id), split="_"),"[", 2)),sep=" ")

#complete coverage? 
plotfreq.all$Species_Name[!plotfreq.all$Species_Name%in%sd.mass.mn$Species_Name] #miss 20 (some genera)
plotfreq.all <- merge(plotfreq.all, sd.mass.mn, by="Species_Name", all.x = T)


#relationship between mass and seedstorage
mass.plot <- ggplot(plotfreq.all, aes(x = sd.mass.mn, y =  FreqIndex.predrought))
mass.plot <- mass.plot + geom_point() + geom_smooth(method=lm, se=F)
mass.plot  <- mass.plot  + xlab("Mean Seed Mass (g)") + ylab("SAI Frequency Index Pre-Drought")
ggsave(mass.plot,path=plots, filename="SeedbankSeedMass.png", dpi=300, width=6, height = 6)

plotfreq.all$sb.abratio <- plotfreq.all$plot.freqsb/plotfreq.all$plot.freqab.pre2013 #frequency relative to aboveground
outlier <- plotfreq.all[which(plotfreq.all$sb.abratio>10),]$Species_Name#three outliers: juncus b, Elymus glaucus, crassula

mass.plot2 <- ggplot(plotfreq.all[which(!plotfreq.all$Species_Name%in%outlier),],
                    aes(x =log(sd.mass.mn), y =  sb.abratio))
mass.plot2 <- mass.plot2 + geom_point() + geom_smooth(method=lm, se=FALSE) +theme_bw()
mass.plot2  <- mass.plot2  + xlab("Log Mean Seed Mass (g)") + ylab("SB/AB Ratio Pre-Drought")
ggsave(mass.plot2,path=plots, filename="SeedbankSeedMassRatioLog.png", dpi=300, width=6, height = 6)

mod.seedmass <- lm(log(sd.mass.mn) ~ sb.abratio, data=plotfreq.all[which(!plotfreq.all$Species_Name%in%outlier),])
summary(mod.seedmass) #ok...but not great - disappears if you remove Sagina apetala
plot(mod.seedmass$residuals) #OK
car::outlierTest(mod.seedmass)

#model approach
data.min <- plotfreq.all[which(!plotfreq.all$Species_Name=="Juncus bufonius"&!plotfreq.all$Species_Name=="Medicago polymorpha"),]
mod.mass <- lm(log(sb.abratio)~seed_mass_g, data=data.min )
plot(mod.mass$residuals)
qqnorm(mod.mass$residuals)#looks ok
car::outlierTest(mod.mass)
summary(mod.mass)#seed mass not significant

#model for general effect of seed bank storage on post vs. pre-drought abundance
library(lme4)
plotabund.AB1 <- ddply(comm.data[which(comm.data$Year>2012&comm.data$Species_Name%in%data.min$Species_Name),],
                      .(Site,Species_Name),summarize, drought.covmn = sum(Cover)/10)#average across years
plotabund.AB2 <- ddply(comm.data[which(comm.data$Year>2005&comm.data$Year<2013&
   comm.data$Species_Name%in%data.min$Species_Name),],.(Site,Species_Name),summarize, predrought.covmn = sum(Cover)/35)
plotabund.AB <- merge(plotabund.AB1, plotabund.AB2, by=c("Species_Name","Site"), all=T)
sb.countlong2 <- sb.countlong2[which(sb.countlong2$Species_Name%in%data.min$Species_Name),] 

data.min2 <- merge(sb.countlong2,plotabund.AB, by=c("Site","Species_Name"),all.x = T)
data.min2$sb.abratio <- data.min2$Count/data.min2$predrought.covmn #pre-drought
mod.drought1 <- lmer(drought.covmn ~ Count * predrought.covmn + (1|Species_Name), data=data.min2)
summary(mod.drought1)
plot(residuals(mod.drought1))
car::Anova(mod.drought1)#highly significant main effect, interaction not significant

mod.drought2 <- lmer(drought.covmn ~ sb.abratio * predrought.covmn + (1|Species_Name), data=data.min2)
summary(mod.drought2)
plot(residuals(mod.drought2))
car::Anova(mod.drought2) #highly significant interction and pre-drought with slightly signifcant sb.abratio

#is SAI related to change in time? 
plotfreq.all$FreqIndex.predrought

cover.yr <- ddply(comm.data[which(comm.data$Year>2005&comm.data$Species_Name%in%data.min$Species_Name),],
    .(Site,Year,Species_Name),summarize, covmn = sum(Cover)/5)
cover.yr.SAI <- merge(cover.yr, plotfreq.all[,c("Species_Name","FreqIndex.predrought")], by="Species_Name", all.x =T)

total.cov <- ddply(cover.yr.SAI, .(Site,Year), summarize, tot.cov = sum(covmn))
cover.yr.SAI <- merge(cover.yr.SAI, total.cov, by=c("Site","Year"))
cover.yr.SAI$wgtSAI <- cover.yr.SAI$FreqIndex.predrought * (cover.yr.SAI$covmn/cover.yr.SAI$tot.cov)

cover.SAI.fin <- ddply(cover.yr.SAI, .(Year,Site), summarize, CWM.SAI=sum(wgtSAI))
cover.SAI.fin2 <- ddply(cover.SAI.fin, .(Year), summarize, CWM.SAI.plotmn=mean(CWM.SAI))
CWMSAI <- ggplot(cover.SAI.fin2, (aes(x = Year, y =CWM.SAI.plotmn)))
CWMSAI <- CWMSAI + geom_point() + geom_smooth( se=F)
CWMSAI  <- CWMSAI + xlab("Year") + ylab("Community Weighted SAI (mean of all transects)") + theme_bw()
ggsave(CWMSAI,path=plots, filename="CommunityWeightedSAIYr.png", dpi=300, width=6, height = 6)
