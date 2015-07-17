# Calculating Seed Accumulation index (Holzel & Otte)

#Plan
#1. Clean up Data
#2. Calculate SAI index 1 in 2012 using aboveground freq in 2012
#3. Calculate SAI in 2014 using aboveground freq in 2014
#4. Calculate mean SAI across 2012 and 2014
#5. Calculate SAI in 2012 using avg aboveground freq 2000-2012
#6. Calculate SAI in 2014 using avg aboveground freq 2000-2014
#7. Calculate mean SAI in 2012 and 2014 using avg long term freq
#8. Export SAIs to csv file

####
#Clean up Data
####
library(plyr)
library(dplyr)
#Set working directory and load packages and files
#setwd("~//Documents//UC Davis//02_McLaughlin_80Sites_Organized//Seedbank Data")
cover <- read.csv("~//Documents//UC-Davis//02_McLaughlin_80Sites_Organized//Data-Storage-Files/Core_Community_Data2014.csv") #Core_Community_Data2014.csv
sb.2012 <- read.csv("SeedbankMcLaughlin_CLEANdata_20150126.csv") #SeedbankMcLaughlin_CLEANdata_20150126.csv
sb.2014 <- read.csv("SeedbankMcLaughlin2014CLEAN.csv") #SeedbankMcLaughlin2014CLEAN.csv
abio <- read.csv("~//Documents//UC-Davis//02_McLaughlin_80Sites_Organized//Data-Storage-Files/Basic Abiotic Data.csv")
traits <- read.csv("~//Documents//UC-Davis//02_McLaughlin_80Sites_Organized//Modified_CombinedFiles/McL_80SitesSpeciesTraits_012615.csv")

#extract columns needed
sb.2014<-sb.2014[,c(2:4)]
sb.2012<-sb.2012[,c(1,2,4,39)] #extract total seedlings per plot per species
sb.2012<-sb.2012[which(sb.2012$SUM>0),] #extract only species present

#change column names
colnames(sb.2012)<-c("Site","Soil","Species_Name","Count")
colnames(sb.2014)<-c("Site","Species_Name","Count")

#change capitalization of unknown forb/grass for consistency 
levels(sb.2014$Species_Name) <- c(levels(sb.2014$Species_Name), "Unknown grass", "Unknown forb")
sb.2014[which(sb.2014$Species_Name == "unknown grass"),2] <- "Unknown grass"
sb.2014[which(sb.2014$Species_Name == "unknown forb"),2] <- "Unknown forb"

#check for name consistency
unique(sb.2012$Species_Name[!sb.2012$Species_Name%in%cover$Species_Name]) # 3 species found belowground not found aboveground: Calochortus sp, Vulpia sp., Bromus sp.
unique(sb.2014$Species_Name[!sb.2014$Species_Name%in%cover$Species_Name]) # 9 species found belowground not found aboveground: Draba verna, Lythrum californicum, bulb, Juncus phaeocephalus, Solanum nigrum, Veronica peregrina, Epilobium ciliatum; note: epilobium ciliatum and juncus phaeo. are perennials  
unique(cover$Species_Name[!cover$Species_Name%in%sb.2012$Species_Name]) # many species found aboveground but not below in 2012
unique(cover$Species_Name[!cover$Species_Name%in%sb.2014$Species_Name]) # many species found aboveground but not below in 2014

#clean up cover data
  # to investigate: Centaurea sp, Nemophila sp, Minuartia sp., Stephanmeria sp., 
  #substitute Nemophila sp for Nemophila heterophylla
  cover[which(cover$Species_Name=="Nemophila sp"),]$Species_Name <- "Nemophila heterophylla"
  # Other minuartia species not present in quadrat in previous years
  filter(cover, Site == 38, Quadrat == 4, Species_Name == "Minuartia californica" | Species_Name == "Minuartia douglasii")
  # look into centaurea sp.; no matching centaurea species in that quadrat
  cover[which(cover$Species_Name == "Centaurea sp."),]
filter(cover, Site == 31, Quadrat == 5, Species_Name == "Centaurea solstitialis" | Species_Name == "Centaurea melitensis")
 #remove abiotic 
  cover <- cover[which(!cover$Species_Name=="Bare"&!cover$Species_Name=="Rock"),]

#look at poorly identified species in seedbank
NoID<- c("Calochortus sp.", "Vulpia sp.","bulb","Bromus sp.","Unknown grass","Unknown forb","Juncus sp.") 
sb.2012[which(sb.2012$Species_Name%in%NoID),] # just five occurrences 
  #Bromus in site 65 unclear, both B. diandrus and hordaceous noted in seedling count already
  cover[which(cover$Species_Name%in%cover$Species_Name[grep("Bromus", cover$Species_Name)] & cover$Site==65),]
  sb.2012[which(sb.2012$Site==65),]
  
  # Calochortus probably luteus
  #cover[which(cover$Species_Name%in%cover$Species_Name[grep("Calochortus", cover$Species_Name)] & cover$Site==3),]
  #sb.2012[which(sb.2012$Site==3),]
  sb.2012[which(sb.2012$Site==3 & sb.2012$Species_Name=="Calochortus sp."),]$Species_Name <-"Calochortus luteus"
  
  #seems likely to be Vulpia microstachys
  cover[which(cover$Species_Name%in%cover$Species_Name[grep("Vulpia", cover$Species_Name)]&cover$Site==46),]
  sb.2012[which(sb.2012$Site==46),] 
  sb.2012[which(sb.2012$Site==46&sb.2012$Species_Name=="Vulpia sp."),]$Species_Name <-"Vulpia microstachys"
  
sb.2014[which(sb.2014$Species_Name%in%NoID),] # many occurences of unknown grass/unknown forb/unknown bulb

# Remove perennial species "Juncus phaeocephalus","Epilobium ciliatum"  
sb.2014 <- sb.2014[which(!sb.2014$Species_Name == "Juncus phaeocephalus" | !sb.2014$Species_Name == "Epilobium ciliatum"),]

#remove unknowns
#sb.2012 <- sb.2012[which(!sb.2012$Species_Name%in%NoID),] 
#sb.2014 <- sb.2014[which(!sb.2014$Species_Name%in%NoID),]
#cover <- cover[which(!cover$Species_Name%in%rm.sp),]

#add up counts in seedbanks
sb.2012 <- ddply(sb.2012, c("Site","Species_Name"), summarize, Count = sum(Count))
sb.2014 <- ddply(sb.2014, c("Site","Species_Name"), summarize, Count = sum(Count))

#### 
#Alter datasets for frequency
####
# Cover (number of sites a species occurs in within a given year)
cover.freq <- ddply(cover, c("Year", "Species_Name"), summarize, freq.AB = length(unique(Site))) 
cover.freq12 <- cover.freq[cover.freq$Year == 2012,]
cover.freq14 <- cover.freq[cover.freq$Year == 2014,]
colnames(cover.freq12)[3] <- "freq.AB12"
colnames(cover.freq14)[3] <- "freq.AB14"
AB.1214 <- merge(cover.freq12[,2:3],cover.freq14[,2:3],by = "Species_Name", all = T)

# SB 2012
SB12.freq <- ddply(sb.2012, "Species_Name", summarize, freq.SB12 = length(unique(Site)))   

# SB 2014
SB14.freq <- ddply(sb.2014, "Species_Name", summarize, freq.SB14 = length(unique(Site))) 

# Merge into one file
freq.SB <- merge(SB12.freq,SB14.freq,by = "Species_Name", all = T)
freq.all<- merge(freq.SB,AB.1214,by = "Species_Name", all = T)
freq.all[is.na(freq.all)] <- 0 # is the absence of a species for the seedbank sampling indicative of sampling error, thus the index should be calculated using only the year in which it appears, or is it indicative of absence from the seedbank so the measure should be averaged with 0? I lean towards the first one.

####  
#Alter datasets for Index 2
####
# in the paper they dont have it as relative abundance in the seedbank, they have total abundance in the seedbank and cumulative cover of each species

#Cumulative cover (which might not necessarily make sense to avg cover across all 80 sites because of serpentine/nonserpentine)
cum.cover<-na.omit(cover)
ind2<-ddply(cum.cover, "Species_Name", summarize, cum.AB = sum(Cover)/length(unique(cum.cover$Year)))

#Total seeds in both years of sampling
SBquant.12 <- ddply(sb.2012, "Species_Name", summarize, Tot.ab = sum(Count))
SBquant.14 <- ddply(sb.2014, "Species_Name", summarize, Tot.ab = sum(Count))
SBquant <- merge(SBquant.12, SBquant.14, by = "Species_Name", all = T)
SBquant[is.na(SBquant)]<-0
SBquant$Tot.ab <- rowSums(SBquant[,2:3], na.rm=F)

#merge datasets
Index2<- merge(SBquant[,c(1,4)], ind2, by = "Species_Name", all = T)

####
#Alter datasets for Relative abundance
####
# Cover
cover.post2006<-na.omit(cover)
cover.post2006<- ddply(cover.post2006, c("Site","Year","Species_Name"), summarize, Cover = sum(Cover))
cover.post2006 <- merge(cover.post2006, ddply(cover.post2006, c("Site","Year"), summarize, Tot.cov = sum(Cover)), by=c("Site","Year"), all.x =T)
cover.post2006$RA <- cover.post2006$Cover/cover.post2006$Tot.cov*100

cover.RA <- ddply(cover.post2006, "Species_Name", summarize, RA = mean(RA))

# SB 2012: relative abundance in seedbank
RA.SB12 <- merge(sb.2012, ddply(sb.2012, "Site", summarize, Tot.ab = sum(Count)), by="Site", all.x =T)
RA.SB12$RA12 <- RA.SB12$Count/RA.SB12$Tot.ab*100
RA.SB12 <- ddply(RA.SB12, "Species_Name", summarize, SB12 = mean(RA12))

# SB 2014
RA.SB14 <- merge(sb.2014, ddply(sb.2014, "Site", summarize, Tot.ab = sum(Count)), by="Site", all.x =T)
RA.SB14$RA14 <- RA.SB14$Count/RA.SB14$Tot.ab*100
RA.SB14 <- ddply(RA.SB14, "Species_Name", summarize, SB14 = mean(RA14))

# Merge into one file
SB.RA <-merge(RA.SB14, RA.SB12, by = "Species_Name", all = T)
SB.RA[is.na(SB.RA)]<-0
SB.RA$RA.SB <- rowMeans(SB.RA[,2:3])
RA.all <- merge(cover.RA, SB.RA, by = "Species_Name", all = T)


#####
#Calculate SAI Index 1 (SBfreq / (SBfreq + ABfreq)) * 100 (SAI INDEX 1)
####
# Index 1 "relates the plot frequency of a certain species in above ground vegetation with its frequency in the soil seed bank"
# this doesn't specify how this is calculated in relation to the various years

# Index 1: 2012
freq.all$SAI1.2012 <- (freq.all$freq.SB12 / (freq.all$freq.SB12 + freq.all$freq.AB12)) * 100

# Index 1: 2014
freq.all$SAI1.2014 <- (freq.all$freq.SB14 / (freq.all$freq.SB14 + freq.all$freq.AB14)) * 100

# Index 1 average
freq.all$SAI1 <- rowMeans(freq.all[,6:7])

#####
#Calculate SAI Index 2 (SBquant / (SBquant + ABcover)) * 100
####
# Index 2: "Relates the cumulative cover of a certain species over all plots to the total number of seeds recorded in the seed bank over all plots in both years of sampling"
# "Cover was calculated as an average of three years" for us this would be the average cumulative cover across 9 years
Index2$SAI2 <- Index2$Tot.ab / (Index2$Tot.ab + Index2$cum.AB) * 100

####
# OVERALL SAI = average(index 1, index 2)
####
SAI <- merge(freq.all[,c(1,8)],Index2[,c(1,4)],by="Species_Name")
SAI$SAI <- rowMeans(SAI[,2:3])
#belowground freq/total frequency averaged across years? 

####
# SAI using relative abundance belowground vs relative cover aboveground
####
RA.all$SAI3 <- RA.all$RA.SB / (RA.all$RA.SB + RA.all$RA) * 100
SAI <- merge(SAI,RA.all[,c(1,6)],by="Species_Name")

SAI<-SAI[!is.na(SAI$SAI2),]
colnames(SAI)<- c("Species_Name","SAI_Index1","SAI_Index2","SAI","SAI_RelAb")

#### Seedbank by year
sb.2012$Year <- 2012
sb.2014$Year <- 2014
sb.by.year <- rbind(sb.2012,sb.2014)
sb.by.year <- merge(abio[,2:3], sb.by.year, by.x = "site", by.y = "Site")
sb.by.year <- merge(sb.by.year, traits[,c("Species_Name", "Grass.Forb.Shrub")], by = "Species_Name", all.x = T)
sb.by.year[which(sb.by.year$Species_Name == "bulb"),6] <- "Forb"
sb.by.year[which(sb.by.year$Species_Name == "Unknown forb"),6] <- "Forb"
sb.by.year[which(sb.by.year$Species_Name == "Unknown grass"),6] <- "Grass"
sb.by.year[which(sb.by.year$Species_Name == "Bromus sp."),6] <- "Grass"
sb.by.year[which(sb.by.year$Species_Name == "Aegilops triuncialis"),6] <- "Grass"
sb.by.year <- filter(sb.by.year, Grass.Forb.Shrub != "Shrub")
sb.by.year.sum <- ddply(sb.by.year, c("Serpentine","Species_Name","Grass.Forb.Shrub","Year"), summarize, Count = sum(Count))
ggplot(sb.by.year.sum, aes(x = Year, y = Count)) + 
  geom_line(aes(by = Species_Name, col = Serpentine)) +
  facet_wrap(~Grass.Forb.Shrub) +
  geom_smooth(method = "lm", aes(col = Serpentine))

####
# SAI per Site
###
colnames(sb.2012)[3] <- "Count.12"
colnames(sb.2014)[3] <- "Count.14"
sb.both.yrs <- merge(sb.2012, sb.2014, by = c("Site","Species_Name"), all = T)
sb.both.yrs[is.na(sb.both.yrs)] <- 0
sb.both.yrs <- merge(abio[,2:3], sb.both.yrs, by.x = "site", by.y = "Site")
sb.both.yrs <- merge(sb.both.yrs, traits[,c("Species_Name", "Grass.Forb.Shrub")], by = "Species_Name", all.x = T)
sb.both.yrs <- sb.both.yrs[,c(2,3,6,1,4,5)]
sb.both.yrs <- sb.both.yrs[order(sb.both.yrs$site),]
sb.both.yrs[which(sb.both.yrs$Species_Name%in%NoID),]
sb.both.yrs[which(sb.both.yrs$Species_Name == "bulb"),3] <- "Forb"
sb.both.yrs[which(sb.both.yrs$Species_Name == "Unknown forb"),3] <- "Forb"
sb.both.yrs[which(sb.both.yrs$Species_Name == "Unknown grass"),3] <- "Grass"
sb.both.yrs[which(sb.both.yrs$Species_Name == "Bromus sp."),3] <- "Grass"
sb.both.yrs[which(sb.both.yrs$Species_Name == "Aegilops triuncialis"),3] <- "Grass"
SAIperSite <- merge(sb.both.yrs, SAI, by = "Species_Name")
SAIperSite <- SAIperSite[,c(2,3,1,4:10)]
SAIperSite <- SAIperSite[order(SAIperSite$site),]
SAIperSite[is.na(SAIperSite),]
write.table(SAIperSite, "Seedbank_counts.csv", sep = ",", row.names = F)
# SAI per species then calculate it per site? or calculate SAI individually for each site? how would this be done?


####
# PLOT SAI VS SLA
####

# Look at correlation of SLA with SAI
SLA.SAIplot <- merge(traits[,c("Species_Name","SLAS","SLANS")], SAI[,c("Species_Name","SAI","SAI_RelAb")], by = "Species_Name")
# SLA values change for serpentine and nonserpentine, so for overall I will look at the average but for many species it might be a good idea to calculate SAI separately (esp if looking at relative abundance on two very different soil types where the species vary so much)
par(mfrow=c(1,1))
plot(SLA.SAIplot$SAI ~ SLA.SAIplot$SLAS)
abline(lm(SLA.SAIplot$SAI ~ SLA.SAIplot$SLAS), col = "red")
plot(SLA.SAIplot$SAI ~ SLA.SAIplot$SLANS)
abline(lm(SLA.SAIplot$SAI ~ SLA.SAIplot$SLANS), col = "red")
SLA.SAIplot$SLA<-rowMeans(SLA.SAIplot[,2:3])
plot(SLA.SAIplot$SAI ~ SLA.SAIplot$SLA, xlab = "SLA", ylab="SAI")
abline(lm(SLA.SAIplot$SAI ~ SLA.SAIplot$SLA), col = "red")
plot(SLA.SAIplot$SAI_RelAb ~ SLA.SAIplot$SLA, xlab = "SLA", ylab="SAI")

plot(SLA.SAIplot$SAI_RelAb ~ SLA.SAIplot$SAI, xlab = "SAI", ylab="SAI_relab")
plot(SAI$SAI_RelAb ~ SAI$SAI_Index2, xlab = "SAI", ylab="SAI_relab")

# break out by functional groups 
# differences between freq and abundance index
# SLA/SAI correlated with abundance over time
# number of years seen aboveground

####
# Export Files
###
write.table(SAI,"SAI.csv",sep=",",row.names=F)

