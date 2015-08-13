## This Script is for Data Cleaning of Seedbank Data to prep for Analysis

# 1. Load packages and datasets
# 2. Clean SB 2012
# 3. Clean SB 2014
# 4. Combine datasets
# 5. Export data tables for analysis

####
# 1. Load packages and datasets and prep datasets for cleaning
####
library(plyr)
library(dplyr)
#Set working directory and load packages and files
#setwd("~//Documents//UC Davis//02_McLaughlin_80Sites_Organized//Seedbank Data")
cover <- read.csv("~//Documents//UC-Davis//02_McLaughlin_80Sites_Organized//Data-Storage-Files/Core_Community_Data2014.csv")
sb.2012 <- read.csv("Raw Data/2012/SeedbankMcLaughlin_CLEANdata_20150126.csv") 
sb.2014 <- read.csv("Raw Data/2014/SeedbankMcLaughlin2014CLEAN.csv") 
abio <- read.csv("~//Documents//UC-Davis//02_McLaughlin_80Sites_Organized//Data-Storage-Files/Basic Abiotic Data.csv")
traits <- read.csv("~//Documents//UC-Davis//02_McLaughlin_80Sites_Organized//Modified_CombinedFiles/McL_80SitesSpeciesTraits_012615.csv")

#extract columns needed
sb.2014<-sb.2014[,c(2:4)] #extract plot, species name, and number of seedlings
sb.2012<-sb.2012[,c(1,2,4,39)] #extract total seedlings per plot per species
sb.2012<-sb.2012[which(sb.2012$SUM>0),] #extract only species present

#change column names
colnames(sb.2012)<-c("Site","Soil","Species_Name","Count")
colnames(sb.2014)<-c("Site","Species_Name","Count")

#change capitalization of unknown forb/grass for consistency 
levels(sb.2014$Species_Name) <- c(levels(sb.2014$Species_Name), "Unknown grass", "Unknown forb")
sb.2014[which(sb.2014$Species_Name == "unknown grass"),2] <- "Unknown grass"
sb.2014[which(sb.2014$Species_Name == "unknown forb"),2] <- "Unknown forb"

#remove abiotic from cover data
cover <- cover[which(!cover$Species_Name=="Bare"&!cover$Species_Name=="Rock"),]

####
# 2. Clean 2012 Seedbank Data
####
#check for name consistency
unique(sb.2012$Species_Name[!sb.2012$Species_Name%in%cover$Species_Name]) # 3 species found belowground not found aboveground: Calochortus sp., Vulpia sp., Bromus sp.

#look at poorly identified species in seedbank
NoID12<- c("Calochortus sp.", "Vulpia sp.","Bromus sp.") 
sb.2012[which(sb.2012$Species_Name%in%NoID12),] # just five occurrences 

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

#remove rest of unknowns
#sb.2012 <- sb.2012[which(!sb.2012$Species_Name%in%NoID12),] 

####
# 3. Clean 2014 Seedbank Data; this part is still being worked on seeing as the data collection and identification is still ongoing
####

unique(sb.2014$Species_Name[!sb.2014$Species_Name%in%cover$Species_Name]) # 21 species found belowground not found aboveground: Draba verna, Lythrum californicum, bulb, Juncus phaeocephalus, Solanum nigrum, Veronica peregrina, Epilobium ciliatum; note: epilobium ciliatum and juncus phaeo. are perennials  

# remove weeds and perennial species
weeds <- c("Sonchus sp.","Erigeron canadensis")
sb.2014 <- sb.2014[which(!sb.2014$Species_Name%in%weeds),]

# Remove perennial species "Juncus phaeocephalus","Epilobium ciliatum"  
sb.2014 <- sb.2014[which(!sb.2014$Species_Name == "Juncus phaeocephalus" | !sb.2014$Species_Name == "Epilobium ciliatum"),]

#remove unknowns?
#NoID14 <- c("Unknown forb", "Unknown grass", "bulb", "grass 8", "grass 73")
#sb.2014 <- sb.2014[which(!sb.2014$Species_Name%in%NoID14),]

#add up counts in seedbanks
sb.2012 <- ddply(sb.2012, c("Site","Species_Name"), summarize, Count = sum(Count))
sb.2014 <- ddply(sb.2014, c("Site","Species_Name"), summarize, Count = sum(Count))

#### 
# 4. Combine into one dataset for Seedbank by year
#### 
sb.2012$Year <- 2012
sb.2014$Year <- 2014
sb.by.year <- rbind(sb.2012,sb.2014)
sb.by.year <- merge(abio[,2:3], sb.by.year, by.x = "site", by.y = "Site")
sb.by.year <- merge(sb.by.year, traits[,c("Species_Name", "Grass.Forb.Shrub")], by = "Species_Name", all.x = T)

# input lifeform type for unknown species
sb.by.year[which(sb.by.year$Species_Name == "bulb"),6] <- "Forb"
sb.by.year[which(sb.by.year$Species_Name == "Unknown forb"),6] <- "Forb"
sb.by.year[which(sb.by.year$Species_Name == "Unknown grass"),6] <- "Grass"
sb.by.year[which(sb.by.year$Species_Name == "grass 8"),6] <- "Grass"
sb.by.year[which(sb.by.year$Species_Name == "grass 73"),6] <- "Grass"
sb.by.year[which(sb.by.year$Species_Name == "Bromus sp."),6] <- "Grass"
sb.by.year[which(sb.by.year$Species_Name == "Aegilops triuncialis"),6] <- "Grass"

sb.by.year <- filter(sb.by.year, Grass.Forb.Shrub != "Shrub")

#rearrange columns
sb.by.year <- sb.by.year[,c(2,3,6,1,5,4)]
colnames(sb.by.year)[3] <- "Grass.Forb"

####
# 5. export data into CSV file
####
write.table(sb.by.year, "Core_Seedbank.csv", sep = ",", row.names = F)
write.table(sb.2012, "SB_2012_QC.csv", sep = ",", row.names = F)
write.table(sb.2014, "SB_2014_QC.csv", sep = ",", row.names = F)
