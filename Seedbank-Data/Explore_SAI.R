## Script for exploring correlations with seedbank abundance
# Things to explore
# 1. Compare frequency data above/below ground for study species
# 2. Correlate SAI with 
    # a. Burned in 1999 and burned in 2000
    # b. grazed and ungrazed
    # c. serpentine and nonserpentine
    # d. Dry and wet years
    # e. grass and forbs
    # f. Various traits
        # - SLA
        # - ??
    # g. year to year variation in frequency
# 3. See which year or sequence of years best predicts abundance in seedbank for different life forms
# 4. See what year "types" best predicts abundance in seedbank for different life forms

## Read in necessary files and packages
library(dplyr)
library(ggplot2)
SAI <- read.csv("SAI.csv")
cover <- read.csv("~//Documents//UC-Davis//02_McLaughlin_80Sites_Organized//Data-Storage-Files/Core_Community_Data2014.csv")
abiotic <- read.csv("~//Documents//UC-Davis//02_McLaughlin_80Sites_Organized//Data-Storage-Files//Basic Abiotic Data.csv")
burn <- read.csv("~//Documents//UC-Davis//02_McLaughlin_80Sites_Organized//Data-Storage-Files//Burn Records.csv")
graze <- read.csv("~//Documents//UC-Davis//02_McLaughlin_80Sites_Organized//Data-Storage-Files//Grazing Record.csv")
sbank <- read.csv("Seedbank_counts.csv")

#####
# SAI correlations 
####

# should i use average SAI per plot or compute a community weighted measure?

# seedlings per plot
ggplot(sbank, aes(x = Grass.Forb.Shrub, y = Count.14, col = Grass.Forb.Shrub)) + 
  geom_point()
test <- reshape(sbank, idvar = c("site","Serpentine","Species_Name","Grass.Forb.Shrub","SAI"), varying = c("Count.12", "Count.14"), timevar = "Year", direction = "long")
# Comparing SAI between burned and unburned plots
head(burn)
unique(burn$Season_Year)
summary(burn)
length(burn)
dim(burn)
burn1999 <- filter(burn, Season_Year == 1999)
  
  
#####
# Shifting Time windows
####
