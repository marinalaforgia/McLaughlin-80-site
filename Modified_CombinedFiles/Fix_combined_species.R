# Fix frequency data on core community file due to merging of species
setwd("~/Documents/UC-Davis/02_McLaughlin_80Sites_Organized/")
cover<-read.csv("Data-Storage-Files/Core_Community_Data2014.csv")

# look for duplicates
dup_sp <- cover[duplicated(cover[,2:6]),] # this is a vector of what is deleted from the data frame
cover <- cover[!duplicated(cover[,2:6]),] # gets rid of duplicated species

write.table(cover, "Core_Community_Data2014.csv", sep = ",", row.names = F)
