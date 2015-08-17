cover <- read.csv("~//Documents//UC-Davis//02_McLaughlin_80Sites_Organized//Data-Storage-Files/OldFiles/Core_Community_Data2014.csv")
cover.new <- read.delim("~//Documents//UC-Davis//02_McLaughlin_80Sites_Organized//Importing-Field-Data-Into-Database/2015 field data for input to db.txt", header = F)

colnames(cover.new) <- colnames(cover)
cover.test <- rbind(cover,cover.new)
cover.test$id <- 1:nrow(cover.test)
write.table(cover.test,"Core_Community_Data2015.csv", sep = ",", row.names = F)
