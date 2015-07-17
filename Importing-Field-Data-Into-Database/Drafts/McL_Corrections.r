#Modification of database (2012) with changed species names
db = read.delim("C:\\Users\\Stella\\Documents\\McLaughlin\\McLaughlin_80Sites\\McLaughlin_80Sites_Organized\\import_field_data_to_db\\SS_2000on.csv", 
sep = ',', header = T,stringsAsFactors=FALSE)

sp.list.corr = read.delim("C:\\Users\\Stella\\Documents\\McLaughlin\\McLaughlin_80Sites\\McLaughlin_80Sites_Organized\\import_field_data_to_db\\SpeciesList_80Sites_Corrections.csv",
 sep = ',', header = T,stringsAsFactors=FALSE)

colnames(sp.list.corr)<-c("Species_Name","NumberOccurrences","Merged_Species","Proposed.Action","Replacement")

db2<-merge(db,sp.list.corr,by="Species_Name",all.x=TRUE,all.y=FALSE)

db3<-db2[!db2$Proposed.Action=="Remove",] #Takes out genera names and other miscellaneous names without clear interpretation
db4<-db3[!is.na(db3$Species_Name),]

db4[which(!db4$Proposed.Action=="No change"),]$Species_Name<-db4[which(!db4$Proposed.Action=="No change"),]$Replacement  #Replacing species names

db5<-db4[,c(2:5,1,6:7,9)] 
db6<-db5[order(db5$ID),]#Final corrected data ordered by number will become "Core Community Data" in database

db6[,"ID"] <- 1:length(db6[,1])       #get the number of rows 

removed.data<-db[!db$ID%in%db5$ID,]
removed.spp<-unique(removed.data$Species_Name)

write.table(db6, 'C:\\Users\\Stella\\Documents\\McLaughlin\\McLaughlin_80Sites\\McLaughlin_80Sites_Organized\\import_field_data_to_db\\CoreCommunityData2013.txt', quote = F, row.name = F, sep = '\t', col.names = T)
write.table(removed.data, 'C:\\Users\\Stella\\Documents\\McLaughlin\\McLaughlin_80Sites\\McLaughlin_80Sites_Organized\\import_field_data_to_db\\removed_records2013.txt', quote = F, row.name = F, sep = '\t', col.names = T)
write.table(removed.spp, 'C:\\Users\\Stella\\Documents\\McLaughlin\\McLaughlin_80Sites\\McLaughlin_80Sites_Organized\\import_field_data_to_db\\removed_spp2013.txt', quote = F, row.name = F, sep = '\t', col.names = T)





 

