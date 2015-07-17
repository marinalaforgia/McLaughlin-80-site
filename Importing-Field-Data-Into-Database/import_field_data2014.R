
                                                        
# 01. read in this years data - change file location as needed... 
 d2014 = read.delim("Y:\\01_PrimaryDatasets\\02_McLaughlin_80Sites_Organized\\Importing Field Data Into Database\\2014_LateSpringField.csv",
 sep = ',', header = T)
 head(d2014)
 
# 02. prepare for rbind
 names(d2014)[3:7] = 'a'  # homogenize quadrat names to allow rbind to concatenate into long format 
 head(d2014)#check it out
 
 # 03. rbind

#make into long format with each unique species cover within quad within site represented by one row
d2014b <- rbind(d2014[, c(1:2, 3,8)], d2014[ ,c(1:2, 4,8)], d2014[ ,c(1:2, 5,8)], d2014[ ,c(1:2, 6,8)], d2014[ ,c(1:2, 7,8)]) 
head(d2014b)
 
dims <- nrow(d2014)  #get the number of rows
quad <- c(rep(1, dims), rep(2, dims), rep(3, dims), rep(4, dims), rep(5, dims)) #make one row for each quad
d2014c = data.frame(num = '',year = 2014, d2014b[,1:3], quad, notes = d2014b[,4])    # num will become the accession number in the database
d2014c$a <- as.numeric(d2014c[, 'a'])                 
d2014d = d2014c[!is.na(d2014c$a),] #takes out all NA results for a
d2014d = d2014d[ , c(1:3, 6, 4, 5, 7)] #arrange with quad in front of species and site

# 04. add an accession - check the last row in the database to get the correct row number before this step!
acc <- 75150:(75149+nrow(d2014d))   #for 2013 - 78671
d2014d[,1] <- acc
head(d2014d)

# 05. write new table

write.table(d2014d, 'Y:\\01_PrimaryDatasets\\02_McLaughlin_80Sites_Organized\\Importing Field Data Into Database\\2014 field data for iput to db.txt',
quote = F, row.name = F, sep = '\t', col.names = F)    #no colnames
