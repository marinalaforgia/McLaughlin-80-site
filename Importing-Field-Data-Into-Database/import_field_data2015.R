
                                                        
# 01. read in this years data - change file location as needed... 
 d2015 = read.delim("~//Documents//UC-Davis//02_McLaughlin_80Sites_Organized//Importing-Field-Data-Into-Database/McL_LateSpring2015_QC.csv",
 sep = ',', header = T)
 head(d2015)
 
# 02. prepare for rbind
 names(d2015)[3:7] = 'a'  # homogenize quadrat names to allow rbind to concatenate into long format 
 head(d2015)#check it out
 
 # 03. rbind

#make into long format with each unique species cover within quad within site represented by one row
d2015b <- rbind(d2015[, c(1:2, 3,8)], d2015[ ,c(1:2, 4,8)], d2015[ ,c(1:2, 5,8)], d2015[ ,c(1:2, 6,8)], d2015[ ,c(1:2, 7,8)]) 
head(d2015b)
 
dims <- nrow(d2015)  #get the number of rows
quad <- c(rep(1, dims), rep(2, dims), rep(3, dims), rep(4, dims), rep(5, dims)) #make one row for each quad
d2015c = data.frame(num = '',year = 2015, d2015b[,1:3], quad, notes = d2015b[,4])    # num will become the accession number in the database
d2015c$a <- as.numeric(d2015c[, 'a'])                 
d2015d = d2015c[!is.na(d2015c$a),] #takes out all NA results for a
d2015d = d2015d[ , c(1:3, 6, 4, 5, 7)] #arrange with quad in front of species and site

# 04. add an accession - check the last row in the database to get the correct row number before this step!
acc <- 79652:(79651+nrow(d2015d))   #for 2013 - 78671
d2015d[,1] <- acc
head(d2015d)

# 05. write new table

write.table(d2015d, '~//Documents//UC-Davis//02_McLaughlin_80Sites_Organized//Importing-Field-Data-Into-Database/2015 field data for input to db.txt',
quote = F, row.name = F, sep = '\t', col.names = F)    #no colnames
