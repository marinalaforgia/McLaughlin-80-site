
                                                        
# 01. read in this years data - change file location as needed... 
 d2013 = read.delim("C:\\Users\\Stella\\Documents\\McLaughlin\\McLaughlin_80Sites\\McLaughlin_80Sites_Organized\\import_field_data_to_db\\2013_LateSpringField_corr.csv",
 sep = ',', header = T)
   
# 02. prepare for rbind, drop crap rows
  names(d2013)[3:7] = 'a'     # homogenize quadrat names to allow rbind to concatenate into long format
  d2013b <- d2013[-grep('NOTES:', d2013[, 1]), ] # drop notes rows
  d2013c <- d2013b[d2013b[, 'site'] %in% 1:80, ] # drop notes rows that follow the word notes (ie the blank rows)

# 03. rbind -     
# this is optional: d2013c<-d2013b #if d2013c results in error because of no blank rows, use d2013b 

#make into long format with each unique species cover within quad within site represented by one row
d2013d <- rbind(d2013c[, c(1:2, 3,8)], d2013c[ ,c(1:2, 4,8)], d2013c[ ,c(1:2, 5,8)], d2013c[ ,c(1:2, 6,8)], d2013c[ ,c(1:2, 7,8)]) 
head(d2013d)
dims <- nrow(d2013c)  #get the number of rows
quad <- c(rep(1, dims), rep(2, dims), rep(3, dims), rep(4, dims), rep(5, dims)) #make one row for each quad
d2013e = data.frame(num = '',year = 2013, d2013d[,1:3], quad, notes = d2013d[,4])    # num will become the accession number in the database
d2013e$a <- as.numeric(d2013e[, 'a'])                 
d2013f = d2013e[!is.na(d2013e$a),] #takes out all NA results for a
d2013f = d2013f[ , c(1:3, 6, 4, 5, 7)] #arrange with quad in front of species and site

# 04. add an accession - check the last row in the database to get the correct row number before this step!
acc <- 78672:(78671+nrow(d2013f))   #for 2013 - 78671
d2013f[,1] <- acc
head(d2013f)

# 05. write new table

write.table(d2013f, 'C:\\Users\\Stella\\Documents\\McLaughlin\\McLaughlin_80Sites\\McLaughlin_80Sites_Organized\\import_field_data_to_db\\2013 field data for iput to db.txt',
quote = F, row.name = F, sep = '\t', col.names = F)    #no colnames
