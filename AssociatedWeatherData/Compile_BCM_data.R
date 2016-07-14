#########################################

# Compile BCM data from Jim Thorne / Hyeyeong Choe and join to other McLaughlin weather data

setwd("/users/latimer/GoogleDriveUCD/Rapid/Climate_Data")

aet = read.csv("./BCM_Model/aet_hist.csv")
cwd = read.csv("./BCM_Model/cwd_hist.csv")
pet = read.csv("./BCM_Model/pet_hist.csv")
ppt = read.csv("./BCM_Model/ppt_hist.csv")
tmn = read.csv("./BCM_Model/tmn_hist.csv")
tmx = read.csv("./BCM_Model/tmx_hist.csv")

# create a mean temp layer by taking monthly average of tmin and tmax
tmean = tmn
tmean[,3] = (tmn[,3] + tmx[,3])/2

head(aet)

split.yearmonths <- function(x) { # function to take Hyeyeong's combined "yrmts" column and split it
  year = as.integer(unlist(sapply(x$yrmts, substr, start=1, stop=4)))
  month = sapply(x$yrmts, substr, start=5, stop=7)
  return(cbind(year, month))
}

summarize.BCM <- function(x, sum.fun) { # summarize to annual time step the individual monthly BCM data output files
    # x is one of the BCM output files 
    # sum.fun is the function to apply to it (e.g. sum or mean)
  
  # split out year and month
  x = cbind(x, split.yearmonths(x))
  # adjust water year by making sept-dec part of the next year
  x$wateryr = as.integer(as.character(x$year))
  x$wateryr[x$month %in% c("sep", "oct", "nov", "dec")] = x$wateryr[x$month %in% c("sep", "oct", "nov", "dec")] + 1 
  # include an indicator for whether a month is in "winter" 
  x$winter = x$month %in% c("dec", "jan", "feb")
  
  # summarize to annual time step
  x.annual = aggregate(x[,3], by=list(x$year), FUN=sum.fun)
  x.wateryr = aggregate(x[,3], by=list(x$wateryr), FUN=sum.fun)
  x.winter = aggregate(x[,3], by=list(x$wateryr, x$winter), FUN=sum.fun)
  x.winter = x.winter[which(x.winter$Group.2),]
  x.winter = x.winter[,-2]

  # merge 
  x.all = merge(x.annual, x.wateryr, by="Group.1")
  x.all = merge(x.all, x.winter, by="Group.1")
  names(x.all) = c("year", "annual", "wateryr", "winter")
  return(x.all)
}

aet.sum = summarize.BCM(aet, sum.fun = sum)
cwd.sum = summarize.BCM(cwd, sum.fun = sum)
pet.sum = summarize.BCM(pet, sum.fun = sum) # extra year here for some reason
pet.sum = pet.sum[2:nrow(pet.sum),] # remove it
ppt.sum = summarize.BCM(ppt, sum.fun = sum) 
ppt.sum = ppt.sum[2:nrow(ppt.sum),] # remove extra year
tmean.mean = summarize.BCM(tmean, sum.fun=mean)
tmean.mean = tmean.mean[2:nrow(tmean.mean),] # remove extra year

#### put these all together 
names(aet.sum) = c("year", "aet.annual", "aet.wateryr", "aet.winter")
names(cwd.sum) = c("year", "cwd.annual", "cwd.wateryr", "cwd.winter")
names(pet.sum) = c("year", "pet.annual", "pet.wateryr", "pet.winter")
names(ppt.sum) = c("year", "ppt.annual", "ppt.wateryr", "ppt.winter")
names(tmean.mean) = c("year", "tmean.annual", "tmean.wateryr", "tmean.winter")

BCMdata = merge(aet.sum, cwd.sum, by="year")
BCMdata = merge(BCMdata, pet.sum, by="year")
BCMdata = merge(BCMdata, pet.sum, by="year")
BCMdata = merge(BCMdata, ppt.sum, by="year")
BCMdata = merge(BCMdata, tmean.mean, by="year")
head(BCMdata)

write.csv(BCMdata, "BCM_data_annual.csv")

BCMdata=read.csv("BCM_data_annual.csv")

#########################################
#### compare to Knoxville station data

# precipitation
k = read.csv("knoxville_annual_ppt.csv")
head(k)

BCMdata$year = as.integer(as.character(BCMdata$year))
par(mfrow=c(2,1))
hist(BCMdata$ppt.wateryr[BCMdata$year>1985]/25.4)
hist(k$wateryrppt)

plot(k$wateryrppt, BCMdata$ppt.wateryr[BCMdata$year>1985]/25.4)
abline(0,1)
# Generally BCM is a bit wetter than Knoxville, with 2 - 4 big outliers to the wetter side

plot(1986:2015, BCMdata$ppt.wateryr[BCMdata$year>1985]/25.4, type="b", col="blue", xlab="year", ylab="Total water year precipitation (inches)", ylim=c(10, 60))
lines(1986:2015, k$wateryrppt[k$year>1985], type="b", col="orange3")
legend("topright", c("Knoxville","BCM"), lwd=c(2,2), col=c("orange3","blue"))
# Big outliers are 1995, 1998, 1998, 199 where knoxville seems clearly too low. 

plot(1986:2015, BCMdata$ppt.winter[BCMdata$year>1985]/25.4, type="b", col="blue")
lines(1986:2015, k$winterppt[k$year>1985], type="b", col="orange3")

yr = 2000:2015

summary(lm(BCMdata$ppt.winter[BCMdata$year>1999]~yr))
summary(lm(k$winterppt[k$year>1999]~yr))
# significant declining trend in Knoxville winter precip, but not in BCM
summary(lm(BCMdata$ppt.wateryr[BCMdata$year>1999]~yr))
summary(lm(k$wateryrppt[k$year>1999]~yr))
# suggestive water year ppt trend in Knoxville, but not in BCM

# oddly in 2000 knoxville is wetter than BCM. 
# check knoxville 2000 values? 
d = read.table("knoxville_daily_data.txt", header=F)
head(d)
d$precip.in[d$precip.in==-9999] = NA
d$precip.in[d$precip.in>=10] = NA

stem(d$precip.in)

hist(d$precip.in[d$year==2000])
plot(d$precip.in[d$year==2000])
plot(d$precip.in[d$year==1999])
# big rain event in Nov 1999 but it comes after a string of missing values
# is it there in BCM data? 
ppt[grep("1999nov", ppt$yrmts),]
87.54/25.4 # yes, more or less. 
ppt[grep("1999oct", ppt$yrmts),]

# Temperature
t = read.csv("knoxville_annual_meantemp.csv")
head(t)

plot(t$wateryr.mean.temp, BCMdata$tmean.wateryr[BCMdata$year>1985]*8/5+32)
abline(0,1)
# BCM montly mean temps much different from weather station daily means
# but strong linear relationship

plot(1896:2015, BCMdata$tmean.wateryr[BCMdata$year>1895]*8/5+32, type="b", col="blue", ylim = c(50,64))

plot(1986:2015, BCMdata$tmean.wateryr[BCMdata$year>1985]*8/5+32, type="b", col="blue", ylim = c(54,64))
lines(1986:2015, t$wateryr.mean.temp[k$year>1985], type="b", col="orange3")
# The time series agree that the last 3 years have been historical hot outliers 


