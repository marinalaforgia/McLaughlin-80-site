## Script for exploring correlations with seedbank abundance
rm(list=ls())
# 1. Load necessary packages and files
# 2. SAI correlations
    # a. Various traits
    # b. Burned in 1999 and burned in 2000
    # b. grazed and ungrazed
    # c. serpentine and nonserpentine
    # d. Dry and wet years
    # e. grass and forbs
    # g. year to year variation in frequency
# 3. Compare frequency data above/below ground for study species
# 4. See which year or sequence of years best predicts abundance in seedbank for different life forms
# 5. See what year "types" best predicts abundance in seedbank for different life forms
# SLA/SAI correlated with abundance over time
# number of years seen aboveground

####
# 1. Load files and packages
####

library(plyr)
library(ggplot2)
SAI.sp.S <- read.csv("SAI-by-species-S.csv")
SAI.sp <- read.csv("SAI-by-species.csv")
SAI.site.S <- read.csv("SAI-by-site-S.csv")
SAI.site <- read.csv("SAI-by-site.csv")
SB <- read.csv("Core_Seedbank.csv")
cover <- read.csv("/Users/Marina/Documents/UC-Davis/02_McLaughlin_80Sites_Organized/Data-Storage-Files/Core_Community_Data2015.csv")
abiotic <- read.csv("/Users/Marina/Documents/UC-Davis/02_McLaughlin_80Sites_Organized/Data-Storage-Files/Basic Abiotic Data.csv")
burn <- read.csv("/Users/Marina/Documents/UC-Davis/02_McLaughlin_80Sites_Organized/Data-Storage-Files/Burn Records.csv")
graze <- read.csv("/Users/Marina/Documents/UC-Davis/02_McLaughlin_80Sites_Organized/Data-Storage-Files/Grazing Record.csv")
trait <- read.csv("/Users/Marina/Documents/UC-Davis/02_McLaughlin_80Sites_Organized/Modified_CombinedFiles/McL_80SitesSpeciesTraits_012615.csv")
trait$PerNS <- as.numeric(trait$PerNS)
clim <- read.csv("/Users/Marina/Documents/UC-Davis/Research/Phenology/Final Data/climate.csv")

####
# 2. Alter Pairs plot to look at correlations among variables
#####
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  rreal = cor(x,y)
  r <- abs(cor(x, y))
  txt <- format(c(rreal, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  test <- cor.test(x,y)
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))
  
  text(0.5, 0.5, txt, cex = cex * r +0.5)
  text(.8, .8, Signif, cex=cex, col=2)}

#add a regression line to each plot
panel.lmline = function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                         cex = 1, col.smooth = "red", ...) 
{points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    abline(lm(y[ok] ~ x[ok]), 
           col = col.smooth, ...)}

####
# 3. SAI correlations 
####
# NOTE: should i use average SAI per plot or compute a community weighted measure?

# (a) Correlate SAI with traits
  # Treating serp/nonserp separately for traits is the same as averaging across soil types either for traits/SAI or both
  trait <- trait[,c(1:11,15,16)] 
  t.trait <- reshape(trait, varying = list(c(2,7),c(3,8),c(4,9),c(5,10),c(6,11)), v.names = c("Height","SLA","LWC","CN","PerN"), timevar = "Serpentine", times = c("S","N"), idvar = c("Species_Name","Native.Exotic", "Grass.Forb.Shrub"), direction = "long")
  SAI.trait.S <- merge(SAI.sp.S, t.trait, by = c("Species_Name", "Serpentine"))
  SAI.trait.S <- na.omit(SAI.trait.S)
  
  # Pairs plots
  pairs(SAI.trait.S[SAI.trait.S$Serpentine == "N",c(3:11)],lower.panel=panel.lmline, 
               upper.panel=panel.cor,main="Nonserpentine")
  
  pairs(SAI.trait.S[SAI.trait.S$Serpentine == "S",c(3:11)],lower.panel=panel.lmline, 
        upper.panel=panel.cor,main="Serpentine")
 
  #averaging traits across soil types
  t.trait <- na.omit(t.trait)
  trait.avg <- ddply(t.trait, "Species_Name", summarize, Height = mean(Height), SLA = mean(SLA), LWC = mean(LWC), CN = mean(CN), PerN = mean(PerN))
  SAI.trait <- merge(SAI.sp, trait.avg, by = "Species_Name")
  SAI.trait <- na.omit(SAI.trait)
  
  pairs(SAI.trait[,c(2:10)],lower.panel=panel.lmline, upper.panel=panel.cor)
  
  #pulling out traits by forbs and grasses
  t.trait <- na.omit(t.trait)
  trait.avg <- ddply(t.trait, c("Species_Name","Grass.Forb.Shrub"), summarize, Height = mean(Height), SLA = mean(SLA), LWC = mean(LWC), CN = mean(CN), PerN = mean(PerN))
  SAI.trait <- merge(SAI.sp, trait.avg, by = "Species_Name")
  SAI.trait <- na.omit(SAI.trait)
  
  pairs(SAI.trait[SAI.trait$Grass.Forb.Shrub == "Forb",c(2:10)],lower.panel=panel.lmline, upper.panel=panel.cor)
# (b) Burned in 1999 and burned in 2000
  head(burn)
  burn1999 <- filter(burn, Season_Year == 1999)
  
  
# (c) grazed and ungrazed
# (d) serpentine and nonserpentine
  ggplot(SAI.site.S, aes(x = SAI)) +
    facet_wrap(~Serpentine) +
    geom_density(aes(col = Grass.Forb))
  
  ggplot(SAI.site.S, aes(x = SAI, col = Grass.Forb)) +
    geom_density()
  
  SAI.site.S <- na.omit(SAI.site.S)
  SAI.site.avg <- ddply(SAI.site.S, c("site", "Year", "Serpentine"), summarize, SAI = mean(SAI), SAI = mean(SAI), SAI_Index1 = mean(SAI_Index1), SAI_Index2 = mean(SAI_Index2), SAI_RelAb = mean(SAI_RelAb))
 
  ggplot(SAI.site.S, aes(x = Year, y = Count, col = Serpentine)) +
    geom_line(aes(by = Species_Name, col = Serpentine)) +
    facet_wrap(~Grass.Forb) +
    geom_smooth(method = "lm", aes(col = Serpentine)) +
    ggtitle("Count by Site")  
  
  ggplot(SAI.site.avg, aes(x = Year, y = SAI)) +
    geom_smooth(method = "lm", aes(col = Serpentine, by = site)) +
    ggtitle("SAI Between Years by Site")  

  # (e) Dry and wet years
# (f) grass and forbs
# (g) year to year variation in frequency
  
####
# Seeds per plot
####
sb.site.sum <- ddply(SAI.site.S, c("Serpentine","Species_Name","Grass.Forb","Year"), summarize, Count = sum(Count))


ggplot(sb.site.sum, aes(x = Year, y = Count)) + 
  geom_line(aes(by = Species_Name, col = Serpentine)) +
  facet_wrap(~Grass.Forb) +
  geom_smooth(method = "lm", aes(col = Serpentine)) +
  ggtitle("Overall Species Counts")


#####
# Shifting Time windows
####
 # seedbank abundance of forbs ~ 2012 cover
####
# Fun with bayes
####
library(rethinking)
m1stan <- map2stan(
  alist(
    Count ~ dnorm(mu, sigma),
    mu <- a + s*Serpentine + y*Year + f*Grass.Forb.Shrub,
    a
    s
    y
    f
    sigma
  ), data = sbank.short
)