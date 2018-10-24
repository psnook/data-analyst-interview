

RaleighOpenData <- read.csv(url("https://s3.amazonaws.com/cc-analytics-datasets/Building_Permits.csv"))
nrow(RaleighOpenData)
ncol(RaleighOpenData)
length(unique(RaleighOpenData$const_type))
mean(RaleighOpenData$numberstories, na.rm=TRUE)
median(RaleighOpenData$numberstories, na.rm = TRUE)
sd(RaleighOpenData$X, na.rm = TRUE)
sd(RaleighOpenData$Y, na.rm = TRUE)

mean(RaleighOpenData$estprojectcost)
median(RaleighOpenData$estprojectcost)
sd(RaleighOpenData$estprojectcost)
range(RaleighOpenData$estprojectcost)
length(unique(RaleighOpenData$contractorcity))


hist(RaleighOpenData$issueddate_mth, main = "Histogram of Issue Date Month", xlab = 'Issue Date Month')
max(RaleighOpenData$estprojectcost)
hist(RaleighOpenData$estprojectcost) #,xlim =c(0,1.7e+08))

populatedEstProjCost <- RaleighOpenData$estprojectcost > 0
hist(RaleighOpenData$estprojectcost(populatedEstProjCost))

if (RaleighOpenData$estprojectcost > 0){
  a <- RaleighOpenData
}

library(dplyr)
FilteredRaleightOpenData <- RaleighOpenData %>% filter(workclassmapped == "New" & const_type == "V  B" & numberstories < 3)
list(unique(FilteredRaleightOpenData$numberstories))

FilteredRaleightOpenData <- FilteredRaleightOpenData %>% filter(numberstories > 0 & estprojectcost > 0 & issueddate_yr != 'NA')
# Remove any with zero
myfit = lm(FilteredRaleightOpenData$estprojectcost ~ FilteredRaleightOpenData$issueddate_yr )
summary(myfit)
cor(FilteredRaleightOpenData$estprojectcost,FilteredRaleightOpenData$issueddate_yr)
