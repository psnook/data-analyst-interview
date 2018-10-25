
#Reading in the dataset provided on github
RaleighOpenData <- read.csv(url("https://s3.amazonaws.com/cc-analytics-datasets/Building_Permits.csv"))

#finding the total number of rows in the dataset
nrow(RaleighOpenData)

#finding the total number of columns in the dataset
ncol(RaleighOpenData)

#finding the total different types of construction, using the unique function to find unique values and length
#to print how many unique values there are
length(unique(RaleighOpenData$const_type))

#Calculating the mean of the number of stories, using na.rm=TRUE so we only calculate the mean with populated stories
mean(RaleighOpenData$numberstories, na.rm=TRUE)

#Calculating the median of the number of stories, using na.rm=TRUE so we only calculate the median with populated stories
median(RaleighOpenData$numberstories, na.rm = TRUE)

#Calculating the standard deviation of the X coordinate of the permits
sd(RaleighOpenData$X, na.rm = TRUE)

#Calculating the standard deviation of the Y coordinate of the permits
sd(RaleighOpenData$Y, na.rm = TRUE)

#Calculating the mean of the estimated project costs
mean(RaleighOpenData$estprojectcost)

#Calculating the median of the estimated project costs
median(RaleighOpenData$estprojectcost)

#Calculating the standard deviation of the estimated project costs
sd(RaleighOpenData$estprojectcost)

#Calculating the range of the estimated project costs
range(RaleighOpenData$estprojectcost)

#Finding the amount of unique cities contractors are in
length(unique(RaleighOpenData$contractorcity))

#Exploratory Data Analysis:
#From the calculations conducted above, it is first important to look at the size of the dataset, where there
# are 141,953 rows (or observations) and 87 columns (or variables). The total amount of different types of 
# construction present in the dataset are 19. By removing the missing data in the number of stories column,
# the mean was able to be calculated as 9.707421 stories and the median to be 2 stories. The relatively large
# difference between the mean and median is due to outliers, as some buildings in the dataset have 
# many floors, most likely buildings to be located in downtown Raleigh.By following the 
# same logic with missing data for X and Y coordinates of the permits, the X coordinate was able to be
# calculated as 0.06880534 and the Y coordinate as 0.05839208. The small standard deviations show us that
# the locations of the permits are within a close vicinity to each other. This is understandable, as the
# dataset is based in Raleigh. 
# Along with the required calculations listed, I was interested in the estimated project cost. The mean
# for the estimated project cost was 192,012.7 and the median was 54,404. The large difference indicates
# that there are outliers present within the data. The standard deviation for estimated project cost is
# 1,493,678. This shows us that the costs of the projects are very spread out. The range of the estimated
# project costs are from 0 to 170,000,000.

# Creating a histogram for the issue date month. Based upon the histogram, we can see that the histogram appears
# to follow a bell-curve shape, as the months that have the most permits issued occur in the summer months.
hist(RaleighOpenData$issueddate_mth, main = "Histogram of Issue Date Month", xlab = 'Issue Date Month', col = "red")

#Created a histogram to show the distribution of the estimated project costs. In the first histogram, it can
# be seen that there is only one bar in the histogram. This was due to the outliers heavily skewing the 
# data. To try to create a histogram that can show the distribution a little better, I created a new dataset
# title 'removeoutlierRalegih', keeping observations where the estimated project costs were less than 500000.
# In this step as well as others, I created a new dataset instead of filtering the original to be able to
# maintain the original dataset. So, based upon the two histograms, it is evident that the estimated 
# project costs are heavily skewed right. To use the filter function in R, I included install.packages and 
# library to install and load in the apporpriate things necessary for it.

install.packages('dplyr')
library(dplyr)

hist(RaleighOpenData$estprojectcost, main = "Histogram of Estimated Project Costs", xlab = 'Estimated Project Cost', col = 'blue')
removeoutlierRaleigh <- RaleighOpenData %>% filter(estprojectcost <500000)
hist(removeoutlierRaleigh$estprojectcost, main = "Histogram of Estimated Project Costs", xlab = 'Estimated Project Cost', col = 'blue')


# To first approach the this part of the project, I created a new dataset titled 'FilteredRaleighOpenData',
# where I was able to filter the data from RaleighOpenData and bring over observations for "New" construction
# of type "V  B" and where stories were less than 3.
FilteredRaleightOpenData <- RaleighOpenData %>% filter(workclassmapped == "New" & const_type == "V  B" & numberstories < 3)

#To make sure I had the correct number of stories in the new dataset, I listed out the unique number of stories.
# When initially running this, I saw that I had observations where there were 0 number of stories.
list(unique(FilteredRaleightOpenData$numberstories))


# I assume that having 0 number of stories in incorrect so I did not want to include those observations in the 
# analysis. It is also not likely that the estimated cost of a project was $0, so I also did not want those to 
# impact the analysis as well. Even with removing the missing data, there was still a large sample given the
# conditions listed above.

#Filtering out the data from FilteredRaleighOpenData where the number of stories is 0, estimated project cost
# is 0, and where the issue month year is missing.
FilteredRaleightOpenData <- FilteredRaleightOpenData %>% filter(numberstories > 0 & estprojectcost > 0 & issueddate_yr != 'NA')

#Created a simple linear regression for estimated project cost and issue date year
myfit = lm(FilteredRaleightOpenData$issueddate_yr ~ FilteredRaleightOpenData$estprojectcost)
summary(myfit)
cor(FilteredRaleightOpenData$estprojectcost,FilteredRaleightOpenData$issueddate_yr)

# When looking at the summary of the regression and the correlation of the two, it is shown that the two 
# variables are not related. Considering that the estimated project costs are heavily skewed, I 
# wanted to take a look at other regressions that could address that issue.

# Created a simple log regression for estimated project cost and issue date year.
# Based upon the summary output, it is shown that this is still not a good model for the two variables, a 
# great indicator of that is the very high AIC value.
mylogfit = glm(FilteredRaleightOpenData$issueddate_yr ~ FilteredRaleightOpenData$estprojectcost)
summary(mylogfit)

#Created a regression for issue date year and the log of the estimated project cost. Still not a greate model,
# however is better than a simple linear regression.
newfit = lm(FilteredRaleightOpenData$issueddate_yr ~ log(FilteredRaleightOpenData$estprojectcost))
summary(newfit)

# Regression with the estimated project cost squared, not a good model, AIC very high.
newsqfit = lm(FilteredRaleightOpenData$issueddate_yr ~ (FilteredRaleightOpenData$estprojectcost)^2)
summary(myfit)

# Based upon the regression analyses performed above, I don't believe that there are any accurate insights
# we can gleam upon based on the results. A more in-depth analysis consisting with more variables may 
# be able to provide better insight.
