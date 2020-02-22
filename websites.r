############################################################################
## Replication File for 
## "How Chinese Officials Use the Internet to Construct their Public Image"
## November, 2016
############################################################################


######################################################################
## Section 3.1: Website Content
######################################################################
# loading libraries
library(tidyverse)


# loading in the countywebsites data set (which lists the websites for all chinese counties that had them in place (2,867 counties))

countyweb <- read.csv("raw-data/countywebsites.csv") 


# Returning the dimensions in the data set (2,867 lines (counties) and 5 variables)

dim(countyweb) #2,867 counties


# Filtering out the counties for which there is no website and assigning them to a data frame called countyweb_na

countyweb_na <- countyweb[countyweb$Website == "",]


# Counting the number of counties in that data frame

dim(countyweb_na) # 80 counties without websites


# Counting the number of counties that have websites by subtracting the first dimension of the countyweb_na (that contains the counties that do not have a website) data frame from the first dimension of all counties included in the countyweb data frame

dim(countyweb)[1] - dim(countyweb_na)[1]  # 2,787 counties with websites 


# Counting the number of provinces represented through the 80 counties by counting the length of a vector that includes only the unique provinces in the countyweb_na data frame 

length(unique(countyweb_na$Province)) # 80 counties in 20 provinces


# Fig 1: Proportion of counties without websites by province
# Creating the prportion of counties without website in all provinces by dividing the tabled number of rows in a province from the countyweb_na data frame by the tabled number of rows in a province from the data frame that includes all counties

table(countyweb_na$Province) / table(countyweb$Province)


# 100 randomly sampled counties
# Loading in a random sample of 100 county websites, that are separated by a tab

county100 <- read.csv("raw-data/countywebsites_sampled100.csv", sep="\t") 


# Listing the column names of this sample

colnames(county100)


# Counting the number of provinces represented through the 100 random counties by counting the length of a vector that includes only the unique provinces in the countyweb_na data frame 

length(unique(county100$province_en))  # 100 counties in 29 provinces


# Building a count table of the two different county types (county-level cities or counties and districts)

table(county100$countytype) # 61 are county-level cities or counties, 39 are districts


# Building a count table of the three different macro-regions (Central, East, West)

table(county100$macroregion) # 34 in West China, 31 in Central, 35 in East


# For the random sample of 100, summing up the total number of links found from root url of countyurl across all counties, removing missing values with na.rm = TRUE 

sum(county100$linksall, na.rm=T) #2,015,061 links among all 100


# For the random sample of 100, summing up the total number of links found from root url of countyurl that were internal to website across all counties, removing missing values with na.rm = TRUE

sum(county100$linksint, na.rm=T) # 1,547,239 internal links


# Getting a summary of descriptive statistics for the number of links internal to the website in the random sample of 100 counties, given that they had more than 0 internal links, indicating a range from 49 to 162403 internal links

summary(county100$linksint[county100$linksint >0]) # range from 49 to 162,400, for website where links could be obtained


# Number of websites that had more than 100 scraped links that contained chinese text in a count table 

table(county100$links_chtxt > 100) # 71 counties with >100 Chinese web pages


######################################################################
## Section 4.1: Topics
######################################################################

# Loading in output of LDA topic model, including topic number (topic), hand label (label), corresponding ogi category (obi), and the top 50 words in Chinese with the word frequency (word1, word2, etc.), separating the columns by tabs

topics <- read.csv("raw-data/topics.csv",stringsAsFactors=F, sep="\t")


# listing the column headers of the topics data frame

colnames(topics)


# Counting the number of dimensions in the data set (50 different topics)

dim(topics)  # 50 topics


# Create count table of number of topics that were unable to label (11) vs. 39 that were able to lable

table(topics$label == "unable to label")  # could lable 39 topics


# Table 1
# Looking at the topics that are included under the respective ogi clusters

topics$label[topics$ogi=="administrative rules and regulations"]
topics$label[topics$ogi=="economic development plans"]
topics$label[topics$ogi=="statistical information"]
topics$label[topics$ogi=="budgets and financial accounts"]
topics$label[topics$ogi=="procurement standards"]
topics$label[topics$ogi=="administrative licensing"]
topics$label[topics$ogi=="major construction projects"]
topics$label[topics$ogi=="land acquisition and housing demolition"]
topics$label[topics$ogi=="poverty alleviation, education, health care, social security, employment"]
topics$label[topics$ogi=="emergency management plans"]
topics$label[topics$ogi=="environment, product quality and supervision"]
topics$label[topics$ogi=="other"]


# Install RCOlorBrewer and word cloud packages and load them
#install.packages("wordcloud")

library(RColorBrewer)
library(wordcloud)


# Load word cloud data, not turning strings into factors by default (stringAsFactors) and using the tab separation

wc <- read.csv("raw-data/wordcloud.csv", stringsAsFactors =F, sep="\t")


# splititng the wordcloud into groups by topics 

wc.plot <- split(wc, f=wc$topic)

## NOTE: word arrangement changes each time code is run, frequencies remain unchanged

# Using the wordcloud function in order to create a word cloud for each of the ogi clusters

# Fig 2(a) Economic development
wordcloud(wc.plot[[1]]$word_en, wc.plot[[1]]$freq, random.order =F, rot.per=0, max.words=50, scale=c(4,0.5))

# Fig 2(b) Health and social security
wordcloud(wc.plot[[3]]$word_en, wc.plot[[3]]$freq, random.order =F, rot.per=0, max.words=50, scale=c(4,0.5))

# Fig 2(c) Land rights and housing
wordcloud(wc.plot[[6]]$word_en, wc.plot[[6]]$freq, random.order =F, rot.per=0, max.words=50, scale=c(4,0.5))

# Fig (d) Emergency response
wordcloud(wc.plot[[4]]$word_en, wc.plot[[4]]$freq, random.order =F, rot.per=0, max.words=50, scale=c(4,0.5))

# Fig (e) Government approval process
wordcloud(wc.plot[[5]]$word_en, wc.plot[[5]]$freq, random.order =F, rot.per=0, max.words=50, scale=c(4,0.5))

# Fig (f) Public procurement and tenders
wordcloud(wc.plot[[2]]$word_en, wc.plot[[2]]$freq, random.order =F, rot.per=0, max.words=50, scale=c(4,0.5))


######################################################################
## Section 5.1: Measuring Tenure
######################################################################
   
# Again loading in 100 randomly sampled counties

county100 <- read.csv("raw-data/countywebsites_sampled100.csv", sep="\t") 


# Table 2: Distribution of Year in Office
# Creating a count table based of the number of counties in which the mayor has been in office for a certain number of years ( 1- 6 years)

table(county100$mayor_tenure2)


# Creating proportion table based on the number of years in office split by the status the mayor is in

prop.table(table(county100$mayor_tenure2, county100$mayor_status), margin=1)


# Proximity to Leaving Office as a count table based on number of counties in whoch a certain proximity applied
# proximity to leaving office for county executive; 3=if 2011 was last year of tenure, 2 = if 2011 was in middle of tenure, 1= if 2011 

table(county100$mayor_2011)  
  # 21 at beginning of tenure
  # 12 at end of tenure
  # 38 in them middle of tenure


######################################################################
## Section 5.2: Descriptive Results
######################################################################

# Loading in statistical analysis estimating the competence/benevolence level and lower and upper boundaries based on how many years an official has been in office 
# readme_out_yroff.csv: estimated proportion of posts for competence and benevolence by year in office, where group refers to the year, with 95% confidence intervals in lwr and upr. Generated using Hopkins and King (2010) ReadMe software

yroff <- read.csv("raw-data/readme_out_yroff.csv", header=TRUE)


# Loading in statistical analysis estimating the competence/benevolence level and lower and upper boundaries based on the proximity to tenure

# readme_out_tenure.csv: estimated proportion of posts for competence and benevolence by proximity to leaving office, where group = 1 refers beginning of tenure, group = 2 to middle of tenure, and group = 3 to end of tenure; 95% confidence intervals in lwr and upr. Generated using Hopkins and King (2010) ReadMe software

tenure <- read.csv("raw-data/readme_out_tenure.csv", header=TRUE)


# Defining y axis

yaxtloc <- c(0,.1,.2,.3,.4,.5)


# Labeling y axis

laxtlab <- c("0%", "10%","20%","30%","40%","50%")


# Defining x axis

xaxtloc <- c(0,.2,.4,.6,.8,1)


# Labeling x axis

lxaxtlab <- c("Beginning", "Middle","End")


# Fig 3: Proportion of web pages with content focused on competence   

# Setting the layout parameters in terms of how much they ought to be magnified in comparison to the defaul for the graph as 1,2 

par(mfrow=c(1,2)) 


# Creating a plot containing all x values from 1 to 5 years of office, and the respective estimate values as the dots in the graph, setting the y axis to run from 0 to 50%, setting the size of the dots to 16 and defining the x and y labels, the width a and the size of the ticks on c and y axis and the labels

plot(1:5, yroff[1:5,]$estimate,ylim=c(0,0.5), pch=16,xlab="Year in Office",ylab="% Web Pages for Competence",cex=1.5, cex.axis=1.5, cex.lab=1.5,yaxt="n")
axis(2, at= yaxtloc,labels= laxtlab, cex.axis=1.5)


# Using a loop to add the upper and lower boundaries of width 2 for each of the years in office (1-5)

for(i in 1:5){
	segments(i,yroff[i,]$lwr,i,yroff[i,]$upr, lwd=2)
}


# Creating a plot containing all x values from 1 to 3 proximity to tenure, and the respective estimate values as the dots in the graph, setting the y axis to run from 0 to 50%, setting the size of the dots to 16 and defining the x and y labels, the width a and the size of the ticks on c and y axis and the labels

plot(1:3, tenure[1:3,]$estimate,ylim=c(0,0.5), pch=16,xlab="Proximity to Leaving Office",ylab="% Web Pages for Competence",cex=1.5, cex.axis=1.5, cex.lab=1.5,xaxt="n",xlim=c(0.8,3.2),yaxt="n")
axis(2, at= yaxtloc,labels= laxtlab, cex.axis=1.5)
axis(1,at=c(1,2,3),labels= lxaxtlab,cex.axis=1.5)


# Using a loop to add the upper and lower boundaries of width 2 for each of the proximities to tenure (1-3)

for(i in 1:3){
	segments(i,tenure[i,]$lwr,i,tenure[i,]$upr, lwd=2)
}


# Fig 4: Proportion of web pages with content focused on benevolence

# Setting the layout parameters in terms of how much they ought to be magnified in comparison to the defaul for the graph as 1,2 

par(mfrow=c(1,2)) 


# Creating a plot containing all x values from 1 to 5 years of office, and the respective estimate values as the dots in the graph, setting the y axis to run from 0 to 50%, setting the size of the dots to 16 and defining the x and y labels, the width a and the size of the ticks on c and y axis and the labels

plot(1:5, yroff[6:10,]$estimate,ylim=c(0,0.5), pch=16,xlab="Year in Office",ylab="% Web Pages for Benevolence",cex=1.5, cex.axis=1.5, cex.lab=1.5,yaxt="n")
axis(2, at= xaxtloc,labels= laxtlab, cex.axis=1.5)


# Using a loop to add the upper and lower boundaries of width 2 for each of the years in office (1-5)

for(i in 1:5){
	segments(i,yroff[i+5,]$lwr,i,yroff[i+5,]$upr, lwd=2)
}


# Creating a plot containing all x values from 1 to 3 proximity to tenure, and the respective estimate values as the dots in the graph, setting the y axis to run from 0 to 50%, setting the size of the dots to 16 and defining the x and y labels, the width a and the size of the ticks on c and y axis and the labels

plot(1:3, tenure[4:6,]$estimate,ylim=c(0,0.5), pch=16,xlab="Proximity to Leaving Office",ylab="% Web Pages for Benevolence",cex=1.5, cex.axis=1.5, cex.lab=1.5,xaxt="n",xlim=c(0.8,3.2),yaxt="n")
axis(2, at= yaxtloc,labels= laxtlab, cex.axis=1.5)
axis(1,at=c(1,2,3),labels= lxaxtlab,cex.axis=1.5)


# Using a loop to add the upper and lower boundaries of width 2 for each of the proximities to tenure (1-3)

for(i in 1:3){
	segments(i,tenure[i+3,]$lwr,i,tenure[i+3,]$upr, lwd=2)
}

######################################################################
## Section 5.3: Predictive Inference
######################################################################

# load in 71 counties with over 100 Chinese language web pages, with svm predictions 

bycounty2 <- read.csv("raw-data/predict.csv")


# Create a statistical summary of the linear regression model that regresses a mayor first year or last year binary indicator on  he number of competence metnions on the websites by county

# Table 3 (1)
  summary(lm(comp ~ mayor_first + mayor_last, data=bycounty2))  # ALMOST: mayo first yr more compolence

  
  # Adds county 2009 gdp per capita and proportion of county population over the age of 15 who are illiterate and number of county resident employed in information production, wholesale and retail from the 2010 census and total links found from root url of countyurl and 2009 provincial level expenditure on culture and media multiplied by the ratio of county GDP to province GDP
  
# Table 3 (2)
summary(lm(comp ~ mayor_first +mayor_last +X2009_gdppc_cny+X2010_illiterateprop+itemploy + linksall+ county_mediaexp, data=bycounty2))


# Adds whether county party secretary is in first year of office; takes on value of 1 if sec_2011 = 1, 0 otherwise and whether county party secretary is in last year of office; takes on value of 1 if sec_2011 = 1, 0 otherwise
# Table 3 (3)

summary(lm(comp ~ mayor_first + mayor_last + X2009_gdppc_cny + X2010_illiterateprop+itemploy + linksall+ county_mediaexp + sec_first + sec_last, data=bycounty2)) 


# Adds whether whether prefecture party secretary is in first or last year of office; takes on value of 1 if pref_ps_2011 = 1, 0 otherwise and evel of education of the prefecture party secretary at the end of formal schooling (not education while a government official); 6 = Doctoral degree; 5 = Master's degree; 4 = Bachelor's degree; 3 = degree from junior college; 2 = high school, 1 = lower than junior college and gdp per capita of the prefecture in 2010 

# Table 3 (4)
summary(lm(comp ~ mayor_first + mayor_last + X2009_gdppc_cny + X2010_illiterateprop+itemploy + linksall+ county_mediaexp + sec_first + sec_last + pref_ps_first + pref_ps_last + pref_ps_edulevel + pref_2010_gdppc, data=bycounty2)) 


# Adds mayor age, mayor gender and mayor education level

# Table 3 (5)
summary(lm(comp ~ mayor_first + mayor_last + X2009_gdppc_cny + X2010_illiterateprop+itemploy + linksall+ county_mediaexp + sec_first + sec_last + pref_ps_first + pref_ps_last + pref_ps_edulevel + pref_2010_gdppc + mayor_age + mayor_gender + mayor_edulevel, data=bycounty2)) 


# Adds mayor promotion

# Table 3 (6)
summary(lm(comp ~ mayor_first + mayor_last + X2009_gdppc_cny + X2010_illiterateprop+itemploy + linksall+ county_mediaexp + sec_first + sec_last + pref_ps_first + pref_ps_last + pref_ps_edulevel + pref_2010_gdppc + mayor_age + mayor_gender + mayor_edulevel+ mayor_promote, data=bycounty2)) 


# Same as above for competence but for benevolence

# Table 4 (1)
summary(lm(benev ~ mayor_first + mayor_last, data=bycounty2))

# Table 4 (2)
summary(lm(benev ~ mayor_first +mayor_last +X2009_gdppc_cny+X2010_illiterateprop+itemploy + linksall+ county_mediaexp , data=bycounty2))

# Table 4 (3)
summary(lm(benev ~ mayor_first + mayor_last + X2009_gdppc_cny + X2010_illiterateprop+itemploy + linksall+ county_mediaexp  + sec_first + sec_last, data=bycounty2)) 

# Table 4 (4)
summary(lm(benev ~ mayor_first + mayor_last + X2009_gdppc_cny + X2010_illiterateprop+itemploy + linksall+ county_mediaexp  + sec_first + sec_last + pref_ps_first + pref_ps_last + pref_ps_edulevel + pref_2010_gdppc, data=bycounty2)) 

# Table 4 (5)
summary(lm(benev ~ mayor_first + mayor_last + X2009_gdppc_cny + X2010_illiterateprop+itemploy + linksall+ county_mediaexp + sec_first + sec_last + pref_ps_first + pref_ps_last + pref_ps_edulevel + pref_2010_gdppc + mayor_age + mayor_gender + mayor_edulevel, data=bycounty2)) 

# Table 4 (6)
summary(lm(benev ~ mayor_first + mayor_last + X2009_gdppc_cny + X2010_illiterateprop+itemploy + linksall+ county_mediaexp + sec_first + sec_last + pref_ps_first + pref_ps_last + pref_ps_edulevel + pref_2010_gdppc + mayor_age + mayor_gender + mayor_edulevel+ mayor_promote, data=bycounty2)) 

sink()
