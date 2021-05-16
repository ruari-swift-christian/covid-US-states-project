# covid-US-states-project

## PROJECT Goals: F

#First Use Linear Regression To Find A Linear Relationship between Total Population and Cases and Total Population.
#Secondly, Do a Principal Component Analysis of each variable to see the importance of each one and the distribution of Variance
#Thirdly, Test out Clustering methods via Kmeans as well as a Principal Component bar graph for each component.

#DATA VISUALIZATION:

#Create Scatterplot Matrix to see aboutth e relationship between 6 variables: Total Covid-19 Tests, Total Covid-19 Cases, total people vaccinated in the entire State
# Continued.. Total State Population, Total negative tests, total Positive Tests.
#Next a Correlation Plot for all 6 variables: there was a linear relationship and correlation between cases, total vaccinated, and total state population
#make a added variable Plot
#create a Animated KMeans Plot




## R Markdown Code:

---
title: "Covid data per US State"
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
library(plyr)
library(readxl)
library(randomForest)
library(tidyverse)
library(rpart)
library(dplyr)
library(rsample)
library(rpart.plot)
library(caret)
library(ipred)
library(stats)
library(factoextra)
library(FactoMineR)
library(graphics)
library(lattice)
library(arules)
library(car)
library(corrplot)
library(corrgram)
library(mclust)
library(fpc)
library(e1071)
library(scatterplot3d)
library(effects)
library(coefplot)
library(animation)
library(tweenr)
```

###Load the Data, give summary of data set, then use various data mining methods
```{r}
dat1=read.csv("covid_project1.csv", header=TRUE, sep=',', dec='.', stringsAsFactors=FALSE)
dat1
summary(dat1)
# eliminate NA rows
dat1<-dat1[,-26]
dat1<-dat1[,-25]
```
```{r}
dat1<-dat1[,-26]
dat1<-dat1[,-25]
dat1$daily.positive.<- as.numeric(sub("%","",dat1$daily.positive.))/100

#make every other column numerical
dat1$Total.Covid.19.Deaths <-as.numeric(gsub(",","",dat1$Total.Covid.19.Deaths))
dat1$Total.Covid.19.Cases <- as.numeric(gsub(",","",dat1$Total.Covid.19.Cases))
dat1$weekly.average.cases <- as.numeric(gsub(",","",dat1$weekly.average.cases))
dat1$vaccines.distributed <- as.numeric(gsub(",","",dat1$vaccines.distributed))
dat1$vaccinated <- as.numeric(gsub(",","",dat1$vaccinated))
dat1$Total.Population <- as.numeric(gsub(",","",dat1$Total.Population))
dat1$Covid.Cases.65. <- as.numeric(gsub(",","",dat1$Covid.Cases.65.))
dat1$Covid.Cases.under.65 <- as.numeric(gsub(",","",dat1$Covid.Cases.under.65))
dat1$recovered <- as.numeric(gsub(",","",dat1$recovered))
dat1$total.number.of.tests <- as.numeric(gsub(",","",dat1$total.number.of.tests))
dat1$daily.positive. <- as.numeric(gsub(",","",dat1$daily.positive.))
dat1$PCR. <- as.numeric(gsub(",","",dat1$PCR.))
dat1$PCR..1 <- as.numeric(gsub(",","",dat1$PCR.))
dat1$antigen. <- as.numeric(gsub(",","",dat1$antigen.))
dat1$antigen..1 <- as.numeric(gsub(",","",dat1$antigen..1))
dat1$Total.. <- as.numeric(gsub(",","",dat1$Total..))
dat1$Total...1 <- as.numeric(gsub(",","",dat1$Total...1))
#multiple Linear Regression Model for Cases and tests
 multi.fit <- lm(dat1$Total.Covid.19.Cases~dat1$vaccinated+dat1$Total.Covid.19.Deaths+dat1$Covid.Cases.65.+dat1$Covid.Cases.under.65, na.action=na.exclude, data=dat1)
 summary(multi.fit)
 #96.2% of the Variability can be explained in the data model for COVID 19 cases, the p value is incredibly small so the target coefficient (cases) depends on the predictors (Total Covid deaths, vaccinations, and cases by old and young age groups). The Null hypothesis is not rejected and there is no linear relationship with total cases and the predictors given the large pr(>|T|) for the intercept
multi.fit2 <- lm(dat1$total.number.of.tests ~ dat1$Total..+ dat1$Total...1+dat1$PCR.+ dat1$PCR..1+ dat1$antigen.+ dat1$antigen..1 ,data=dat1)
summary(multi.fit2)
#33.11% variability can be explained in COVID 19 test model however the p value is greater than .0001 so the target variable (Test) does not depend on the predictor variables : total positive tests, total negative tests, PCR positive, PCR negative, antigen negative and positive. the null hypothesis is rejected and there is a linear relationship with Total tests and the positivity and negativity predictors given the small pr(>|T|) for the intercept

#anova test for Case model
anova(multi.fit)
#all predictors show good F values and contribute to the reduction of the total error of the  case model
#anova test for Test model
anova(multi.fit2)
#the predictors of both positive and negative PCR and antigen show a very small F value and contribute the least to the reduction of the fitting error but to no surprise given the small amount of data for these variables

# remove PCR and antigen positive and negative in the Test Model
multi.fit2U <- update(multi.fit2, . ~ . - dat1$PCR.)
multi.fit2U <- update(multi.fit2U, . ~ . -dat1$PCR..1)
multi.fit2U <- update(multi.fit2U, . ~ . -dat1$antigen.)
multi.fit2U <- update(multi.fit2U, . ~ . -dat1$antigen..1)
multi.fit2U
#test for improvement of the model after the elimination of PCR +/- and antigen +/- coefficients
summary(multi.fit2U)
# Test Model line of fit improved by 30.07%

```

```{r}
# k means clustering
#set random seed
set.seed(123)
#change the Y/N columns to 1/2 aswell as change the numbers in characters "" to factors
#age case data
dat1$state.provided.age.case.data.<-factor(dat1$state.provided.age.case.data.)
dat1$state.provided.age.case.data.<-as.numeric(dat1$state.provided.age.case.data.)
dat1$state.provided.age.case.data.
#recovery data provided
dat1$State.provided.recovered.data.<-factor(dat1$State.provided.recovered.data.)
dat1$State.provided.recovered.data.<-as.numeric(dat1$State.provided.recovered.data.)
#test data provided by State
dat1$State.provided.total.test.data.<-factor(dat1$State.provided.total.test.data.)
dat1$State.provided.total.test.data.<-as.numeric(dat1$State.provided.total.test.data.)
#daily positive data given 
dat1$Daily.positve..given.<-factor(dat1$Daily.positve..given.)
dat1$Daily.positve..given.<-as.numeric(dat1$Daily.positve..given.)
#antigen and pcr given
dat1$PCR.antigen.data.given.<-factor(dat1$PCR.antigen.data.given.)
dat1$PCR.antigen.data.given.<-as.numeric(dat1$PCR.antigen.data.given.)
# total positive and negative given
dat1$Total.....data.given.<-factor(dat1$Total.....data.given.)
dat1$Total.....data.given.<-as.numeric(dat1$Total.....data.given.)

#convert daily positive percentages to decimals
dat1$daily.positive.<- as.numeric(sub("%","",dat1$daily.positive.))/100

#make every other column numerical
dat1$Total.Covid.19.Deaths <-as.numeric(gsub(",","",dat1$Total.Covid.19.Deaths))
dat1$Total.Covid.19.Cases <- as.numeric(gsub(",","",dat1$Total.Covid.19.Cases))
dat1$weekly.average.cases <- as.numeric(gsub(",","",dat1$weekly.average.cases))
dat1$vaccines.distributed <- as.numeric(gsub(",","",dat1$vaccines.distributed))
dat1$vaccinated <- as.numeric(gsub(",","",dat1$vaccinated))
dat1$Total.Population <- as.numeric(gsub(",","",dat1$Total.Population))
dat1$Covid.Cases.65. <- as.numeric(gsub(",","",dat1$Covid.Cases.65.))
dat1$Covid.Cases.under.65 <- as.numeric(gsub(",","",dat1$Covid.Cases.under.65))
dat1$recovered <- as.numeric(gsub(",","",dat1$recovered))
dat1$total.number.of.tests <- as.numeric(gsub(",","",dat1$total.number.of.tests))
dat1$daily.positive. <- as.numeric(gsub(",","",dat1$daily.positive.))
dat1$PCR. <- as.numeric(gsub(",","",dat1$PCR.))
dat1$PCR..1 <- as.numeric(gsub(",","",dat1$PCR.))
dat1$antigen. <- as.numeric(gsub(",","",dat1$antigen.))
dat1$antigen..1 <- as.numeric(gsub(",","",dat1$antigen..1))
dat1$Total.. <- as.numeric(gsub(",","",dat1$Total..))
dat1$Total...1 <- as.numeric(gsub(",","",dat1$Total...1))

#convert dat1 to data frame
covid_df<-as.data.frame(dat1, stringsAsFactors = FALSE)
covid_df<- as.data.frame(sapply(covid_df, function(x) gsub("\"", "", x)))
#create data function to fix data types and round
to_numeric_and_round_func <- function(x){
  round(as.numeric(as.character(x)),2)
}
#Mutate the columns to proper data type
dat1 <- dat1%>% mutate_at(vars(-one_of("State")), to_numeric_and_round_func)
#get rid of state column
dat1$State <- NULL
#Set Random Seed
set.seed(1234)
#Kmeans function for cases,deaths,weekly cases, vaccines distributed, vaccines taken
covid_means<- kmeans(dat1[,1:5], centers = 5)
covid_means_table <- data.frame(covid_means$size, covid_means$centers)
covid_means_df <- data.frame(Cluster = covid_means$cluster, dat1)
print(covid_means_df)
##ggplot of covid cases with the cluster analysis
ggplot(data = covid_means_df, aes(y = Cluster)) +
  geom_bar(aes(fill = Total.Covid.19.Cases)) +
  ggtitle("Count of Clusters by Total.Covid.19.Cases") +
  theme(plot.title = element_text(hjust = 0.5))
#Fancy Kmeans
fviz_nbclust(scale(dat1[,1:5]), kmeans, nstart=100, method = "wss") + 
  geom_vline(xintercept = 5, linetype = 1)
covid_means_fancy <- kmeans(scale(dat1[,1:5]), 5, nstart = 100)
#plot the clusters
fviz_cluster(covid_means_fancy, data = scale(dat1[,1:5]), geom = c("point"),ellipse.type = "euclid")
#All Eclipses are compact for 2,3,4 clusters so they are compact but there is very high variability especially in clusters 1 and 5 making it harder to  find the mean in teh sum of square errors
```

```{r}
#Principal Component Analysis
dat1<-dat1[,-26]
dat1<-dat1[,-25]
dat1$State <- NULL

#make every other column numerical
dat1$Total.Covid.19.Deaths <-as.numeric(gsub(",","",dat1$Total.Covid.19.Deaths))
dat1$Total.Covid.19.Cases <- as.numeric(gsub(",","",dat1$Total.Covid.19.Cases))
dat1$weekly.average.cases <- as.numeric(gsub(",","",dat1$weekly.average.cases))
dat1$vaccines.distributed <- as.numeric(gsub(",","",dat1$vaccines.distributed))
dat1$vaccinated <- as.numeric(gsub(",","",dat1$vaccinated))
dat1$Total.Population <- as.numeric(gsub(",","",dat1$Total.Population))
dat1$Covid.Cases.65. <- as.numeric(gsub(",","",dat1$Covid.Cases.65.))
dat1$Covid.Cases.under.65 <- as.numeric(gsub(",","",dat1$Covid.Cases.under.65))
dat1$recovered <- as.numeric(gsub(",","",dat1$recovered))
dat1$total.number.of.tests <- as.numeric(gsub(",","",dat1$total.number.of.tests))
dat1$daily.positive. <- as.numeric(gsub(",","",dat1$daily.positive.))
dat1$PCR. <- as.numeric(gsub(",","",dat1$PCR.))
dat1$PCR..1 <- as.numeric(gsub(",","",dat1$PCR.))
dat1$antigen. <- as.numeric(gsub(",","",dat1$antigen.))
dat1$antigen..1 <- as.numeric(gsub(",","",dat1$antigen..1))
dat1$Total.. <- as.numeric(gsub(",","",dat1$Total..))
dat1$Total...1 <- as.numeric(gsub(",","",dat1$Total...1))
#make a matrix version of dat1
dat1_mat <- as.matrix(dat1)
princomp(dat1, cor = FALSE, scores = TRUE, covmat = NULL, subset = rep(TRUE, nrow(dat1_mat)))
#load and plot principal components
summary(pc.cr <- princomp(dat1_mat, cor = FALSE))
loadings(pc.cr)
plot(pc.cr)
biplot(pc.cr)
#second plot
pc.cr$scores
pca.plot <- xyplot(pc.cr$scores[,2] ~ pc.cr$scores[,1])
pca.plot$xlab <- "First Component"
  pca.plot$ylab <- "Second Component"
pca.plot  

```

```{r}
#Fuzzy C-means
x<-rbind(dat1$Total.Covid.19.Deaths,dat1$Total.Covid.19.Cases,dat1$weekly.average.cases)
x<-t(x)
result<-cmeans(x,5,50, verbose=TRUE, method = "cmeans")
print(result)

```



```{r}
#Scatterplot  finding the relationship between number of tests, number vaccinated, total covid cases, total negative and positive covid tests, and total population
pairs(~ Total.Covid.19.Cases + total.number.of.tests + vaccinated + Total...1 + Total.. + Total.Population, data=dat1)
#make data look good 
#Scatterplot matrix
scatterplotMatrix(~Total.Covid.19.Cases + vaccinated + total.number.of.tests + Total.. + Total...1 + Total.Population, data=dat1, id.n=50)
#there is a strong linear relationship between cases and vaccinated, cases and population, vaccinated and total population, total cases and total tests, vaccinated and total tests, total cases/total vaccinated/ total # of tests/ total negative tests/total population  all have a linear relationship with total positive tests. same goes for total negative tests which has a linear relationship with all other variables


#correlation plots
corrgram(dat1[1:8], upper.panel=panel.cor, diag.panel=panel.density)
#added variable plot
dat1.mod <- lm(vaccinated~ vaccines.distributed + weekly.average.cases, data=dat1)
plot(dat1.mod)
avPlots(dat1.mod, id.n=2, ellipse=TRUE)
#effects plot
dat1.eff1 <- allEffects(dat1.mod)
plot(dat1.eff1)
#coefficient plot
dat1.mod2 <- lm(weekly.average.cases~ vaccinated * vaccines.distributed, data=dat1)
coefplot(dat1.mod2, intercept=FALSE, lwdInner=2, lwdOuter=1, title="Coefficient Plot for weekly cases vs vaccinated * vaccines distr.")
```
```{r}
#kmeans animation
ani.options(interval = 1)
kmeans.ani(dat1, centers = 5)
```

