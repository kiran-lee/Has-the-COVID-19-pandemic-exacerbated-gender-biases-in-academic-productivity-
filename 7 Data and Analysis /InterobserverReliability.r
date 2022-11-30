
# You probably will have to install these two packages first:
library(psych)
library(dplyr)

# Based on Kiran's report:
# In Dieter's classification, there were: 30 included, 110 excluded
# In Kiran's classification: of the 30 Dieter included, 25 included ; of the 110 Dieter excluded, 3 included

# Setting up the data: for Dieter, we have 30 included study (with value 1 = included) and 110 excluded studies (with value 0 = excluded)
DieterClassification<-c(rep(1,26),rep(0,114))
DieterClassification<-c(rep(1,59),rep(0,361))

# For Kiran: the first 25 are the article both agree to be included, the next five are the articles Dieter included but Kiran excluded, than there are 107 articles which both excluded, and 3 of the articles Dieter excluded that Kiran included
KiranClassification<-c(rep(1,22),rep(0,4),rep(0,11),rep(1,1))
KiranClassification<-c(rep(1,49),rep(0,10),rep(0,357),rep(1,4))

# We combine these two in a dataframe, where each row is an article and the two columns are the respective classifications
Classification<-as.data.frame(DieterClassification)
Classification<-cbind(Classification,KiranClassification)

# Next, we assess the Cohen's kappa of agreement
cohen.kappa( Classification[,c("DieterClassification", "KiranClassification")], w = NULL, 
            n.obs = NULL, alpha = 0.05, levels = NULL)

# The estimate of 0.83 indicates decent agreement. Psychologists however sometimes strive to get this value to be 0.90 or higher.


# Next, we work directly from the data we can download from Rayyan
# Save the file from Rayyan as csv. After downloading them (via an email link), copy the customizations_log.csv file in the same folder as this script

# We then tell R to look for the file in this folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load the file
articleclassifications<-read.csv("customizations_log.csv")

#There is a single row for each classification, which lists the article id, the id of the classifier, and how they scored it. We can first separate the classifications for each of the two people

DieterClassification<-articleclassifications[articleclassifications$user_email =="dieter_lukas@eva.mpg.de",]


KiranClassification<-articleclassifications[articleclassifications$user_email =="k.g.l.lee@student.rug.nl",]


# Next, we combine them. Whereas in the simulated example we had set up the order to match so that one row would always indicate one article, here we can link the classifications through the article id. Since Dieter performed fewer classifications, we join only those articles from Kiran that both classified
Classification<-left_join(DieterClassification,KiranClassification,by="article_id")

# We subset the dataframe to focus only on the columsn we are interested in, and rename them to match the example above
Classification<-select(Classification,article_id,value.x ,value.y)
colnames(Classification)<-c("article_id","DieterClassification","KiranClassification")

# Rayyan appears to store the full history of classifications, so we exclude some articles that are still listed as maybe (exclude = -1, maybe = 0, include = 1)
Classification<-Classification[Classification$DieterClassification!=0,]
Classification<-Classification[Classification$KiranClassification!=0,]

# Now, we can assess the agreement again. As mentioned above, there are sometimes multiple classifications for the same article, so there are more than 140 rows - and accordingly Cohen's kappa here is a bit smaller.
cohen.kappa( Classification[,c("DieterClassification", "KiranClassification")], w = NULL, 
             n.obs = NULL, alpha = 0.05, levels = NULL)


if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("MathiasHarrer/dmetar")
library(dmetar)
library(meta)
library(tidyverse)
data(OpioidMisuse)
glimpse(OpioidMisuse)
