#Install and load in Packages####
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("caTools", quietly = TRUE)) {
  install.packages("caTools")
}
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
if (!requireNamespace("downloader", quietly = TRUE)) {
  install.packages("downloader")
}
if (!requireNamespace("caTools", quietly = TRUE)) {
  install.packages("caTools")
}
if (!requireNamespace("FNN", quietly = TRUE)) {
  install.packages("FNN")
}
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("nnet", quietly = TRUE)) {
  install.packages("nnet")
}

library(ggplot2)
library(caTools)
library(readr)
library(downloader)
library(caTools)
library(FNN)
library(readxl)
library(dplyr)
library(nnet)

#CORRELATION BETWEEN CUISINE, ZIPCODE, PEST VIOLATION TYPE
#Cuisine Description from Zipcode - Chi Squared Test####
chisq_cuisine_zipcode <- table(df_anova$zipcode, df_anova$cuisine_description) 
chisq.test(chisq_cuisine_zipcode)
fisher.test(chisq_cuisine_zipcode,simulate.p.value=TRUE)

#Violation Codes from Cuisine and Zipcode - Anova Test####
#prepare a frame to add zipcode to existing
df_zipcodeanova <- df_clean|>
  filter(boro == "manhattan", violation_code %in% c("04k", "04L", "04N", "04M"), !is.na(zipcode))|>
  select(c("dba", "cuisine_description", "zipcode", "latitude", "longitude"))

#add a zipcode column to the existing dataset
df_anova <- left_join(df_zipcodeanova, df_codeencoded, by = "cuisine_description")

df_anova

#perform anova tests
anova_04K <- aov(ave_04K ~ cuisine_description + factor(zipcode), data = df_anova)
summary(anova_04K)
anova_04L <- aov(ave_04L ~ cuisine_description + factor(zipcode), data = df_anova)
summary(anova_04L)
anova_04M <- aov(ave_04M ~ cuisine_description + factor(zipcode), data = df_anova)
summary(anova_04M)
anova_04N <- aov(ave_04N ~ cuisine_description + factor(zipcode), data = df_anova)
summary(anova_04N)


#Cuisine Description from Violation Codes - Multinom regression####
# Prepaare cuisine column for multinomial logistical regression
df_anova$cuisine_description <- as.factor(df_anova$cuisine_description)

# Fit the multinomial logistic regression model
multinom_model <- multinom(cuisine_description ~ ave_04K + ave_04L + ave_04M + ave_04N, data = df_anova)

# Calculate p-values
z_values <- summary(multinom_model)$coefficients / summary(multinom_model)$standard.errors
2 * (1 - pnorm(abs(z_values)))