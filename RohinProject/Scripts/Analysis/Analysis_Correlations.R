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

#Data preprocessing####
df_codeencoded <- df_clean|>
  filter(violation_code %in% c("04K", "04L", "04M", "04N"), boro == "manhattan")|>
  mutate(value = 1) |>
  spread(key = violation_code, value = value, fill = 0)|>
  select(c("dba", "zipcode", "latitude", "longitude", "cuisine_description","04K", "04L", "04M", "04N"))|>
  distinct()|>
  filter(longitude != 0, latitude != 0)|>
  arrange(dba)

df_joined <- left_join(df_clean, df_codeencoded, by = "dba")|>
  filter(boro == "manhattan")|>
  mutate(`04K` = ifelse(is.na(`04K`), 0, `04K`))|>
  mutate(`04L` = ifelse(is.na(`04L`), 0, `04L`))|>
  mutate(`04M` = ifelse(is.na(`04M`), 0, `04M`))|>
  mutate(`04N` = ifelse(is.na(`04N`), 0, `04N`))

df_codeencoded <- df_joined|>
  group_by(cuisine_description.x) |>
  summarise(across(starts_with("0"), ~sum(., na.rm = TRUE)))|>
  rename(cuisine_description = cuisine_description.x)

df_codeencoded <- left_join(df_codeencoded, df_count_by_cuisine, by = "cuisine_description")

df_codeencoded <- df_codeencoded|>
  mutate(ave_04K = `04K` / count_by_cuisine)|>
  mutate(ave_04L = `04L` / count_by_cuisine)|>
  mutate(ave_04M = `04M` / count_by_cuisine)|>
  mutate(ave_04N = `04N` / count_by_cuisine)|>
  select(!c("04K", "04L", "04M", "04N"))

#CORRELATION BETWEEN CUISINE, ZIPCODE, PEST VIOLATION TYPE




#Cuisine Description from Zipcode - Chi Squared Test####
chisq_cuisine_zipcode <- table(df_anova$zipcode, df_anova$cuisine_description) 
chisq_cuisinezipcode <- chisq.test(chisq_cuisine_zipcode)
fisher_cuisinezipcode <- fisher.test(chisq_cuisine_zipcode,simulate.p.value=TRUE)

sum_chisqtest1 = unlist(summary(chisq_cuisinezipcode))
p_chisq <- sum_chisqtest1["Pr(>F)"]

sum_fishertest1 = unlist(summary(fisher_cuisinezipcode))
p_fisher <- sum_fishertest1["Pr(>F)"]

p_chisq
p_fisher

Test_Type <- c("Fisher Test", "Chi-Squared Test")
P_values <- c(0.0004998, 0)

df_chisq_graph <- data.frame(Test_Type = Test_Type,  P_values = P_values)

ggplot(df_chisq_graph, aes(x = Test_Type, y = P_values, fill = Test_Type)) + 
  geom_bar(stat = "identity")+
  geom_hline(yintercept = 0.05, color = "black")+
  ylim(0,1)+
  labs(x = "Test Type", y = "P-Values", title = "Cuisine Description Vs. Zipcode P-values")


#Violation Codes from Cuisine and Zipcode - Anova Test####
#prepare a frame to add zipcode to existing
df_zipcodeanova <- df_clean|>
  filter(boro == "manhattan", violation_code %in% c("04K", "04L", "04N", "04M"), !is.na(zipcode))|>
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





#Plot P Values for zipcode
sum_test1 = unlist(summary(anova_04K))
sum_test2 = unlist(summary(anova_04L))
sum_test3 = unlist(summary(anova_04M))
sum_test4 = unlist(summary(anova_04N))

p_04K <- sum_test1["Pr(>F)2"]
p_04L <- sum_test2["Pr(>F)2"]
p_04M <- sum_test3["Pr(>F)2"]
p_04N <- sum_test4["Pr(>F)2"]

violation_codes <- c("04K", "04L", "04M", "04N")
p_values <- c(p_04K, p_04L, p_04M, p_04N)

anova_graph <- data.frame(Violation_Codes = violation_codes, P_Values = p_values)

ggplot(anova_graph, aes(x = violation_codes, y = p_values, fill = violation_codes)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Violation Codes", y = "P-Values", title = "Violation Codes Vs. Zipcode P-Values")+
  geom_hline(yintercept = 0.05, color = "black")+
  ylim(0, 1)

#Plot P Values for Cuisine Description
sum_test1 = unlist(summary(anova_04K))
sum_test2 = unlist(summary(anova_04L))
sum_test3 = unlist(summary(anova_04M))
sum_test4 = unlist(summary(anova_04N))

p_04K <- sum_test1["Pr(>F)1"]
p_04L <- sum_test2["Pr(>F)1"]
p_04M <- sum_test3["Pr(>F)1"]
p_04N <- sum_test4["Pr(>F)1"]

violation_codes <- c("04K", "04L", "04M", "04N")
p_values <- c(p_04K, p_04L, p_04M, p_04N)

anova_graph <- data.frame(Violation_Codes = violation_codes, P_Values = p_values)

ggplot(anova_graph, aes(x = violation_codes, y = p_values, fill = violation_codes)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Violation Codes", y = "P-Values", title = "Violation Codes Vs. Cuisine Description P-Values", fill = "Violation Codes")+
  geom_hline(yintercept = 0.05, color = "black")+
  ylim(0, 1)


#ANOVA Test Zipcode Vs. Cuisine description####
df_codeencoded <- df_clean|>
  filter(violation_code %in% c("04K", "04L", "04M", "04N"), boro == "manhattan")|>
  mutate(value = 1) |>
  spread(key = violation_code, value = value, fill = 0)|>
  select(c("dba", "zipcode", "latitude", "longitude", "cuisine_description","04K", "04L", "04M", "04N"))|>
  distinct()|>
  filter(longitude != 0, latitude != 0)|>
  arrange(dba)

df_count_by_zipcode <- df_clean|>
  filter(!is.na(zipcode))|>
  group_by(zipcode)|>
  distinct(dba)|>
  summarise(number = n())

df_codeencoded <- left_join(df_codeencoded, df_count_by_zipcode, by = "zipcode")

df_codeencoded <- df_codeencoded|>
  mutate(ave_04K = `04K` / number)|>
  mutate(ave_04L = `04L` / number)|>
  mutate(ave_04M = `04M` / number)|>
  mutate(ave_04N = `04N` / number)|>
  select(!c("04K", "04L", "04M", "04N"))

df_combined <- df_codeencoded |>
  group_by(dba, zipcode, latitude, longitude, cuisine_description, number) |>
  summarise(
    `04K` = sum(ave_04K, na.rm = TRUE),
    `04L` = sum(ave_04L, na.rm = TRUE),
    `04M` = sum(ave_04M, na.rm = TRUE),
    `04N` = sum(ave_04N, na.rm = TRUE)
  ) |>
  ungroup()

anova_04K <- aov(`04K` ~ factor(zipcode), data = df_combined)
summary(anova_04K)
anova_04L <- aov(`04L` ~  factor(zipcode), data = df_combined)
summary(anova_04L)
anova_04M <- aov(`04M` ~  factor(zipcode), data = df_combined)
summary(anova_04M)
anova_04N <- aov(`04N` ~  factor(zipcode), data = df_combined)
summary(anova_04N)



#Cuisine Description from Violation Codes - Multinom regression####
# Prepaare cuisine column for multinomial logistical regression
df_anova$cuisine_description <- as.factor(df_anova$cuisine_description)

# Fit the multinomial logistic regression model
multinom_model <- multinom(cuisine_description ~ ave_04K + ave_04L + ave_04M + ave_04N, data = df_anova)

# Calculate p-values
z_values <- summary(multinom_model)$coefficients / summary(multinom_model)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_values)))
