library(ggplot2)
library(caTools)
library(readr)
library(downloader)
library(caTools)
library(FNN)
library(readxl)
library(dplyr)
if (!requireNamespace("coin", quietly = TRUE)) {
  install.packages("coin")
}
library(coin)

#KNN, location vs violation code####
df_pestViolations <- df_clean|>
  filter(violation_code %in% c("04K", "04L", "04M", "04N"), !is.na(latitude), !is.na(longitude), boro == "manhattan")

#split the data
split <- sample.split(df_pestViolations$violation_code, SplitRatio = 0.8)
train_data <- subset(df_pestViolations, split == TRUE)
test_data <- subset(df_pestViolations, split == FALSE)

#train knn model based on latitude, longitude, zipcode, and cuisine description
knn_model <- knn(train = train_data[, c('latitude', 'longitude', 'zipcode')],
                 test = test_data[, c('latitude', 'longitude', 'zipcode')],
                 cl = train_data$violation_code, k = 15)

pred <- knn_model

#create confusion matrix
confusion_matrix <- table(test_data$violation_code, pred)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print the evaluation metrics
cat('Confusion Matrix:\n')
print(confusion_matrix)
cat('\nAccuracy:', accuracy, '\n')



#one hot encode cuisine description####
df_onehot <- df_pestViolations|>
  filter(cuisine_description %in% c("chinese/cuban", "african", "new american", "vegetarian", "tapas", "armenian", "egyptian", "english", "irish", "turkish", "barbecue", "eastern european", "continental", "indian", "creole/cajun", "salads", "australian", "soul food"))|>
  mutate(cuisine_description1 = cuisine_description)|>
  mutate(value = 1) |>
  spread(key = cuisine_description1, value = value, fill = 0)

df_encoded1 <- left_join(df_pestViolations, df_onehot, by = "dba")|>
  select(c("dba", "zipcode.x", "cuisine_description.x", "violation_code.x", "latitude.x", "longitude.x", "african", "armenian", "chinese/cuban", "new american", "vegetarian", "tapas", "egyptian", "english", "irish", "turkish", "barbecue", "eastern european", "continental", "indian", "creole/cajun", "salads", "australian", "soul food"))

df_encoded <- df_encoded1|>
  mutate_all(~ ifelse(is.na(.), 0, .))

df_encoded


#KNN with cuisine####
#split the data
split <- sample.split(df_encoded$violation_code.x, SplitRatio = 0.8)
train_data <- subset(df_encoded, split == TRUE)
test_data <- subset(df_encoded, split == FALSE)

#train knn model based on latitude, longitude, zipcode, and cuisine description
knn_model <- knn(train = train_data[, c("latitude.x", "longitude.x", "african", "armenian", "chinese/cuban", "new american", "vegetarian", "tapas", "egyptian", "english", "irish", "turkish", "barbecue", "eastern european", "continental", "indian", "creole/cajun", "salads", "australian", "soul food")],
                 test = test_data[, c("latitude.x", "longitude.x", "african", "armenian", "chinese/cuban", "new american", "vegetarian", "tapas", "egyptian", "english", "irish", "turkish", "barbecue", "eastern european", "continental", "indian", "creole/cajun", "salads", "australian", "soul food")],
                 cl = train_data$violation_code.x, k = 15)

#create confusion matrix
confusion_matrix <- table(test_data$violation_code.x, knn_model)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print the evaluation metrics
cat('Confusion Matrix:\n')
print(confusion_matrix)
cat('\nAccuracy:', accuracy, '\n')





#Chi squared test for Conducive to Pests vs. every pest infestation####
df_codeencoded <- df_clean|>
  filter(violation_code %in% c("04K", "04L", "04M", "04N", "08A"), boro == "manhattan")|>
  mutate(value = 1) |>
  spread(key = violation_code, value = value, fill = 0)|>
  select(c("dba", "zipcode", "latitude", "longitude", "04K", "04L", "04M", "04N", "08A"))|>
  distinct()|>
  filter(longitude != 0, latitude != 0)|>
  arrange(dba)

df_joined <- left_join(df_clean, df_codeencoded, by = "dba")|>
  filter(boro == "manhattan")|>
  mutate(`04K` = ifelse(is.na(`04K`), 0, `04K`))|>
  mutate(`08A` = ifelse(is.na(`08A`), 0, `08A`))|>
  mutate(`04L` = ifelse(is.na(`04L`), 0, `04L`))|>
  mutate(`04M` = ifelse(is.na(`04M`), 0, `04M`))|>
  mutate(`04N` = ifelse(is.na(`04N`), 0, `04N`))

df_codeencoded <- df_joined|>
  group_by(dba) |>
  summarise(across(starts_with("0"), ~sum(., na.rm = TRUE)))|>
  mutate(`04K` = ifelse(`04K` != 0 & `04K` != 1, 1, `04K`))|>
  mutate(`08A` = ifelse(`08A` != 0 & `08A` != 1, 1, `08A`))|>
  mutate(`04L` = ifelse(`04L` != 0 & `04L` != 1, 1, `04L`))|>
  mutate(`04M` = ifelse(`04M` != 0 & `04M` != 1, 1, `04M`))|>
  mutate(`04N` = ifelse(`04N` != 0 & `04N` != 1, 1, `04N`))

df_codeencoded

chisq_rats <- table(df_codeencoded$`08A`, df_codeencoded$`04K`) 
chisq_mice <- table(df_codeencoded$`08A`, df_codeencoded$`04L`) 
chisq_roaches <- table(df_codeencoded$`08A`, df_codeencoded$`04M`) 
chisq_flies <- table(df_codeencoded$`08A`, df_codeencoded$`04N`) 

chisq.test(chisq_rats)
chisq.test(chisq_mice)
chisq.test(chisq_roaches)
chisq.test(chisq_flies)



#Chi squared test for bad upkeep vs. every pest infestation####
df_codeencoded <- df_clean|>
  filter(violation_code %in% c("04K", "04L", "04M", "04N", "10F"), boro == "manhattan")|>
  mutate(value = 1) |>
  spread(key = violation_code, value = value, fill = 0)|>
  select(c("dba", "zipcode", "latitude", "longitude", "04K", "04L", "04M", "04N", "10F"))|>
  distinct()|>
  filter(longitude != 0, latitude != 0)|>
  arrange(dba)

df_joined <- left_join(df_clean, df_codeencoded, by = "dba")|>
  filter(boro == "manhattan")|>
  mutate(`04K` = ifelse(is.na(`04K`), 0, `04K`))|>
  mutate(`04L` = ifelse(is.na(`04L`), 0, `04L`))|>
  mutate(`04M` = ifelse(is.na(`04M`), 0, `04M`))|>
  mutate(`04N` = ifelse(is.na(`04N`), 0, `04N`))|>
  mutate(`10F` = ifelse(is.na(`10F`), 0, `10F`))


cols <- c("10F", "04K", "04L", "04M", "04N")

df_codeencoded <- df_joined|>
  group_by(dba) |>
  summarise(across(all_of(cols), ~sum(., na.rm = TRUE)))|>
  mutate(`04K` = ifelse(`04K` != 0 & `04K` != 1, 1, `04K`))|>
  mutate(`04L` = ifelse(`04L` != 0 & `04L` != 1, 1, `04L`))|>
  mutate(`04M` = ifelse(`04M` != 0 & `04M` != 1, 1, `04M`))|>
  mutate(`04N` = ifelse(`04N` != 0 & `04N` != 1, 1, `04N`))|>
  mutate(`10F` = ifelse(`10F` != 0 & `10F` != 1, 1, `10F`))

df_codeencoded

chisq_rats <- table(df_codeencoded$`10F`, df_codeencoded$`04K`) 
chisq_mice <- table(df_codeencoded$`10F`, df_codeencoded$`04L`) 
chisq_roaches <- table(df_codeencoded$`10F`, df_codeencoded$`04M`) 
chisq_flies <- table(df_codeencoded$`10F`, df_codeencoded$`04N`) 

chisq.test(chisq_rats)
chisq.test(chisq_mice)
chisq.test(chisq_roaches)
chisq.test(chisq_flies)



##Chi squared tests violation code vs. cuisine####
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

df_joined

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
  
chisq_rats <- table(df_codeencoded$cuisine_description, df_codeencoded$ave_04K) 
chisq_mice <- table(df_codeencoded$cuisine_description, df_codeencoded$ave_04L) 
chisq_roaches <- table(df_codeencoded$cuisine_description, df_codeencoded$ave_04M) 
chisq_flies <- table(df_codeencoded$cuisine_description, df_codeencoded$ave_04N) 

fisher.test(chisq_rats, simulate.p.value=TRUE)
fisher.test(chisq_mice, simulate.p.value=TRUE)
fisher.test(chisq_roaches, simulate.p.value=TRUE)
fisher.test(chisq_flies, simulate.p.value=TRUE)


#Anova test for average number of pest violations ~ cuisine and zipcode####
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
