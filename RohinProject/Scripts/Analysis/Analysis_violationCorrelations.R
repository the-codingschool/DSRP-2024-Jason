#CORRELATION BETWEEN ZIPCODE, LATITUDE, LONGITUDE AND PEST VIOLATION TYPE

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






#CORRELATION BETWEEN RESTAURANT CONDITION AND PEST VIOLATION TYPE
#Chi squared test for Conducive to Pests vs. every pest infestation####
possible_codes = c("04C",
                   "04E","04H","04J","04K","04L","04M","04N","04O","05C","05D","05F","05H","06A",
                   "06B","06C","06D","06E","06F","08A","08B","08C","09A","09B","09C","09E","10A",
                   "10B","10C","10D","10E","10F","10G","10H")

#"02A","02B","02C","02F","02G","02H","02I","03B","03C","03F","03I","04A",

for (violation_code in possible_codes) {
cat("\nResults for Violation Code:", violation_code, "\n")

df_codeencoded <- df_clean|>
 filter(violation_code %in% c("04K", "04L", "04M", "04N", violation_code), boro == "manhattan")|>
 mutate(value = 1) |>
 spread(key = violation_code, value = value, fill = 0)|>
 select(c("dba", "zipcode", "latitude", "longitude", "04K", "04L", "04M", "04N", violation_code))|>
 distinct()|>
 filter(longitude != 0, latitude != 0)|>
 arrange(dba)

df_joined <- left_join(df_clean, df_codeencoded, by = "dba", relationship = "many-to-many")|>
 filter(boro == "manhattan")|>
 mutate(`04K` = ifelse(is.na(`04K`), 0, `04K`))|>
 mutate(`04L` = ifelse(is.na(`04L`), 0, `04L`))|>
 mutate(`04M` = ifelse(is.na(`04M`), 0, `04M`))|>
 mutate(`04N` = ifelse(is.na(`04N`), 0, `04N`))|>
 mutate_at(vars(violation_code), ~ifelse(is.na(.), 0, .))

cols <- c("04K", "04L", "04M", "04N", violation_code)

df_codeencoded <- df_joined|>
 group_by(dba) |>
 summarise(across(all_of(cols), ~sum(., na.rm = TRUE)))|>
 mutate(`04K` = ifelse(`04K` != 0 & `04K` != 1, 1, `04K`))|>
 mutate(`04L` = ifelse(`04L` != 0 & `04L` != 1, 1, `04L`))|>
 mutate(`04M` = ifelse(`04M` != 0 & `04M` != 1, 1, `04M`))|>
 mutate(`04N` = ifelse(`04N` != 0 & `04N` != 1, 1, `04N`))|>
 mutate_at(vars(violation_code), ~ifelse(. != 0 & . != 1, 1, .))

chisq_rats <- table(df_codeencoded[[violation_code]], df_codeencoded$`04K`) 
chisq_mice <- table(df_codeencoded[[violation_code]], df_codeencoded$`04L`) 
chisq_roaches <- table(df_codeencoded[[violation_code]], df_codeencoded$`04M`) 
chisq_flies <- table(df_codeencoded[[violation_code]], df_codeencoded$`04N`) 

print(chisq.test(chisq_rats))
print(chisq.test(chisq_mice))
print(chisq.test(chisq_roaches))
print(chisq.test(chisq_flies))
cat("\n--------------------------------------\n")
}
