#KNN####
df_pestViolations <- df_clean|>
  filter(violation_code %in% c("04K", "04L", "04M", "04N"), !is.na(latitude), !is.na(longitude), boro == "manhattan")

#split the data
split <- sample.split(df_pestViolations$violation_code, SplitRatio = 0.8)
train_data <- subset(df_pestViolations, split == TRUE)
test_data <- subset(df_pestViolations, split == FALSE)

#train knn model based on latitude, longitude, zipcode, and cuisine description
knn_model <- knn(train = train_data[, c('latitude', 'longitude', 'zipcode')],
                 test = test_data[, c('latitude', 'longitude', 'zipcode')],
                 cl = train_data$violation_code, k = 13)

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
                 cl = train_data$violation_code.x, k = 13)

#create confusion matrix
confusion_matrix <- table(test_data$violation_code.x, knn_model)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print the evaluation metrics
cat('Confusion Matrix:\n')
print(confusion_matrix)
cat('\nAccuracy:', accuracy, '\n')
