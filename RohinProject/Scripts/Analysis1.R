##Create a KNN for predicting which pest violation the establishment has to be most concerned about####
df_pestViolations <- df_clean|>
  filter(violation_code %in% c("04K","04L", "04M", "04N"), !is.na(latitude), !is.na(longitude), boro == "manhattan")

#split the data
split <- sample.split(df_pestViolations$violation_code, SplitRatio = 0.8)
train_data <- subset(df_pestViolations, split == TRUE)
test_data <- subset(df_pestViolations, split == FALSE)

#train knn model based on latitude, longitude, zipcode, and cuisine description
knn_model <- knn(train = train_data[, c('latitude', 'longitude', 'zipcode')],
                 test = test_data[, c('latitude', 'longitude', 'zipcode')],
                 cl = train_data$violation_code, k = 9)

#create confusion matrix
confusion_matrix <- table(test_data$violation_code, knn_model)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print the evaluation metrics
cat('Confusion Matrix:\n')
print(confusion_matrix)
cat('\nAccuracy:', accuracy, '\n')


##Encode every cuisine_description in a 6 digit binary string####
#one hot encode cuisine description
df_onehot <- df_pestViolations|>
  filter(cuisine_description != "basque")

# Create a mapping of unique cuisine descriptions to binary strings
unique_cuisines <- unique(df_onehot$cuisine_description)
binary_strings <- sapply(seq_along(unique_cuisines), function(x) paste0(as.integer(intToBits(x)[1:6]), collapse = ""))

df_cuisine_to_binary <- data.frame(
  cuisine_description = unique_cuisines,
  cuisine_encoded = binary_strings
)

# Display the mapping
print(cuisine_to_binary)

# Apply the mapping to your dataframe
df_temp <- df_pestViolations |>
  left_join(df_cuisine_to_binary, by = "cuisine_description")

df_encoded <- df_temp |>
  mutate(cuisine_encoded = if_else(is.na(cuisine_encoded), "00000", cuisine_encoded))

df_encoded$cuisine_encoded <- as.character(df_encoded$cuisine_encoded)


##KNN model including cuisine_description####

#split the data
split <- sample.split(df_encoded$violation_code, SplitRatio = 0.8)
train_data <- subset(df_encoded, split == TRUE)
test_data <- subset(df_encoded, split == FALSE)

#train knn model based on latitude, longitude, zipcode, and cuisine description
knn_model <- knn(train = train_data[, c('latitude', 'longitude', 'zipcode', "cuisine_encoded")],
                 test = test_data[, c('latitude', 'longitude', 'zipcode', "cuisine_encoded")],
                 cl = train_data$violation_code, k = 15)

pred <- knn_model

#create confusion matrix
confusion_matrix <- table(test_data$violation_code, pred)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
# Print the evaluation metrics
cat('Confusion Matrix:\n')
print(confusion_matrix)
cat('\nAccuracy:', accuracy, '\n')
