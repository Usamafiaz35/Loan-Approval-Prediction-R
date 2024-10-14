#loading the data set
dataset <- read.csv(file = file.choose())
data <- dataset

#inspect the data set
View(data)
str(data)
anyNA(data)

################################################################################
#                                   data cleaning

# Calculate the percentage of missing values in each column
missing_percentage <- sapply(data, function(x) sum(is.na(x)) / length(x) * 100)
missing_percentage
###
# Separate numeric columns
numeric_columns <- data[, sapply(data, is.numeric)]
head(numeric_columns)
# Separate character or factor columns
character_columns <- data[, sapply(data, is.character)]
head(character_columns)



#                           filling the data (mode , mean)
#############
# Define a function to calculate the mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Fill all character columns with mode
for (col in names(data)) {
  if (is.character(data[[col]])) {
    data[[col]][is.na(data[[col]])] <- get_mode(data[[col]])
  }
}

# Fill all numeric columns with mean
for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
  }
}
#check again the missing values in data
missing_percentage <- sapply(data, function(x) sum(is.na(x)) / length(x) * 100)
missing_percentage

###
# Replace "3+" with "3" in the Dependents column
data$Dependents <- gsub("3\\+", "3", data$Dependents)
# Fill empty strings in the Dependents column with the mode
data$Dependents[data$Dependents == ""] <- get_mode(data$Dependents)
# Convert Dependents to numeric
data$Dependents <- as.numeric(data$Dependents)
#######################



                            #now find the outliers in numeric
summary(numeric_columns)

# Create boxplots for all numeric columns
par(mfrow = c(2, 2))  # Adjust the plotting area for multiple plots
for (col in names(numeric_columns)) {
  boxplot(numeric_columns[[col]], main = paste("Boxplot of", col), ylab = col)
}

# Function to remove outliers
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  return(x >= lower_bound & x <= upper_bound)
}

# Apply the function to all numeric columns
for (col in names(numeric_columns)) {
  data <- data[remove_outliers(data[[col]]), ]
}
# Check the updated dataset
print(data)

# update the numeric_column
numeric_columns <- data[, sapply(data, is.numeric)]
head(numeric_columns)


#also again check the boxplots
par(mfrow = c(2, 2))
for (col in names(numeric_columns)) {
  boxplot(numeric_columns[[col]], main = paste("Boxplot of", col), ylab = col)
}

summary(numeric_columns)

########################



#                                     one hot encoding
str(data)
# Convert Gender to numeric: Male = 1, Female = 0
data$Gender <- ifelse(data$Gender == "Male", 1, 0)
# Convert Married to numeric: Yes = 1, No = 0
data$Married <- ifelse(data$Married == "Yes", 1, 0)
# Convert Education to numeric: Graduate = 1, Not Graduate = 0
data$Education <- ifelse(data$Education == "Graduate", 1, 0)
# Convert Self_Employed to numeric: Yes = 1, No = 0
data$Self_Employed <- ifelse(data$Self_Employed == "Yes", 1, 0)
# Convert Property_Area to numeric using factor
data$Property_Area <- as.numeric(factor(data$Property_Area,
                                        levels = c("Rural", "Semiurban", "Urban")))
# Convert Loan_Status to numeric: Y = 1, N = 0
data$Loan_Status <- ifelse(data$Loan_Status == "Y", 1, 0)
data$Loan_Status <- as.factor(data$Loan_Status)

###################################################################################
# Save the data frame to a CSV file
#write.csv(data, "cleanedloan_data.csv", row.names = FALSE)

###################################################################################


# working on cleaned data
datt <- read.csv(file = file.choose())
df <- datt
df$Loan_Status <- as.factor(df$Loan_Status)

library(dplyr)
# Remove Loan_Status column
df1 <- df %>% select(-Loan_ID)

# Check the updated dataset
print(colnames(data))

# filter only significant variables cab be used
library(caret)
library(vip)

cv_model <- train(
  Loan_Status~.,
  data = df1 ,
  method = "glm" ,
  family = "binomial" ,
  trControl = trainControl(method = "cv" , number = 10))

#graph to see sig variables
vip(cv_model , num_features = 10)

#making new data of sig_variables only
sig_data <- df1 %>% select(CoapplicantIncome,LoanAmount,Gender,Education,
                           ApplicantIncome,Property_Area,Dependents,
                           Self_Employed,Loan_Status)
head(sig_data)
str(sig_data)

set.seed(0106)
#preparing the samples of data
ind <- sample(2 , nrow(sig_data) , replace = TRUE , prob = c(0.8 , 0.2))
train_data <- sig_data[ind==1 , ]
test_data <- sig_data[ind==2 , ]

################################################################################

                                  #1) Logistic model
model1 <- train(
  Loan_Status~.,
  data = train_data ,
  method = "glm" ,
  family = "binomial" ,
  trControl = trainControl(method = "cv" , number = 10))
model1

# prediction
pred <- predict(model1 , test_data)
# confusion matrix 
tab <- table(Predicted = pred , Actual = test_data$Loan_Status)
tab
#Accuracy
sum(diag(tab))/sum(tab)

 "0.7878788 Accuracy"

################################################################################
#                                 2) Random Forest
library(randomForest)

rf <- randomForest(Loan_Status~. , data = train_data,
                   ntree = 300,
                   mtry = 8,
                   importance = TRUE,
                   proximity = TRUE)
rf

# prediction
pred1 <- predict(rf , test_data)
# confusion matrix 
tab1 <- table(Predicted = pred1 , Actual = test_data$Loan_Status)
tab1
#Accuracy
sum(diag(tab1))/sum(tab1)

" 0.8032787 Accuracy"

################################################################################

#                                     3) Xgboost
library(xgboost)
library(caret)

#sig_data <- sig_data (data already in sig_data)

#preparing the samples of data
ind <- sample(2 , nrow(sig_data) , replace = TRUE , prob = c(0.8 , 0.2))
train_data <- sig_data[ind==1 , ]
test_data <- sig_data[ind==2 , ]

#define precdictor and responce variables in (training)
train_x <- data.matrix(train_data[ , -9])
train_y <- train_data[ , 9]

#define precdictor and responce variables in (testing)
test_x <- data.matrix(test_data[ , -9])
test_y <- test_data[ , 9]

#define final training and testing sets
xgb_train <- xgb.DMatrix(data=train_x , label=train_y)
xgb_test <- xgb.DMatrix(data=test_x , label=test_y)

#define watchlist
watchlist <- list(train = xgb_train , test = xgb_test)

#fitt XGBOOST model and display training and testing data at each round

model <- xgb.train(data=xgb_train,
                   max.depth=3,
                   watchlist=watchlist,
                   nrounds=70)

final <- xgboost(data=xgb_train,
                 max.depth=3,
                 nrounds=8,
                 verbose=0)

pred_y <- predict(model, newdata = xgb_test)


# Convert predicted probabilities to binary labels (0 or 1)
predicted_labels <- ifelse(pred_y > 0.5, 1, 0)

# Assuming you have your actual test labels in `test_y`
# Calculate accuracy
accuracy <- mean(predicted_labels == test_y)

# Display accuracy
print(paste("Accuracy of XGBoost model:", accuracy))

"0.770491803278688 Accuracy"


################################################################################

#interpretation:

#1) Logistic Regression Model:

#This model predicts the probability of loan approval based on the features using 
#a logistic regression algorithm.
#The model was trained using 10-fold cross-validation.
#Accuracy: 78.78% (i.e., the model correctly predicts loan approval 78.78% of the time).

#2) Random Forest Model:

#This model uses an ensemble of decision trees (300 trees) to predict loan approval.
#It considers 8 random features at each split to grow the trees.
#Accuracy: 80.33% (better performance than logistic regression).

#3) XGBoost Model:

#This is a boosted tree model, which improves performance by iteratively correcting
#errors of previous models.
#The model was trained with a depth of 3 for 70 rounds.
#Accuracy: 77.05% (slightly lower than the Random Forest but comparable to
#logistic regression).

#Conclusion: The Random Forest model had the best accuracy at 80.33%, followed 
#by Logistic Regression at 78.78%, and XGBoost at 77.05%.



















