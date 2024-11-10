# Data manipulation
# For calculating metrics (precision, recall, F1 score)
install.packages("Metrics")

# For AdaBoost models
install.packages("adabag")

# For Gradient Boosting Models
install.packages("gbm")

# For regularized logistic regression (Lasso, Ridge)
install.packages("glmnet")

# For XGBoost
install.packages("xgboost")

# CatBoost - requires devtools for installation from GitHub
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("catboost/catboost", subdir = "catboost/R-package")

install.packages("dplyr")
install.packages("tidyr")

# Visualization
install.packages("ggplot2")
install.packages("plotly")

# Data preprocessing
install.packages("caret")
install.packages("scales")
install.packages("forcats")

# Model performance metrics
install.packages("pROC")      # For ROC curves and AUC
install.packages("Metrics")    # For metrics like precision, recall, and F1 score

# Machine learning models
install.packages("rpart")         # Decision Trees
install.packages("randomForest")  # Random Forest
install.packages("e1071")         # SVM (Support Vector Machines)
install.packages("adabag")        # AdaBoost
install.packages("gbm")           # Gradient Boosting
install.packages("glmnet")        # Logistic Regression with regularization
install.packages("xgboost")       # XGBoost

# CatBoost (install from GitHub if not on CRAN)
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("catboost/catboost", subdir = "catboost/R-package")
# Data Manipulation
library(dplyr)
library(tidyr)

# Visualization
library(ggplot2)         # For data visualization (similar to matplotlib and seaborn)
library(plotly)          # For interactive plots (similar to Plotly in Python)

# Suppress warnings (similar to warnings.filterwarnings in Python)
options(warn = -1)

# Data Preprocessing
library(caret)           # For data preprocessing and machine learning models
library(scales)          # For scaling functions (similar to StandardScaler)
library(forcats)         # For categorical data handling (similar to LabelEncoder)

# Model Performance Metrics
library(pROC)            # For ROC curves and AUC
library(Metrics)         # For precision, recall, and F1 score

# Machine Learning Models
library(rpart)           # Decision Trees
library(randomForest)    # Random Forests
library(e1071)           # SVM (Support Vector Machines)
library(adabag)          # AdaBoost
library(gbm)             # Gradient Boosting
library(glmnet)          # Logistic Regression
library(xgboost)         # XGBoost
library(catboost)        # CatBoost (requires installation and setup of CatBoost package)
#load the dataset
df <- read.csv("/Users/mittulkumar/Downloads/WA_Fn-UseC_-Telco-Customer-Churn (1).csv")
str(df)
library(dplyr)

# Drop the 'customerID' column
df <- df %>% select(-customerID)

# View the first few rows of the updated dataset
head(df)
# Convert 'TotalCharges' column to numeric
df$TotalCharges <- as.numeric(df$TotalCharges)

# Check for missing values in each column
colSums(is.na(df))
# Fill missing values in 'TotalCharges' column with the mean value
df$TotalCharges[is.na(df$TotalCharges)] <- mean(df$TotalCharges, na.rm = TRUE)
# Remove rows with any missing values
df <- na.omit(df)
# Removing rows where 'tenure' equals 0
df <- df %>% filter(tenure != 0)
# Categorizing SeniorCitizen column
df$SeniorCitizen <- recode(df$SeniorCitizen, `0` = "No", `1` = "Yes")

# View the updated data
head(df)


# Finding summary statistics for numerical columns
numerical_cols <- c("tenure", "MonthlyCharges", "TotalCharges")
summary(df[numerical_cols])

# Install and load psych package for more detailed summary

library(psych)

# Detailed summary statistics
describe(df[numerical_cols])

# Loading required libraries
library(dplyr)

# Replacing Churn values (Yes/No) with 1/0
df2 <- df %>%
  mutate(Churn = ifelse(Churn == "Yes", 1, 0))

# Creating dummy variables for categorical columns
df_dummies <- model.matrix(~ . -1, data = df2)  # -1 removes the intercept column

# Convert the matrix back to a dataframe
df_dummies <- as.data.frame(df_dummies)

# Check the first few rows of the dataframe
head(df_dummies)


cor_churn <- cor(df_dummies)  # Calculate the correlation matrix

# Create a data frame of correlations with Churn
cor_churn_df <- data.frame(Feature = names(cor_churn[, "Churn"]), Correlation = cor_churn[, "Churn"])

# Remove the "Churn" column itself from the correlation data
cor_churn_df <- cor_churn_df[cor_churn_df$Feature != "Churn", ]

# Sort the correlation data by correlation values
cor_churn_df <- cor_churn_df[order(abs(cor_churn_df$Correlation), decreasing = TRUE), ]

# Plotting the correlations with Churn
ggplot(cor_churn_df, aes(x = reorder(Feature, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Correlation of Features with Churn", x = "Features", y = "Correlation with Churn") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Install and load the required libraries
install.packages("devtools")
devtools::install_github("ropensci/plotly")


library(plotly)

# Define labels for gender and churn
gender_labels <- c('Male', 'Female')
churn_labels <- c('No', 'Yes')

# Gender distribution count
gender_counts <- table(df$gender)

# Churn distribution count
churn_counts <- table(df$Churn)

# Create subplots: use 'domain' type for Pie subplot
fig <- subplot(
  plot_ly(labels = gender_labels, values = gender_counts, type = 'pie', hole = 0.55, name = "Gender", 
          textinfo = 'label+percent', textfont = list(size = 16), 
          marker = list(colors = c('#7fcdff', '#326ada'))),
  plot_ly(labels = churn_labels, values = churn_counts, type = 'pie', hole = 0.55, name = "Churn", 
          textinfo = 'label+percent', textfont = list(size = 16), 
          marker = list(colors = c('#56c175', '#ff9b35'))),
  nrows = 1, shareX = TRUE, shareY = TRUE
)

# Customize layout and titles
fig <- fig %>% layout(
  title = "Gender and Churn Distributions",
  annotations = list(
    list(x = 0.16, y = 0.5, text = 'Gender', font = list(size = 20), showarrow = FALSE),
    list(x = 0.83, y = 0.5, text = 'Churn', font = list(size = 20), showarrow = FALSE)
  )
)

# Show the plot
fig


# Data for Churn and Gender Distribution
churn_labels <- c("Churn: Yes", "Churn: No")
churn_values <- c(1869, 5163)

gender_labels <- c("F", "M", "F", "M")
gender_sizes <- c(939, 930, 2544, 2619)

# Colors
churn_colors <- c('#ff9b35', '#56c175')
gender_colors <- c('#7fcdff', '#326ada', '#7fcdff', '#326ada')

# Create pie chart for churn distribution
churn_pie <- plot_ly(labels = churn_labels, values = churn_values, type = 'pie',
                     marker = list(colors = churn_colors),
                     textinfo = 'label+percent', showlegend = TRUE) %>%
  layout(title = 'Churn Distribution', 
         annotations = list(
           list(text = 'Churn', x = 0.5, y = 0.5, font = list(size = 16), showarrow = FALSE)))

# Create pie chart for gender distribution
gender_pie <- plot_ly(labels = gender_labels, values = gender_sizes, type = 'pie',
                      marker = list(colors = gender_colors),
                      textinfo = 'label+percent', showlegend = TRUE) %>%
  layout(title = 'Gender Distribution',
         annotations = list(
           list(text = 'Gender', x = 0.5, y = 0.5, font = list(size = 16), showarrow = FALSE)))

# Combine both plots into one subplot
fig <- subplot(churn_pie, gender_pie, nrows = 1, shareX = TRUE, shareY = TRUE)
fig <- fig %>% layout(title = 'Churn Distribution with Gender Breakdown')

fig


color_discrete_map <- c("Month-to-month" = "#7fcdff", "One year" = "#326ada", "Two year" = "#ff9b35")

# Create the histogram for Churn distribution with respect to Contract
fig <- plot_ly(df, x = ~Churn, color = ~Contract, type = 'histogram', 
               colors = color_discrete_map, barmode = "group", 
               name = "Contract",
               marker = list(line = list(width = 1, color = 'rgb(255, 255, 255)'))) %>%
  layout(title = "<b>Churn Distribution w.r.t Contract</b>",
         xaxis = list(title = "Churn"),
         yaxis = list(title = "Count"),
         bargap = 0.1, 
         width = 700, 
         height = 500)

fig


# Assuming 'df' is your dataframe loaded with the necessary data

# Create the histogram for Churn distribution with respect to PaymentMethod
fig <- plot_ly(df, x = ~Churn, color = ~PaymentMethod, type = 'histogram', text = ~PaymentMethod,
               textposition = 'auto', 
               marker = list(color = c('#7fcdff', '#326ada', '#ff9b35', '#56c175'))) %>%
  layout(title = "<b>Churn distribution w.r.t. Customer Payment Method</b>",
         xaxis = list(title = "Churn"),
         yaxis = list(title = "Count"),
         bargap = 0.1, 
         width = 700, 
         height = 500)

# Show the figure
fig

# Assuming 'df' is your dataframe

# Filter data for males and then count occurrences of combinations of 'InternetService' and 'Churn'
table(df[df$gender == "Male", c("InternetService", "Churn")])
# Assuming 'df' is your dataframe

# Filter data for females and then count occurrences of combinations of 'InternetService' and 'Churn'
table(df[df$gender == "Female", c("InternetService", "Churn")])


library(plotly)

# Data for the plot
x_vals <- c('Churn:No Female', 'Churn:No Male', 'Churn:Yes Female', 'Churn:Yes Male',
            'Churn:No Female', 'Churn:No Male', 'Churn:Yes Female', 'Churn:Yes Male',
            'Churn:No Female', 'Churn:No Male', 'Churn:Yes Female', 'Churn:Yes Male')

y_vals_dsl <- c(965, 992, 219, 240, 889, 910, 664, 633, 690, 717, 56, 57)

# Colors for each trace
colors <- c('Female' = 'steelblue', 'Male' = 'firebrick')

# Create the bar plot
fig <- plot_ly()

# Adding traces for different internet services
fig <- fig %>% add_trace(
  x = x_vals,
  y = y_vals_dsl,
  type = 'bar',
  name = 'DSL',
  marker = list(color = c('#7fcdff', '#7fcdff', '#7fcdff', '#7fcdff',
                          '#326ada', '#326ada', '#326ada', '#326ada',
                          '#ff9b35', '#ff9b35', '#ff9b35', '#ff9b35'))
)

# Update layout
fig <- fig %>% layout(
  title = "<b>Churn Distribution w.r.t Internet Service and Gender</b>",
  barmode = 'group'
)

# Show the plot
fig


# Color map for the dependents
color_map <- c("Yes" = "#7fcdff", "No" = "#326ada")

# Create the histogram plot
fig <- plot_ly(data = df, x = ~Churn, color = ~Dependents, type = 'histogram', 
               barmode = 'group', 
               colors = color_map) %>%
  layout(
    title = "<b>Churn distribution w.r.t. Dependents</b>",
    xaxis = list(title = 'Churn'),
    yaxis = list(title = 'Count'),
    bargap = 0.1, 
    width = 700, 
    height = 500
  )

# Show the plot
fig


# Assuming df is your data frame and it has 'Churn' and 'Partner' columns.

# Color map for the Partner
color_map <- c("Yes" = "#7fcdff", "No" = "#326ada")

# Create the histogram plot
fig <- plot_ly(data = df, x = ~Churn, color = ~Partner, type = 'histogram', 
               barmode = 'group', 
               colors = color_map) %>%
  layout(
    title = "<b>Churn distribution w.r.t. Partners</b>",
    xaxis = list(title = 'Churn'),
    yaxis = list(title = 'Count'),
    bargap = 0.1, 
    width = 700, 
    height = 500
  )

# Show the plot
fig




# Assuming df is your data frame and it has 'Churn' and 'SeniorCitizen' columns.

# Color map for the SeniorCitizen
color_map <- c("1" = "#7fcdff", "0" = "#326ada")  # "1" represents Yes and "0" represents No

# Create the histogram plot
fig <- plot_ly(data = df, x = ~Churn, color = ~as.factor(SeniorCitizen), type = 'histogram', 
               barmode = 'group', 
               colors = color_map) %>%
  layout(
    title = "<b>Churn distribution w.r.t. Senior Citizen</b>",
    xaxis = list(title = 'Churn'),
    yaxis = list(title = 'Count'),
    bargap = 0.1, 
    width = 700, 
    height = 500
  )

# Show the plot
fig


library(plotly)

# Assuming df is your data frame and it has 'Churn' and 'OnlineSecurity' columns.

# Color map for the OnlineSecurity categories
color_map <- c("Yes" = "#7fcdff", "No" = "#326ada", "No internet service" = "#ff9b35")

# Create the histogram plot
fig <- plot_ly(data = df, x = ~Churn, color = ~OnlineSecurity, type = 'histogram', 
               barmode = 'group', 
               colors = color_map) %>%
  layout(
    title = "<b>Churn w.r.t Online Security</b>",
    xaxis = list(title = 'Churn'),
    yaxis = list(title = 'Count'),
    bargap = 0.1, 
    width = 700, 
    height = 500
  )

# Show the plot
fig


# Color map for the PaperlessBilling categories
color_map <- c("Yes" = "#7fcdff", "No" = "#326ada")

# Create the histogram plot
fig <- plot_ly(data = df, x = ~Churn, color = ~PaperlessBilling, type = 'histogram', 
               colors = color_map) %>%
  layout(
    title = "<b>Churn distribution w.r.t. Paperless Billing</b>",
    xaxis = list(title = 'Churn'),
    yaxis = list(title = 'Count'),
    bargap = 0.1, 
    width = 700, 
    height = 500
  )

# Show the plot
fig


color_map <- c("Yes" = "#7fcdff", "No" = "#326ada", "No internet service" = "#ff9b35")

# Create the histogram plot
fig <- plot_ly(data = df, x = ~Churn, color = ~TechSupport, type = 'histogram', 
               colors = color_map, barmode = 'group') %>%
  layout(
    title = "<b>Churn distribution w.r.t. TechSupport</b>",
    xaxis = list(title = 'Churn'),
    yaxis = list(title = 'Count'),
    bargap = 0.1, 
    width = 700, 
    height = 500
  )

# Show the plot
fig


# Color map for the PhoneService categories
color_map <- c("Yes" = "#7fcdff", "No" = "#326ada")

# Create the histogram plot
fig <- plot_ly(data = df, x = ~Churn, color = ~PhoneService, type = 'histogram', 
               colors = color_map) %>%
  layout(
    title = "<b>Churn distribution w.r.t. Phone Service</b>",
    xaxis = list(title = 'Churn'),
    yaxis = list(title = 'Count'),
    bargap = 0.1, 
    width = 700, 
    height = 500
  )

# Show the plot
fig
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assuming df is your data frame and it has 'MonthlyCharges' and 'Churn' columns.
# Make sure 'Churn' is a factor with labels for better visualization
df$Churn <- factor(df$Churn, levels = c(0, 1), labels = c("Not Churn", "Churn"))

# Create the plot
ggplot(df, aes(x = MonthlyCharges, color = Churn, fill = Churn)) +
  geom_density(alpha = 0.5) + # Density plot with transparency
  scale_color_manual(values = c("Red", "Blue")) + # Custom line colors for each Churn type
  scale_fill_manual(values = c("Red", "Blue")) +  # Custom fill colors for each Churn type
  labs(
    title = "Monthly Charges VS Churn Distribution",
    x = "Monthly Charges",
    y = "Density",
    fill = "Churn",
    color = "Churn"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.position = "top"  # Move legend to the top
  )



df_clean <- df[!is.na(df$TotalCharges), ]

# Plotting Total Charges vs Churn distribution
ggplot(df_clean, aes(x = TotalCharges, fill = factor(Churn))) +
  geom_density(alpha = 0.5) + # alpha controls transparency for colors
  scale_fill_manual(values = c("0" = "blue", "1" = "cyan")) + # Colors for Churn categories
  labs(title = "Total Charges VS Churn Distribution",
       x = "Total Charges",
       y = "Density",
       fill = "Churn") +  # Adding a label for the fill legend
  theme_minimal() # Clean theme


library(ggplot2)
library(corrplot)

# Apply factorization to all columns and compute correlation matrix
# First, ensure the data is numeric (factorize categorical variables)
df_numeric <- as.data.frame(lapply(df, function(x) as.numeric(as.factor(x))))

# Compute the correlation matrix
corr_matrix <- cor(df_numeric, use = "pairwise.complete.obs")

# Create the correlation plot
corrplot(corr_matrix, 
         method = "color",          # Color method for visualization
         col = colorRampPalette(c("blue", "white", "red"))(200),  # Color scale
         type = "upper",            # Display upper triangle
         diag = FALSE,              # No diagonal values in plot
         addCoef.col = "black",     # Add correlation coefficients
         tl.col = "black",          # Text label color
         tl.srt = 45,               # Rotate text labels
         number.cex = 0.7,          # Adjust the size of the numbers
         title = "Correlation Heatmap",
         mar = c(0,0,2,0))          # Adjust margins




# Function to convert categorical variables (factor/character) into numeric
object_to_int <- function(series) {
  if (is.factor(series) || is.character(series)) {
    series <- as.numeric(factor(series))  # Convert to numeric
  }
  return(series)
}

# Copy original dataframe
df2 <- df

# Apply the conversion function to all columns
df[] <- lapply(df, object_to_int)

# Remove rows where 'Churn' or any other variable is NA after conversion
df <- df[complete.cases(df), ]

# Separate features (X) and target variable (y)
X <- df %>% select(-Churn)  # All columns except 'Churn'
y <- df$Churn               # Target variable 'Churn'

# Check the first few rows of the dataframe after conversion and cleaning
head(df)

# Check the column names
colnames(df)

# If missing values are still present, display count
cat("Missing values in 'Churn': ", sum(is.na(y)), "\n")

# Load caret library for data partitioning
library(caret)

# Set seed for reproducibility
set.seed(99)

# Split the data into training (80%) and testing (20%) sets
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)

# Create training and testing datasets
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]

# Verify the data split
cat("Training Data: ", nrow(X_train), " rows\n")
cat("Testing Data: ", nrow(X_test), " rows\n")


head(df)
df <- df[complete.cases(df), ]
head(df)  # Check the first few rows
head(y)  # Check the first few values of the target variable


summary(df)

# Load required libraries
library(ggplot2)

# Define the distplot function in R
distplot <- function(feature, frame, color='red') {
  # Generate the plot
  plot <- ggplot(frame, aes_string(x = feature)) + 
    geom_density(fill=color, alpha=0.5) +
    ggtitle(paste("Distribution for", feature)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) # Center title
  
  # Explicitly print the plot (necessary for certain environments)
  print(plot)
}

# Example dataframe (use your actual dataframe here)
# df <- your_data_frame

# Define numeric columns
num_cols <- c("tenure", "MonthlyCharges", "TotalCharges")

# Loop through each numeric column and generate plots
for (feat in num_cols) {
  distplot(feat, df)
}



distplot <- function(feature, data, color='red') {
  # Create a distribution plot
  ggplot(data, aes_string(x = feature)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill=color, color="black", alpha=0.7) + 
    geom_density(color = 'black') +
    ggtitle(paste("Distribution for", feature)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))  # Center the title
}

# Example data (replace this with your dataframe `df`)
df <- data.frame(
  tenure = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  MonthlyCharges = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
  TotalCharges = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
)

# Define the numeric columns to scale
num_cols <- c("tenure", "MonthlyCharges", "TotalCharges")

# Scale the numeric columns (StandardScaler equivalent)
df_std <- as.data.frame(scale(df[num_cols]))

# Distribution Graph Before Scaling
print("Distribution Graph Before Scaling")
for (feat in num_cols) {
  distplot(feat, df, color='red')  # Plot before scaling
}

# Distribution Graph After Scaling
print("Distribution Graph After Scaling")
for (feat in num_cols) {
  distplot(feat, df_std, color='cyan')  # Plot after scaling
}




# Load necessary libraries
library(e1071)         # for SVM
library(caret)         # for confusionMatrix
library(ggplot2)       # for visualization
library(reshape2)      # for heatmap

# Fit a Support Vector Classifier (SVM)
svc_model <- svm(X_train, y_train, type = 'C-classification', kernel = 'linear')

# Make predictions
predict_y <- predict(svc_model, X_test)

# Calculate accuracy
accuracy_svc <- mean(predict_y == y_test)  # You can also use caret::confusionMatrix for more details
cat("SVM accuracy is:", accuracy_svc, "\n")
cat(rep("-", 60), "\n")

# Convert y_test and predict_y to factors with the same levels
y_test_factor <- factor(y_test)
predict_y_factor <- factor(predict_y, levels = levels(y_test_factor))

# Print confusion matrix
conf_mat <- confusionMatrix(predict_y_factor, y_test_factor)
print(conf_mat)

# Visualize confusion matrix using heatmap
conf_matrix_data <- as.matrix(conf_mat[['table']])

# Melt the confusion matrix for ggplot
conf_matrix_melted <- melt(conf_matrix_data)

# Fix column names to match ggplot
colnames(conf_matrix_melted) <- c("Actual", "Predicted", "Count")

# Plot the confusion matrix heatmap
ggplot(conf_matrix_melted, aes(Actual, Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "black") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "SUPPORT VECTOR CLASSIFIER CONFUSION MATRIX", x = "Predicted", y = "Actual") +
  theme_minimal()



