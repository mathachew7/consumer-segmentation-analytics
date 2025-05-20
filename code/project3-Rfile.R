# ----------------------Setting up the working directory---------------------#
setwd("//Users//subashyadav//Documents//Practicum//Project 3//")

# ----------------------Loading the necessary libraries-------------------------#
library(dplyr)
library(ggplot2)
library(caret) 
library(psych)
library(skimr)
library(VIM)
library(car)
library(cluster)
library(factoextra)
library(corrplot)
library(randomForest)
library(gridExtra)
library(tidyverse)
library(reshape2)
library(smotefamily)


# Setting plot options
options(scipen = 999)

# ----------------------Loading the dataset-------------------------#
# Load the consumer dataset
consumer_data <- read.csv("../data/Consumer.csv", stringsAsFactors = FALSE)

# View the first few rows of the dataset
head(consumer_data)

# View the structure of the dataset
str(consumer_data)

# ----------------------Basic Data Inspection---------------------#
# Check for duplicated rows
duplicated_rows <- sum(duplicated(consumer_data))
print(paste("Number of duplicated rows:", duplicated_rows))

# Summary statistics for the dataset
summary(consumer_data)

# For detailed summary of numeric variables
describe(consumer_data)

# Alternatively, using skimr for comprehensive summary
skim(consumer_data)

colnames(consumer_data)


#------------------------Renaming the variables------------------------------#
# Rename columns with underscores and lowercase
colnames(consumer_data) <- c(
  "member_id", "sec", "feh", "mt", "sex", "age", "edu", "hs", 
  "child", "cs", "affluence_index", "no_of_brands", "brand_runs", 
  "total_volume", "no_of_trans", "value", "trans_brand_runs", 
  "vol_tran", "avg_price", "pur_vol_no_promo", "pur_vol_promo_6", 
  "pur_vol_other_promo", "br_cd_57_144", "br_cd_55", "br_cd_272", 
  "br_cd_286", "br_cd_24", "br_cd_481", "br_cd_352", "br_cd_5", 
  "others_999", "pr_cat_1", "pr_cat_2", "pr_cat_3", "pr_cat_4", 
  "propcat_5", "propcat_6", "propcat_7", "propcat_8", "propcat_9", 
  "propcat_10", "propcat_11", "propcat_12", "propcat_13", "propcat_14", 
  "propcat_15"
)

# Verify the changes
colnames(consumer_data)

#---------------------Removing member.id as it is not reauired ----------#
# Remove 'Member.id' column using base R
consumer_data <- consumer_data[, !names(consumer_data) %in% c("member_id")]


#------------------------Zero Values ----------------------------------#
# Function to count zero values in each column
count_zeros <- function(df) {
  sapply(df, function(x) sum(x == 0, na.rm = TRUE))
}

# Count zero values for the entire consumer_data dataset
zero_counts <- count_zeros(consumer_data)
zero_counts

# Impute missing values in edu with median education level
median_edu <- median(consumer_data$edu[consumer_data$edu != 0])
consumer_data$edu[consumer_data$edu == 0] <- median_edu

# Impute missing values in HS with median household size
median_hs <- median(consumer_data$hs[consumer_data$hs != 0])
consumer_data$hs[consumer_data$hs == 0] <- median_hs

# Impute missing values in CS with mode (assuming 1 for Available is most common)
consumer_data$cs[consumer_data$cs == 0] <- as.integer(names(sort(table(consumer_data$cs), decreasing = TRUE))[1])

# Impute missing values in Affluence Index with median value
median_affluence <- median(consumer_data$affluence_index[consumer_data$affluence_index != 0])
consumer_data$affluence_index[consumer_data$affluence_index == 0] <- median_affluence


# prepared dataset 
head(consumer_data)

#---------------------------Visualization-------------------------------------#
# Let's see the data distribution to get to know the data better.
# 1. Age Group Distribution with updated notation
ggplot(consumer_data, aes(x = factor(age))) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Age Group Distribution", x = "Age Group", y = "Count")

# Gender Distribution with Labels for Male, Female, and Not Specified
ggplot(consumer_data, aes(x = factor(sex, levels = c(1, 2, 0), labels = c("Male", "Female", "Not Specified")))) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Gender Distribution", x = "Gender", y = "Count")

# 3. Education Level Distribution
ggplot(consumer_data, aes(x = factor(edu))) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Education Level Distribution", x = "Education Level", y = "Count")

# 4. Affluence Index vs. Total Volume
ggplot(consumer_data, aes(x = affluence_index, y = total_volume)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  labs(title = "Affluence Index vs. Total Volume", x = "Affluence Index", y = "Total Volume")

# 5. Number of Brands Purchased by Age Group
ggplot(consumer_data, aes(x = factor(age), y = no_of_brands)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Number of Brands Purchased by Age Group", x = "Age Group", y = "Number of Brands")

# 6. Average Price per Transaction by Age Group
ggplot(consumer_data, aes(x = factor(age), y = avg_price)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Average Price per Transaction by Age Group", x = "Age Group", y = "Average Price")

# 7. Total Volume vs. Average Price
ggplot(consumer_data, aes(x = total_volume, y = avg_price)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  labs(title = "Total Volume vs. Average Price", x = "Total Volume", y = "Average Price")

# 8. Number of Transactions per Gender
ggplot(consumer_data, aes(x = factor(sex), y = no_of_trans)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Number of Transactions per Gender", x = "Gender", y = "Number of Transactions")

# 9. Brand Runs vs. Transaction Value
ggplot(consumer_data, aes(x = brand_runs, y = value)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  labs(title = "Brand Runs vs. Transaction Value", x = "Brand Runs", y = "Transaction Value")

# 10. Top Brands by Purchase Volume
top_brands <- consumer_data %>%
  select(starts_with("br_cd_")) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(cols = everything(), names_to = "Brand", values_to = "Volume") %>%
  arrange(desc(Volume))

ggplot(top_brands, aes(x = reorder(Brand, Volume), y = Volume)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top Brands by Purchase Volume", x = "Brand", y = "Purchase Volume")

# Promotion Usage Distribution
promotion_data <- consumer_data %>%
  summarise(No_Promo = sum(pur_vol_no_promo),
            Promo_6 = sum(pur_vol_promo_6),
            Other_Promo = sum(pur_vol_other_promo)) %>%
  pivot_longer(cols = everything(), names_to = "Promotion_Type", values_to = "Volume")

# Define custom colors
custom_colors <- c("No_Promo" = "steelblue", "Promo_6" = "lightblue", "Other_Promo" = "skyblue")

# Plot with customized colors
ggplot(promotion_data, aes(x = "", y = Volume, fill = Promotion_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Promotion Usage Distribution", x = "", y = "Volume") +
  theme_minimal()

# 12. Volume Distribution across Different Promotion Types
ggplot(promotion_data, aes(x = Promotion_Type, y = Volume)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Volume), vjust = -0.3) +
  labs(title = "Volume Distribution across Promotion Types", x = "Promotion Type", y = "Volume")

# 13. Child Dependency and Affluence Index
ggplot(consumer_data, aes(x = factor(child), y = affluence_index)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Child Dependency and Affluence Index", x = "Number of Children", y = "Affluence Index")

# 14. Transaction Volume Distribution
ggplot(consumer_data, aes(x = total_volume)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 30) +
  labs(title = "Transaction Volume Distribution", x = "Total Volume", y = "Frequency") +
  theme_minimal()

# 15. Number of Brands Purchased by Affluence Index
ggplot(consumer_data, aes(x = affluence_index, y = no_of_brands)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  labs(title = "Number of Brands Purchased by Affluence Index", x = "Affluence Index", y = "Number of Brands")


# Subset the columns related to promotion categories only
promo_data <- consumer_data[, c("pr_cat_1", "pr_cat_2", "pr_cat_3", "pr_cat_4")]

# Add a row ID column to avoid using all columns as measure variables
promo_data$ID <- 1:nrow(promo_data)

# Melt the data into a long format for easier plotting
promo_data_long <- melt(promo_data, id.vars = "ID", variable.name = "Promotion_Category", value.name = "Volume")

# Generate the box plot for promotion categories
ggplot(promo_data_long, aes(x = Promotion_Category, y = Volume)) +
  geom_boxplot(fill = "steelblue", color = "black", outlier.color = "red", outlier.size = 1.5) +
  labs(title = "Volume Distribution by Promotion Category", 
       x = "Promotion Category", 
       y = "Volume") +
  theme_minimal()


# Subset the brand columns only and add an ID column
brand_data <- consumer_data[, c("br_cd_57_144", "br_cd_55", "br_cd_272", "br_cd_286", 
                                "br_cd_24", "br_cd_481", "br_cd_352", "br_cd_5")]
brand_data$ID <- 1:nrow(brand_data)  # Add an ID column for melting

# Melt the data for heatmap format, specifying ID as the id variable
melted_data_brand <- melt(brand_data, id.vars = "ID", variable.name = "Brand", value.name = "Value")

# Apply a log transformation to the Value column, adding 1 to avoid log(0) issues
melted_data_brand$LogValue <- log10(melted_data_brand$Value + 1)

# Generate heatmap with log-scaled values for brand data
ggplot(melted_data_brand, aes(Brand, factor(ID))) +
  geom_tile(aes(fill = LogValue), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue", name = "Log(Value + 1)") +
  labs(title = "Heatmap of Brand Data (Log Scale)", 
       x = "Brand", y = "ID") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#----------------------Removing the columns that are the not required -----------------#
# Remove specified columns from the dataset
consumer_data <- consumer_data[, !names(consumer_data) %in% c("feh", "mt", "age", "child", "cs")]

# Verify the changes
colnames(consumer_data)

# preserving the original datast 
original_data <- consumer_data


# Data Relevancy 
# ----------------------Correlation Analysis-------------------------#
# Calculate the correlation matrix for all numeric columns
cor_matrix <- cor(consumer_data, use = "complete.obs")

# Display the correlation matrix
print(cor_matrix)

# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle", type = "lower", tl.cex = 0.7, tl.col = "black",
         title = "Correlation Matrix of Consumer Data", mar = c(0,0,1,0))

# Set a threshold for high correlation (e.g., 0.6)
cor_threshold <- 0.6

# Find pairs of highly correlated variables
highly_corr_pairs <- which(abs(cor_matrix) > cor_threshold, arr.ind = TRUE)

# Filter out diagonal elements (self-correlation)
highly_corr_pairs <- highly_corr_pairs[highly_corr_pairs[, 1] != highly_corr_pairs[, 2], ]

# Prepare a data frame to show highly correlated pairs and their values
high_corr_df <- data.frame(
  Variable_1 = rownames(cor_matrix)[highly_corr_pairs[, 1]],
  Variable_2 = colnames(cor_matrix)[highly_corr_pairs[, 2]],
  Correlation = cor_matrix[highly_corr_pairs]
) %>%
  distinct() %>%
  arrange(desc(abs(Correlation)))

# Print the table of highly correlated variables with correlation values
print("Highly Correlated Variable Pairs:")
print(high_corr_df)



# ---------------------- Data Transformation ---------------------- #
# Log transformations for skewed variables
consumer_data$log_total_purchase_volume <- log1p(consumer_data$total_volume)
consumer_data$log_avg_spend_per_transaction <- log1p(consumer_data$value / consumer_data$no_of_trans)

# Derived variables
consumer_data$brand_loyalty_score <- consumer_data$brand_runs / (consumer_data$no_of_brands + 1)
consumer_data$deal_sensitivity <- (consumer_data$pur_vol_promo_6 + consumer_data$pur_vol_other_promo) / (consumer_data$total_volume + 1)
category_columns <- grep("pr_cat|propcat", names(consumer_data), value = TRUE)
consumer_data$category_diversity <- rowSums(consumer_data[, category_columns] > 0)

# Select relevant columns for each clustering segment
behavior_data <- consumer_data %>%
  select(log_total_purchase_volume, brand_loyalty_score, no_of_trans)

basis_data <- consumer_data %>%
  select(deal_sensitivity, category_diversity, no_of_trans)

combined_data <- consumer_data %>%
  select(log_total_purchase_volume, brand_loyalty_score, no_of_trans, deal_sensitivity, category_diversity)


# ----------------------E lbow Plot and Silhouette Plot for optimal number of cluster---------------#

# ---------------------- Elbow Method ---------------------- #
# Function to calculate WSS (within-cluster sum of squares) for a range of clusters
elbow_method <- function(data, max_clusters = 10) {
  wss <- sapply(1:max_clusters, function(k) {
    kmeans(data, centers = k, nstart = 25)$tot.withinss
  })
  plot(1:max_clusters, wss, type = "b", pch = 19, frame = FALSE,
       xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares",
       main = paste("Elbow Method for Optimal Clusters in", deparse(substitute(data))))
}

# Elbow plots for each dataset
elbow_method(behavior_data, max_clusters = 10)
elbow_method(basis_data, max_clusters = 10)
elbow_method(combined_data, max_clusters = 10)

# ---------------------- Silhouette Analysis ---------------------- #
# Function to plot silhouette scores for a range of clusters
silhouette_analysis <- function(data, max_clusters = 10) {
  avg_sil_width <- sapply(2:max_clusters, function(k) {
    km <- kmeans(data, centers = k, nstart = 25)
    ss <- silhouette(km$cluster, dist(data))
    mean(ss[, 3])
  })
  plot(2:max_clusters, avg_sil_width, type = "b", pch = 19, frame = FALSE,
       xlab = "Number of Clusters", ylab = "Average Silhouette Width",
       main = paste("Silhouette Analysis for Optimal Clusters in", deparse(substitute(data))))
}

# Silhouette analysis for each dataset
silhouette_analysis(behavior_data, max_clusters = 10)
silhouette_analysis(basis_data, max_clusters = 10)
silhouette_analysis(combined_data, max_clusters = 10)



# ---------------------- K-Means Clustering with 3 Clusters ---------------------- #

# 1. Purchase Behavior Clustering with 3 Clusters
cat("---- Purchase Behavior Clustering with 3 Clusters ----\n")
set.seed(123)
optimal_clusters_behavior <- 3
kmeans_behavior <- kmeans(behavior_data, centers = optimal_clusters_behavior, nstart = 25)
consumer_data$cluster_behavior <- kmeans_behavior$cluster

# Silhouette Analysis for Purchase Behavior Clustering
silhouette_behavior <- silhouette(kmeans_behavior$cluster, dist(behavior_data))
avg_silhouette_behavior <- mean(silhouette_behavior[, 3])

# Print silhouette values
cat("Average Silhouette Score for Purchase Behavior Clustering with 3 clusters:", round(avg_silhouette_behavior, 3), "\n")
print(table(consumer_data$cluster_behavior))
plot(
  silhouette_behavior,
  col = 1:optimal_clusters_behavior,
  border = NA,
  main = "Silhouette Plot for Purchase Behavior Clustering with 3 Clusters"
)

# Cluster Centers Plot for Purchase Behaviour
# Purchase Behavior Clustering: Cluster Centers Plot
cluster_centers_behavior <- as.data.frame(kmeans_behavior$centers)
cluster_centers_behavior$Feature <- rownames(cluster_centers_behavior)
cluster_centers_behavior_long <- tidyr::pivot_longer(
  cluster_centers_behavior,
  cols = -Feature,
  names_to = "Cluster",
  values_to = "Average Value"
)

# Basic plot without customizing colors
ggplot(cluster_centers_behavior_long, aes(x = Feature, y = `Average Value`, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cluster Centers by Feature (Purchase Behavior)", 
       x = "Feature", y = "Average Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 2. Basis for Purchase Clustering with 3 Clusters
cat("\n---- Basis for Purchase Clustering with 3 Clusters ----\n")
set.seed(123)
optimal_clusters_basis <- 3
kmeans_basis <- kmeans(basis_data, centers = optimal_clusters_basis, nstart = 25)
consumer_data$cluster_basis <- kmeans_basis$cluster

# Silhouette Analysis for Basis for Purchase Clustering
silhouette_basis <- silhouette(kmeans_basis$cluster, dist(basis_data))
avg_silhouette_basis <- mean(silhouette_basis[, 3])

# Print silhouette values
cat("Average Silhouette Score for Basis for Purchase Clustering with 3 clusters:", round(avg_silhouette_basis, 3), "\n")
print(table(consumer_data$cluster_basis))
plot(
  silhouette_basis,
  col = 1:optimal_clusters_basis,
  border = NA,
  main = "Silhouette Plot for Basis for Purchase Clustering with 3 Clusters"
)

# Basis for Purchase Clustering: Cluster Centers Plot
cluster_centers_basis <- as.data.frame(kmeans_basis$centers)
cluster_centers_basis$Feature <- rownames(cluster_centers_basis)

# Reshape the cluster centers to long format for easy plotting
cluster_centers_basis_long <- tidyr::pivot_longer(
  cluster_centers_basis,
  cols = -Feature,
  names_to = "Cluster",
  values_to = "Average Value"
)

# Ensure 'Cluster' is a factor
cluster_centers_basis_long$Cluster <- as.factor(cluster_centers_basis_long$Cluster)

# Plot cluster centers
ggplot(cluster_centers_basis_long, aes(x = Feature, y = `Average Value`, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cluster Centers by Feature (Basis for Purchase)", 
       x = "Feature", y = "Average Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# 3. Combined Clustering with 3 Clusters
cat("\n---- Combined Clustering with 3 Clusters ----\n")
set.seed(123)
optimal_clusters_combined <- 3
kmeans_combined <- kmeans(combined_data, centers = optimal_clusters_combined, nstart = 25)
consumer_data$cluster_combined <- kmeans_combined$cluster

# Silhouette Analysis for Combined Clustering
silhouette_combined <- silhouette(kmeans_combined$cluster, dist(combined_data))
avg_silhouette_combined <- mean(silhouette_combined[, 3])

# Print silhouette values
cat("Average Silhouette Score for Combined Clustering with 3 clusters:", round(avg_silhouette_combined, 3), "\n")
print(table(consumer_data$cluster_combined))
plot(
  silhouette_combined,
  col = 1:optimal_clusters_combined,
  border = NA,
  main = "Silhouette Plot for Combined Clustering with 3 Clusters"
)

# Combined Clustering: Cluster Centers Plot
cluster_centers_combined <- as.data.frame(kmeans_combined$centers)
cluster_centers_combined$Feature <- rownames(cluster_centers_combined)
cluster_centers_combined_long <- tidyr::pivot_longer(
  cluster_centers_combined,
  cols = -Feature,
  names_to = "Cluster",
  values_to = "Average Value"
)
cluster_centers_combined_long$Cluster <- as.factor(cluster_centers_combined_long$Cluster)

ggplot(cluster_centers_combined_long, aes(x = Feature, y = `Average Value`, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cluster Centers by Feature (Combined)", 
       x = "Feature", y = "Average Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Print the variables used for each clustering segment

# Variables used for Purchase Behavior Clustering
cat("Variables used for Purchase Behavior Clustering:\n")
print(colnames(behavior_data))

# Variables used for Basis for Purchase Clustering
cat("\nVariables used for Basis for Purchase Clustering:\n")
print(colnames(basis_data))

# Variables used for Combined Clustering
cat("\nVariables used for Combined Clustering:\n")
print(colnames(combined_data))






# --------------------- Step 1: Data Preparation for Combined Clustering ---------------------- #
# Ensure the dataset includes the combined clusters
combined_cluster_data <- consumer_data %>%
  filter(cluster_combined %in% c(1, 2)) %>%  # Adjust clusters based on business goal
  mutate(
    # Define total spend for potential high-value consumers
    total_spend = total_volume * avg_price,
    
    # Target for value-conscious classification: high spenders or frequent buyers (top 25% in spend or transactions)
    value_conscious = ifelse(total_spend > quantile(total_spend, 0.75) | 
                               no_of_trans > quantile(no_of_trans, 0.75), 1, 0),
    
    # Target for brand loyalty prediction: define based on 'brand_runs' (frequency of same brand purchases)
    brand_loyalty = ifelse(brand_runs > quantile(brand_runs, 0.75), 1, 0)
  ) %>%
  select(-total_spend, -cluster_behavior, -cluster_basis, -cluster_combined)  # Remove clustering columns post-target creation

# --------------------- Step 2: Create Datasets for Each Task ---------------------- #
# Dataset for Value-Conscious Classification
classification_data <- combined_cluster_data %>%
  select(value_conscious, brand_loyalty_score, log_total_purchase_volume, 
         category_diversity, affluence_index, no_of_trans, avg_price)

# Dataset for Brand Loyalty Prediction
prediction_data <- combined_cluster_data %>%
  select(brand_loyalty, brand_loyalty_score, log_total_purchase_volume, 
         category_diversity, affluence_index, no_of_trans, avg_price)

# Verify target distribution for each dataset
cat("Distribution of Value-Conscious Target:\n")
print(table(classification_data$value_conscious))

cat("Distribution of Brand Loyalty Target:\n")
print(table(prediction_data$brand_loyalty))


colnames(classification_data)
colnames(prediction_data)



# --------------------- Refined Datasets to Avoid Data Leakage --------------------- #

# Remove direct predictors of the target for each task

# Refine classification data by removing predictors tied to value_conscious
classification_data_refined <- classification_data %>%
  select(value_conscious, category_diversity, affluence_index, avg_price)

# Refine prediction data by removing predictors tied to brand_loyalty
prediction_data_refined <- prediction_data %>%
  select(brand_loyalty, category_diversity, affluence_index, avg_price)

# Print refined column names to confirm removal
cat("Refined Columns for Classification:\n")
print(colnames(classification_data_refined))

cat("\nRefined Columns for Prediction:\n")
print(colnames(prediction_data_refined))

# -------------------- Enhanced Datasets with Additional Predictors -------------------- #

colnames(classification_data)

# Refine the classification dataset by removing derived features
classification_data_refined <- classification_data %>%
  select(value_conscious, category_diversity, affluence_index, avg_price, no_of_trans)  # Exclude brand_loyalty_score, log_total_purchase_volume

colnames(classification_data_refined)


# Set seed for reproducibility
set.seed(123)

# Split data into training (70%) and test (30%) sets
split_index <- sample(seq_len(nrow(classification_data_refined)), size = 0.7 * nrow(classification_data_refined))
train_data <- classification_data_refined[split_index, ]
test_data <- classification_data_refined[-split_index, ]

# Check the distribution of the target variable in the training and test sets
cat("Training set distribution:\n")
print(table(train_data$value_conscious))
cat("Test set distribution:\n")
print(table(test_data$value_conscious))

# Fit logistic regression model on the training data
value_conscious_model <- glm(value_conscious ~ ., data = train_data, family = binomial)

# Summarize the model to inspect coefficients and significance
summary(value_conscious_model)

# Step 3: Predict on Test Set
pred_value_conscious <- predict(value_conscious_model, newdata = test_data, type = "response")
pred_value_conscious_class <- ifelse(pred_value_conscious > 0.5, 1, 0)

# Confusion Matrix and Model Performance with explicit positive class = 1
conf_matrix_value <- confusionMatrix(factor(pred_value_conscious_class), factor(test_data$value_conscious), positive = "1")

cat("Confusion Matrix for Value-Conscious Classification:\n")
print(conf_matrix_value)


# Model Accuracy
cat("Model Accuracy:\n")
print(conf_matrix_value$overall["Accuracy"])



# --------------------- Step 1: Data Preparation for Brand Loyalty Prediction ---------------------- #
# Ensure the dataset includes the combined clusters for high-value consumers
brand_loyalty_data <- consumer_data %>%
  filter(cluster_combined %in% c(1, 2)) %>%  # Adjust clusters based on business goal
  mutate(
    # Define total spend for potential high-value consumers
    total_spend = total_volume * avg_price,
    
    # Target for brand loyalty prediction: high-frequency buyers or consistent brand purchases
    brand_loyalty = ifelse(brand_runs > quantile(brand_runs, 0.75), 1, 0)
  ) %>%
  select(-total_spend, -cluster_behavior, -cluster_basis, -cluster_combined)  # Remove clustering columns

# --------------------- Step 2: Selecting Relevant Columns to Avoid Data Leakage ---------------------- #

# Dataset for Brand Loyalty Prediction with refined columns
# Avoid derived predictors or those that correlate highly with the target variable
prediction_data <- brand_loyalty_data %>%
  select(brand_loyalty, category_diversity, affluence_index, avg_price, no_of_trans)  # Remove brand_loyalty_score, log_total_purchase_volume

# Confirm the columns for prediction to ensure minimal multicollinearity and relevance
cat("Columns for Brand Loyalty Prediction:\n")
print(colnames(prediction_data))

# -------------------- Step 3: Data Partitioning for Brand Loyalty Prediction -------------------- #

# Set seed for reproducibility
set.seed(123)

# Split data into training (70%) and test (30%) sets
split_index_loyalty <- sample(seq_len(nrow(prediction_data)), size = 0.7 * nrow(prediction_data))
train_data_loyalty <- prediction_data[split_index_loyalty, ]
test_data_loyalty <- prediction_data[-split_index_loyalty, ]

# Check the distribution of the target variable in the training and test sets
cat("Training set distribution for Brand Loyalty:\n")
print(table(train_data_loyalty$brand_loyalty))
cat("Test set distribution for Brand Loyalty:\n")
print(table(test_data_loyalty$brand_loyalty))

# --------------------- Step 4: Model Training and Evaluation ---------------------- #

# Fit logistic regression model for brand loyalty on the training data
brand_loyalty_model <- glm(brand_loyalty ~ ., data = train_data_loyalty, family = binomial)

# Summarize the model to inspect coefficients and significance
summary(brand_loyalty_model)

# Predictions on the test set
pred_brand_loyalty <- predict(brand_loyalty_model, newdata = test_data_loyalty, type = "response")
pred_brand_loyalty_class <- ifelse(pred_brand_loyalty > 0.5, 1, 0)

# Confusion Matrix and Model Performance
conf_matrix_loyalty <- confusionMatrix(factor(pred_brand_loyalty_class), factor(test_data_loyalty$brand_loyalty), positive = "1")
cat("Confusion Matrix for Brand Loyalty Prediction:\n")
print(conf_matrix_loyalty)

# Model Accuracy
cat("Model Accuracy for Brand Loyalty Prediction:\n")
print(conf_matrix_loyalty$overall["Accuracy"])





# Load required library
library(pROC)

# ROC Curve for Value-Conscious Classification Model
roc_value_conscious <- roc(test_data$value_conscious, pred_value_conscious)
auc_value_conscious <- auc(roc_value_conscious)

# ROC Curve for Brand Loyalty Prediction Model
roc_brand_loyalty <- roc(test_data_loyalty$brand_loyalty, pred_brand_loyalty)
auc_brand_loyalty <- auc(roc_brand_loyalty)

# Plotting both ROC curves
plot(roc_value_conscious, col = "blue", lwd = 2, main = "ROC Curves for Classification Models")
lines(roc_brand_loyalty, col = "green", lwd = 2)

# Adding legend and AUC labels
legend("bottomright", legend = c(paste("Value-Conscious Model (AUC =", round(auc_value_conscious, 2), ")"),
                                 paste("Brand Loyalty Model (AUC =", round(auc_brand_loyalty, 2), ")")),
       col = c("blue", "green"), lwd = 2, cex = 0.7)


#-------------------------Random Forest Model ---------------------------#

#---------------------Value Conscious Classification ---------------Random Forest------------#
# Convert target variable to a factor for classification
train_data$value_conscious <- as.factor(train_data$value_conscious)
test_data$value_conscious <- as.factor(test_data$value_conscious)

# Fit a Random Forest model on the training data for classification
value_conscious_rf_model <- randomForest(value_conscious ~ ., data = train_data, ntree = 500, mtry = 2, importance = TRUE)

# Display the model summary
print(value_conscious_rf_model)

# Feature importance
importance(value_conscious_rf_model)
varImpPlot(value_conscious_rf_model, main = "Feature Importance for Value-Conscious Classification")

# Predict on the test set
pred_value_conscious_rf <- predict(value_conscious_rf_model, newdata = test_data)

# Confusion Matrix and Model Performance
conf_matrix_value_rf <- confusionMatrix(factor(pred_value_conscious_rf), test_data$value_conscious)
cat("Confusion Matrix for Value-Conscious Classification (Random Forest):\n")
print(conf_matrix_value_rf)

# Model Accuracy
cat("Random Forest Model Accuracy:\n")
print(conf_matrix_value_rf$overall["Accuracy"])

#---------------------Brand Loyalty Classification ---------------Random Forest------------#

# Convert the target variable to a factor for classification
train_data_loyalty$brand_loyalty <- as.factor(train_data_loyalty$brand_loyalty)
test_data_loyalty$brand_loyalty <- as.factor(test_data_loyalty$brand_loyalty)

# Fit a Random Forest model on the training data for brand loyalty classification
set.seed(123)  # for reproducibility
brand_loyalty_rf_model <- randomForest(brand_loyalty ~ ., data = train_data_loyalty, ntree = 500, mtry = 2, importance = TRUE)

# Display the model summary
print(brand_loyalty_rf_model)

# Feature importance
importance(brand_loyalty_rf_model)
varImpPlot(brand_loyalty_rf_model, main = "Feature Importance for Brand Loyalty Classification")

# Predict on the test set
pred_brand_loyalty_rf <- predict(brand_loyalty_rf_model, newdata = test_data_loyalty)

# Confusion Matrix and Model Performance
conf_matrix_loyalty_rf <- confusionMatrix(pred_brand_loyalty_rf, test_data_loyalty$brand_loyalty)
cat("Confusion Matrix for Brand Loyalty Classification (Random Forest):\n")
print(conf_matrix_loyalty_rf)

# Model Accuracy
cat("Random Forest Model Accuracy for Brand Loyalty:\n")
print(conf_matrix_loyalty_rf$overall["Accuracy"])



# --------------------- Step 1: Data Preparation for Brand Runs Regression ---------------------- #

# Ensure the dataset includes combined clusters for high-value consumers
brand_runs_data <- consumer_data %>%
  filter(cluster_combined %in% c(1, 2)) %>%  # Adjust clusters based on business goal
  select(brand_runs, category_diversity, affluence_index, avg_price, no_of_trans, total_volume)

# --------------------- Step 2: Train-Test Split ---------------------- #

# Set seed for reproducibility
set.seed(123)

# Split data into training (70%) and test (30%) sets
split_index_runs <- sample(seq_len(nrow(brand_runs_data)), size = 0.7 * nrow(brand_runs_data))
train_data_runs <- brand_runs_data[split_index_runs, ]
test_data_runs <- brand_runs_data[-split_index_runs, ]

# Check distribution of brand runs in training and test sets
cat("Training set summary:\n")
print(summary(train_data_runs$brand_runs))

cat("Test set summary:\n")
print(summary(test_data_runs$brand_runs))

# --------------------- Step 3: Fit Regression Model ---------------------- #

# Fit linear regression model on the training set
brand_runs_model <- lm(brand_runs ~ ., data = train_data_runs)

# Summarize the model to inspect coefficients and significance
summary(brand_runs_model)

# --------------------- Step 4: Predict on Test Set and Evaluate ---------------------- #

# Predictions on the test set
pred_brand_runs <- predict(brand_runs_model, newdata = test_data_runs)

# Calculate Mean Absolute Error (MAE) and Mean Squared Error (MSE) for model evaluation
mae <- mean(abs(pred_brand_runs - test_data_runs$brand_runs))
mse <- mean((pred_brand_runs - test_data_runs$brand_runs)^2)

# Print evaluation metrics
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")

# Calculate RMSE (Root Mean Squared Error) and MAPE (Mean Absolute Percentage Error)
rmse <- sqrt(mean((pred_brand_runs - test_data_runs$brand_runs)^2))
mape <- mean(abs((test_data_runs$brand_runs - pred_brand_runs) / test_data_runs$brand_runs)) * 100

# Print RMSE and MAPE
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")


# --------------------- Step 5: Plot Predicted vs. Actual Values ---------------------- #

# Plot to visualize predicted vs. actual values
library(ggplot2)
ggplot(data = NULL, aes(x = test_data_runs$brand_runs, y = pred_brand_runs)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs. Actual Brand Runs",
       x = "Actual Brand Runs",
       y = "Predicted Brand Runs") +
  theme_minimal()



