# Load necessary libraries
# If not installed, uncomment the following lines to install required packages
# install.packages("devtools")
# install.packages("remotes")
# remotes::install_github("vqv/ggbiplot")
# install.packages("ggplot2")
# install.packages("factoextra")

library(ggbiplot)
library(ggplot2)
# library(factoextra)
install.packages("depmixS4")


# Read the data
df <- read.csv("D:\\SFU\\CMPT 318\\CMPT_318_Final_Project\\TermProjectData.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Interpolate missing values (NA) using linear interpolation
interpolate_na <- function(x) {
  if (is.numeric(x)) {
    return(approx(seq_along(x), x, xout = seq_along(x), method = "linear", rule = 2)$y)
  } else {
    return(x)
  }
}

# Apply interpolation to the entire dataset
df_interpolated <- as.data.frame(lapply(df, interpolate_na))

# Standardize the data (center and scale)
standardize <- function(x) {
  if (is.numeric(x)) {
    return(scale(x, center = TRUE, scale = TRUE))
  } else {
    return(x)
  }
}

df_standardized <- as.data.frame(lapply(df_interpolated, standardize))

# Print standardized dataset (optional)
print(df_standardized)

# Extract only numeric columns for PCA
numeric_cols <- df_standardized[, sapply(df_standardized, is.numeric), drop = FALSE]

# Perform PCA using prcomp
pca_result <- prcomp(numeric_cols, center = TRUE, scale. = TRUE)

# Print PCA summary to understand variance explained by each PC
summary(pca_result)

# Print the first few principal components (scores)
print(head(pca_result$x))

# Print the rotation (loadings) to understand how each variable contributes to the PCs
print(pca_result$rotation)
pc1_loadings <- pca_result$rotation[, 1]  # Loadings for PC1
pc2_loadings <- pca_result$rotation[, 2]  # Loadings for PC2
pc3_loadings <- pca_result$rotation[, 3]  # Loadings for PC3

# Print out the loadings
cat("PCA1 loadings:\n")
print(pc1_loadings)

cat("PCA2 loadings:\n")
print(pc2_loadings)

cat("PCA3 loadings:\n")
print(pc3_loadings)

# Select the first three principal components for further analysis (PC1, PC2, PC3)
selected_pca_data <- pca_result$x[, 1:3]
head(selected_pca_data)


# get top features
abs_loadings <- abs(pca_result$rotation[, 1:3])
feature_importance <- rowSums(abs_loadings)
top_features <- names(sort(feature_importance, decreasing = TRUE)[1:3])

# Plot the first two principal components using ggbiplot with improvements
plot <- ggbiplot(pca_result, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE,
                 alpha = 0.3,  # Reduce point density
                 varname.adjust = 2)  # Adjust variable name positions to avoid overlap

# Save the plot
ggsave("plot.png", plot, width = 8, height = 6, dpi = 300)


# Selecting Time Slot
time_window_data <- subset(df_standardized, Time >= "12:00:00" & Time <= "16:00:00")
time_window_data$Year <- format(time_window_data$newDate, "%Y")

# Partition
training_data <- subset(time_window_data, Year <= 2008)
testing_data <- subset(time_window_data, Year <= 2009)


# Training Model
selected_training_data <- training_data[, top_features]

# Step 2: Set up the Hidden Markov Model (HMM) with different number of states
nstates_range <- seq(4, 16, 2)
log_likelihoods <- c()
bic_values <- c()


response_matrix <- cbind(selected_training_data)
family_list <- rep(list(gaussian()), ncol(response_matrix))

library(depmixS4)
for (n_states in nstates_range) {
  model <- depmix(response = cbind(selected_training_data), 
                  data = training_data, 
                  nstates = n_states, 
                  family = family_list)
  fitModel <- fit(model)
  
  log_likelihood <- logLik(fitModel)
  bic_value <- BIC(fitModel)
  
  log_likelihoods <- c(log_likelihoods, log_likelihood)
  bic_values <- c(bic_values, bic_value)
  
  cat("Number of States:", num_states, "\n")
  cat("Log-Likelihood:", log_likelihood, "\n")
  cat("BIC:", bic_value, "\n\n")
  
  
}

best_model_index <- which.min(bic_values)  # Find the index of the best model
best_model_states <- states[best_model_index]

cat("Best model has", best_model_states, "states based on BIC and log-likelihood.\n")



