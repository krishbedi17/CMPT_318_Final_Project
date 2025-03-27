# Load necessary libraries
library(ggbiplot)
library(ggplot2)
library(depmixS4)

# Read the data
df <- read.csv("TermProjectData.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Interpolate missing values (NA) using linear interpolation
interpolate_na <- function(x) {
  if (is.numeric(x)) {
    return(approx(seq_along(x), x, xout = seq_along(x), method = "linear", rule = 2)$y)
  } else {
    return(x)
  }
}

# Apply interpolation to the entire dataset


# clean_outliers <- function(x) {
#   if (is.numeric(x)) {
#     q1 <- quantile(x, 0.25, na.rm = TRUE)
#     q3 <- quantile(x, 0.75, na.rm = TRUE)
#     iqr <- q3 - q1
#     lower_bound <- q1 - 1.5 * iqr
#     upper_bound <- q3 + 1.5 * iqr
#     x[x < lower_bound | x > upper_bound] <- NA # Replace outliers with NA
#   }
#   return(x)
# }

getAnomalies <- function(x) {
  numeric_cols <- sapply(x, is.numeric)
  anomalies <- abs(x[, numeric_cols, drop = FALSE]) > 3
  row_anomalies <- rowSums(anomalies) > 0
  return(row_anomalies)
}

df_interpolated <- as.data.frame(lapply(df, interpolate_na))

z_scores <- as.data.frame(scale(df_interpolated[, sapply(df_interpolated, is.numeric)], center = TRUE, scale = TRUE))
anomaly_rows <- getAnomalies(z_scores)
df_cleaned <- df_interpolated[!anomaly_rows, ]


# Standardize the data (center and scale)
standardize <- function(x) {
  if (is.numeric(x)) {
    return(scale(x, center = TRUE, scale = TRUE))
  } else {
    return(x)
  }
}

df_standardized <- as.data.frame(lapply(df_cleaned, standardize))

# # PCA
# numeric_cols <- df_standardized[, sapply(df_standardized, is.numeric), drop = FALSE]
# pca_result <- prcomp(numeric_cols, center = TRUE, scale. = TRUE)
# 
# # Select top features based on PCA loadings
# abs_loadings <- abs(pca_result$rotation[, 1:3])
# feature_importance <- rowSums(abs_loadings)
# top_features <- names(sort(feature_importance, decreasing = TRUE)[1:3])

# PCA
numeric_cols <- df_standardized[, sapply(df_standardized, is.numeric), drop = FALSE]
pca_result <- prcomp(numeric_cols, center = TRUE, scale. = TRUE)

# Select top features based on PCA loadings of the first principal component (PC1)
abs_loadings <- abs(pca_result$rotation[, 1])  # Loadings for PC1
feature_importance <- abs_loadings
top_features <- names(sort(feature_importance, decreasing = TRUE)[1:3])

# Plot the first two principal components using ggbiplot with improvements
plot <- ggbiplot(pca_result$rotation[,1], obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE,
                 alpha = 0.1,  # Reduce point density
                 varname.adjust = 2)  # Adjust variable name positions to avoid overlap

# Save the plot
ggsave("plot.png", plot, width = 8, height = 6, dpi = 300)


# Selecting Time Slot
df_standardized$newDate <- as.POSIXlt(df_standardized$Date, format = "%d/%m/%Y")
df_standardized$week <- strftime(df_standardized$newDate, format = "%W")
df_standardized$Time<- format(as.POSIXct(df_standardized$Time, format = "%H:%M:%S"), "%H:%M:%S")
time_window_data <- subset(df_standardized, Time >= "12:00:00" & Time <= "16:00:00")
time_window_data$Year <- format(time_window_data$newDate, "%Y")

# Partition the data into training and testing sets
training_data <- subset(time_window_data, Year <= 2008)
testing_data <- subset(time_window_data, Year <= 2009)

# Train Model
selected_training_data <- training_data[, top_features]

#Discretize
# discretize_feature <- function(x, bins = 4) {
#   if (is.numeric(x)) {
#     return(as.factor(cut(x, breaks = quantile(x, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE),
#                          include.lowest = TRUE, labels = FALSE)))
#   } else {
#     return(x)
#   }
# }

# Apply discretization
# training_data_discretized <- as.data.frame(lapply(selected_training_data, discretize_feature))

# Prepare response formulas and families for each feature
families <- list()
responses <- list()

for (feature in top_features) {
  if (is.numeric(training_data[[feature]])) {
    families[[feature]] <- gaussian()
    responses[[feature]] <- as.formula(paste(feature, "~ 1"))
  } else {
    training_data[[feature]] <- as.factor(training_data[[feature]])  # Convert to factor if categorical
    families[[feature]] <- multinomial()
    responses[[feature]] <- as.formula(paste(feature, "~ 1"))
  }
}

# Set up the Hidden Markov Model (HMM)
nstates_range <- seq(4, 16, 2)
log_likelihoods <- c()
bic_values <- c()

for (n_states in nstates_range) {
  model <- depmix(response = responses,
                  data = training_data,
                  nstates = n_states,
                  family = families,
                  ntimes = nrow(training_data))
  
  fitModel <- fit(model)
  
  log_likelihood <- logLik(fitModel)
  bic_value <- BIC(fitModel)
  
  log_likelihoods <- c(log_likelihoods, log_likelihood)
  bic_values <- c(bic_values, bic_value)
  
  cat("Number of States:", n_states, "\n")
  cat("Log-Likelihood:", log_likelihood, "\n")
  cat("BIC:", bic_value, "\n\n")
}

# Find the best model based on BIC
best_model_index <- which.min(bic_values)
best_model_states <- nstates_range[best_model_index]

cat("Best model has", best_model_states, "states based on BIC and log-likelihood.\n")
