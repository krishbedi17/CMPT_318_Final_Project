# Load necessary libraries
library(ggbiplot)
library(ggplot2)
library(depmixS4)

# Read the data
df <- read.csv("TermProjectData.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# cols_to_convert <- c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
# print(paste("Remaining NA values:", sum(is.na(df[cols_to_convert]))))

# Interpolate missing values (NA) using linear interpolation
interpolate_na <- function(x) {
  if (is.numeric(x)) {
    return(approx(seq_along(x), x, xout = seq_along(x), method = "linear", rule = 2)$y)
  } else {
    return(x)
  }
}

# Apply interpolation to the entire dataset

# getAnomalies <- function(x) {
#   numeric_cols <- sapply(x, is.numeric)
#   anomalies <- abs(x[, numeric_cols, drop = FALSE]) > 3
#   row_anomalies <- rowSums(anomalies) > 0
#   return(row_anomalies)
# }

df_interpolated <- as.data.frame(lapply(df, interpolate_na))
cols_to_convert <- c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
print(paste("Remaining NA values:", sum(is.na(df_interpolated[cols_to_convert]))))

# 
# z_scores <- as.data.frame(scale(df_interpolated[, sapply(df_interpolated, is.numeric)], center = TRUE, scale = TRUE))
# anomaly_rows <- getAnomalies(z_scores)
# df_cleaned <- df_interpolated[!anomaly_rows, ]

# clean_outliers <- function(x) {
#   if (is.numeric(x)) {
#     q1 <- quantile(x, 0.25, na.rm = TRUE)
#     q3 <- quantile(x, 0.75, na.rm = TRUE)
#     iqr <- q3 - q1
#     lower_bound <- q1 - 1.5 * iqr
#     upper_bound <- q3 + 1.5 * iqr
#     x[x < lower_bound] <- lower_bound # Cap lower outliers
#     x[x > upper_bound] <- upper_bound # Cap upper outliers
#   }
#   return(x)
# }




# Standardize the data (center and scale)
standardize <- function(x) {
  if (is.numeric(x)) {
    return(scale(x, center = TRUE, scale = TRUE))
  } else {
    return(x)
  }
}

df_standardized <- as.data.frame(lapply(df_interpolated, standardize))

# PCA
numeric_cols <- df_standardized[, sapply(df_standardized, is.numeric), drop = FALSE]
pca_result <- prcomp(numeric_cols, center = TRUE, scale. = TRUE)

# Select top features based on PCA loadings of the first principal component (PC1)
abs_loadings <- abs(pca_result$rotation[, 1])  # Loadings for PC1
feature_importance <- abs_loadings
top_features <- names(sort(feature_importance, decreasing = TRUE)[1:3])

print(top_features)
print(abs_loadings)

biplot(pca_result, scale = 0)

pca_result.var <- pca_result$sdev^2
pca_result.var.per <- round(pca_result.var/sum(pca_result.var)*100,1)
barplot(pca_result.var.per,main="Scree Plot", xlab ="Principal Component", ylab ="Percent Variation")

# Open a PNG graphics device
png("scree_plot.png", width = 800, height = 600)

# Generate the Scree Plot
barplot(pca_result.var.per,
        main = "Scree Plot", 
        xlab = "Principal Component", 
        ylab = "Percent Variation")

# Close the graphics device
dev.off()

# Define current date and user info
# Update date and time
# current_datetime <- "2025-03-28 02:25:55"
# user_login <- "pga47"
# 
# # Load ggrepel for non-overlapping labels (if not already loaded)
# # If you don't have ggrepel installed, uncomment and run:
# # install.packages("ggrepel")
# library(ggrepel)
# 
# # Define scaling factors explicitly
# var.scale <- 1        # Scale factor for variable vectors
# varname.adjust <- 1.2 # Adjustment factor for variable labels 
# arrow.size <- 1.5     # Size of arrow heads
# 
# # Calculate variance explained
# var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100
# 
# # Sample data points to reduce plot rendering time
# set.seed(123) # For reproducibility
# sample_size <- min(1000, nrow(pca_result$x)) # Limit to 1000 points
# sampled_indices <- sample(1:nrow(pca_result$x), sample_size)
# sampled_points <- as.data.frame(pca_result$x[sampled_indices, ])
# 
# # Create a simplified PCA biplot with clear layers and non-overlapping labels
# pca_plot <- ggplot() + 
#   # Add sampled points as the bottom layer
#   geom_point(data = sampled_points, 
#              aes(x = PC1, y = PC2), 
#              color = "lightgray", 
#              alpha = 0.2,
#              size = 1) +
#   # Add variable vectors on top as arrows
#   geom_segment(data = as.data.frame(pca_result$rotation), 
#                aes(x = 0, y = 0, 
#                    xend = PC1 * var.scale, 
#                    yend = PC2 * var.scale),
#                arrow = arrow(length = unit(arrow.size, "mm")), 
#                color = "red", 
#                size = 0.8) +
#   # Add variable names with repelling labels to avoid overlap
#   geom_text_repel(data = as.data.frame(pca_result$rotation),
#                   aes(x = PC1 * var.scale * varname.adjust, 
#                       y = PC2 * var.scale * varname.adjust, 
#                       label = rownames(pca_result$rotation)),
#                   color = "red",
#                   size = 5,
#                   box.padding = 0.35,
#                   point.padding = 0.5,
#                   segment.color = "gray50",
#                   max.overlaps = 15,
#                   min.segment.length = 0) +
#   # Add theme and labels
#   theme_minimal() +
#   theme(
#     panel.grid.major = element_line(color = "gray90"),
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(face = "bold", size = 14),
#     plot.subtitle = element_text(size = 12)
#   ) +
#   labs(
#     title = "PCA Biplot (PC1 vs PC2)",
#     subtitle = paste0("Analysis date: ", current_datetime, " | User: ", user_login),
#     x = sprintf("PC1 (%.1f%%)", var_explained[1]),
#     y = sprintf("PC2 (%.1f%%)", var_explained[2]),
#     caption = paste("Top features:", paste(top_features, collapse = ", "))
#   ) +
#   coord_fixed()  # Keep aspect ratio 1:1
# 
# # Print the plot
# print(pca_plot)

# >>>>>>> Stashed changes
# # Plot the first two principal components using ggbiplot with improvements
# plot <- ggbiplot(pca_result, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE,
#                  alpha = 0.1,  # Reduce point density
#                  varname.adjust = 2)  # Adjust variable name positions to avoid overlap
# 
# Save the plot
# ggsave("plot.png", plot, width = 8, height = 6, dpi = 300)
# plot(pca_result$x[, 1], pca_result$x[, 2],
#      xlab = "PC1", ylab = "PC2",
#      main = "PCA: First Two Principal Components")

# pca_scores <- pca_result$x
# 
# # Convert PCA scores to a data frame for ggplot
# pca_df <- data.frame(PC1 = pca_scores[, 1], PC2 = pca_scores[, 2])
# 
# # Plot the first two principal components
# p <- ggplot(pca_df, aes(x = PC1, y = PC2)) +
#   geom_point(alpha = 0.1) +  # Adjust point transparency
#   theme_minimal() +
#   labs(title = "PCA - First Two Principal Components",
#        x = "PC1", y = "PC2")
# 
# # Save the plot
# ggsave("pca_plot.png", p, width = 8, height = 6, dpi = 300)


# Selecting Time Slot
df_standardized$newDate <- as.POSIXlt(df_standardized$Date, format = "%d/%m/%Y")
df_standardized$week <- strftime(df_standardized$newDate, format = "%W")
df_standardized$Time<- format(as.POSIXct(df_standardized$Time, format = "%H:%M:%S"), "%H:%M:%S")


time_window_monday_data <- subset(df_standardized, 
                                  Time >= "12:00:00" & Time <= "16:00:00" & 
                                  strftime(as.POSIXlt(Date, format = "%d/%m/%Y"), format = "%A") == "Monday")

# Extract Year and Day after filtering
time_window_monday_data$Year <- format(time_window_monday_data$newDate, "%Y")
time_window_monday_data$Day <- format(time_window_monday_data$newDate, "%d")

# Partition the data into training and testing sets
training_data <- subset(time_window_monday_data, Year <= 2008)
testing_data <- subset(time_window_monday_data, Year == 2009)

# Train Model
selected_training_data <- training_data[, top_features]
selected_test_data <- testing_data[, top_features]

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
discretize <- function(x) {
  lower <- floor(x * 2) / 2
  upper <- ceiling(x * 2) / 2
  ifelse(abs(x - lower) < abs(x - upper), lower, upper)
}
training_data_discretized <- as.data.frame(lapply(selected_training_data, discretize))
selected_test_data_discretized<- as.data.frame(lapply(selected_test_data, discretize))
# Prepare response formulas and families for each feature
families <- list()
responses <- list()

for (feature in top_features) {
  if (is.numeric(training_data_discretized[[feature]])) {
    families[[feature]] <- gaussian()
    responses[[feature]] <- as.formula(paste(feature, "~ 1"))
  } else {
    training_data_discretized[[feature]] <- as.factor(training_data_discretized[[feature]])  # Convert to factor if categorical
    families[[feature]] <- multinomial()
    responses[[feature]] <- as.formula(paste(feature, "~ 1"))
  }
}

test_families <- list()
test_responses <- list()

for (feature in top_features) {
  if (is.numeric(selected_test_data_discretized[[feature]])) {
    test_families[[feature]] <- gaussian()
    test_responses[[feature]] <- as.formula(paste(feature, "~ 1"))
  } else {
    selected_test_data_discretized[[feature]] <- as.factor(selected_test_data_discretized[[feature]])  # Convert to factor if categorical
    test_families[[feature]] <- multinomial()
    test_responses[[feature]] <- as.formula(paste(feature, "~ 1"))
  }
}


# Set up the Hidden Markov Model (HMM)
nstates_range <- seq(4, 16, 2)
log_likelihoods <- c()
bic_values <- c()

for (n_states in nstates_range) {
  model <- depmix(response = responses,
                  data = training_data_discretized,
                  nstates = n_states,
                  family = families,
                  ntimes = nrow(training_data_discretized))
  
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


test_model <- depmix(response = test_responses,
                data = selected_test_data_discretized,
                nstates = best_model_states,
                family = test_families,
                ntimes = nrow(selected_test_data_discretized))

# Set parameters from the trained model
setpars(test_model, getpars(fit_model))

train_log_likelihood <- logLik(fit_model)
test_log_likelihood <- logLik(test_model)

normalized_train_log_likelihood <- as.numeric(train_log_likelihood) / nrow(training_data_discretized)
normalized_test_log_likelihood <- as.numeric(test_log_likelihood) / nrow(selected_test_data_discretized)

cat("Normalized Train Log-Likelihood:", normalized_train_log_likelihood, "\n")
cat("Normalized Test Log-Likelihood:", normalized_test_log_likelihood, "\n")
