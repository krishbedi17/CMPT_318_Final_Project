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

# Select the first three principal components for further analysis (PC1, PC2, PC3)
selected_pca_data <- pca_result$x[, 1:3]
head(selected_pca_data)

# Plot the first two principal components using ggbiplot with improvements
plot <- ggbiplot(pca_result, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE,
                 alpha = 0.3,  # Reduce point density
                 varname.adjust = 2)  # Adjust variable name positions to avoid overlap

# Save the plot
ggsave("plot.png", plot, width = 8, height = 6, dpi = 300)

# # Alternative plot using factoextra for better label management
# fviz_pca_biplot(pca_result, 
#                 repel = TRUE,           # Prevent label overlap
#                 col.var = "blue",       # Color for variable vectors
#                 col.ind = "darkred",    # Color for data points
#                 alpha.ind = 0.3)        # Add transparency to reduce clutter
# 
# # Optional: Plot PC1 vs PC3 to visualize more variance
# fviz_pca_biplot(pca_result, choices = c(1, 3), 
#                 repel = TRUE, col.var = "blue", col.ind = "darkred", alpha.ind = 0.3) 
