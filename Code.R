 


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
