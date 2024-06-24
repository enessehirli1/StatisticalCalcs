data <- read.table("data.txt", header=T)
data

# Function to scale variables in a data frame
scale_variable <- function(data, variables, method = "z-score") {
  scaled_values <- matrix(NA, nrow = nrow(data), ncol = length(variables))
  means <- numeric(length(variables))
  sds <- numeric(length(variables))
  
  for (i in seq_along(variables)) {
    var <- variables[i]
    if (method == "z-score") {
      mean_val <- mean(data[[var]], na.rm = TRUE)
      sd_val <- sd(data[[var]], na.rm = TRUE)
      scaled_values[, i] <- (data[[var]] - mean_val) / sd_val
      means[i] <- mean_val
      sds[i] <- sd_val
    } else if (method == "min-max") {
      min_val <- min(data[[var]], na.rm = TRUE)
      max_val <- max(data[[var]], na.rm = TRUE)
      scaled_values[, i] <- (data[[var]] - min_val) / (max_val - min_val)
      means[i] <- min_val
      sds[i] <- max_val - min_val
    } else {
      cat("Invalid method. Please choose either 'z-score' or 'min-max'.")
      return(NULL)
    }
  }
  
  colnames(scaled_values) <- variables
  rownames(scaled_values) <- rownames(data)
  return(list(scaled = scaled_values, means = means, sds = sds))
}

# Example usage:
# Scale Var1 and Var2 using z-score method
scaled_output <- scale_variable(data, c("Var1", "Var2"), method = "z-score")
scaled_values <- scaled_output$scaled
means <- scaled_output$means
sds <- scaled_output$sds

# Print scaled values
print(scaled_values)

# Print means
print(means)

# Print standard deviations
print(sds)
