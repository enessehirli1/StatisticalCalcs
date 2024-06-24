data <- read.table(file="data.txt", header=TRUE)
head(data)
tail(data)

length(data)

### Number of observations
count_observations <- function(dataset) {
  counts <- sapply(dataset, function(x) sum(!is.na(x)))
  return(counts)
}

# Testing the function
result <- count_observations(data[, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")])
print(result)
### 


### Minimum
min_values <- function(data) {
  num_vars <- ncol(data)
  min_vals <- numeric(num_vars)
  
  for (i in 1:num_vars) {
    var_name <- names(data)[i]
    var <- unlist(data[[i]])
    var <- var[!is.na(var)]  # Remove NA values
    min_val <- var[1]  # Initialize min_val with the first non-NA value
    
    for (j in 2:length(var)) {
      if (!is.na(var[j]) && var[j] < min_val) {
        min_val <- var[j]
      }
    }
    
    min_vals[i] <- min_val
  }
  
  names(min_vals) <- names(data)
  
  return(min_vals)
}

# Testing the function
min_vals <- min_values(data[, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")])
print(min_vals)
### 


### Maximum
max_values <- function(data) {
  num_vars <- ncol(data)
  max_vals <- numeric(num_vars)
  
  for (i in 1:num_vars) {
    var_name <- names(data)[i]
    var <- unlist(data[[i]])
    var <- var[!is.na(var)]  # Remove NA values
    max_val <- var[1]  # Initialize max_val with the first non-NA value
    
    for (j in 2:length(var)) {
      if (!is.na(var[j]) && var[j] > max_val) {
        max_val <- var[j]
      }
    }
    
    max_vals[i] <- max_val
  }
  
  names(max_vals) <- names(data)
  
  return(max_vals)
}

# Testing the function
max_vals <- max_values(data[, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")])
print(max_vals)
###



### Range
find_range <- function(data) {
  num_vars <- ncol(data)
  
  for (i in 1:num_vars) {
    var_name <- names(data)[i]
    var <- unlist(data[[i]])
    var <- var[!is.na(var)]  # Remove NA values
    
    min_val <- var[1]  # Initialize min_val with the first non-NA value
    max_val <- var[1]  # Initialize max_val with the first non-NA value
    
    for (j in 2:length(var)) {
      if (!is.na(var[j])) {
        if (var[j] < min_val) {
          min_val <- var[j]
        }
        if (var[j] > max_val) {
          max_val <- var[j]
        }
      }
    }
    
    range_val <- paste0(min_val, " ", max_val)
    cat(var_name, ": ", range_val, "\n")
  }
}

# Testing the function
find_range(data[, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")])
###


### Sum
calculate_sum <- function(data) {
  num_vars <- ncol(data)
  
  for (i in 1:num_vars) {
    var_name <- names(data)[i]
    var <- unlist(data[[i]])
    var <- var[!is.na(var)]  # Remove NA values
    
    sum_val <- 0
    
    for (value in var) {
      sum_val <- sum_val + value
    }
    
    cat(var_name, ": ", sum_val, "\n")
  }
}

# Testing the function
calculate_sum(data[, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")])
###



### Mean
calculate_mean <- function(data) {
  num_vars <- ncol(data)
  
  for (i in 1:num_vars) {
    var_name <- names(data)[i]
    var <- unlist(data[[i]])
    var <- var[!is.na(var)]  # Remove NA values
    
    sum_val <- 0
    count <- 0
    
    for (value in var) {
      sum_val <- sum_val + value
      count <- count + 1
    }
    
    mean_val <- sum_val / count
    
    cat(var_name, ": ", mean_val, "\n")
  }
}

# Testing the function
calculate_mean(data[, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")])
###




### Median
calculate_median <- function(data) {
  num_vars <- ncol(data)
  
  for (i in 1:num_vars) {
    var_name <- names(data)[i]
    var <- unlist(data[[i]])
    var <- var[!is.na(var)]  # Remove NA values
    
    var <- sort(var)  # Sort the values
    
    n <- length(var)
    if (n %% 2 == 0) {
      median_val <- (var[n / 2] + var[n / 2 + 1]) / 2
    } else {
      median_val <- var[(n + 1) / 2]
    }
    
    cat(var_name, ": ", median_val, "\n")
  }
}

# Testing the function
calculate_median(data[, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")])
###





### Sum of squares
calculate_sum_of_squares <- function(data) {
  num_vars <- ncol(data)
  
  for (i in 1:num_vars) {
    var_name <- names(data)[i]
    var <- unlist(data[[i]])
    var <- var[!is.na(var)]  # Remove NA values
    
    var_sum <- 0
    var_sum_of_squares <- 0
    num_values <- 0
    
    for (value in var) {
      var_sum <- var_sum + value
      var_sum_of_squares <- var_sum_of_squares + value^2
      num_values <- num_values + 1
    }
    
    var_mean <- var_sum / num_values
    
    sum_of_squares <- 0
    for (value in var) {
      sum_of_squares <- sum_of_squares + (value - var_mean)^2
    }
    
    cat(var_name, ": ", sum_of_squares, "\n")
  }
}

# Testing the function
calculate_sum_of_squares(data[, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")])
###


### Variance
calculate_variance <- function(data) {
  num_vars <- ncol(data)
  
  for (i in 1:num_vars) {
    var_name <- names(data)[i]
    var <- unlist(data[[i]])
    var <- var[!is.na(var)]  # Remove NA values
    
    var_sum <- 0
    var_sum_of_squares <- 0
    num_values <- 0
    
    for (value in var) {
      var_sum <- var_sum + value
      var_sum_of_squares <- var_sum_of_squares + value^2
      num_values <- num_values + 1
    }
    
    var_mean <- var_sum / num_values
    
    sum_of_squares <- 0
    for (value in var) {
      sum_of_squares <- sum_of_squares + (value - var_mean)^2
    }
    
    var_variance <- sum_of_squares / num_values
    
    cat(var_name, ": ", var_variance, "\n")
  }
}

# Testing the function
calculate_variance(data[, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")])
###


### Standart Deviation
calculate_standard_deviation <- function(data) {
  num_vars <- ncol(data)
  
  for (i in 1:num_vars) {
    var_name <- names(data)[i]
    var <- unlist(data[[i]])
    var <- var[!is.na(var)]  # Remove NA values
    
    var_sum <- 0
    var_sum_of_squares <- 0
    num_values <- 0
    
    for (value in var) {
      var_sum <- var_sum + value
      var_sum_of_squares <- var_sum_of_squares + value^2
      num_values <- num_values + 1
    }
    
    var_mean <- var_sum / num_values
    
    sum_of_squares <- 0
    for (value in var) {
      sum_of_squares <- sum_of_squares + (value - var_mean)^2
    }
    
    var_variance <- sum_of_squares / num_values
    
    var_standard_deviation <- sqrt(var_variance)
    
    cat(var_name, ": ", var_standard_deviation, "\n")
  }
}

# Testing the function
calculate_standard_deviation(data[, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8")])
###









###  cross-products 
calculate_cross_products <- function(x, y) {
  mean_x <- mean(x, na.rm = TRUE)
  mean_y <- mean(y, na.rm = TRUE)
  cross_product <- 0
  
  for (i in 1:length(x)) {
    if (!is.na(x[i]) && !is.na(y[i])) {
      cross_product <- cross_product + (x[i] - mean_x) * (y[i] - mean_y)
    }
  }
  
  return(cross_product)
}

# Testing the function
calculate_cross_products(data$Var1, data$Var2)
calculate_cross_products(data$Var3, data$Var4)
###


### Covariance
calculate_covariance <- function(x, y) {
  n <- length(x)
  cross_product <- calculate_cross_products(x, y)
  covariance <- cross_product / (n - 1)
  return(covariance)
}
calculate_covariance(data$Var1, data$Var2)
calculate_covariance(data$Var3, data$Var4)
###




### Correlation
# Custom function to calculate correlation
calculate_correlation <- function(x, y) {
  mean_x <- mean(x, na.rm = TRUE)
  mean_y <- mean(y, na.rm = TRUE)
  
  cross_product <- 0
  sum_sq_diff_x <- 0
  sum_sq_diff_y <- 0
  
  for (i in 1:length(x)) {
    if (!is.na(x[i]) && !is.na(y[i])) {
      cross_product <- cross_product + (x[i] - mean_x) * (y[i] - mean_y)
      sum_sq_diff_x <- sum_sq_diff_x + (x[i] - mean_x) ^ 2
      sum_sq_diff_y <- sum_sq_diff_y + (y[i] - mean_y) ^ 2
    }
  }
  
  correlation <- cross_product / sqrt(sum_sq_diff_x * sum_sq_diff_y)
  
  return(correlation)
}

# Testing the function
calculate_correlation(data$Var1, data$Var2)
calculate_correlation(data$Var3, data$Var4)
###
