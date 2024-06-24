data <- read.table("data.txt", header=T)
data


# Define the factors
factors <- list(
  Group = data$Group,
  Gender = data$Gender
)

# Function to calculate statistics
calculate_stats <- function(x) {
  obs <- sum(!is.na(x))
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  range_val <- max_val - min_val
  sum_val <- sum(x, na.rm = TRUE)
  mean_val <- mean(x, na.rm = TRUE)
  median_val <- median(x, na.rm = TRUE)
  sum_squares <- sum((x - mean_val)^2, na.rm = TRUE)
  variance <- sum_squares / (obs - 1)
  std_dev <- sqrt(variance)
  return(c(Observations = obs, Minimum = min_val, Maximum = max_val, Range = range_val,
           Sum = sum_val, Mean = mean_val, Median = median_val, 
           Sum_of_squares = sum_squares, Variance = variance, 
           Standard_deviation = std_dev))
}

# Calculate statistics for each combination of factors
result <- aggregate(. ~ Group + Gender, data, calculate_stats)

# Print the result
print(result)





# Custom function to calculate cross-products
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

# Custom function to calculate covariance
calculate_covariance <- function(x, y) {
  n <- length(x)
  cross_product <- calculate_cross_products(x, y)
  covariance <- cross_product / (n - 1)
  return(covariance)
}

# Custom function to calculate correlation
calculate_correlation <- function(x, y) {
  cov <- calculate_covariance(x, y)
  sd_x <- sqrt(calculate_variance(x))
  sd_y <- sqrt(calculate_variance(y))
  correlation <- cov / (sd_x * sd_y)
  return(correlation)
}

# Function to calculate variance
calculate_variance <- function(x) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sum_sq_diff <- sum((x - mean_x)^2, na.rm = TRUE)
  variance <- sum_sq_diff / (n - 1)
  return(variance)
}




# Examples

# Calculate cross-products by group
cross_product_group <- list()
covariance_group <- list()
correlation_group <- list()

# Unique groups in the dataset
groups <- unique(data$Group)

# Loop through each group
for (group in groups) {
  # Filter the data by group
  data_group <- subset(data, Group == group)
  
  # Calculate cross-products for Var1 and Var2
  cross_product <- calculate_cross_products(data_group$Var1, data_group$Var2)
  cross_product_group[[group]] <- cross_product
  
  # Calculate covariance for Var1 and Var2
  covariance <- calculate_covariance(data_group$Var1, data_group$Var2)
  covariance_group[[group]] <- covariance
  
  # Calculate correlation for Var1 and Var2
  correlation <- calculate_correlation(data_group$Var1, data_group$Var2)
  correlation_group[[group]] <- correlation
}

# Print the outputs
for (group in groups) {
  cat("Group:", group, "\n")
  cat("Cross-product for Var1 and Var2:", cross_product_group[[group]], "\n")
  cat("Covariance for Var1 and Var2:", covariance_group[[group]], "\n")
  cat("Correlation for Var1 and Var2:", correlation_group[[group]], "\n\n")
}



# Calculate cross-products by gender
cross_product_gender <- list()
covariance_gender <- list()
correlation_gender <- list()

# Unique genders in the dataset
genders <- unique(data$Gender)

# Loop through each gender
for (gender in genders) {
  # Filter the data by gender
  data_gender <- subset(data, Gender == gender)
  
  # Calculate cross-products for Var1 and Var2
  cross_product <- calculate_cross_products(data_gender$Var1, data_gender$Var2)
  cross_product_gender[[gender]] <- cross_product
  
  # Calculate covariance for Var1 and Var2
  covariance <- calculate_covariance(data_gender$Var1, data_gender$Var2)
  covariance_gender[[gender]] <- covariance
  
  # Calculate correlation for Var1 and Var2
  correlation <- calculate_correlation(data_gender$Var1, data_gender$Var2)
  correlation_gender[[gender]] <- correlation
}

# Print the outputs
for (gender in genders) {
  cat("Gender:", gender, "\n")
  cat("Cross-product for Var1 and Var2:", cross_product_gender[[gender]], "\n")
  cat("Covariance for Var1 and Var2:", covariance_gender[[gender]], "\n")
  cat("Correlation for Var1 and Var2:", correlation_gender[[gender]], "\n\n")
}




# Calculate cross-products by group and gender combination
cross_product_group_gender <- list()
covariance_group_gender <- list()
correlation_group_gender <- list()

# Unique groups and genders in the dataset
groups <- unique(data$Group)
genders <- unique(data$Gender)

# Loop through each group and gender combination
for (group in groups) {
  for (gender in genders) {
    # Filter the data by group and gender
    data_group_gender <- subset(data, Group == group & Gender == gender)
    
    # Calculate cross-products for Var1 and Var2
    cross_product <- calculate_cross_products(data_group_gender$Var1, data_group_gender$Var2)
    cross_product_group_gender[[paste(group, gender, sep = "_")]] <- cross_product
    
    # Calculate covariance for Var1 and Var2
    covariance <- calculate_covariance(data_group_gender$Var1, data_group_gender$Var2)
    covariance_group_gender[[paste(group, gender, sep = "_")]] <- covariance
    
    # Calculate correlation for Var1 and Var2
    correlation <- calculate_correlation(data_group_gender$Var1, data_group_gender$Var2)
    correlation_group_gender[[paste(group, gender, sep = "_")]] <- correlation
  }
}

# Print the outputs
for (group in groups) {
  for (gender in genders) {
    cat("Group:", group, "Gender:", gender, "\n")
    cat("Cross-product for Var1 and Var2:", cross_product_group_gender[[paste(group, gender, sep = "_")]], "\n")
    cat("Covariance for Var1 and Var2:", covariance_group_gender[[paste(group, gender, sep = "_")]], "\n")
    cat("Correlation for Var1 and Var2:", correlation_group_gender[[paste(group, gender, sep = "_")]], "\n\n")
  }
}




