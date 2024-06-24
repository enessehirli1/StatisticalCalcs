data <- read.table("data.txt", header=T)
data

par(mfrow=c(1,1))

# Custom function to draw scatterplot between two variables
draw_scatterplot <- function(x, y, x_label = "X", y_label = "Y") {
  plot(x, y, xlab = x_label, ylab = y_label, main = paste("Scatterplot of", x_label, "vs", y_label))
}

# Custom function to draw scatterplot matrix among variables in a data frame
draw_scatterplot_matrix <- function(data) {
  num_vars <- ncol(data)
  par(mfrow = c(num_vars, num_vars))  # Set up the layout of the plot
  
  for (i in 1:num_vars) {
    for (j in 1:num_vars) {
      if (i == j) {
        # Draw histograms for diagonal plots
        hist(data[[i]], main = paste("Histogram of", names(data)[i]), xlab = names(data)[i], col = "lightblue", border = "black")
      } else {
        # Draw scatterplots for off-diagonal plots
        plot(data[[i]], data[[j]], xlab = names(data)[i], ylab = names(data)[j], main = paste(names(data)[i], "vs", names(data)[j]))
      }
    }
  }
  par(mfrow=c(1,1))
}

# Example usage:
# Draw scatterplot between Var1 and Var2
draw_scatterplot(data$Var1, data$Var2, x_label = "Var1", y_label = "Var2")

# Draw scatterplot matrix for Var1, Var2, Var3, Var4
draw_scatterplot_matrix(data[, c("Var1", "Var2", "Var3", "Var4")])


