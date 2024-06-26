# Common
ggplot() + ggtitle("Sample from G32") + theme(plot.title = element_text(hjust = 0.5)) + ylab("count") + xlab("t") + xlim(0, 5)

# Density Plot
density.plot <- ggplot(df, aes(x=col)) + geom_density() + xlab("col")
density.plot

ggplot() + geom_density(aes(list_of_values)) + xlab("col")

# Plot from Function
f = function(t){
  return(100*ifelse(t > 0, 2./9.*t*exp(-(t/3)^2), 0))
}
ggplot() + geom_function(fun = f, aes(col = "Density"))
ggplot() + geom_function(fun = Vectorize(f), aes(col = "Density"))

# Histogram
ggplot() + geom_histogram(colour="black", fill="white", aes(list_of_values))
ggplot() + geom_histogram(binwidth=1, colour="black", fill="white", aes(list_of_values))
# Two histograms overlaid with transparency
ggplot() + geom_histogram(colour="black", fill="red", alpha=0.2, aes(residual.enhanced))+ geom_histogram(colour="black", fill="blue", alpha=0.2, aes(residual.ordinary))
# Plot with density
ggplot() + geom_histogram(colour="black", fill="red", alpha=0.2, aes(residual.enhanced, y = ..density.. * 100), stat="bin")+ geom_histogram(colour="black", fill="blue", alpha=0.2, aes(residual.train.enhanced, y = ..density.. * 100), stat="bin") + geom_vline(xintercept = mean.residual.train.enhanced, color = "blue", linetype = "dashed", linewidth = 0.5) + geom_vline(xintercept = mean.residual.enhanced, color = "red", linetype = "dashed", linewidth = 0.5)
# Plot with proportion
ggplot() + geom_histogram(colour="black", fill="red", alpha=0.2, aes(residual.enhanced, y = ..count../sum(..count..)*100), stat="bin")+ geom_histogram(colour="black", fill="blue", alpha=0.2, aes(residual.train.enhanced, y = ..count../sum(..count..)*100), stat="bin") + geom_vline(xintercept = mean.residual.train.enhanced, color = "blue", linetype = "dashed", linewidth = 0.5) + geom_vline(xintercept = mean.residual.enhanced, color = "red", linetype = "dashed", linewidth = 0.5)
# Plot histogram with legends and stuff
data <- data.frame(
  residual = c(residual.ordinary, residual.train.ordinary),
  dataset = factor(rep(c("Residual Ordinary", "Residual Train Ordinary"), c(length(residual.ordinary), length(residual.train.ordinary))))
)

# Calculate mean values
mean_residual_ordinary <- mean(residual.ordinary)
mean_residual_train_ordinary <- mean(residual.train.ordinary)

# Create the plot with legend, mean line
plot_residual_histograms <- function(residual1, residual2) {
  # Create a data frame to store the residuals and dataset labels
  data <- data.frame(
    residual = c(residual1, residual2),
    dataset = factor(rep(c("Residual 1", "Residual 2"), c(length(residual1), length(residual2))))
  )
  
  # Calculate mean values
  mean_residual1 <- mean(residual1)
  mean_residual2 <- mean(residual2)
  
  # Create the plot
  ggplot(data, aes(x = residual, fill = dataset)) +
    geom_histogram(colour = "black", alpha = 0.2, aes(y = ..density.. * 100), stat = "bin", show.legend = TRUE) +
    geom_vline(aes(xintercept = mean_residual1, color = "Residual 1 Mean"), linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = mean_residual2, color = "Residual 2 Mean"), linetype = "dashed", size = 1) +
    scale_fill_manual(values = c("Residual 1" = "red", "Residual 2" = "blue"), name = "Data") +
    scale_color_manual(values = c("Residual 1 Mean" = "red", "Residual 2 Mean" = "blue"), name = "Mean") +
    labs(title = "Histogram of Residuals",
         x = "Residual Value",
         y = "Percentage") +
    theme_minimal()
}

# Line Plot
ggplot(df) + geom_line(aes(x = as.numeric(rownames(df)), y = X1))
ggplot() + geom_line(aes(x=1:length(list_of_values),y=list_of_values))

# Scatter Plot
ggplot(df) + geom_point(aes(x = as.numeric(rownames(df)), y = X1)) # time series
ggplot(df) + geom_point(aes(x = x, y = t)) + xlab("x: fraction of voting in 1992") + ylab("t: fraction of voting in 1994") # two columns of df

# Regression Plot
ggplot(fultongen) + geom_point(aes(x = x, y = t), color='black') + geom_line(color='red', data = overall.df, aes(x=fultongen$x, y=ols.pred)) + geom_line(color='blue', data = overall.df, aes(x=fultongen$x, y=wls.pred)) + xlab("x: fraction of voting in 1992") + ylab("t: fraction of voting in 1994")

ols.fit = lm(t ~ x, data = fultongen)
wls.fit = lm(t ~ x, weights = n, data = fultongen)
overall.df = data.frame(ols.pred = predict(ols.fit, fultongen), wls.pred = predict(wls.fit, fultongen), x=fultongen$x, t=fultongen$t, n=fultongen$n)


# What is geom_smooth?

plot_scatter <- function(x, y, color = "blue") {
  # Create data frame for x,y values
  data <- data.frame(x = x, y = y)
  
  # Create scatter plot
  ggplot(data, aes(x = x, y = y)) +
    geom_point(color = color) +
    labs(title = "Scatter Plot of X,Y Values",
         x = "X values",
         y = "Y values") +
    theme_minimal()
}

plot_line <- function(x, y, color = "blue") {
  # Create data frame for x,y values
  data <- data.frame(x = x, y = y)
  
  # Create line plot
  ggplot(data, aes(x = x, y = y)) +
    geom_line(color = color) +
    labs(title = "Line Plot of X,Y Values",
         x = "X values",
         y = "Y values") +
    theme_minimal()
}
