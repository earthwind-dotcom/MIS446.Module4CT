# Name: Irwin Ortega
# Course: MIS446
# Module: 4

# Step 2: Explore the 'qsec' (1/4 mile time) variable from the mtcars dataset.

# The mtcars dataset is built into R, so we can access it directly.
# We'll work specifically with the 'qsec' column.
qsec_data <- mtcars$qsec

# --- Calculate Measures of Location for qsec ---
cat("--- Measures of Location for qsec ---\n")

# Mean: The average 1/4 mile time.
mean_qsec <- mean(qsec_data)
cat("Mean of qsec:", mean_qsec, "\n")

# Median: The middle value of the 1/4 mile times when sorted.
median_qsec <- median(qsec_data)
cat("Median of qsec:", median_qsec, "\n")

# Mode: The most frequent value. R doesn't have a built-in mode function,
# so we'll define a simple function to find it.
get_mode <- function(v) {
  uniqv <- unique(v)
  # tabulate counts occurrences of each unique value
  # which.max finds the index of the highest count
  # univq[index] gives the value itself
  mode_val <- uniqv[which.max(tabulate(match(v, uniqv)))]
  # Can handle multimodal cases if needed, but for typical assignment, single mode is fine.
  return(mode_val)
}
mode_qsec <- get_mode(qsec_data)
cat("Mode of qsec:", mode_qsec, "\n")

# 1st Quartile (Q1): The value below which 25% of the data falls.
q1_qsec <- quantile(qsec_data, 0.25)
cat("1st Quartile (Q1) of qsec:", q1_qsec, "\n")

# 3rd Quartile (Q3): The value below which 75% of the data falls.
q3_qsec <- quantile(qsec_data, 0.75)
cat("3rd Quartile (Q3) of qsec:", q3_qsec, "\n")

# Range: The difference between the maximum and minimum values.
range_qsec_val <- max(qsec_data) - min(qsec_data)
cat("Range of qsec:", range_qsec_val, "\n")


# --- Visually Display Measures of Location (Box Plot with highlights) ---
# A box plot is excellent for showing median, quartiles, and range visually.
# Adding points/lines for mean and mode can further illustrate location measures.
boxplot(qsec_data,
        main = "Distribution of 1/4 Mile Time (qsec) with Location Measures",
        ylab = "1/4 Mile Time (seconds)",
        col = "lightblue", # Light blue color for the box
        border = "darkblue", # Dark blue border
        outline = TRUE, # Show outliers
        cex.axis = 0.8 # Adjust axis label size
)

# Adding mean as a red point
points(x = 1, y = mean_qsec, col = "red", pch = 8, cex = 1.5, lwd = 2) # pch=8 is an asterisk
text(x = 1.1, y = mean_qsec, labels = paste("Mean:", round(mean_qsec, 2)), col = "red", pos = 4, cex = 0.8)

# Adding median as a blue horizontal line (already drawn by boxplot, but explicitly marking)
# abline(h = median_qsec, col = "blue", lty = 2) # lty=2 for dashed line

# Add mode if it's a single value (can be tricky if multimodal or not an exact data point)
# For simplicity, we'll assume a single mode for plotting.
# if (length(mode_qsec) == 1) {
#   points(x = 1, y = mode_qsec, col = "purple", pch = 17, cex = 1.5) # pch=17 is a triangle
#   text(x = 1.1, y = mode_qsec, labels = paste("Mode:", round(mode_qsec, 2)), col = "purple", pos = 4, cex = 0.8)
# }


# --- Calculate Measures of Dispersion for qsec ---
cat("\n--- Measures of Dispersion for qsec ---\n")

# Variance: Average of the squared differences from the mean.
variance_qsec <- var(qsec_data)
cat("Variance of qsec:", variance_qsec, "\n")

# Standard Deviation: Square root of the variance, typical spread around the mean.
sd_qsec <- sd(qsec_data)
cat("Standard Deviation of qsec:", sd_qsec, "\n")

# Coefficient of Variation (CV): Relative variability (SD/Mean * 100).
cv_qsec <- (sd_qsec / mean_qsec) * 100
cat("Coefficient of Variation (CV) of qsec:", cv_qsec, "%\n")


# --- Visually Display Variability (Frequency Distribution - Histogram) ---
hist(qsec_data,
     main = "Frequency Distribution of 1/4 Mile Time (qsec)",
     xlab = "1/4 Mile Time (seconds)",
     ylab = "Frequency",
     col = "lightgreen", # Light green bars
     border = "darkgreen", # Dark green border
     breaks = 5, # Adjust number of bins if needed, or let R decide.
     cex.axis = 0.8 # Adjust axis label size
)

# Step 3: Print current date and time and username (First Instance)
cat("\n--- System Information (First Instance) ---\n")
cat("Current Date and Time:\n")
print(Sys.time())

# Get the username from the system. This code handles if the username is blank.
username_val <- Sys.getenv("username")
if (username_val == "") {
  username_val <- "User_Not_Detected_or_Blank" # Use a placeholder if system username is empty
}
cat("Username:\n")
print(username_val)

# Step 4: Explore the association between 'qsec' and 'hp' (Gross Horsepower).
# Access the 'hp' (gross horsepower) column from the mtcars dataset.
hp_data <- mtcars$hp

# --- Calculate Measures of Association ---
cat("\n--- Measures of Association between qsec and hp ---\n")

# Covariance: Measures how two variables change together.
# A positive covariance indicates they tend to increase/decrease together.
# A negative covariance indicates one tends to increase as the other decreases.
covariance_qsec_hp <- cov(qsec_data, hp_data)
cat("Covariance (qsec, hp):", covariance_qsec_hp, "\n")

# Correlation: Standardized measure of linear association (between -1 and 1).
# Closer to -1 or 1 means stronger linear relationship.
correlation_qsec_hp <- cor(qsec_data, hp_data)
cat("Correlation (qsec, hp):", correlation_qsec_hp, "\n")


# --- Visually Display Association (Scatter Plot) ---
# A scatter plot is ideal for visualizing the relationship between two quantitative variables.
plot(hp_data, qsec_data, # x-axis is hp, y-axis is qsec
     main = "Scatter Plot of 1/4 Mile Time (qsec) vs. Gross Horsepower (hp)",
     xlab = "Gross Horsepower (hp)",
     ylab = "1/4 Mile Time (seconds)",
     col = "darkorange", # Points color
     pch = 19, # Solid circles for points
     cex = 1.2, # Larger points
     cex.axis = 0.8, # Adjust axis label size
     cex.lab = 1 # Adjust axis title size
)

# Linear regression line optional to show the trend 
abline(lm(qsec_data ~ hp_data), col = "pink", lty = 2, lwd = 2) # Dashed pink line for linear trend

# Step 5: Print current date and time and username (Second Instance)
cat("\n--- System Information (Second Instance) ---\n")
cat("Current Date and Time:\n")
print(Sys.time())

# Re-using the username_val variable from Step 3
cat("Username:\n")
print(username_val)
