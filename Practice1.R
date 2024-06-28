#############################
# Statistical Literacy Course
# by Oksana Chernova
#
# Part 1. Descriptive statistics and  -----
#         elements of visual analysis 
#############################

# Read dataset ----
mydata <- read.csv("C:/Users/ochernova/Downloads/HeightSurvey2024.csv", header = TRUE, sep = ";")

head(mydata)
str(mydata)
summary(mydata)

# Create vector with Female heights
height_f = mydata[mydata$Gender=="F", "Height.in.cm"]
print(height_f)

# Create vector with Male heights
height_m = mydata[mydata$Gender=="M", "Height.in.cm"]
print(height_m)

# Create vector with Female Shoe size
shoe_f = mydata[mydata$Gender=="F", "Shoe.size"]
print(shoe_f)

# Create vector with Male Shoe size
shoe_m = mydata[mydata$Gender=="M", "Shoe.size"]
print(shoe_m)

## Calculate descriptive statistics for female and male heights ----
# Means
mean_f <- mean(height_f)
mean_m <- mean(height_m)

# Medians
median_f <- median(height_f)
median_m <- median(height_m)

# Variances (corrected for bias - same as ddof=1 in Python)
variance_f <- var(height_f)
variance_m <- var(height_m)

# Standard deviations (corrected for bias)
std_dev_f <- sd(height_f)
std_dev_m <- sd(height_m)

# IQR (Interquartile Range)
iqr_f <- IQR(height_f)  # Uses IQR function from R's 'stats' package
iqr_m <- IQR(height_m)

# Range
range_f <- max(height_f) - min(height_f)
range_m <- max(height_m) - min(height_m)

# Print Female Statistics
cat("Female Statistics:\n")
cat("  Mean:", mean_f, "\n")
cat("  Median:", median_f, "\n")
cat("  Variance:", variance_f, "\n")
cat("  Standard Deviation:", std_dev_f, "\n")
cat("  IQR:", iqr_f, "\n")
cat("  Range:", range_f, "\n\n")  # Add two newlines for separation

# Print Male Statistics
cat("Male Statistics:\n")
cat("  Mean:", mean_m, "\n")
cat("  Median:", median_m, "\n")
cat("  Variance:", variance_m, "\n")
cat("  Standard Deviation:", std_dev_m, "\n")
cat("  IQR:", iqr_m, "\n")
cat("  Range:", range_m, "\n")

### Use function
# Function to calculate descriptive statistics
descr_stats <- function(data) {
  cat("  Mean:", mean(data), "\n")
  cat("  Median:", median(data), "\n")
  cat("  Variance:", var(data), "\n")  # No ddof argument needed in R's var function
  cat("  Standard Deviation:", sd(data), "\n")  # No ddof argument needed in R's sd function
  cat("  IQR:", IQR(data), "\n")  # Uses IQR function from 'stats' package
  cat("  Range:", max(data) - min(data), "\n")
}

# Print Female Statistics
cat("Female Statistics:\n")
descr_stats(height_f)

# Print Male Statistics
cat("\nMale Statistics:\n")
descr_stats(height_m)

# Shoe size frequency
table(as.factor(shoe_f))
prop.table(table(shoe_f))*100

table(shoe_m)
prop.table(table(shoe_m))*100

########## Histograms ----
# Create histogram for Females Height
hist(height_f, 
     breaks = 8,
     main = "Histogram of Height F",
     xlab="Height (in cm)",
     col = "blue")

# Create histogram for Males Height
hist(height_m, 
     breaks = 5,
     main = "Histogram of Height M",
     xlab="Height (in cm)",
     col = "yellow")

# Create histogram (bar plot) Female Shoe size
barplot(table(shoe_f), col = "blue", xlab = "Shoe size")

# Create histogram (bar plot) Male Shoe size
barplot(table(shoe_m), col = "yellow", xlab = "Shoe size")

boxplot(mydata$Height.in.cm ~ mydata$Gender, 
        horizontal = F, 
        ylab = "Height", 
        xlab = "Gender",
        col = c("orange", "yellow"))

## Part 2. Confidence intervals and hypothesis testing ----

# Confidence intervals for proportion
cip <- function(n1, n, alpha){
  p <- n1/n
  print(p)
  a <- c(p - qnorm(1- alpha/2) *sqrt(p*(1-p)/n), p + qnorm(1- alpha/2) *sqrt(p*(1-p)/n) )
  return(a)
}

n <- length(mydata[,1])

# 95% CI for proportion of F
cip(length(mydata[mydata$Gender=="F",1]), n, alpha = 0.05)
# 95% CI for proportion of M
cip(length(mydata[mydata$Gender=="F",1]), n, alpha = 0.01)

# Confidence intervals for mean
cimu <- function(xbar, s, n, alpha){
  print(xbar)
  L <- xbar - qt(1- alpha/2, df=n-1)* s/sqrt(n)
  U <- xbar + qt(1- alpha/2, df=n-1)* s/sqrt(n)
  a <-  c(L, U)
  return(a)
}

# 95% CI for mean height of F
cimu(xbar = mean(mydata[mydata$Gender=="F",1]), 
     s=sd(mydata[mydata$Gender=="F",1]), 
     n = length(mydata[mydata$Gender=="F",1]), 
     alpha = 0.05)

# 95% CI for mean height of M
cimu(xbar = mean(mydata[mydata$Gender=="M",1]), 
     s=sd(mydata[mydata$Gender=="M",1]), 
     n = length(mydata[mydata$Gender=="M",1]), 
     alpha = 0.05)

# Perform one-sample t-test for mean ---

# H0: mu=164 and alternative hypothesis "greater"
t.test(mydata[mydata$Gender=="F",1], mu=164, alternative="greater")

# H0: mu=175 and alternative hypothesis "greater"
t.test(mydata[mydata$Gender=="M",1], mu=175, alternative="greater")

# Bonus ----
# violin plot
# install.packages("ggplot2") # if needed
library(ggplot2)
p <- ggplot(mydata, aes(x= Gender, y=Height.in.cm, fill=Gender)) + 
  geom_violin() + geom_boxplot(width=.1)
p
