# Set your working directory
# setwd("<YOUR_DIR>")
# Exercise 0

# Install R:
# https://cran.r-project.org/

# Install R studio:
# https://www.rstudio.com/products/rstudio/download/

# - Assigning values to variables
x <- c(1, 2, 3) # Use either <- or =, but stick with either one.
rm(list = ls()) # Removing variables, brush on the upper right corner
# - How to set working directory: ctrl + shift + H or from panel
getwd()
# - Console and scripts:
#    -- Run line by line: ctrl + enter
#    -- Run whole script: ctrl + shift + enter
# - Search help:
?c
?matrix

?"%*%"

# Exercise 1.1

# a) Histograms
# Import data
emis <- read.table("text_data/emissions.txt", header = TRUE, sep = "\t",
                   row.names = 1)
?read.table
# See first/last rows of the table
head(emis)
tail(emis)

# Other useful functions for getting first impressions.
View(emis)
summary(emis)
class(emis)
str(emis)


# Read one element
emis[2, 2]
# Read second row
emis[2, ]
# Read first column.
emis[, 1] # or
emis$NOx # or
emis[, "NOx"]

# Histograms
?hist
hist(emis$NOx)

par(mfrow = c(2, 2))
hist(emis$NOx, main = "NOx", xlab = "kg/cm^2", col = "green")
hist(emis$Humidity, main = "Humidity", xlab = "Unit", col = "green")
hist(emis$Temp, main = "Temperature", xlab = "Unit", col = "green")
hist(emis$Pressure, main = "Pressure", xlab = "Unit", col = "green")
par(mfrow = c(1, 1))

# Changing the number of bins
?seq
bvect <- seq(min(emis$NOx), max(emis$NOx), length.out = 10)
hist(emis$NOx, breaks = bvect)

# b) Linear regression model
?lm
# lm = make linear regression model
# lm(response var ~ explatonary var)
fit <- lm(NOx ~ Humidity + Temp + Pressure, data = emis)

# Equivalent options
#fit <- lm(emis$NOx ~ emis$Humidity + emis$Temp + emis$Pressure, data = emis)
#fit <- lm(NOx ~ ., data = emis)

# c) R squared
summary(fit)
names(summary(fit))

rsq <- summary(fit)$r.squared

# Or calculate "by hand"
sst <- sum((emis$NOx - mean(emis$NOx))^2)
sse <- sum(fit$res^2)
rsq2 <- 1 - sse / sst

rsq
rsq2

# Summary(fit):
# 1st col (Estimate): Estimates for regression coefficients
# 2nd col (Std.Error): Standard deviation of regression coefficients.
# 3rd col (t value): Test statistics for t-test
# H0: Beta_j == 0
# H1: Beta_j != 0
# 4th col Pr(>|t|): p-values for t-test.
# Small p-value => H0 is rejected => B_j is significant
# F-test:
# H0: B_j == 0 for all j = 1,2,...,k
# H1: B_j != 0 for some j = 1,2,...,k

# d) Permutation test
k <- 2000
alpha <- 0.05
# Number of variables and number of observations.
m <- ncol(emis)
n <- nrow(emis)

# H0: Beta_j = 0
# H1: Beta_j != 0
# Conduct significance test for every j = 0,1,2,3

# Create matrix for collecting values of R^2 of permutated samples.
rmatrix <- matrix(NA, nrow = k, ncol = m - 1)

for (i in 1:k) {
  for (j in 2:m) {
    # Permutate explatonary variable corresponding to B_(j-1).
    tmp <- emis
    tmp[, j] <- sample(tmp[, j], n, replace = FALSE)

    # Calculate R^2.
    fit_tmp <- lm(NOx ~ ., data = tmp)
    rmatrix[i, (j - 1)] <- summary(fit_tmp)$r.squared
  }
}

# Notice that we can sum boolean values
TRUE + FALSE + TRUE

# P-values: P("R^2_perm >= R^2")
hum <- sum(rmatrix[, 1] >= rsq) / k
temp <- sum(rmatrix[, 2] >= rsq) / k
pre <- sum(rmatrix[, 3] >= rsq) / k

hum
temp
pre

hum > alpha # H0 rejected
temp > alpha # H0 rejected
pre > alpha # No evidence against H0

# Or compare (1-alpha) percentile and rsq
rsq > quantile(rmatrix[, 1], 1 - alpha)  # H0 rejected
rsq > quantile(rmatrix[, 2], 1 - alpha)  # H0 rejected
rsq > quantile(rmatrix[, 3], 1 - alpha) # No evidence against H0

# All in all, pressure is the only insignificant variable

# e) Linear regression model without variable pressure
fit2 <- lm(NOx ~ Humidity + Temp, data = emis)
summary(fit2)

# f) Standard deviations of b_i
# Construct matrix "X".
colnames(emis)
tmp <- as.matrix(emis[, c(2, 3)])
xmatrix <- cbind(rep(1, n), tmp)
head(xmatrix)

# Calculate covariance matrix of b.
l <- ncol(xmatrix)
s2 <- sum(fit2$res^2) / (n - l)
covb <- s2 * solve(t(xmatrix) %*% xmatrix)
stdev <- sqrt(diag(covb))
stdev

# g) Confidence intervals assuming normality of residuals
confint(fit2, level = 0.95)

# h) Confidence intervals calculated manually
coef <- fit2$coef
up <- coef + stdev * qt(1 - alpha / 2, n - l)
down <- coef - stdev * qt(1 - alpha / 2, n - l)
down
up

# i) Bootstrap confidence interval
k <- 2000
alpha <- 0.05
# Number of variables and number of observations.
m <- ncol(xmatrix)
n <- nrow(xmatrix)

# Create matrix for collecting values of b.
bmatrix <- matrix(NA, nrow = k, ncol = m)
y <- emis$NOx

for (i in 1:(k - 1)) {
  # take random sample of size n (n size of the original sample).
  ind <- sample(1:n, n, replace = TRUE)
  xtmp <- xmatrix[ind, ]
  ytmp <- y[ind]

  # Calculate bootstrap estimates
  btmp <- solve(t(xtmp) %*% xtmp) %*% t(xtmp) %*% ytmp
  bmatrix[i, ] <- t(btmp)
}

# Include original estimate
b <- solve(t(xmatrix) %*% xmatrix) %*% t(xmatrix) %*% y
bmatrix[k, ] <- t(b)

# Calculate confidence interval
qconst <- quantile(bmatrix[, 1], probs = c(0.025, 0.975))
qhum <- quantile(bmatrix[, 2], probs = c(0.025, 0.975))
qtemp <- quantile(bmatrix[, 3], probs = c(0.025, 0.975))

qconst
qhum
qtemp

# Plot histograms
par(mfrow = c(2, 2))
hist(bmatrix[, 1], main = "B_0", col = "dodgerblue")
abline(v = qconst, lwd = 2, lty = "dashed", main = "B_0")

hist(bmatrix[, 2], main = "B_1", col = "dodgerblue")
abline(v = qhum, lwd = 2, lty = "dashed", main = "B_0")

hist(bmatrix[, 3], main = "B_2", col = "dodgerblue")
abline(v = qtemp, lwd = 2, lty = "dashed", main = "B_0")
par(mfrow = c(1, 1))
