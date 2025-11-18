### TP1 ###
### https://iqss.github.io/dss-workshops/R/Rintro/base-r-cheat-sheet.pdf

#### EX1 ####

## Create a vector of numbers 1 to 20 : v1 = 1:20
v1 <- 1:20

## Create a vector v2 with same dimension with random values between 1 and 20, using c()
v2 <- c(sample(1:20,20,replace = TRUE))
## Find the elements, and their indices, greater than 10 in both vectors
## Replace all values lower than 5 with 0 in both vectors
v1[v1 < 10] <- 0
v2[v2 < 10] <- 0

## Compute the sum, the mean, the median, the standard deviation of v1.
v1_mean <- mean(v1)
v1_med <- median(v1)
v1_sd <- sd(v1)

## Compute the two products: the dot product and the Hadamard (element-wise) product of v1 and v2 Note : Just use operators here, no functions
dot_product_v1v2 <- v1 %*% v2
hadamard_product_v1v2 <- v1 * v2

## Create a 5x5 matrix M1, filled by row with the sequence from 1 to 25
M1 <- matrix(1:25,5,5, byrow = TRUE)

## Create a 5x5 matrix M2, filled by column, with random values between 1 and 20
M2 <- matrix(sample(1:20,25,replace = TRUE), 5,5)

## Explain the result of the command matrix(c(1,2,3),2,4) (including the warning).
# print(matrix(c(1,2,3),2,4))
## The command tries to create a 2x4 matrix with only 3 elements (1,2,3).
## Since there are not enough elements to fill the matrix, R recycles the elements of the vector c(1,2,3) until the matrix is filled.

## Access element in row 2, column 3
# print(M1)
# print(M1[2,3]) #M[row,col]

## Get entire second row
# print(M1[2,])

## Get entire third column
# print(M1[,3])

## Compute the two products: the matrix product and the Hadamard (element-wise) product of M1 and M2.
dot_product_m1m2 <- M1 %*% M2
hadamard_product_m1m2 <- M1 * M2

# Compute the trace of the transpose of M1.
trace_transpose_M1 <- sum(diag(t(M1)))

## Compute the row and column means of M2
row_means_M2 <- rowMeans(M2)
col_means_M2 <- colMeans(M2)

## Using apply(), find the minimum value of each row of M2. Do the same for each column of M1.
min_row_M2 <- apply(M2, 1, min)
min_col_M1 <- apply(M1, 2, min)

#### EX2 ###

##  Create a data frame df with 3 columns: id (from 1 to 100), age (random values between 18 and 30) and height (random values between 150 and 200). Display the first rows of the data frame
df <- data.frame(
  AGE = sample(18:30, 100, replace = TRUE ),
  HEIGHT = sample(150:200, 100, replace = TRUE)
)

print(head(df))

## Add a new column weight (random values between 50 and 100) to the data frame. Display the first rows of the data frame
df <- cbind(df, WEIGHT = sample(50:100, 100, replace = TRUE))

print(head(df))

## Compute the mean, median, standard deviation of all columns.
age_mean <- mean(df$AGE)
age_med <- median(df$AGE)
age_sd <- sd(df$AGE)

height_mean <- mean(df$HEIGHT)
height_med <- median(df$HEIGHT)
height_sd <- sd(df$HEIGHT)

weight_mean <- mean(df$WEIGHT)
weight_med <- median(df$WEIGHT)
weight_sd <- sd(df$WEIGHT)

## Display the scatterplot of height vs weight, coloring the points according to BMI (use col parameter of plot())
bmi <- df$WEIGHT / ( (df$HEIGHT/100) ^ 2)
plot(df$HEIGHT, df$WEIGHT, col = bmi, xlab = "Height (cm)", ylab = "Weight (kg)", main = "Height vs Weight colored by BMI")

## Save the data frame in a CSV file using write.csv()
write.csv(df, 'df.csv')

## Generate the dataset 𝑍 = 𝑋 ∪ 𝑌 as follows. 𝑋 is a set of 100 two-dimensional observations following a normal distribution with parameters 𝜇𝑥 = [1, 4] and 𝜎𝑥 = [2, 0.4]. 𝑌 is a set of 100 two-dimensional observations following a normal distribution with parameters 𝜇𝑦 = [3, 1] and 𝜎𝑦 = [0.7, 1.2]. Display these data points in a scatterplot, coloring the observations from 𝑋 in red and those from 𝑌 in green
set.seed(123) # For reproducibility
n <- 100
mu_x <- c(1, 4)
sigma_x <- matrix(c(2, 0.4, 0.4, 2), nrow = 2)
X <- MASS::mvrnorm(n, mu_x, sigma_x)
mu_y <- c(3, 1)
sigma_y <- matrix(c(0.7, 0, 0, 1.2), nrow = 2)
Y <- MASS::mvrnorm(n, mu_y, sigma_y)
Z <- rbind(X, Y)
plot(X[,1], X[,2], col = 'red', xlim = range(Z[,1]), ylim = range(Z[,2]), xlab = "X1", ylab = "X2", main = "Scatterplot of Z = X ∪ Y")
points(Y[,1], Y[,2], col = 'green')
legend("topright", legend = c("X", "Y"), col = c("red", "green"), pch = 1)

