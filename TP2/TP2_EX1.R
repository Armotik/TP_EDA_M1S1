## Perform PCA « manually » (without using the prcomp function) on the iris dataset (available by default in R). You may use the eigen function to compute the eigenvalues and eigenvectors of a matrix.

X <- iris[, 1:4]
apply(X, 2, sd)

# Centrer et réduire les données
Z <- scale(X)

# Calculer la matrice de covariance
cov_matrix <- cov(Z)

# Calculer les valeurs propres et les vecteurs propres
eigen_decomp <- eigen(cov_matrix)
eigen_values <- eigen_decomp$values
eigen_vectors <- eigen_decomp$vectors

## What do the eigenvalues and eigenvectors represent in the context of PCA?
# Les valeurs propres représentent la variance expliquée par chaque composante principale, tandis que les vecteurs propres représentent les directions des nouvelles composantes principales dans l'espace des données.

# Compute the explained variance
explained_variance <- eigen_values / sum(eigen_values)

# Project the individuals onto the first factorial plane, colored by species
projected_data <- Z %*% eigen_vectors[, 1:2]
plot(projected_data, col = iris$Species, pch = 19, xlab = "PC1", ylab = "PC2", main = "PCA on Iris Dataset")
legend("topright", legend = levels(iris$Species), col = 1:3, pch = 19)