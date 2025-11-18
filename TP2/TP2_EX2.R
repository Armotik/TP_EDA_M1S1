X <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data"))

colnames(X) <- c("Id", "RI", "Na", "Mg", "Al", "Si", "K", "Ca", "Ba", "Fe", "Type")
X <- X[, -1]  # Remove the Id column

for (i in 1 : ncol(X)) {
  boxplot(X[, i], main = colnames(X)[i])
}

variance <- apply(X[, -ncol(X)], 2, var)
ecart_type <- apply(X[, -ncol(X)], 2, sd)

print("variance")
sort(variance, decreasing = TRUE)

print("ecart_type")
sort(ecart_type, decreasing = TRUE)

cv <- ecart_type / colMeans(X[, -ncol(X)])
print("coefficient de variation")
sort(cv, decreasing = TRUE)

# Matrice de covariance
## 1. Centrer les données
Y <- scale(X[, -ncol(X)], center = TRUE, scale = FALSE)

## 2. Calcul de la matrice de covariance
n <- nrow(Y)
V <- (t(Y) %*% Y) / (n - 1)

# Matrice de correlation
R <- V/(ecart_type %o% ecart_type)

print("Matrice de covariance")
print(V)
### RI : variance très faible (~9×10⁻⁶) car mesures proches de 1.5
### Mg, Ca : variances élevées (~2.07, ~2.03) → grande dispersion
### Ca-Mg : -0.913 (négative forte) → quand Ca ↑, Mg ↓
### Ca-RI : +0.0035 (positive)

print("Matrice de correlation")
print(R)
## Corrélations fortes positives (> 0.5) :
### RI-Ca : 0.81 → très forte relation linéaire positive

## Corrélations fortes négatives (< -0.4) :
### RI-Si : -0.54 → relation inverse
### Ca-Mg : -0.44 → quand Ca augmente, Mg diminue
### Ba-Mg : -0.49 → relation inverse modérée
### Mg-Al : -0.48 → relation inverse

## Corrélations moyennes :
### Ba-Al : 0.48 (positive)
### Al-K : 0.32 (positive faible)

## Corrélations faibles (proche de 0) :
### Mg-K : 0.009 → variables quasi indépendantes
### Ba-Fe, K-Fe → pas de relation linéaire

# ---------------------------------------------

# ACP
pX <- prcomp(X[, -ncol(X)], center = TRUE, scale. = TRUE)

rotation_matrix <- pX$rotation
produit <- t(rotation_matrix) %*% rotation_matrix

print("Matrice identité ?")
print(all.equal(produit, diag(ncol(rotation_matrix))))

scores <- pX$x
produit_scores <- t(scores) %*% scores / (nrow(scores) - 1)

# Visualisation du % de variance expliquée
variance_explained <- pX$sdev^2 / sum(pX$sdev^2)
barplot(variance_explained,
        names.arg = paste0("PC", 1:9),
        main = "Variance expliquée par chaque composante",
        ylab = "Proportion",
        col = "steelblue")

# Critère du coude
plot(variance_explained, type = "b",
     xlab = "Composante", ylab = "Variance",
     main = "Scree plot")
abline(h = 1/9, col = "red", lty = 2)  # Seuil théorique

# Projection
Z <- scale(X[, -ncol(X)], center = TRUE, scale = TRUE)
projections <- Z %*% rotation_matrix

# Visualisation
plot(pX$x[, 1], pX$x[, 2],
     xlab = paste0("PC1 (", round(variance_explained[1] * 100, 1), "%)"),
     ylab = paste0("PC2 (", round(variance_explained[2] * 100, 1), "%)"),
     main = "Premier plan factoriel",
     pch = 19, col = "steelblue")

text(pX$x[, 1], pX$x[, 2], labels = 1:nrow(X), pos = 3, cex = 0.6)

# 4. Identifier les individus atypiques (distances au centre > 3 écarts-types)
distances_PC1 <- abs(pX$x[, 1])
distances_PC2 <- abs(pX$x[, 2])

outliers_PC1 <- which(distances_PC1 > 3)
outliers_PC2 <- which(distances_PC2 > 3)

print("\nIndividus atypiques sur PC1 (>3 SD):")
print(outliers_PC1)

print("\nIndividus atypiques sur PC2 (>3 SD):")
print(outliers_PC2)

# Marquer les individus atypiques en rouge
points(pX$x[outliers_PC1, 1], pX$x[outliers_PC1, 2],
       col = "red", pch = 19, cex = 1.5)
points(pX$x[outliers_PC2, 1], pX$x[outliers_PC2, 2],
       col = "orange", pch = 19, cex = 1.5)

type_colors <- rainbow(length(unique(X$Type)))[as.factor(X$Type)]

plot(pX$x[, 1], pX$x[, 2],
     xlab = paste0("PC1 (", round(variance_explained[1] * 100, 1), "%)"),
     ylab = paste0("PC2 (", round(variance_explained[2] * 100, 1), "%)"),
     main = "Premier plan factoriel (coloré par Type)",
     pch = 19, col = type_colors, cex = 1.2)

# Légende
legend("topright", legend = unique(X$Type),
       col = rainbow(length(unique(X$Type))),
       pch = 19, title = "Type de verre", cex = 0.8)

# Ajouter les individus les plus extrêmes
extreme_indices <- c(
  which.max(pX$x[, 1]),  # Plus à droite sur PC1
  which.min(pX$x[, 1]),  # Plus à gauche sur PC1
  which.max(pX$x[, 2]),  # Plus haut sur PC2
  which.min(pX$x[, 2])   # Plus bas sur PC2
)

text(pX$x[extreme_indices, 1], pX$x[extreme_indices, 2],
     labels = extreme_indices, pos = 3, col = "black", font = 2)