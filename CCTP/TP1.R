# ============================================================================
# TP1 - Analyse Exploratoire des Données
# ============================================================================

# =========================
# PROBLEM 1 : Vecteurs et Matrices
# =========================

# --- VECTEURS ---

# Créer un vecteur de 1 à 20
v1 <- 1:20
print("v1 :"); print(v1)

# Créer un vecteur v2 de même dimension avec des valeurs aléatoires entre 1 et 20
# sample() tire des valeurs aléatoires, replace=TRUE permet les répétitions
v2 <- c(sample(1:20, 20, replace = TRUE))
print("v2 :"); print(v2)

# Trouver les éléments > 10 et leurs indices dans les deux vecteurs
# which() retourne les indices des éléments satisfaisant la condition
elements_v1_gt10 <- v1[v1 > 10]
indices_v1_gt10 <- which(v1 > 10)
print("Éléments de v1 > 10 :"); print(elements_v1_gt10)
print("Indices de v1 > 10 :"); print(indices_v1_gt10)

elements_v2_gt10 <- v2[v2 > 10]
indices_v2_gt10 <- which(v2 > 10)
print("Éléments de v2 > 10 :"); print(elements_v2_gt10)
print("Indices de v2 > 10 :"); print(indices_v2_gt10)

# Remplacer toutes les valeurs < 5 par 0
# On utilise l'indexation logique pour modifier les valeurs
v1[v1 < 5] <- 0
v2[v2 < 5] <- 0
print("v1 après remplacement (< 5 -> 0) :"); print(v1)
print("v2 après remplacement (< 5 -> 0) :"); print(v2)

# Recréer v1 pour les calculs suivants (car on l'a modifié)
v1 <- 1:20
v2 <- c(sample(1:20, 20, replace = TRUE))

# Calculer somme, moyenne, médiane, écart-type de v1
somme_v1 <- sum(v1)
moyenne_v1 <- mean(v1)
mediane_v1 <- median(v1)
ecart_type_v1 <- sd(v1)

print(paste("Somme de v1 :", somme_v1))
print(paste("Moyenne de v1 :", moyenne_v1))
print(paste("Médiane de v1 :", mediane_v1))
print(paste("Écart-type de v1 :", ecart_type_v1))

# Produit scalaire (dot product) : somme des produits élément par élément
# En R : sum(v1 * v2) ou v1 %*% v2 (produit matriciel qui donne un scalaire pour vecteurs)
produit_scalaire <- sum(v1 * v2)
print(paste("Produit scalaire v1 . v2 :", produit_scalaire))

# Produit de Hadamard (élément par élément) : simplement *
produit_hadamard <- v1 * v2
print("Produit de Hadamard v1 * v2 :"); print(produit_hadamard)


# --- MATRICES ---

# Créer une matrice 5x5 M1, remplie par ligne avec les valeurs 1 à 25
# byrow = TRUE pour remplir par ligne (par défaut c'est par colonne)
M1 <- matrix(1:25, nrow = 5, ncol = 5, byrow = TRUE)
print("Matrice M1 (remplie par ligne) :"); print(M1)

# Créer une matrice 5x5 M2, remplie par colonne, avec des valeurs aléatoires entre 1 et 20
M2 <- matrix(sample(1:20, 25, replace = TRUE), nrow = 5, ncol = 5, byrow = FALSE)
print("Matrice M2 (remplie par colonne, valeurs aléatoires) :"); print(M2)

# Explication de matrix(c(1,2,3), 2, 4)
# On essaie de créer une matrice 2x4 (8 éléments) avec seulement 3 valeurs
# R recycle les valeurs : 1,2,3,1,2,3,1,2 mais comme 8 n'est pas un multiple de 3
# on obtient un warning "data length is not a sub-multiple..."
print("Résultat de matrix(c(1,2,3), 2, 4) :")
print(matrix(c(1,2,3), 2, 4))
# Le warning indique que le vecteur a été recyclé de manière incomplète

# Accéder à l'élément ligne 2, colonne 3
element_2_3 <- M1[2, 3]
print(paste("Élément M1[2,3] :", element_2_3))

# Obtenir la 2ème ligne entière
ligne_2 <- M1[2, ]
print("2ème ligne de M1 :"); print(ligne_2)

# Obtenir la 3ème colonne entière
colonne_3 <- M1[, 3]
print("3ème colonne de M1 :"); print(colonne_3)

# Produit matriciel : %*%
produit_matriciel <- M1 %*% M2
print("Produit matriciel M1 %*% M2 :"); print(produit_matriciel)

# Produit de Hadamard (élément par élément) : *
produit_hadamard_mat <- M1 * M2
print("Produit de Hadamard M1 * M2 :"); print(produit_hadamard_mat)

# Trace de la transposée de M1
# La trace = somme des éléments diagonaux
# t() pour transposer, diag() pour extraire la diagonale
trace_M1_t <- sum(diag(t(M1)))
print(paste("Trace de t(M1) :", trace_M1_t))
# Note : trace(M) = trace(t(M)) toujours

# Moyennes par ligne et par colonne de M2
moyennes_lignes_M2 <- rowMeans(M2)
moyennes_colonnes_M2 <- colMeans(M2)
print("Moyennes par ligne de M2 :"); print(moyennes_lignes_M2)
print("Moyennes par colonne de M2 :"); print(moyennes_colonnes_M2)

# Utiliser apply() pour trouver le minimum de chaque ligne de M2
# apply(X, MARGIN, FUN) : MARGIN=1 pour lignes, MARGIN=2 pour colonnes
min_par_ligne_M2 <- apply(M2, 1, min)
print("Minimum par ligne de M2 :"); print(min_par_ligne_M2)

# Minimum de chaque colonne de M1
min_par_colonne_M1 <- apply(M1, 2, min)
print("Minimum par colonne de M1 :"); print(min_par_colonne_M1)


# =========================
# PROBLEM 2 : Données (Data Frames)
# =========================

# Fixer la graine pour reproductibilité (optionnel)
set.seed(42)

# Créer un data frame avec id, age, height
df <- data.frame(
  id = 1:100,
  age = sample(18:30, 100, replace = TRUE),
  height = sample(150:200, 100, replace = TRUE)
)
print("Premières lignes du data frame :"); print(head(df))

# Ajouter la colonne weight
df$weight <- sample(50:100, 100, replace = TRUE)
print("Après ajout de weight :"); print(head(df))

# Ajouter la colonne BMI (Body Mass Index)
# BMI = poids (kg) / taille² (m²)
# Attention : height est en cm, donc on divise par 100 pour avoir des mètres
df$BMI <- df$weight / (df$height / 100)^2
print("Après ajout de BMI :"); print(head(df))

# Calculer moyenne, médiane, écart-type de toutes les colonnes
print("=== Statistiques descriptives ===")
for (col in names(df)) {
  print(paste("--- Colonne:", col, "---"))
  print(paste("  Moyenne:", round(mean(df[[col]]), 2)))
  print(paste("  Médiane:", median(df[[col]])))
  print(paste("  Écart-type:", round(sd(df[[col]]), 2)))
}

# Alternative plus élégante avec summary()
print("Summary du data frame :"); print(summary(df))

# Scatterplot height vs weight, coloré selon BMI
# On crée une palette de couleurs basée sur le BMI
# heat.colors() ou rainbow() peuvent être utilisés
couleurs_bmi <- heat.colors(100)[cut(df$BMI, 100)]
plot(df$height, df$weight,
     col = couleurs_bmi,
     pch = 19,  # points pleins
     xlab = "Taille (cm)",
     ylab = "Poids (kg)",
     main = "Scatterplot Taille vs Poids (couleur = BMI)")

# Sauvegarder le data frame en CSV
write.csv(df, "dataframe_tp1.csv", row.names = FALSE)
print("Data frame sauvegardé dans 'dataframe_tp1.csv'")


# --- Générer le dataset Z = X ∪ Y ---

# X : 100 observations 2D, normale avec μx = [1, 4] et σx = [2, 0.4]
# Y : 100 observations 2D, normale avec μy = [3, 1] et σy = [0.7, 1.2]

# rnorm(n, mean, sd) génère n valeurs selon une loi normale
X <- cbind(
  rnorm(100, mean = 1, sd = 2),      # 1ère dimension
  rnorm(100, mean = 4, sd = 0.4)     # 2ème dimension
)

Y <- cbind(
  rnorm(100, mean = 3, sd = 0.7),    # 1ère dimension
  rnorm(100, mean = 1, sd = 1.2)     # 2ème dimension
)

# Union Z = X ∪ Y (rbind pour empiler les lignes)
Z <- rbind(X, Y)
print(paste("Dimensions de Z :", nrow(Z), "x", ncol(Z)))

# Créer un vecteur de labels pour les couleurs
labels <- c(rep("X", 100), rep("Y", 100))

# Scatterplot avec X en rouge et Y en vert
plot(Z[, 1], Z[, 2],
     col = ifelse(labels == "X", "red", "green"),
     pch = 19,
     xlab = "Dimension 1",
     ylab = "Dimension 2",
     main = "Dataset Z = X (rouge) ∪ Y (vert)")
legend("topright", legend = c("X", "Y"), col = c("red", "green"), pch = 19)


# =========================
# PROBLEM 3 : Distances
# =========================

# Prendre deux lignes aléatoires de Z
set.seed(123)
indices_aleatoires <- sample(1:nrow(Z), 2)
point1 <- Z[indices_aleatoires[1], ]
point2 <- Z[indices_aleatoires[2], ]
print(paste("Point 1 (indice", indices_aleatoires[1], "):"))
print(point1)
print(paste("Point 2 (indice", indices_aleatoires[2], "):"))
print(point2)

# Distance euclidienne : sqrt(sum((x - y)^2))
dist_euclidienne <- sqrt(sum((point1 - point2)^2))
print(paste("Distance euclidienne :", round(dist_euclidienne, 4)))

# Distance de Manhattan : sum(|x - y|)
dist_manhattan <- sum(abs(point1 - point2))
print(paste("Distance de Manhattan :", round(dist_manhattan, 4)))


# Calculer la distance euclidienne par paire entre tous les points de Z
# dist() retourne un objet de type "dist" (triangulaire inférieur)
dist_Z <- dist(Z, method = "euclidean")
print(paste("Classe de dist_Z :", class(dist_Z)))
print(paste("Longueur de dist_Z :", length(dist_Z)))
# La dimension est n*(n-1)/2 = 200*199/2 = 19900 (paires uniques)

# Convertir en matrice
dist_matrix <- as.matrix(dist_Z)
print(paste("Dimensions de la matrice de distances :", nrow(dist_matrix), "x", ncol(dist_matrix)))

# Heatmap de la matrice de distances
# La diagonale est 0 (distance d'un point à lui-même)
# Les valeurs proches de 0 = points proches, valeurs élevées = points éloignés
heatmap(dist_matrix,
        main = "Heatmap des distances euclidiennes",
        symm = TRUE)  # matrice symétrique

# Trouver les deux points les plus proches
# On met la diagonale à Inf pour ne pas la considérer
dist_matrix_no_diag <- dist_matrix
diag(dist_matrix_no_diag) <- Inf

# Points les plus proches
min_dist <- min(dist_matrix_no_diag)
indices_min <- which(dist_matrix_no_diag == min_dist, arr.ind = TRUE)[1, ]
print(paste("Distance minimale :", round(min_dist, 4)))
print(paste("Entre les points", indices_min[1], "et", indices_min[2]))

# Points les plus éloignés
max_dist <- max(dist_matrix)
indices_max <- which(dist_matrix == max_dist, arr.ind = TRUE)[1, ]
print(paste("Distance maximale :", round(max_dist, 4)))
print(paste("Entre les points", indices_max[1], "et", indices_max[2]))


# --- Fonction distance de Mahalanobis ---
# d_M(x, y) = sqrt((x - y)^T * M * (x - y))
distanceMat <- function(x, y, M) {
  diff <- x - y
  # t() pour transposer, %*% pour produit matriciel
  result <- sqrt(t(diff) %*% M %*% diff)
  return(as.numeric(result))
}

# Vérification avec M = I (identité) -> doit donner la distance euclidienne
M_identite <- diag(2)  # matrice identité 2x2
dist_mahal_I <- distanceMat(point1, point2, M_identite)
print(paste("Distance Mahalanobis avec M=I :", round(dist_mahal_I, 4)))
print(paste("Distance euclidienne (vérification) :", round(dist_euclidienne, 4)))
# Les deux valeurs doivent être égales !


# --- Fonction inertie ---
# Inertie = somme des distances² au barycentre (ou somme des distances² entre paires / n)
# Ici on utilise la définition avec la distance de Mahalanobis
inertie <- function(X, M) {
  n <- nrow(X)
  # Calculer le barycentre (centre de gravité)
  barycentre <- colMeans(X)

  # Somme des distances² au barycentre
  total_inertie <- 0
  for (i in 1:n) {
    d <- distanceMat(X[i, ], barycentre, M)
    total_inertie <- total_inertie + d^2
  }
  return(total_inertie / n)  # inertie moyenne
}

# Inertie avec distance euclidienne (M = I)
inertie_eucl <- inertie(Z, diag(2))
print(paste("Inertie (distance euclidienne, M=I) :", round(inertie_eucl, 4)))

# Inertie avec M = σ^(-2) = diag(1/σ²)
# On doit estimer les écarts-types de chaque dimension
sigma_Z <- apply(Z, 2, sd)
M_sigma <- diag(1 / sigma_Z^2)
print("Matrice M = diag(1/σ²) :"); print(M_sigma)

inertie_mahal <- inertie(Z, M_sigma)
print(paste("Inertie (distance Mahalanobis, M=σ^(-2)) :", round(inertie_mahal, 4)))

# Commentaire : L'inertie avec M=σ^(-2) normalise par la variance de chaque dimension
# Cela donne une mesure plus équilibrée quand les dimensions ont des échelles différentes
# L'inertie normalisée est généralement proche de 2 (nombre de dimensions) si les données
# sont bien centrées-réduites


# =========================
# PROBLEM 4 : ACP (PCA)
# =========================

# Effectuer une ACP avec prcomp()
# scale. = TRUE pour standardiser les données (centrer et réduire)
# center = TRUE pour centrer (soustrait la moyenne)
pca_result <- prcomp(Z, center = TRUE, scale. = TRUE)

# Afficher le résumé
print("=== Résumé de l'ACP ===")
print(summary(pca_result))

# Le summary montre :
# - Standard deviation : écart-type de chaque composante principale
# - Proportion of Variance : % de variance expliquée par chaque CP
# - Cumulative Proportion : % cumulé

# Pour expliquer 90% de la variance, on regarde Cumulative Proportion
# Avec 2 dimensions, la 1ère CP explique ~X% et les 2 ensemble expliquent 100%
# Généralement 1 ou 2 CPs suffisent pour >90%

# Nombre de CPs nécessaires pour 90% de variance
variance_cumulee <- summary(pca_result)$importance[3, ]  # 3ème ligne = cumulative
n_cp_90 <- which(variance_cumulee >= 0.90)[1]
print(paste("Nombre de CPs pour 90% de variance :", n_cp_90))


# --- Plot des individus dans l'espace des 2 premières CPs ---
# Les scores sont dans pca_result$x
scores <- pca_result$x

plot(scores[, 1], scores[, 2],
     col = ifelse(labels == "X", "red", "green"),
     pch = 19,
     xlab = paste("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "%)", sep = ""),
     ylab = paste("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "%)", sep = ""),
     main = "Individus dans l'espace des CPs")
legend("topright", legend = c("X", "Y"), col = c("red", "green"), pch = 19)


# --- Plot des variables (cercle des corrélations) ---
# Les loadings sont dans pca_result$rotation
loadings <- pca_result$rotation

# Créer le cercle de corrélation
plot(NULL, xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
     xlab = "PC1", ylab = "PC2",
     main = "Variables dans l'espace des CPs (Cercle de corrélation)",
     asp = 1)  # aspect ratio = 1 pour un cercle

# Dessiner le cercle unité
theta <- seq(0, 2 * pi, length.out = 100)
lines(cos(theta), sin(theta), col = "gray")

# Tracer les vecteurs des variables
# On multiplie par les écarts-types pour avoir les corrélations
sdev <- pca_result$sdev
arrows(0, 0, loadings[1, 1], loadings[1, 2], col = "blue", length = 0.1)
arrows(0, 0, loadings[2, 1], loadings[2, 2], col = "red", length = 0.1)
text(loadings[, 1] * 1.1, loadings[, 2] * 1.1, labels = c("Dim1", "Dim2"), col = c("blue", "red"))

# Ajouter les axes
abline(h = 0, v = 0, col = "gray", lty = 2)

# Commentaire : Les deux variables contribuent différemment aux composantes principales
# La direction des flèches indique la corrélation avec chaque CP
# Des flèches dans la même direction = variables corrélées positivement


# --- Reconstruction des données et erreur ---
# Reconstruction avec les 2 premières CPs (ici on a 2 dims, donc reconstruction parfaite)
# Formule : X_reconstruit = scores %*% t(loadings) * sdev + mean

# Pour une reconstruction avec k composantes :
k <- 2  # nombre de composantes à utiliser

# Reconstruction
# X_center_scale = scores %*% t(rotation)
# X_original = X_center_scale * scale + center
X_reconstruit_scaled <- scores[, 1:k] %*% t(loadings[, 1:k])

# Dé-standardiser : multiplier par sd et ajouter la moyenne
X_mean <- colMeans(Z)
X_sd <- apply(Z, 2, sd)

X_reconstruit <- sweep(X_reconstruit_scaled, 2, X_sd, "*")
X_reconstruit <- sweep(X_reconstruit, 2, X_mean, "+")

# Erreur de reconstruction (Mean Squared Error)
MSE <- mean((Z - X_reconstruit)^2)
print(paste("Erreur de reconstruction (MSE) avec", k, "CPs :", round(MSE, 6)))

# Avec 2 CPs sur des données 2D, l'erreur devrait être très proche de 0
# (la petite erreur vient des arrondis numériques)

# Si on n'utilisait qu'une seule CP :
k <- 1
X_reconstruit_1cp <- scores[, 1:k, drop = FALSE] %*% t(loadings[, 1:k, drop = FALSE])
X_reconstruit_1cp <- sweep(X_reconstruit_1cp, 2, X_sd, "*")
X_reconstruit_1cp <- sweep(X_reconstruit_1cp, 2, X_mean, "+")
MSE_1cp <- mean((Z - X_reconstruit_1cp)^2)
print(paste("Erreur de reconstruction (MSE) avec 1 CP :", round(MSE_1cp, 4)))


# =========================
# FIN DU TP1
# =========================
print("=== TP1 terminé avec succès ! ===")