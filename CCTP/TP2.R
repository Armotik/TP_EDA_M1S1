# ============================================================================
# TP2 - Analyse en Composantes Principales (PCA)
# ============================================================================

# =========================
# PROBLEM 1 : ACP manuelle sur iris
# =========================

# Charger le dataset iris (disponible par défaut dans R)
data(iris)
print("Aperçu du dataset iris :")
print(head(iris))
print(str(iris))

# Extraire uniquement les variables numériques (colonnes 1 à 4)
# On exclut la colonne 5 qui est l'espèce (variable qualitative)
X <- as.matrix(iris[, 1:4])
print(paste("Dimensions de X :", nrow(X), "x", ncol(X)))

# --- ÉTAPE 1 : Centrer les données ---
# Soustraire la moyenne de chaque colonne
moyennes <- colMeans(X)
print("Moyennes de chaque variable :"); print(moyennes)

# Centrage : X_centre = X - moyennes (broadcasting automatique en R)
X_centre <- scale(X, center = TRUE, scale = FALSE)
# Équivalent manuel : X_centre <- sweep(X, 2, moyennes, "-")

# Vérification : les moyennes des colonnes centrées doivent être ~0
print("Moyennes après centrage (doivent être ~0) :")
print(colMeans(X_centre))

# --- ÉTAPE 2 : Calculer la matrice de variance-covariance ---
# Formule : V = (1/(n-1)) * t(X_centre) %*% X_centre
n <- nrow(X_centre)
V <- (1 / (n - 1)) * t(X_centre) %*% X_centre
print("Matrice de variance-covariance V :")
print(V)

# Vérification avec la fonction cov()
print("Vérification avec cov() :")
print(cov(X))

# --- ÉTAPE 3 : Calculer les valeurs propres et vecteurs propres ---
# eigen() retourne les valeurs propres (values) et vecteurs propres (vectors)
decomposition <- eigen(V)
valeurs_propres <- decomposition$values
vecteurs_propres <- decomposition$vectors

print("Valeurs propres (eigenvalues) :")
print(valeurs_propres)
print("Vecteurs propres (eigenvectors) :")
print(vecteurs_propres)

# --- INTERPRÉTATION ---
# Les VALEURS PROPRES représentent la variance expliquée par chaque composante principale
# Plus la valeur propre est grande, plus la CP capture de variance
#
# Les VECTEURS PROPRES représentent les directions des composantes principales
# Chaque vecteur propre définit une nouvelle axis dans l'espace des variables
# Les coefficients indiquent la contribution de chaque variable originale à la CP

# --- ÉTAPE 4 : Calculer la variance expliquée ---
variance_totale <- sum(valeurs_propres)
variance_expliquee <- valeurs_propres / variance_totale
variance_cumulee <- cumsum(variance_expliquee)

print("Variance expliquée par chaque CP :")
print(round(variance_expliquee * 100, 2))
print("Variance cumulée :")
print(round(variance_cumulee * 100, 2))

# --- ÉTAPE 5 : Projeter les individus sur le premier plan factoriel ---
# Projection : scores = X_centre %*% vecteurs_propres
scores_manuels <- X_centre %*% vecteurs_propres
colnames(scores_manuels) <- paste0("PC", 1:4)

print("Premières lignes des scores (coordonnées projetées) :")
print(head(scores_manuels))

# --- ÉTAPE 6 : Visualisation sur le premier plan factoriel (PC1 vs PC2) ---
# Créer un vecteur de couleurs selon l'espèce
couleurs <- as.numeric(iris$Species)
palette_couleurs <- c("red", "green", "blue")

plot(scores_manuels[, 1], scores_manuels[, 2],
     col = palette_couleurs[couleurs],
     pch = 19,
     xlab = paste0("PC1 (", round(variance_expliquee[1] * 100, 1), "%)"),
     ylab = paste0("PC2 (", round(variance_expliquee[2] * 100, 1), "%)"),
     main = "ACP manuelle sur Iris - Premier plan factoriel")
legend("topright",
       legend = levels(iris$Species),
       col = palette_couleurs,
       pch = 19)

# --- Vérification avec prcomp() ---
pca_verif <- prcomp(X, center = TRUE, scale. = FALSE)
print("Vérification : comparaison avec prcomp()")
print("Valeurs propres (variance) de prcomp :")
print(pca_verif$sdev^2)  # sdev² = valeurs propres
print("Nos valeurs propres :")
print(valeurs_propres)


# =========================
# PROBLEM 2 : Dataset Glass (données réelles)
# =========================

# --- Chargement des données ---
# Le dataset glass est disponible sur UCI
# Structure : 214 observations, 10 attributs + 1 classe

# Charger depuis un fichier local ou URL
# Note : Le fichier n'a pas d'en-têtes, on doit les ajouter
url_glass <- "https://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data"

# Noms des colonnes selon glass.names
noms_colonnes <- c("Id", "RI", "Na", "Mg", "Al", "Si", "K", "Ca", "Ba", "Fe", "Type")

# Charger les données
glass <- read.csv(url_glass, header = FALSE, col.names = noms_colonnes)

# Vérifier le chargement
print("Aperçu du dataset glass :")
print(head(glass))
print(str(glass))
print(paste("Dimensions :", nrow(glass), "lignes,", ncol(glass), "colonnes"))

# Description des variables :
# RI : Indice de réfraction
# Na : Sodium (pourcentage en poids d'oxyde)
# Mg : Magnésium
# Al : Aluminium
# Si : Silicium
# K  : Potassium
# Ca : Calcium
# Ba : Baryum
# Fe : Fer
# Type : Type de verre (1-7)

# Mettre l'Id comme nom de ligne et supprimer la colonne Id
rownames(glass) <- glass$Id
glass$Id <- NULL

# Type est une variable catégorielle, on la convertit en facteur
glass$Type <- as.factor(glass$Type)

# Extraire les variables numériques pour l'ACP (exclure Type)
glass_num <- glass[, 1:9]
print("Variables numériques pour l'ACP :")
print(head(glass_num))


# --- BOXPLOTS pour exploration initiale ---
par(mfrow = c(3, 3))  # Grille 3x3 pour afficher 9 boxplots
for (i in 1:ncol(glass_num)) {
  boxplot(glass_num[, i],
          main = names(glass_num)[i],
          col = "lightblue",
          horizontal = TRUE)
}
par(mfrow = c(1, 1))  # Réinitialiser

# Boxplot global pour comparer les échelles
boxplot(glass_num,
        main = "Distribution des variables - Glass dataset",
        las = 2,  # Labels verticaux
        col = rainbow(9))

# --- Observations initiales ---
# On peut voir que :
# - RI a des valeurs très concentrées (faible variance)
# - Ba et Fe ont beaucoup de valeurs à 0 (variables asymétriques)
# - Si a les valeurs les plus élevées (~70-75)
# - Les échelles sont très différentes -> nécessité de standardiser

# Statistiques descriptives
print("Résumé statistique :")
print(summary(glass_num))

print("Écarts-types :")
print(round(apply(glass_num, 2, sd), 4))


# --- MATRICE DE VARIANCE-COVARIANCE (sans cov(), sans boucle) ---
# Formule : V = (1/(n-1)) * t(X_centre) %*% X_centre

# Centrer les données
X_glass <- as.matrix(glass_num)
n_glass <- nrow(X_glass)
moyennes_glass <- colMeans(X_glass)

# Centrage sans boucle avec sweep()
X_glass_centre <- sweep(X_glass, 2, moyennes_glass, "-")
# Ou : X_glass_centre <- scale(X_glass, center = TRUE, scale = FALSE)

# Matrice de variance-covariance
V_glass <- (1 / (n_glass - 1)) * t(X_glass_centre) %*% X_glass_centre
print("Matrice de variance-covariance V (calculée manuellement) :")
print(round(V_glass, 4))

# Vérification
print("Vérification avec cov() :")
print(round(cov(glass_num), 4))


# --- MATRICE DE CORRÉLATION ---
# Corrélation = covariance normalisée par les écarts-types
# Cor(X,Y) = Cov(X,Y) / (sd(X) * sd(Y))

ecarts_types <- sqrt(diag(V_glass))  # Écarts-types = racine des variances (diagonale)

# Matrice de corrélation : diviser chaque élément par le produit des écarts-types
# R_ij = V_ij / (sd_i * sd_j)
matrice_correlation <- V_glass / outer(ecarts_types, ecarts_types)
print("Matrice de corrélation (calculée manuellement) :")
print(round(matrice_correlation, 3))

# Vérification avec cor()
print("Vérification avec cor() :")
print(round(cor(glass_num), 3))

# --- Interprétation des corrélations ---
# Corrélations fortes positives : variables qui varient dans le même sens
# Corrélations fortes négatives : variables qui varient en sens inverse
# Exemples notables :
# - RI et Ca : corrélation positive forte
# - Mg et Ca : corrélation négative
# - Al et Ba : corrélation positive


# --- ACP avec prcomp() ---
# center = TRUE : centre les données (soustrait la moyenne)
# scale. = TRUE : standardise les données (divise par l'écart-type)

# ACP standardisée (recommandée quand les échelles sont différentes)
pca_glass <- prcomp(glass_num, center = TRUE, scale. = TRUE)

print("Résultat de prcomp() :")
print(summary(pca_glass))


# --- Les composantes forment-elles une base orthonormée ? ---
# Les vecteurs propres (rotation) doivent être :
# 1. Orthogonaux : le produit scalaire entre deux vecteurs différents = 0
# 2. Normés : la norme de chaque vecteur = 1

loadings <- pca_glass$rotation
print("Matrice des loadings (vecteurs propres) :")
print(round(loadings, 4))

# Vérification numérique : t(loadings) %*% loadings doit donner l'identité
orthonormalite <- t(loadings) %*% loadings
print("Vérification orthonormalité (doit être ≈ Identité) :")
print(round(orthonormalite, 10))
# La diagonale doit être 1 (normé), les autres éléments ~0 (orthogonaux)


# --- Paramètres center et scale ---
print("=== Influence de center et scale ===")

# Sans centrage ni scaling
pca_brut <- prcomp(glass_num, center = FALSE, scale. = FALSE)
print("ACP sans center ni scale - Variance expliquée :")
print(summary(pca_brut)$importance[2, ])

# Avec centrage seulement
pca_centre <- prcomp(glass_num, center = TRUE, scale. = FALSE)
print("ACP avec center seulement - Variance expliquée :")
print(summary(pca_centre)$importance[2, ])

# Avec centrage et scaling (standard)
print("ACP avec center et scale - Variance expliquée :")
print(summary(pca_glass)$importance[2, ])

# Commentaire :
# - Sans scaling, les variables avec grande variance dominent l'ACP
# - Avec scaling (standardisation), toutes les variables ont le même poids
# - Pour des données avec échelles différentes (comme glass), scale=TRUE est recommandé


# --- Projection manuelle et vérification ---
# Les coordonnées projetées sont : scores = X_standardisé %*% loadings

# Standardiser les données manuellement
X_standardise <- scale(glass_num, center = TRUE, scale = TRUE)

# Projection manuelle
scores_manuels_glass <- X_standardise %*% loadings
print("Scores calculés manuellement (premières lignes) :")
print(head(round(scores_manuels_glass, 4)))

# Scores de prcomp
print("Scores de prcomp (premières lignes) :")
print(head(round(pca_glass$x, 4)))

# Les valeurs doivent être identiques !


# --- Plot sur le premier plan factoriel avec labels ---
# Utiliser les numéros de ligne comme labels
labels_glass <- rownames(glass)

# Couleurs selon le type de verre
couleurs_type <- as.numeric(glass$Type)
palette_type <- rainbow(7)

# Plot avec labels
plot(pca_glass$x[, 1], pca_glass$x[, 2],
     type = "n",  # Ne pas tracer les points d'abord
     xlab = paste0("PC1 (", round(summary(pca_glass)$importance[2, 1] * 100, 1), "%)"),
     ylab = paste0("PC2 (", round(summary(pca_glass)$importance[2, 2] * 100, 1), "%)"),
     main = "Glass - Premier plan factoriel avec labels")

# Ajouter les labels
text(pca_glass$x[, 1], pca_glass$x[, 2],
     labels = labels_glass,
     col = palette_type[couleurs_type],
     cex = 0.7)

legend("topright",
       legend = paste("Type", levels(glass$Type)),
       col = palette_type[1:length(levels(glass$Type))],
       pch = 15,
       cex = 0.8)

# --- Individus qui se distinguent ---
# Identifier les individus avec des coordonnées extrêmes sur PC1 ou PC2
scores_glass <- pca_glass$x

# Seuils pour identifier les outliers (ex: > 2.5 écarts-types)
seuil <- 2.5

outliers_PC1 <- which(abs(scores_glass[, 1]) > seuil * sd(scores_glass[, 1]))
outliers_PC2 <- which(abs(scores_glass[, 2]) > seuil * sd(scores_glass[, 2]))

print("Individus atypiques sur PC1 :")
print(outliers_PC1)
print("Individus atypiques sur PC2 :")
print(outliers_PC2)


# =========================
# PROBLEM 3 : Analyse approfondie
# =========================

# --- SCREE PLOT (graphique des valeurs propres) ---
# Les valeurs propres sont : (sdev)²
valeurs_propres_glass <- pca_glass$sdev^2
print("Valeurs propres :")
print(round(valeurs_propres_glass, 4))

# Pourcentage de variance expliquée
var_expliquee_glass <- valeurs_propres_glass / sum(valeurs_propres_glass) * 100
var_cumulee_glass <- cumsum(var_expliquee_glass)

print("Variance expliquée (%) :")
print(round(var_expliquee_glass, 2))
print("Variance cumulée (%) :")
print(round(var_cumulee_glass, 2))

# Scree plot
par(mfrow = c(1, 2))

# Plot 1 : Valeurs propres (éboulis)
barplot(valeurs_propres_glass,
        names.arg = paste0("PC", 1:length(valeurs_propres_glass)),
        main = "Scree Plot - Valeurs propres",
        ylab = "Valeur propre",
        col = "steelblue")
abline(h = 1, col = "red", lty = 2)  # Critère de Kaiser : garder VP > 1

# Plot 2 : Variance cumulée
plot(1:length(var_cumulee_glass), var_cumulee_glass,
     type = "b",
     pch = 19,
     xlab = "Nombre de composantes",
     ylab = "Variance cumulée (%)",
     main = "Variance cumulée",
     ylim = c(0, 100))
abline(h = 80, col = "red", lty = 2)  # Seuil de 80%
abline(h = 90, col = "orange", lty = 2)  # Seuil de 90%

par(mfrow = c(1, 1))

# --- Combien de composantes garder ? ---
# Critères :
# 1. Critère de Kaiser : garder les CP avec valeur propre > 1
n_kaiser <- sum(valeurs_propres_glass > 1)
print(paste("Critère de Kaiser : garder", n_kaiser, "composantes"))

# 2. Critère du coude : point d'inflexion sur le scree plot
# Visuellement, souvent après PC3 ou PC4

# 3. Critère du pourcentage de variance (80% ou 90%)
n_80 <- which(var_cumulee_glass >= 80)[1]
n_90 <- which(var_cumulee_glass >= 90)[1]
print(paste("Pour 80% de variance :", n_80, "composantes"))
print(paste("Pour 90% de variance :", n_90, "composantes"))

# Justification :
# On garde généralement 3-4 composantes selon le critère utilisé
# PC1 + PC2 + PC3 expliquent environ 65-70% de la variance
# Pour une analyse complète, on pourrait garder 4-5 composantes


# --- BIPLOT ---
# Le biplot superpose les individus et les variables sur le même graphique
biplot(pca_glass,
       main = "Biplot - Individus et Variables",
       cex = 0.6)

# Interprétation du biplot :
# - Les flèches représentent les variables originales
# - La direction indique la corrélation avec les axes PC1 et PC2
# - La longueur indique la qualité de représentation
# - Les points sont les individus
# - Des individus proches ont des profils similaires
# - Un individu dans la direction d'une variable a des valeurs élevées pour cette variable


# --- AUTRES PLANS FACTORIELS ---
par(mfrow = c(2, 2))

# Plan PC1 vs PC2
plot(scores_glass[, 1], scores_glass[, 2],
     col = palette_type[couleurs_type], pch = 19,
     xlab = "PC1", ylab = "PC2", main = "Plan PC1 x PC2")

# Plan PC1 vs PC3
plot(scores_glass[, 1], scores_glass[, 3],
     col = palette_type[couleurs_type], pch = 19,
     xlab = "PC1", ylab = "PC3", main = "Plan PC1 x PC3")

# Plan PC2 vs PC3
plot(scores_glass[, 2], scores_glass[, 3],
     col = palette_type[couleurs_type], pch = 19,
     xlab = "PC2", ylab = "PC3", main = "Plan PC2 x PC3")

# Plan PC3 vs PC4
plot(scores_glass[, 3], scores_glass[, 4],
     col = palette_type[couleurs_type], pch = 19,
     xlab = "PC3", ylab = "PC4", main = "Plan PC3 x PC4")

par(mfrow = c(1, 1))

# Commentaires :
# - PC1 sépare principalement certains types de verre
# - PC3 peut révéler d'autres structures non visibles sur PC1-PC2
# - Le plan PC2-PC3 peut montrer des groupes différents


# --- QUALITÉ DE REPRÉSENTATION DES VARIABLES ---
# cos² = (loading)² indique la qualité de représentation d'une variable sur un axe
# Pour le premier plan factoriel : cos²_plan = cos²_PC1 + cos²_PC2

loadings_glass <- pca_glass$rotation

# cos² pour chaque variable sur chaque composante
cos2_variables <- loadings_glass^2
print("cos² des variables (qualité de représentation) :")
print(round(cos2_variables, 4))

# Qualité sur le premier plan factoriel (PC1 + PC2)
cos2_plan12 <- cos2_variables[, 1] + cos2_variables[, 2]
print("Qualité de représentation sur le plan PC1-PC2 :")
print(round(sort(cos2_plan12, decreasing = TRUE), 4))

# Variables les mieux représentées : celles avec cos² proche de 1
# Variables mal représentées : cos² faible -> on perd de l'information


# --- CONTRIBUTION DES INDIVIDUS ---
# Contribution d'un individu i à une composante k :
# contrib_ik = (score_ik)² / (n * valeur_propre_k) * 100

n_individus <- nrow(scores_glass)
contributions <- matrix(0, nrow = n_individus, ncol = ncol(scores_glass))

for (k in 1:ncol(scores_glass)) {
  contributions[, k] <- (scores_glass[, k]^2) / (n_individus * valeurs_propres_glass[k]) * 100
}
colnames(contributions) <- paste0("PC", 1:ncol(contributions))
rownames(contributions) <- rownames(glass)

print("Contributions des individus (premières lignes) :")
print(head(round(contributions, 3)))

# Contribution moyenne attendue : 100/n = 100/214 ≈ 0.47%
contrib_moyenne <- 100 / n_individus
print(paste("Contribution moyenne attendue :", round(contrib_moyenne, 3), "%"))

# Individus avec contribution anormalement élevée sur PC1
individus_contrib_elevee <- which(contributions[, 1] > 3 * contrib_moyenne)
print("Individus avec forte contribution sur PC1 (> 3x moyenne) :")
print(individus_contrib_elevee)

# Ces individus influencent fortement la construction des axes
# On peut envisager de les exclure si ce sont des outliers

# Visualisation des contributions pour PC1
barplot(sort(contributions[, 1], decreasing = TRUE)[1:20],
        main = "Top 20 contributions à PC1",
        ylab = "Contribution (%)",
        las = 2,
        col = "steelblue")
abline(h = contrib_moyenne, col = "red", lty = 2)
abline(h = 3 * contrib_moyenne, col = "orange", lty = 2)


# --- EXCLUSION D'INDIVIDUS ? ---
# Un individu devrait être exclu si :
# 1. Sa contribution est anormalement élevée (> 3-4x la moyenne)
# 2. C'est un outlier dû à une erreur de mesure
# 3. Il ne représente pas la population étudiée

# Pour le dataset glass, certains individus peuvent avoir des compositions
# chimiques très différentes (verres spéciaux)
# La décision d'exclure dépend du contexte de l'analyse


# --- SIMILARITÉS ENTRE INDIVIDUS ---
# La projection sur les CPs regroupe les individus similaires
# On s'attend à ce que les types de verre similaires soient regroupés

# Calcul des distances entre individus dans l'espace des 3 premières CPs
dist_pca <- dist(scores_glass[, 1:3])
dist_mat_pca <- as.matrix(dist_pca)

# Heatmap des distances (ordonnée par type de verre)
ordre <- order(glass$Type)
heatmap(dist_mat_pca[ordre, ordre],
        Rowv = NA, Colv = NA,  # Pas de réordonnancement
        main = "Distances dans l'espace PCA (ordonnées par Type)",
        labRow = NA, labCol = NA)

# Les blocs diagonaux foncés indiquent que les individus du même type
# sont proches dans l'espace des composantes principales

# Test : les moyennes par type de verre
print("Coordonnées moyennes par type de verre sur les 3 premières CPs :")
aggregate(scores_glass[, 1:3], by = list(Type = glass$Type), FUN = mean)


# =========================
# RÉSUMÉ ET CONCLUSIONS
# =========================

cat("\n")
cat("=================================================\n")
cat("           RÉSUMÉ DE L'ANALYSE PCA              \n")
cat("=================================================\n")
cat("\n")
cat("Dataset Glass : 214 observations, 9 variables chimiques\n")
cat("\n")
cat("Variance expliquée :\n")
for (i in 1:5) {
  cat(paste0("  PC", i, " : ", round(var_expliquee_glass[i], 1),
             "% (cumulé : ", round(var_cumulee_glass[i], 1), "%)\n"))
}
cat("\n")
cat(paste("Nombre de composantes recommandé : 4-5\n"))
cat("  - Critère de Kaiser (VP>1) :", n_kaiser, "composantes\n")
cat("  - Pour 80% de variance :", n_80, "composantes\n")
cat("\n")
cat("Variables les mieux représentées sur le plan PC1-PC2 :\n")
top_vars <- names(sort(cos2_plan12, decreasing = TRUE))[1:3]
cat(paste("  -", top_vars, "\n"))
cat("\n")
cat("=================================================\n")

# =========================
# FIN DU TP2
# =========================
print("=== TP2 terminé avec succès ! ===")