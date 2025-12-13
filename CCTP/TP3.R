# ============================================================================
# TP3 - Clustering et Analyse des Correspondances
# ============================================================================

# install.packages("palmerpenguins")
# install.packages("kernlab")
# install.packages("FactoMineR")

# Chargement des packages
library(palmerpenguins)
library(kernlab)
library(FactoMineR)

# =========================
# PROBLEM 1 : K-means Clustering sur Penguins
# =========================

# --- Chargement des données ---
data(penguins)
print("Aperçu du dataset penguins :")
print(head(penguins))
print(str(penguins))

# --- Identification des variables ---
# Variables QUALITATIVES (catégorielles) :
#   - species : espèce du pingouin (Adelie, Chinstrap, Gentoo)
#   - island : île d'observation (Biscoe, Dream, Torgersen)
#   - sex : sexe (female, male)
#
# Variables QUANTITATIVES (numériques) :
#   - bill_length_mm : longueur du bec (mm)
#   - bill_depth_mm : profondeur du bec (mm)
#   - flipper_length_mm : longueur de la nageoire (mm)
#   - body_mass_g : masse corporelle (g)
#   - year : année d'observation

print("Types des variables :")
print(sapply(penguins, class))


# --- Nettoyage : supprimer les lignes avec valeurs manquantes ---
print(paste("Nombre de lignes avant nettoyage :", nrow(penguins)))
print("Valeurs manquantes par colonne :")
print(colSums(is.na(penguins)))

# Supprimer les lignes avec NA
penguins_clean <- na.omit(penguins)
print(paste("Nombre de lignes après nettoyage :", nrow(penguins_clean)))

# POURQUOI supprimer les valeurs manquantes avant le clustering ?
# 1. K-means utilise des distances euclidiennes qui ne peuvent pas être calculées avec des NA
# 2. Les NA fausseraient le calcul des centroïdes
# 3. La plupart des algorithmes de clustering ne gèrent pas les NA nativement


# --- Extraire uniquement les variables quantitatives ---
# On exclut : species, island, sex, year (year est quantitatif mais pas pertinent ici)
penguins_quant <- penguins_clean[, c("bill_length_mm", "bill_depth_mm",
                                      "flipper_length_mm", "body_mass_g")]
print("Variables quantitatives sélectionnées :")
print(head(penguins_quant))


# --- Standardisation des données ---
# scale() centre (moyenne = 0) et réduit (écart-type = 1) les données
penguins_scaled <- scale(penguins_quant)

print("Moyennes après standardisation (doivent être ~0) :")
print(round(colMeans(penguins_scaled), 10))
print("Écarts-types après standardisation (doivent être 1) :")
print(round(apply(penguins_scaled, 2, sd), 10))

# POURQUOI standardiser avant le clustering ?
# 1. K-means utilise la distance euclidienne qui est sensible à l'échelle
# 2. Sans standardisation, les variables avec grande variance dominent
# 3. Exemple : body_mass_g (en grammes) dominerait bill_depth_mm (en mm)
# 4. La standardisation donne le même poids à toutes les variables


# --- K-means avec k=3 clusters ---
set.seed(42)  # Pour reproductibilité

# nstart = 25 : l'algorithme est exécuté 25 fois avec différentes initialisations
# et le meilleur résultat (inertie minimale) est retourné
kmeans_result <- kmeans(penguins_scaled, centers = 3, nstart = 25)

# Explication du paramètre nstart :
# K-means est sensible à l'initialisation aléatoire des centroïdes
# Une mauvaise initialisation peut mener à un optimum local
# nstart = 25 exécute l'algorithme 25 fois et garde le meilleur résultat
# Cela augmente les chances de trouver le minimum global

print("Résultat du K-means :")
print(kmeans_result)

# Informations clés :
print(paste("Nombre de clusters :", length(kmeans_result$size)))
print("Taille des clusters :")
print(kmeans_result$size)
print("Centroïdes des clusters (données standardisées) :")
print(round(kmeans_result$centers, 3))
print(paste("Inertie intra-cluster totale (within SS) :",
            round(kmeans_result$tot.withinss, 2)))


# --- Visualisation des clusters ---
# Récupérer les assignations de clusters
clusters_kmeans <- kmeans_result$cluster

# Palette de couleurs pour les clusters
couleurs_clusters <- c("red", "green", "blue")

# Scatter plot : bill_length vs bill_depth
par(mfrow = c(2, 2))

# Plot 1 : bill_length vs bill_depth
plot(penguins_quant$bill_length_mm, penguins_quant$bill_depth_mm,
     col = couleurs_clusters[clusters_kmeans],
     pch = 19,
     xlab = "Longueur du bec (mm)",
     ylab = "Profondeur du bec (mm)",
     main = "K-means : Bill length vs Bill depth")

# Ajouter les centroïdes (attention : il faut les dé-standardiser)
centres_originaux <- t(apply(kmeans_result$centers, 1, function(x) {
  x * attr(penguins_scaled, "scaled:scale") + attr(penguins_scaled, "scaled:center")
}))
points(centres_originaux[, 1], centres_originaux[, 2],
       pch = 8, cex = 2, col = "black", lwd = 3)

# Plot 2 : flipper_length vs body_mass
plot(penguins_quant$flipper_length_mm, penguins_quant$body_mass_g,
     col = couleurs_clusters[clusters_kmeans],
     pch = 19,
     xlab = "Longueur nageoire (mm)",
     ylab = "Masse corporelle (g)",
     main = "K-means : Flipper length vs Body mass")
points(centres_originaux[, 3], centres_originaux[, 4],
       pch = 8, cex = 2, col = "black", lwd = 3)

# Plot 3 : bill_length vs flipper_length
plot(penguins_quant$bill_length_mm, penguins_quant$flipper_length_mm,
     col = couleurs_clusters[clusters_kmeans],
     pch = 19,
     xlab = "Longueur du bec (mm)",
     ylab = "Longueur nageoire (mm)",
     main = "K-means : Bill length vs Flipper length")
points(centres_originaux[, 1], centres_originaux[, 3],
       pch = 8, cex = 2, col = "black", lwd = 3)

# Plot 4 : bill_depth vs body_mass
plot(penguins_quant$bill_depth_mm, penguins_quant$body_mass_g,
     col = couleurs_clusters[clusters_kmeans],
     pch = 19,
     xlab = "Profondeur du bec (mm)",
     ylab = "Masse corporelle (g)",
     main = "K-means : Bill depth vs Body mass")
points(centres_originaux[, 2], centres_originaux[, 4],
       pch = 8, cex = 2, col = "black", lwd = 3)

par(mfrow = c(1, 1))

# Observations :
# - Les clusters sont relativement bien séparés
# - Les centroïdes (étoiles) sont au centre de chaque groupe
# - Certaines observations sont à la frontière entre clusters


# --- Comparaison avec les vraies espèces ---
# Matrice de confusion
especes_vraies <- penguins_clean$species
confusion_matrix <- table(Cluster = clusters_kmeans, Species = especes_vraies)
print("Matrice de confusion (K-means vs Espèces réelles) :")
print(confusion_matrix)

# Visualisation de la matrice de confusion
print("Proportions par cluster :")
print(round(prop.table(confusion_matrix, 1) * 100, 1))

# Calcul de la pureté (accuracy approximative)
# On associe chaque cluster à l'espèce majoritaire
purete <- sum(apply(confusion_matrix, 1, max)) / sum(confusion_matrix)
print(paste("Pureté du clustering :", round(purete * 100, 1), "%"))

# Insights :
# - Le clustering retrouve assez bien les 3 espèces
# - Adelie et Chinstrap peuvent être confondus (morphologie similaire)
# - Gentoo est généralement bien séparé (plus grand)


# =========================
# PROBLEM 2 : Clustering Hiérarchique et Spectral
# =========================

# --- Clustering Hiérarchique avec 3 méthodes de liaison ---

# Calcul de la matrice de distances
dist_matrix <- dist(penguins_scaled, method = "euclidean")

# Méthode 1 : Single linkage (plus proche voisin)
hc_single <- hclust(dist_matrix, method = "single")

# Méthode 2 : Complete linkage (plus lointain voisin)
hc_complete <- hclust(dist_matrix, method = "complete")

# Méthode 3 : Ward (minimise la variance intra-cluster)
hc_ward <- hclust(dist_matrix, method = "ward.D2")

# --- Affichage des dendrogrammes ---
par(mfrow = c(1, 3))

plot(hc_single, main = "Single Linkage", xlab = "", sub = "",
     labels = FALSE, hang = -1)
rect.hclust(hc_single, k = 3, border = c("red", "green", "blue"))

plot(hc_complete, main = "Complete Linkage", xlab = "", sub = "",
     labels = FALSE, hang = -1)
rect.hclust(hc_complete, k = 3, border = c("red", "green", "blue"))

plot(hc_ward, main = "Ward's Method", xlab = "", sub = "",
     labels = FALSE, hang = -1)
rect.hclust(hc_ward, k = 3, border = c("red", "green", "blue"))

par(mfrow = c(1, 1))

# Interprétation des dendrogrammes :
# - La hauteur de fusion indique la dissimilarité entre les groupes fusionnés
# - Single linkage : tend à créer des chaînes (effet de chaînage)
# - Complete linkage : clusters plus compacts et sphériques
# - Ward : minimise l'augmentation de variance, clusters équilibrés


# --- Couper les dendrogrammes pour obtenir 3 clusters ---
clusters_single <- cutree(hc_single, k = 3)
clusters_complete <- cutree(hc_complete, k = 3)
clusters_ward <- cutree(hc_ward, k = 3)

# Tailles des clusters pour chaque méthode
print("Taille des clusters - Single linkage :")
print(table(clusters_single))

print("Taille des clusters - Complete linkage :")
print(table(clusters_complete))

print("Taille des clusters - Ward :")
print(table(clusters_ward))


# --- Comparaison avec les vraies espèces ---
print("=== Matrices de confusion ===")

print("Single linkage vs Espèces :")
conf_single <- table(Cluster = clusters_single, Species = especes_vraies)
print(conf_single)
purete_single <- sum(apply(conf_single, 1, max)) / sum(conf_single)
print(paste("Pureté :", round(purete_single * 100, 1), "%"))

print("Complete linkage vs Espèces :")
conf_complete <- table(Cluster = clusters_complete, Species = especes_vraies)
print(conf_complete)
purete_complete <- sum(apply(conf_complete, 1, max)) / sum(conf_complete)
print(paste("Pureté :", round(purete_complete * 100, 1), "%"))

print("Ward vs Espèces :")
conf_ward <- table(Cluster = clusters_ward, Species = especes_vraies)
print(conf_ward)
purete_ward <- sum(apply(conf_ward, 1, max)) / sum(conf_ward)
print(paste("Pureté :", round(purete_ward * 100, 1), "%"))

# Quelle méthode produit les clusters les plus significatifs ?
# - Single linkage : souvent un gros cluster + quelques singletons (effet de chaînage)
# - Complete linkage : clusters plus équilibrés
# - Ward : généralement les meilleurs résultats pour des clusters compacts
# La méthode de Ward est souvent la plus appropriée pour ce type de données


# --- Clustering Spectral ---
# Le clustering spectral utilise la décomposition spectrale de la matrice de similarité
# Il est particulièrement efficace pour des clusters non convexes

set.seed(42)
spectral_result <- specc(penguins_scaled, centers = 3)

# Récupérer les assignations de clusters
clusters_spectral <- spectral_result@.Data

print("Taille des clusters - Spectral :")
print(table(clusters_spectral))

# Matrice de confusion
print("Spectral clustering vs Espèces :")
conf_spectral <- table(Cluster = clusters_spectral, Species = especes_vraies)
print(conf_spectral)
purete_spectral <- sum(apply(conf_spectral, 1, max)) / sum(conf_spectral)
print(paste("Pureté :", round(purete_spectral * 100, 1), "%"))


# --- Comparaison des trois méthodes de clustering ---
print("=== Résumé des performances ===")
print(paste("K-means      :", round(purete * 100, 1), "%"))
print(paste("Single link  :", round(purete_single * 100, 1), "%"))
print(paste("Complete link:", round(purete_complete * 100, 1), "%"))
print(paste("Ward         :", round(purete_ward * 100, 1), "%"))
print(paste("Spectral     :", round(purete_spectral * 100, 1), "%"))


# --- Visualisation du clustering spectral ---
par(mfrow = c(2, 2))

# K-means
plot(penguins_quant$bill_length_mm, penguins_quant$flipper_length_mm,
     col = couleurs_clusters[clusters_kmeans], pch = 19,
     xlab = "Longueur bec (mm)", ylab = "Longueur nageoire (mm)",
     main = "K-means")

# Ward
plot(penguins_quant$bill_length_mm, penguins_quant$flipper_length_mm,
     col = couleurs_clusters[clusters_ward], pch = 19,
     xlab = "Longueur bec (mm)", ylab = "Longueur nageoire (mm)",
     main = "Hiérarchique (Ward)")

# Spectral
plot(penguins_quant$bill_length_mm, penguins_quant$flipper_length_mm,
     col = couleurs_clusters[clusters_spectral], pch = 19,
     xlab = "Longueur bec (mm)", ylab = "Longueur nageoire (mm)",
     main = "Spectral")

# Vraies espèces
plot(penguins_quant$bill_length_mm, penguins_quant$flipper_length_mm,
     col = couleurs_clusters[as.numeric(especes_vraies)], pch = 19,
     xlab = "Longueur bec (mm)", ylab = "Longueur nageoire (mm)",
     main = "Espèces réelles")

par(mfrow = c(1, 1))


# --- Avantages et Inconvénients des méthodes ---
cat("\n")
cat("=== AVANTAGES ET INCONVÉNIENTS ===\n")
cat("\n")
cat("K-MEANS :\n")
cat("  + Rapide et efficace (O(nkt))\n")
cat("  + Simple à comprendre et implémenter\n")
cat("  + Bon pour les clusters sphériques\n")
cat("  - Nécessite de spécifier k à l'avance\n")
cat("  - Sensible à l'initialisation\n")
cat("  - Ne gère pas bien les clusters non convexes\n")
cat("  - Sensible aux outliers\n")
cat("\n")
cat("CLUSTERING HIÉRARCHIQUE :\n")
cat("  + Pas besoin de spécifier k à l'avance\n")
cat("  + Produit un dendrogramme (visualisation de la structure)\n")
cat("  + Déterministe (pas d'aléa)\n")
cat("  - Plus lent (O(n²) ou O(n³))\n")
cat("  - Choix de la méthode de liaison affecte les résultats\n")
cat("  - Single linkage sensible au chaînage\n")
cat("\n")
cat("CLUSTERING SPECTRAL :\n")
cat("  + Excellent pour clusters non convexes\n")
cat("  + Basé sur la connectivité, pas la distance\n")
cat("  + Peut découvrir des structures complexes\n")
cat("  - Plus lent (décomposition spectrale)\n")
cat("  - Nécessite de spécifier k\n")
cat("  - Sensible au choix du noyau/similarité\n")
cat("\n")


# =========================
# PROBLEM 3 : Analyse des Correspondances (CA et MCA)
# =========================

# --- Analyse des Correspondances (CA) sur HairEyeColor ---

# Charger les données
data(HairEyeColor)
print("Structure de HairEyeColor :")
print(str(HairEyeColor))

# HairEyeColor est un tableau 3D : Hair x Eye x Sex
# On veut créer une table de contingence Hair x Eye (sans le sexe)

# Sommer sur la dimension Sex pour obtenir une table 2D
contingency_table <- apply(HairEyeColor, c(1, 2), sum)
print("Table de contingence Hair x Eye :")
print(contingency_table)

# Vérification des totaux
print(paste("Total des effectifs :", sum(contingency_table)))


# --- Analyse des Correspondances avec FactoMineR ---
ca_result <- CA(contingency_table, graph = FALSE)

# Résumé des résultats
print("=== Résumé de l'Analyse des Correspondances ===")
print(summary(ca_result))

# Valeurs propres (eigenvalues)
eigenvalues <- ca_result$eig
print("Valeurs propres et variance expliquée :")
print(eigenvalues)

# Interprétation :
# - Les valeurs propres indiquent l'importance de chaque dimension
# - Le % de variance expliquée montre combien d'information chaque dimension capture
# - La 1ère dimension capture le plus de variance, etc.
# - On garde généralement les dimensions qui expliquent ~80% de la variance


# --- Biplot de l'Analyse des Correspondances ---
# Le biplot superpose les profils lignes (Hair) et colonnes (Eye)
plot(ca_result,
     title = "Analyse des Correspondances - Hair vs Eye Color",
     col.row = "blue",
     col.col = "red")

# Interprétation du biplot :
# - Les points proches sont associés (profils similaires)
# - La distance à l'origine indique l'écart au profil moyen
# - Les points lignes (Hair) proches de points colonnes (Eye) indiquent une association

# Interprétation des résultats Hair-Eye :
cat("\n")
cat("INTERPRÉTATION DU BIPLOT CA :\n")
cat("- Les cheveux blonds sont proches des yeux bleus (association)\n")
cat("- Les cheveux noirs sont proches des yeux marrons\n")
cat("- Les cheveux roux sont une catégorie plus distincte\n")
cat("- La 1ère dimension oppose clair (blond/bleu) vs foncé (noir/marron)\n")
cat("\n")


# --- Analyse des Correspondances Multiples (MCA) sur mtcars ---

# Charger mtcars
data(mtcars)
print("Aperçu de mtcars :")
print(head(mtcars))

# Convertir certaines variables en facteurs (catégorielles)
# Variables typiquement catégorielles dans mtcars :
# - cyl : nombre de cylindres (4, 6, 8)
# - vs : type de moteur (0 = V-shaped, 1 = straight)
# - am : transmission (0 = automatic, 1 = manual)
# - gear : nombre de vitesses (3, 4, 5)
# - carb : nombre de carburateurs (1, 2, 3, 4, 6, 8)

mtcars_cat <- mtcars
mtcars_cat$cyl <- as.factor(mtcars$cyl)
mtcars_cat$vs <- as.factor(mtcars$vs)
mtcars_cat$am <- as.factor(mtcars$am)
mtcars_cat$gear <- as.factor(mtcars$gear)
mtcars_cat$carb <- as.factor(mtcars$carb)

# Sélectionner uniquement les variables catégorielles pour MCA
mtcars_factors <- mtcars_cat[, c("cyl", "vs", "am", "gear", "carb")]
print("Variables catégorielles pour MCA :")
print(str(mtcars_factors))


# --- MCA avec FactoMineR ---
mca_result <- MCA(mtcars_factors, graph = FALSE)

# Résumé des résultats
print("=== Résumé de l'Analyse des Correspondances Multiples ===")
print(summary(mca_result))

# Valeurs propres
print("Valeurs propres MCA :")
print(mca_result$eig)


# --- Différence entre CA et MCA ---
cat("\n")
cat("=== DIFFÉRENCE ENTRE CA ET MCA ===\n")
cat("\n")
cat("CA (Correspondence Analysis) :\n")
cat("  - Analyse UNE table de contingence (2 variables)\n")
cat("  - Étudie la relation entre les lignes et les colonnes\n")
cat("  - Utilisé pour 2 variables qualitatives\n")
cat("\n")
cat("MCA (Multiple Correspondence Analysis) :\n")
cat("  - Extension de CA à PLUSIEURS variables qualitatives\n")
cat("  - Analyse un tableau disjonctif complet (indicatrices)\n")
cat("  - Permet d'étudier les associations entre toutes les modalités\n")
cat("  - Équivalent de l'ACP pour les variables qualitatives\n")
cat("\n")


# --- Biplot MCA ---
# Graphique des individus
plot(mca_result, choix = "ind",
     title = "MCA - Individus (voitures)",
     label = "ind")

# Graphique des variables (modalités)
plot(mca_result, choix = "var",
     title = "MCA - Variables (modalités)",
     col.var = c("red", "blue", "green", "orange", "purple"))

# Biplot combiné
plot(mca_result,
     title = "MCA - Biplot mtcars",
     col.var = "red",
     col.ind = "blue",
     label = "var")


# --- Interprétation du MCA ---
cat("\n")
cat("INTERPRÉTATION DU BIPLOT MCA :\n")
cat("- Dim 1 : oppose les grosses voitures (8 cyl, auto) aux petites (4 cyl, manual)\n")
cat("- Les modalités proches sont souvent associées dans les données\n")
cat("- cyl=8 proche de gear=3 et am=0 (grosses voitures automatiques)\n")
cat("- cyl=4 proche de gear=4 et am=1 (petites voitures manuelles)\n")
cat("- Les voitures proches dans le graphique ont des profils similaires\n")
cat("\n")

# Contributions des variables aux dimensions
print("Contribution des variables aux dimensions :")
print(round(mca_result$var$contrib, 2))

# Qualité de représentation (cos²)
print("Qualité de représentation des modalités (cos²) :")
print(round(mca_result$var$cos2, 3))


# =========================
# RÉSUMÉ FINAL
# =========================

cat("\n")
cat("=================================================\n")
cat("           RÉSUMÉ DU TP3                         \n")
cat("=================================================\n")
cat("\n")
cat("CLUSTERING :\n")
cat("  - K-means : rapide, nécessite k, clusters sphériques\n")
cat("  - Hiérarchique : dendrogramme, pas de k initial\n")
cat("  - Spectral : clusters non convexes, basé sur graphe\n")
cat("  - Standardisation OBLIGATOIRE avant clustering\n")
cat("\n")
cat("ANALYSE DES CORRESPONDANCES :\n")
cat("  - CA : 2 variables qualitatives (table de contingence)\n")
cat("  - MCA : plusieurs variables qualitatives\n")
cat("  - Équivalent de l'ACP pour données qualitatives\n")
cat("  - Biplot : visualise associations entre modalités\n")
cat("\n")
cat("=================================================\n")

# =========================
# FIN DU TP3
# =========================
print("=== TP3 terminé avec succès ! ===")