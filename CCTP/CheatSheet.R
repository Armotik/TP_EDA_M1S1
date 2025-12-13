# ══════════════════════════════════════════════════════════════════════════════
# 1. BASES R - VECTEURS ET MATRICES
# ══════════════════════════════════════════════════════════════════════════════

# --- CRÉATION DE VECTEURS ---
v <- 1:20                              # Séquence 1 à 20
v <- c(1, 5, 3, 8)                     # Vecteur manuel
v <- seq(1, 10, by=2)                  # Séquence avec pas : 1,3,5,7,9
v <- rep(0, 10)                        # Répéter 0 dix fois
v <- sample(1:20, 10, replace=TRUE)    # 10 valeurs aléatoires entre 1 et 20
v <- rnorm(100, mean=0, sd=1)          # 100 valeurs selon loi normale

# --- OPÉRATIONS SUR VECTEURS ---
length(v)                              # Longueur
sum(v)                                 # Somme
mean(v)                                # Moyenne
median(v)                              # Médiane
sd(v)                                  # Écart-type
var(v)                                 # Variance
min(v); max(v)                         # Min et max
range(v)                               # c(min, max)
quantile(v, 0.25)                      # 1er quartile

# --- INDEXATION ET FILTRAGE ---
v[3]                                   # 3ème élément
v[c(1,3,5)]                            # Éléments 1, 3 et 5
v[v > 10]                              # Éléments > 10
which(v > 10)                          # INDICES des éléments > 10
v[v < 5] <- 0                          # Remplacer les valeurs < 5 par 0

# --- PRODUITS DE VECTEURS ---
v1 * v2                                # Produit de Hadamard (élément par élément)
sum(v1 * v2)                           # Produit scalaire (dot product)
v1 %*% v2                              # Produit matriciel (donne matrice 1x1)

# --- CRÉATION DE MATRICES ---
M <- matrix(1:25, nrow=5, ncol=5)              # Par colonne (défaut)
M <- matrix(1:25, nrow=5, ncol=5, byrow=TRUE)  # Par ligne
M <- diag(5)                                    # Matrice identité 5x5
M <- diag(c(1,2,3))                            # Matrice diagonale

# --- OPÉRATIONS SUR MATRICES ---
dim(M)                                 # Dimensions (nrow, ncol)
nrow(M); ncol(M)                       # Nombre de lignes/colonnes
t(M)                                   # Transposée
diag(M)                                # Extraire la diagonale
sum(diag(M))                           # Trace de la matrice
M %*% N                                # Produit matriciel
M * N                                  # Produit de Hadamard
solve(M)                               # Inverse de M
eigen(M)                               # Valeurs et vecteurs propres
  # eigen(M)$values   -> valeurs propres
  # eigen(M)$vectors  -> vecteurs propres

# --- INDEXATION MATRICES ---
M[2, 3]                                # Élément ligne 2, colonne 3
M[2, ]                                 # Ligne 2 entière
M[, 3]                                 # Colonne 3 entière
M[1:3, 2:4]                            # Sous-matrice

# --- FONCTIONS APPLY ---
apply(M, 1, sum)                       # Somme par LIGNE (MARGIN=1)
apply(M, 2, sum)                       # Somme par COLONNE (MARGIN=2)
apply(M, 1, min)                       # Min par ligne
apply(M, 2, max)                       # Max par colonne
rowMeans(M); colMeans(M)               # Moyennes lignes/colonnes
rowSums(M); colSums(M)                 # Sommes lignes/colonnes


# ══════════════════════════════════════════════════════════════════════════════
# 2. DATA FRAMES
# ══════════════════════════════════════════════════════════════════════════════

# --- CRÉATION ---
df <- data.frame(
  id = 1:100,
  age = sample(18:30, 100, replace=TRUE),
  height = sample(150:200, 100, replace=TRUE)
)

# --- MANIPULATION ---
head(df)                               # 6 premières lignes
tail(df)                               # 6 dernières lignes
str(df)                                # Structure
summary(df)                            # Résumé statistique
names(df)                              # Noms des colonnes
dim(df)                                # Dimensions

df$nouvelle_col <- valeurs             # Ajouter une colonne
df[, c("col1", "col2")]                # Sélectionner colonnes
df[df$age > 25, ]                      # Filtrer lignes
subset(df, age > 25)                   # Alternative pour filtrer

# --- VALEURS MANQUANTES ---
is.na(df)                              # Matrice de booléens
sum(is.na(df))                         # Compter les NA
colSums(is.na(df))                     # NA par colonne
na.omit(df)                            # Supprimer lignes avec NA
complete.cases(df)                     # Lignes complètes (booléen)

# --- LECTURE/ÉCRITURE ---
df <- read.csv("fichier.csv")          # Lire CSV
df <- read.csv(url, header=FALSE, col.names=noms)  # Depuis URL
write.csv(df, "fichier.csv", row.names=FALSE)      # Écrire CSV

# --- CONVERSIONS ---
as.matrix(df)                          # Data frame -> Matrice
as.data.frame(M)                       # Matrice -> Data frame
as.factor(v)                           # Vecteur -> Facteur
as.numeric(v)                          # Vecteur -> Numérique


# ══════════════════════════════════════════════════════════════════════════════
# 3. STATISTIQUES ET DISTANCES
# ══════════════════════════════════════════════════════════════════════════════

# --- MATRICES DE VARIANCE-COVARIANCE ET CORRÉLATION ---
cov(X)                                 # Matrice de covariance
cor(X)                                 # Matrice de corrélation

# Calcul MANUEL de la covariance (sans cov(), sans boucle) :
X_centre <- scale(X, center=TRUE, scale=FALSE)  # Centrer
V <- (1/(n-1)) * t(X_centre) %*% X_centre       # Covariance

# Calcul MANUEL de la corrélation :
ecarts_types <- sqrt(diag(V))
R <- V / outer(ecarts_types, ecarts_types)

# --- STANDARDISATION ---
X_scaled <- scale(X)                   # Centre ET réduit (mean=0, sd=1)
X_centre <- scale(X, center=TRUE, scale=FALSE)   # Centre seulement
X_reduit <- scale(X, center=FALSE, scale=TRUE)   # Réduit seulement

# Attributs de scale() :
attr(X_scaled, "scaled:center")        # Moyennes utilisées
attr(X_scaled, "scaled:scale")         # Écarts-types utilisés

# --- DISTANCES ---
# Distance euclidienne entre 2 points :
dist_eucl <- sqrt(sum((x - y)^2))

# Distance de Manhattan :
dist_manh <- sum(abs(x - y))

# Distance de Mahalanobis :
# d_M(x,y) = sqrt( t(x-y) %*% M %*% (x-y) )
distanceMat <- function(x, y, M) {
  diff <- x - y
  return(as.numeric(sqrt(t(diff) %*% M %*% diff)))
}

# Matrice de distances (tous les pairs) :
D <- dist(X, method="euclidean")       # Objet dist (triangle inférieur)
D <- dist(X, method="manhattan")       # Distance Manhattan
D_mat <- as.matrix(D)                  # Convertir en matrice carrée

# --- INERTIE ---
inertie <- function(X, M) {
  barycentre <- colMeans(X)
  total <- 0
  for (i in 1:nrow(X)) {
    d <- distanceMat(X[i,], barycentre, M)
    total <- total + d^2
  }
  return(total / nrow(X))
}


# ══════════════════════════════════════════════════════════════════════════════
# 4. ACP (Analyse en Composantes Principales)
# ══════════════════════════════════════════════════════════════════════════════

# --- ACP AVEC prcomp() ---
pca <- prcomp(X, center=TRUE, scale.=TRUE)

# center=TRUE  : soustrait la moyenne (OBLIGATOIRE)
# scale.=TRUE  : divise par écart-type (si échelles différentes)

# --- RÉSULTATS DE prcomp() ---
pca$x                                  # Scores (coordonnées des individus)
pca$rotation                           # Loadings (vecteurs propres)
pca$sdev                               # Écarts-types des CPs
pca$sdev^2                             # Valeurs propres
pca$center                             # Moyennes utilisées
pca$scale                              # Écarts-types utilisés

summary(pca)                           # Variance expliquée

# --- VARIANCE EXPLIQUÉE ---
valeurs_propres <- pca$sdev^2
var_expliquee <- valeurs_propres / sum(valeurs_propres) * 100
var_cumulee <- cumsum(var_expliquee)

# Nombre de CPs pour X% de variance :
n_cp <- which(var_cumulee >= 80)[1]

# --- SCREE PLOT ---
barplot(valeurs_propres, names.arg=paste0("PC", 1:length(valeurs_propres)),
        main="Scree Plot", ylab="Valeur propre")
abline(h=1, col="red", lty=2)          # Critère de Kaiser

# --- PROJECTION MANUELLE ---
X_scaled <- scale(X, center=TRUE, scale=TRUE)
scores_manuels <- X_scaled %*% pca$rotation

# --- QUALITÉ DE REPRÉSENTATION (cos²) ---
cos2 <- pca$rotation^2
cos2_plan12 <- cos2[,1] + cos2[,2]     # Sur plan PC1-PC2

# --- CONTRIBUTION DES INDIVIDUS ---
# contrib_ik = (score_ik)² / (n * valeur_propre_k) * 100
contrib <- (pca$x^2) / (nrow(X) * matrix(valeurs_propres, nrow=nrow(X),
                                          ncol=length(valeurs_propres), byrow=TRUE)) * 100

# --- RECONSTRUCTION ---
k <- 2  # Nombre de CPs à garder
X_reconstr <- pca$x[,1:k] %*% t(pca$rotation[,1:k])
X_reconstr <- sweep(X_reconstr, 2, pca$scale, "*")
X_reconstr <- sweep(X_reconstr, 2, pca$center, "+")
MSE <- mean((X - X_reconstr)^2)

# --- BIPLOT ---
biplot(pca, main="Biplot ACP")

# --- ACP MANUELLE (sans prcomp) ---
X_centre <- scale(X, center=TRUE, scale=FALSE)
V <- cov(X)                            # ou (1/(n-1)) * t(X_centre) %*% X_centre
decomp <- eigen(V)
valeurs_propres <- decomp$values
vecteurs_propres <- decomp$vectors
scores <- X_centre %*% vecteurs_propres


# ══════════════════════════════════════════════════════════════════════════════
# 5. CLUSTERING
# ══════════════════════════════════════════════════════════════════════════════

# ⚠️  TOUJOURS STANDARDISER AVANT CLUSTERING : X_scaled <- scale(X)

# --- K-MEANS ---
set.seed(42)                           # Pour reproductibilité
km <- kmeans(X_scaled, centers=3, nstart=25)

# nstart=25 : exécute 25 fois, garde le meilleur (évite optima locaux)

km$cluster                             # Assignations (vecteur)
km$centers                             # Centroïdes
km$size                                # Taille des clusters
km$tot.withinss                        # Inertie intra-cluster totale
km$betweenss                           # Inertie inter-cluster

# --- CLUSTERING HIÉRARCHIQUE ---
D <- dist(X_scaled, method="euclidean")

hc_single <- hclust(D, method="single")      # Plus proche voisin (chaînage)
hc_complete <- hclust(D, method="complete")  # Plus lointain voisin
hc_ward <- hclust(D, method="ward.D2")       # Ward (minimise variance) ★

# Dendrogramme :
plot(hc_ward, main="Dendrogramme", labels=FALSE)
rect.hclust(hc_ward, k=3, border=c("red","green","blue"))

# Couper en k clusters :
clusters <- cutree(hc_ward, k=3)

# --- CLUSTERING SPECTRAL ---
library(kernlab)
set.seed(42)
spec <- specc(X_scaled, centers=3)
clusters_spectral <- spec@.Data

# --- ÉVALUATION ---
# Matrice de confusion :
table(Cluster=clusters, Vrai=labels_vrais)

# Pureté :
conf <- table(clusters, labels_vrais)
purete <- sum(apply(conf, 1, max)) / sum(conf)


# ══════════════════════════════════════════════════════════════════════════════
# 6. ANALYSE DES CORRESPONDANCES (CA / MCA)
# ══════════════════════════════════════════════════════════════════════════════

library(FactoMineR)

# --- CA (2 variables qualitatives) ---
# Créer table de contingence :
tab <- table(var1, var2)
# ou depuis tableau 3D :
tab <- apply(HairEyeColor, c(1,2), sum)

ca <- CA(tab, graph=FALSE)
summary(ca)
ca$eig                                 # Valeurs propres et variance
plot(ca)                               # Biplot

# --- MCA (plusieurs variables qualitatives) ---
# Convertir en facteurs :
df$var <- as.factor(df$var)

mca <- MCA(df_factors, graph=FALSE)
summary(mca)
mca$eig                                # Valeurs propres
mca$var$contrib                        # Contributions des modalités
mca$var$cos2                           # Qualité de représentation

plot(mca, choix="ind")                 # Graphe des individus
plot(mca, choix="var")                 # Graphe des variables
plot(mca)                              # Biplot


# ══════════════════════════════════════════════════════════════════════════════
# 7. VISUALISATION
# ══════════════════════════════════════════════════════════════════════════════

# --- SCATTER PLOT ---
plot(x, y, col=couleurs, pch=19,
     xlab="Label X", ylab="Label Y", main="Titre")
points(x2, y2, col="red", pch=8)       # Ajouter des points
legend("topright", legend=c("A","B"), col=c("red","blue"), pch=19)

# --- BOXPLOT ---
boxplot(df, main="Titre", col="lightblue")
boxplot(var ~ groupe, data=df)         # Par groupe

# --- HISTOGRAMME ---
hist(x, breaks=20, col="steelblue", main="Titre")

# --- HEATMAP ---
heatmap(as.matrix(D), main="Heatmap")

# --- MULTIPLE PLOTS ---
par(mfrow=c(2,2))                      # Grille 2x2
# ... plots ...
par(mfrow=c(1,1))                      # Réinitialiser

# --- COULEURS ---
rainbow(n)                             # n couleurs arc-en-ciel
heat.colors(n)                         # n couleurs chaleur
palette <- c("red","green","blue")
col = palette[as.numeric(factor)]      # Couleurs selon facteur


# ══════════════════════════════════════════════════════════════════════════════
# 8. PACKAGES À CHARGER
# ══════════════════════════════════════════════════════════════════════════════

library(palmerpenguins)                # Dataset penguins
library(kernlab)                       # specc() - clustering spectral
library(FactoMineR)                    # CA(), MCA() - correspondances

# Datasets intégrés :
data(iris)                             # Iris
data(mtcars)                           # Voitures
data(penguins)                         # Pingouins (avec palmerpenguins)
data(HairEyeColor)                     # Cheveux/Yeux


# ══════════════════════════════════════════════════════════════════════════════
# 9. AIDE-MÉMOIRE RAPIDE
# ══════════════════════════════════════════════════════════════════════════════

# ┌─────────────────────────────────────────────────────────────────────────────┐
# │ TÂCHE                        │ FONCTION                                    │
# ├─────────────────────────────────────────────────────────────────────────────┤
# │ Valeurs propres              │ eigen(M)$values                             │
# │ Vecteurs propres             │ eigen(M)$vectors                            │
# │ ACP                          │ prcomp(X, center=TRUE, scale.=TRUE)         │
# │ Scores ACP                   │ pca$x                                       │
# │ Loadings ACP                 │ pca$rotation                                │
# │ Variance expliquée           │ pca$sdev^2 / sum(pca$sdev^2)                │
# │ K-means                      │ kmeans(X, centers=k, nstart=25)             │
# │ Hiérarchique                 │ hclust(dist(X), method="ward.D2")           │
# │ Couper dendrogramme          │ cutree(hc, k=3)                             │
# │ Spectral                     │ specc(X, centers=k)                         │
# │ Distance euclidienne         │ dist(X) ou sqrt(sum((x-y)^2))               │
# │ Covariance                   │ cov(X)                                      │
# │ Corrélation                  │ cor(X)                                      │
# │ Standardiser                 │ scale(X)                                    │
# │ CA                           │ CA(table_contingence)                       │
# │ MCA                          │ MCA(df_facteurs)                            │
# └─────────────────────────────────────────────────────────────────────────────┘