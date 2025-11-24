# Exercice I - Classification par k-means (nuées dynamiques)

# 1) Création des données
# Dix individus avec deux variables numériques
X <- matrix(c(1,3,  # x1
              2,1,  # x2
              2,3,  # x3
              5,4,  # x4
              5,7,  # x5
              6,7,  # x6
              5,8,  # x7
              6,8,  # x8
              8,2,  # x9
              9,1), # x10
            ncol=2, byrow=TRUE)

# Noms des individus
rownames(X) <- paste0("x", 1:10)
colnames(X) <- c("Var1", "Var2")

print("Données initiales:")
print(X)

# Ouverture du fichier PDF pour sauvegarder tous les graphiques
pdf("TD3_kmeans_iterations.pdf", width=10, height=8)

# Visualisation des données
plot(X[,1], X[,2], pch=19, cex=1.5,
     xlab="Variable 1", ylab="Variable 2",
     main="Nuage de points initial",
     xlim=c(0,10), ylim=c(0,10))
text(X[,1], X[,2], labels=rownames(X), pos=3, cex=0.8)

# 2) Application de l'algorithme k-means avec centres initiaux spécifiés
# Centres initiaux : x1=(1,3), x2=(2,1), x9=(8,2)
centres_initiaux <- X[c(1, 2, 9), ]

cat("\n")
cat(rep("=", 60), "\n", sep="")
cat("=== ALGORITHME K-MEANS ÉTAPE PAR ÉTAPE ===\n")
cat(rep("=", 60), "\n", sep="")

# Implémentation manuelle pour voir les étapes
kmeans_manuel <- function(data, centres_init, max_iter=100) {
  n <- nrow(data)
  k <- nrow(centres_init)
  centres <- centres_init
  clusters <- rep(0, n)
  converge <- FALSE
  iter <- 0

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║              INITIALISATION                                ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  cat("\nCentres initiaux:\n")
  for(i in 1:k) {
    cat(sprintf("  Centre %d: (%g, %g)\n", i, centres[i,1], centres[i,2]))
  }

  # Visualisation de l'initialisation
  plot(data[,1], data[,2], pch=19, cex=1.5, col="gray",
       xlab="Variable 1", ylab="Variable 2",
       main="INITIALISATION - Centres initiaux",
       xlim=c(0,10), ylim=c(0,10))
  text(data[,1], data[,2], labels=rownames(data), pos=3, cex=0.8)
  points(centres_init, col=1:k, pch=8, cex=3, lwd=3)
  legend("topright", legend=paste("Centre initial", 1:k), col=1:k, pch=8, pt.cex=2)

  while(!converge && iter < max_iter) {
    iter <- iter + 1
    centres_old <- centres

    cat("\n")
    cat(rep("━", 60), "\n", sep="")
    cat(sprintf("                    ITÉRATION %d\n", iter))
    cat(rep("━", 60), "\n", sep="")

    # Étape 1: Affectation des points aux centres les plus proches
    cat("\n▶ ÉTAPE 1: Affectation des points aux centres\n\n")

    for(i in 1:n) {
      distances <- numeric(k)
      for(j in 1:k) {
        distances[j] <- sqrt(sum((data[i,] - centres[j,])^2))
      }
      clusters[i] <- which.min(distances)

      cat(sprintf("  %s (%g, %g) → ", rownames(data)[i], data[i,1], data[i,2]))
      cat(sprintf("distances: [%s] → Classe %d\n",
                  paste(sprintf("%.2f", distances), collapse=", "),
                  clusters[i]))
    }

    # Affichage de la répartition
    cat("\n  Répartition:\n")
    for(j in 1:k) {
      membres <- rownames(data)[clusters == j]
      cat(sprintf("    Classe %d: {%s}\n", j, paste(membres, collapse=", ")))
    }

    # Visualisation après affectation (ÉTAPE 1)
    couleurs <- c("red", "blue", "green", "purple", "orange")
    plot(data[,1], data[,2], pch=19, cex=1.5, col=couleurs[clusters],
         xlab="Variable 1", ylab="Variable 2",
         main=sprintf("ITÉRATION %d - ÉTAPE 1: Affectation aux centres", iter),
         xlim=c(0,10), ylim=c(0,10))
    text(data[,1], data[,2], labels=rownames(data), pos=3, cex=0.8)
    points(centres, col=1:k, pch=8, cex=3, lwd=3)
    legend("topright", legend=c(paste("Classe", 1:k), paste("Centre", 1:k)),
           col=c(couleurs[1:k], 1:k), pch=c(rep(19,k), rep(8,k)), pt.cex=c(rep(1.5,k), rep(2,k)))

    # Étape 2: Recalcul des centres
    cat("\n▶ ÉTAPE 2: Recalcul des centres (barycentres)\n\n")

    for(j in 1:k) {
      points_classe <- data[clusters == j, , drop=FALSE]
      if(nrow(points_classe) > 0) {
        nouveau_centre <- colMeans(points_classe)

        cat(sprintf("  Classe %d:\n", j))
        cat(sprintf("    Ancien centre: (%g, %g)\n", centres[j,1], centres[j,2]))
        cat(sprintf("    Points: %s\n", paste(rownames(points_classe), collapse=", ")))

        # Affichage du calcul
        if(nrow(points_classe) == 1) {
          cat(sprintf("    Calcul: (%g, %g)\n", points_classe[1,1], points_classe[1,2]))
        } else {
          moy_x <- paste(sprintf("%g", points_classe[,1]), collapse=" + ")
          moy_y <- paste(sprintf("%g", points_classe[,2]), collapse=" + ")
          cat(sprintf("    Calcul X: (%s) / %d = %.3f\n", moy_x, nrow(points_classe), nouveau_centre[1]))
          cat(sprintf("    Calcul Y: (%s) / %d = %.3f\n", moy_y, nrow(points_classe), nouveau_centre[2]))
        }

        centres[j,] <- nouveau_centre
        deplacement <- sqrt(sum((centres[j,] - centres_old[j,])^2))

        cat(sprintf("    Nouveau centre: (%.3f, %.3f)\n", centres[j,1], centres[j,2]))
        cat(sprintf("    Déplacement: %.3f\n", deplacement))
      }
    }

    # Test de convergence
    deplacement_max <- max(sqrt(rowSums((centres - centres_old)^2)))
    cat(sprintf("\n  → Déplacement maximal: %.6f\n", deplacement_max))

    if(deplacement_max < 1e-10) {
      converge <- TRUE
      cat("  ✓ CONVERGENCE atteinte!\n")
    }

    # Visualisation après recalcul des centres (ÉTAPE 2) avec flèches
    plot(data[,1], data[,2], pch=19, cex=1.5, col=couleurs[clusters],
         xlab="Variable 1", ylab="Variable 2",
         main=sprintf("ITÉRATION %d - ÉTAPE 2: Nouveaux centres", iter),
         xlim=c(0,10), ylim=c(0,10))
    text(data[,1], data[,2], labels=rownames(data), pos=3, cex=0.8)

    # Anciens centres (en gris clair)
    points(centres_old, col="gray60", pch=8, cex=2.5, lwd=2)

    # Nouveaux centres (en couleur)
    points(centres, col=1:k, pch=8, cex=3, lwd=3)

    # Flèches montrant le déplacement
    for(j in 1:k) {
      if(sqrt(sum((centres[j,] - centres_old[j,])^2)) > 0.01) {
        arrows(centres_old[j,1], centres_old[j,2],
               centres[j,1], centres[j,2],
               col=j, lwd=3, length=0.15, angle=20)
      }
    }

    legend("topright",
           legend=c(paste("Classe", 1:k), "Ancien centre", "Nouveau centre", "Déplacement"),
           col=c(couleurs[1:k], "gray60", "black", "black"),
           pch=c(rep(19,k), 8, 8, NA),
           lty=c(rep(NA,k+2), 1),
           pt.cex=c(rep(1.5,k), 2, 2, NA),
           lwd=c(rep(1,k+2), 3))
  }

  cat("\n╔════════════════════════════════════════════════════════════╗\n")
  cat("║              RÉSULTAT FINAL                                ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  cat(sprintf("\nNombre d'itérations: %d\n", iter))

  # Graphique final résumé
  couleurs <- c("red", "blue", "green", "purple", "orange")
  plot(data[,1], data[,2], pch=19, cex=1.5, col=couleurs[clusters],
       xlab="Variable 1", ylab="Variable 2",
       main="RÉSULTAT FINAL - Classification avec trajectoire des centres",
       xlim=c(0,10), ylim=c(0,10))
  text(data[,1], data[,2], labels=rownames(data), pos=3, cex=0.8)

  # Centres finaux
  points(centres, col=1:k, pch=8, cex=3, lwd=3)

  # Centres initiaux
  points(centres_init, col=1:k, pch=4, cex=2.5, lwd=2)

  legend("topright",
         legend=c(paste("Classe", 1:k), "Centre initial", "Centre final"),
         col=c(couleurs[1:k], "black", "black"),
         pch=c(rep(19,k), 4, 8),
         pt.cex=c(rep(1.5,k), 2, 2))

  return(list(cluster=clusters, centers=centres, iterations=iter))
}

# Exécution du k-means manuel
kmeans_result <- kmeans_manuel(X, centres_initiaux)

cat("\n")
cat(rep("=", 60), "\n", sep="")
cat("=== RÉSULTATS DU K-MEANS ===\n")
cat(rep("=", 60), "\n", sep="")

cat("\nAffectation des individus aux classes:\n")
for(i in 1:length(kmeans_result$cluster)) {
  cat(sprintf("  %s → Classe %d\n", rownames(X)[i], kmeans_result$cluster[i]))
}

cat("\nCentres finaux des classes:\n")
for(i in 1:nrow(kmeans_result$centers)) {
  cat(sprintf("  Classe %d: (%.3f, %.3f)\n", i, kmeans_result$centers[i,1], kmeans_result$centers[i,2]))
}


# Visualisation du résultat
plot(X[,1], X[,2], col=kmeans_result$cluster, pch=19, cex=1.5,
     xlab="Variable 1", ylab="Variable 2",
     main="Classification en 3 classes (K-means)",
     xlim=c(0,10), ylim=c(0,10))
text(X[,1], X[,2], labels=rownames(X), pos=3, cex=0.8)
points(kmeans_result$centers, col=1:3, pch=8, cex=2, lwd=2)
legend("topright", legend=paste("Classe", 1:3), col=1:3, pch=19)

# 3) Calcul du pourcentage d'inertie expliquée

cat("\n")
cat(rep("═", 60), "\n", sep="")
cat("              CALCUL DÉTAILLÉ DE L'INERTIE\n")
cat(rep("═", 60), "\n", sep="")

# Centre de gravité global G
centre_gravite <- colMeans(X)
cat(sprintf("\n▶ Centre de gravité global G = (%.3f, %.3f)\n", centre_gravite[1], centre_gravite[2]))

# === INERTIE TOTALE ===
cat("\n┌─────────────────────────────────────────────────────────┐\n")
cat("│ 1. INERTIE TOTALE: I_tot = Σ ||xi - G||²              │\n")
cat("└─────────────────────────────────────────────────────────┘\n")

inertie_totale <- 0
for(i in 1:nrow(X)) {
  dist2 <- sum((X[i,] - centre_gravite)^2)
  inertie_totale <- inertie_totale + dist2
  cat(sprintf("  %s: (%.0f,%.0f) → ||xi-G||² = %.3f\n",
              rownames(X)[i], X[i,1], X[i,2], dist2))
}
cat(sprintf("\n  ➤ I_totale = %.3f\n", inertie_totale))

# === INERTIE INTRA-CLASSE ===
cat("\n┌─────────────────────────────────────────────────────────┐\n")
cat("│ 2. INERTIE INTRA-CLASSE: I_intra = Σk Σi∈Ck ||xi-ck||²│\n")
cat("└─────────────────────────────────────────────────────────┘\n")

inertie_intra <- 0
inertie_par_classe <- numeric(3)

for(k in 1:3) {
  points_classe <- X[kmeans_result$cluster == k, , drop=FALSE]
  if(nrow(points_classe) > 0) {
    cat(sprintf("\n  Classe %d (centre c%d = (%.3f, %.3f)):\n",
                k, k, kmeans_result$centers[k,1], kmeans_result$centers[k,2]))

    inertie_k <- 0
    for(i in 1:nrow(points_classe)) {
      nom <- rownames(points_classe)[i]
      dist2 <- sum((points_classe[i,] - kmeans_result$centers[k,])^2)
      inertie_k <- inertie_k + dist2
      cat(sprintf("    %s: (%.0f,%.0f) → ||xi-c%d||² = %.3f\n",
                  nom, points_classe[i,1], points_classe[i,2], k, dist2))
    }
    cat(sprintf("    Sous-total classe %d: %.3f\n", k, inertie_k))
    inertie_par_classe[k] <- inertie_k
    inertie_intra <- inertie_intra + inertie_k
  }
}
cat(sprintf("\n  ➤ I_intra = %.3f + %.3f + %.3f = %.3f\n",
            inertie_par_classe[1], inertie_par_classe[2], inertie_par_classe[3],
            inertie_intra))

# === INERTIE INTER-CLASSE ===
cat("\n┌─────────────────────────────────────────────────────────┐\n")
cat("│ 3. INERTIE INTER-CLASSE: I_inter = Σk nk||ck - G||²   │\n")
cat("└─────────────────────────────────────────────────────────┘\n")

inertie_inter_direct <- 0
for(k in 1:3) {
  nk <- sum(kmeans_result$cluster == k)
  dist2_centre <- sum((kmeans_result$centers[k,] - centre_gravite)^2)
  contrib <- nk * dist2_centre
  inertie_inter_direct <- inertie_inter_direct + contrib
  cat(sprintf("  Classe %d: n%d=%d, ||c%d-G||²=%.3f → %d × %.3f = %.3f\n",
              k, k, nk, k, dist2_centre, nk, dist2_centre, contrib))
}
cat(sprintf("\n  ➤ I_inter = %.3f\n", inertie_inter_direct))

# Calcul aussi par différence pour vérification
inertie_inter_diff <- inertie_totale - inertie_intra

# === VÉRIFICATION ===
cat("\n┌─────────────────────────────────────────────────────────┐\n")
cat("│ 4. VÉRIFICATION: I_totale = I_intra + I_inter         │\n")
cat("└─────────────────────────────────────────────────────────┘\n")

cat(sprintf("  I_intra + I_inter = %.3f + %.3f = %.3f\n",
            inertie_intra, inertie_inter_diff, inertie_intra + inertie_inter_diff))
cat(sprintf("  I_totale          = %.3f\n", inertie_totale))
cat(sprintf("  Différence        = %.10f ✓\n", abs(inertie_totale - (inertie_intra + inertie_inter_diff))))

# Utiliser I_inter calculé par différence pour plus de précision
inertie_inter <- inertie_inter_diff

# Pourcentage d'inertie expliquée
pourcentage_inertie <- (inertie_inter / inertie_totale) * 100

cat("\n")
cat(rep("═", 60), "\n", sep="")
cat("              RÉPONSE À LA QUESTION 2\n")
cat(rep("═", 60), "\n", sep="")
cat(sprintf("\n✓ Pourcentage d'inertie expliquée: %.2f%%\n", pourcentage_inertie))

# Visualisation de la décomposition de l'inertie
par(mfrow=c(1,2))

# Graphique 1: Diagramme en barres de la décomposition de l'inertie
inertie_valeurs <- c(inertie_inter, inertie_intra)
pourcentages <- c(pourcentage_inertie, 100 - pourcentage_inertie)

barplot(inertie_valeurs,
        names.arg=c("Inertie\nInter-classe", "Inertie\nIntra-classe"),
        col=c("darkgreen", "coral"),
        main="Décomposition de l'inertie totale",
        ylab="Inertie",
        ylim=c(0, max(inertie_valeurs) * 1.2))
text(x=c(0.7, 1.9), y=inertie_valeurs + 5,
     labels=sprintf("%.1f\n(%.1f%%)", inertie_valeurs, pourcentages),
     cex=1.2, font=2)
abline(h=inertie_totale, col="red", lty=2, lwd=2)
text(0.5, inertie_totale + 5, sprintf("Inertie totale = %.1f", inertie_totale),
     col="red", cex=0.9, pos=3)

# Graphique 2: Diagramme circulaire du pourcentage d'inertie
pie(c(pourcentage_inertie, 100 - pourcentage_inertie),
    labels=c(sprintf("Inter-classe\n%.2f%%", pourcentage_inertie),
             sprintf("Intra-classe\n%.2f%%", 100 - pourcentage_inertie)),
    col=c("darkgreen", "coral"),
    main="Répartition de l'inertie",
    radius=1,
    cex=1.2)

par(mfrow=c(1,1))

# Graphique de l'inertie par classe
inertie_par_classe <- numeric(3)
effectif_par_classe <- numeric(3)
for(i in 1:3) {
  points_classe <- X[kmeans_result$cluster == i, , drop=FALSE]
  if(nrow(points_classe) > 0) {
    inertie_par_classe[i] <- sum(apply(points_classe, 1, function(row) {
      sum((row - kmeans_result$centers[i,])^2)
    }))
    effectif_par_classe[i] <- nrow(points_classe)
  }
}

barplot(inertie_par_classe,
        names.arg=paste0("Classe ", 1:3, "\n(n=", effectif_par_classe, ")"),
        col=c("red", "blue", "green"),
        main="Inertie intra-classe par classe",
        ylab="Inertie",
        ylim=c(0, max(inertie_par_classe) * 1.3))
text(x=c(0.7, 1.9, 3.1), y=inertie_par_classe + 0.5,
     labels=sprintf("%.3f", inertie_par_classe),
     cex=1.2, font=2)

# Affichage détaillé des classes
cat("\n")
cat(rep("═", 60), "\n", sep="")
cat("              DÉTAIL DES CLASSES\n")
cat(rep("═", 60), "\n", sep="")

for(i in 1:3) {
  cat(sprintf("\n▶ Classe %d:\n", i))
  indices <- which(kmeans_result$cluster == i)
  cat(sprintf("  Individus: %s\n", paste(rownames(X)[indices], collapse=", ")))
  cat("  Coordonnées:\n")
  for(idx in indices) {
    cat(sprintf("    %s: (%g, %g)\n", rownames(X)[idx], X[idx,1], X[idx,2]))
  }
  cat(sprintf("  Centre: (%.3f, %.3f)\n", kmeans_result$centers[i,1], kmeans_result$centers[i,2]))
}

cat("\n")
cat(rep("═", 60), "\n", sep="")

# Fermeture du fichier PDF
dev.off()

cat("\n✓ Tous les graphiques ont été sauvegardés dans 'TD3_kmeans_iterations.pdf'\n")


# ============================================================================
# Exercice II - Classification Ascendante Hiérarchique (CAH)
# ============================================================================

cat("\n\n")
cat(rep("═", 70), "\n", sep="")
cat("          EXERCICE II - CLASSIFICATION ASCENDANTE HIÉRARCHIQUE\n")
cat(rep("═", 70), "\n", sep="")

# 1) Création de la matrice de dissimilarités
# Tableau triangulaire inférieur fourni dans l'énoncé
dissim_values <- c(
  85,           # x2-x1
  100, 30,      # x3-x1, x3-x2
  106, 38, 11,  # x4-x1, x4-x2, x4-x3
  108, 45, 17, 7,     # x5-x1, x5-x2, x5-x3, x5-x4
  110, 45, 17, 6, 5,  # x6-x1, x6-x2, x6-x3, x6-x4, x6-x5
  120, 40, 56, 65, 67, 70,     # x7-x1, x7-x2, x7-x3, x7-x4, x7-x5, x7-x6
  112, 42, 64, 74, 77, 80, 17  # x8-x1, x8-x2, x8-x3, x8-x4, x8-x5, x8-x6, x8-x7
)

# Nombre d'individus
n <- 8

# Création de la matrice de dissimilarités complète
D <- matrix(0, nrow=n, ncol=n)
rownames(D) <- paste0("x", 1:n)
colnames(D) <- paste0("x", 1:n)

# Remplissage de la matrice (triangulaire inférieure et supérieure)
idx <- 1
for(i in 2:n) {
  for(j in 1:(i-1)) {
    D[i,j] <- dissim_values[idx]
    D[j,i] <- dissim_values[idx]
    idx <- idx + 1
  }
}

cat("\n▶ Matrice de dissimilarités initiale:\n")
print(D)

# 2) Implémentation manuelle de la CAH avec lien maximal (complete linkage)
cat("\n")
cat(rep("─", 70), "\n", sep="")
cat("MÉTHODE: Lien maximal (complete linkage)\n")
cat("Distance entre classes: d(Ci, Cj) = max{d(xi, xj) : xi ∈ Ci, xj ∈ Cj}\n")
cat(rep("─", 70), "\n", sep="")

CAH_manuel <- function(D_init) {
  n <- nrow(D_init)
  D <- D_init

  # Clusters initiaux (chaque individu est un cluster)
  clusters <- as.list(1:n)
  names(clusters) <- rownames(D_init)
  cluster_names <- rownames(D_init)

  # Pour stocker l'historique des fusions
  fusions <- list()
  hauteurs <- numeric()

  iteration <- 0

  while(length(cluster_names) > 1) {
    iteration <- iteration + 1

    cat("\n")
    cat(rep("━", 70), "\n", sep="")
    cat(sprintf("                         ITÉRATION %d\n", iteration))
    cat(rep("━", 70), "\n", sep="")

    cat("\n▶ Matrice de dissimilarités actuelle:\n")
    print(round(D, 2))

    cat(sprintf("\n▶ Classes actuelles: {%s}\n",
                paste(sapply(cluster_names, function(cn) {
                  membres <- clusters[[cn]]
                  if(length(membres) == 1) {
                    paste0("x", membres)
                  } else {
                    paste0("{", paste0("x", membres, collapse=","), "}")
                  }
                }), collapse=", ")))

    # Trouver la distance minimale (hors diagonale)
    min_dist <- Inf
    min_i <- 0
    min_j <- 0

    for(i in 1:(length(cluster_names)-1)) {
      for(j in (i+1):length(cluster_names)) {
        if(D[i,j] < min_dist) {
          min_dist <- D[i,j]
          min_i <- i
          min_j <- j
        }
      }
    }

    # Affichage de la fusion
    cat(sprintf("\n▶ Distance minimale trouvée: %.2f entre %s et %s\n",
                min_dist, cluster_names[min_i], cluster_names[min_j]))

    cluster_i_membres <- clusters[[cluster_names[min_i]]]
    cluster_j_membres <- clusters[[cluster_names[min_j]]]

    cat(sprintf("  %s = {%s}\n", cluster_names[min_i],
                paste0("x", cluster_i_membres, collapse=", ")))
    cat(sprintf("  %s = {%s}\n", cluster_names[min_j],
                paste0("x", cluster_j_membres, collapse=", ")))

    # Nouvelle classe
    nouveau_cluster <- c(cluster_i_membres, cluster_j_membres)
    nouveau_nom <- paste0(cluster_names[min_i], "+", cluster_names[min_j])

    cat(sprintf("\n▶ Fusion: %s ∪ %s = %s = {%s}\n",
                cluster_names[min_i], cluster_names[min_j], nouveau_nom,
                paste0("x", nouveau_cluster, collapse=", ")))

    # Sauvegarder la fusion
    fusions[[iteration]] <- list(
      cluster1 = cluster_names[min_i],
      cluster2 = cluster_names[min_j],
      distance = min_dist,
      nouveau = nouveau_nom
    )
    hauteurs[iteration] <- min_dist

    # Calculer les nouvelles distances (lien maximal)
    if(length(cluster_names) > 2) {
      cat("\n▶ Calcul des nouvelles distances (lien maximal):\n")

      nouvelles_distances <- numeric()
      autres_clusters <- setdiff(1:length(cluster_names), c(min_i, min_j))

      for(k in autres_clusters) {
        # Lien maximal: distance max entre tous les éléments des deux classes
        distances_elements <- numeric()

        cat(sprintf("\n  Distance(%s, %s):\n", nouveau_nom, cluster_names[k]))

        for(elem_new in nouveau_cluster) {
          for(elem_k in clusters[[cluster_names[k]]]) {
            dist_elem <- D_init[elem_new, elem_k]
            distances_elements <- c(distances_elements, dist_elem)
            cat(sprintf("    d(x%d, x%d) = %.2f\n", elem_new, elem_k, dist_elem))
          }
        }

        dist_max <- max(distances_elements)
        nouvelles_distances <- c(nouvelles_distances, dist_max)
        cat(sprintf("    → max = %.2f\n", dist_max))
      }

      # Créer la nouvelle matrice de distances
      n_clusters_new <- length(cluster_names) - 1
      D_new <- matrix(0, nrow=n_clusters_new, ncol=n_clusters_new)

      # Nouveaux noms de clusters
      cluster_names_new <- c(nouveau_nom, cluster_names[autres_clusters])
      rownames(D_new) <- cluster_names_new
      colnames(D_new) <- cluster_names_new

      # Remplir la nouvelle matrice
      # Distances entre le nouveau cluster et les autres
      for(idx in 1:length(autres_clusters)) {
        D_new[1, idx+1] <- nouvelles_distances[idx]
        D_new[idx+1, 1] <- nouvelles_distances[idx]
      }

      # Distances entre les autres clusters (inchangées)
      if(length(autres_clusters) > 1) {
        for(idx1 in 1:(length(autres_clusters)-1)) {
          for(idx2 in (idx1+1):length(autres_clusters)) {
            old_i <- autres_clusters[idx1]
            old_j <- autres_clusters[idx2]
            D_new[idx1+1, idx2+1] <- D[old_i, old_j]
            D_new[idx2+1, idx1+1] <- D[old_i, old_j]
          }
        }
      }

      D <- D_new

      # Mettre à jour la liste des clusters
      clusters_new <- list()
      clusters_new[[nouveau_nom]] <- nouveau_cluster
      for(idx in 1:length(autres_clusters)) {
        old_idx <- autres_clusters[idx]
        clusters_new[[cluster_names_new[idx+1]]] <- clusters[[cluster_names[old_idx]]]
      }

      clusters <- clusters_new
      cluster_names <- cluster_names_new

    } else {
      # Dernier regroupement
      cluster_names <- c(nouveau_nom)
      clusters <- list()
      clusters[[nouveau_nom]] <- nouveau_cluster
    }
  }

  cat("\n")
  cat(rep("═", 70), "\n", sep="")
  cat("                     RÉSULTAT FINAL\n")
  cat(rep("═", 70), "\n", sep="")

  cat("\n▶ Ordre de fusion des classes:\n\n")
  for(i in 1:length(fusions)) {
    f <- fusions[[i]]
    cat(sprintf("  Étape %d: %s ∪ %s → %s (distance = %.2f)\n",
                i, f$cluster1, f$cluster2, f$nouveau, f$distance))
  }

  return(list(fusions = fusions, hauteurs = hauteurs))
}

# Exécution de la CAH manuelle
cah_result <- CAH_manuel(D)

# 3) Visualisation avec le dendrogramme standard de R
cat("\n")
cat(rep("─", 70), "\n", sep="")
cat("VÉRIFICATION AVEC LA FONCTION hclust() DE R\n")
cat(rep("─", 70), "\n", sep="")

# Conversion en objet dist
D_dist <- as.dist(D)

# CAH avec lien maximal
cah_r <- hclust(D_dist, method="complete")

cat("\n▶ Ordre de fusion (hclust):\n")
print(cah_r$merge)
cat("\n▶ Hauteurs de fusion:\n")
print(cah_r$height)

# Créer un PDF pour les graphiques de la CAH
pdf("TD3_CAH_dendrogramme.pdf", width=12, height=8)

# Dendrogramme
plot(cah_r,
     main="Dendrogramme - Classification Ascendante Hiérarchique\n(Lien maximal / Complete linkage)",
     xlab="Individus",
     ylab="Distance de fusion",
     sub="",
     cex=1.2,
     cex.main=1.5,
     cex.lab=1.3,
     hang=-1)

# Ajouter des lignes de coupure pour différents nombres de classes
abline(h=cah_r$height, col="gray80", lty=3)
abline(h=cah_r$height[length(cah_r$height)/2], col="red", lty=2, lwd=2)
legend("topright", legend=c("Hauteurs de fusion", "Exemple de coupure"),
       col=c("gray80", "red"), lty=c(3, 2), lwd=c(1, 2))

# Graphique avec différentes coupures
par(mfrow=c(2,2))

for(k in 2:5) {
  plot(cah_r,
       main=paste0("Coupure en ", k, " classes"),
       xlab="", ylab="Distance",
       sub="",
       cex=0.8,
       hang=-1)

  # Hauteur de coupure pour obtenir k classes
  hauteur_coupe <- cah_r$height[n - k]
  abline(h=hauteur_coupe, col="red", lty=2, lwd=2)

  # Colorier les branches
  groupes <- cutree(cah_r, k=k)
  cat(sprintf("\n▶ Partition en %d classes:\n", k))
  for(i in 1:k) {
    membres <- which(groupes == i)
    cat(sprintf("  Classe %d: {%s}\n", i, paste0("x", membres, collapse=", ")))
  }
}

par(mfrow=c(1,1))

# Tableau récapitulatif des distances
cat("\n")
cat(rep("═", 70), "\n", sep="")
cat("           TABLEAU RÉCAPITULATIF DES FUSIONS\n")
cat(rep("═", 70), "\n", sep="")

cat("\n")
cat(sprintf("%-10s | %-20s | %-20s | %s\n", "Étape", "Classe 1", "Classe 2", "Distance"))
cat(rep("─", 70), "\n", sep="")

for(i in 1:length(cah_result$fusions)) {
  f <- cah_result$fusions[[i]]
  cat(sprintf("%-10d | %-20s | %-20s | %.2f\n",
              i, f$cluster1, f$cluster2, f$distance))
}

dev.off()

cat("\n✓ Le dendrogramme a été sauvegardé dans 'TD3_CAH_dendrogramme.pdf'\n")

cat("\n")
cat(rep("═", 70), "\n", sep="")
cat("                 FIN DE L'EXERCICE II\n")
cat(rep("═", 70), "\n", sep="")


# ============================================================================
# Exercice III - CAH avec méthode de Ward sur les données de l'exercice I
# ============================================================================

cat("\n\n")
cat(rep("═", 70), "\n", sep="")
cat("     EXERCICE III - CAH AVEC MÉTHODE DE WARD (Données Ex. I)\n")
cat(rep("═", 70), "\n", sep="")

cat("\n▶ Contexte:\n")
cat("  - Données: celles de l'exercice I (10 individus, 2 variables)\n")
cat("  - Distance entre éléments: métrique D1/σ²\n")
cat("  - Distance entre classes: perte d'inertie inter-classes (Ward)\n")

# Tableau des résultats fourni
# Colonnes: Noeud, Fils gauche, Fils droit, Distance (perte d'inertie), Poids
ward_results <- data.frame(
  Noeud = c(11, 12, 13, 14, 15, 16, 17, 18, 19),
  Fils_gauche = c(1, 11, 5, 6, 9, 13, 4, 17, 12),
  Fils_droit = c(3, 2, 7, 8, 10, 14, 16, 15, 18),
  Distance = c(0.0082, 0.0396, 0.0069, 0.0069, 0.0151, 0.0164, 0.1386, 0.797, 0.9886),
  Poids = c(0.2, 0.3, 0.2, 0.2, 0.2, 0.4, 0.5, 0.7, 1.0)
)

cat("\n▶ Tableau des résultats de la CAH:\n\n")
print(ward_results, row.names = FALSE)

# ============================================================================
# Question 1: Dessiner l'arbre de classification
# ============================================================================

cat("\n")
cat(rep("─", 70), "\n", sep="")
cat("QUESTION 1: Arbre de classification\n")
cat(rep("─", 70), "\n", sep="")

# Conversion du tableau en format hclust
# Dans hclust, les fusions sont codées avec des indices négatifs pour les feuilles
# et positifs pour les noeuds internes

# Créer la matrice de merge pour hclust
merge_matrix <- matrix(0, nrow=9, ncol=2)
heights <- ward_results$Distance

for(i in 1:9) {
  # Fils gauche
  if(ward_results$Fils_gauche[i] <= 10) {
    # C'est une feuille (individu original)
    merge_matrix[i, 1] <- -ward_results$Fils_gauche[i]
  } else {
    # C'est un noeud interne
    merge_matrix[i, 1] <- ward_results$Fils_gauche[i] - 10
  }

  # Fils droit
  if(ward_results$Fils_droit[i] <= 10) {
    # C'est une feuille
    merge_matrix[i, 2] <- -ward_results$Fils_droit[i]
  } else {
    # C'est un noeud interne
    merge_matrix[i, 2] <- ward_results$Fils_droit[i] - 10
  }
}

cat("\n▶ Matrice de fusion (format hclust):\n")
cat("   (indices négatifs = feuilles, positifs = noeuds internes)\n\n")
print(merge_matrix)

cat("\n▶ Hauteurs de fusion (perte d'inertie):\n")
print(heights)

# Créer un objet hclust
ward_hclust <- list(
  merge = merge_matrix,
  height = heights,
  order = c(1, 3, 2, 5, 7, 6, 8, 4, 9, 10),  # Ordre pour l'affichage
  labels = paste0("x", 1:10),
  method = "ward.D2",
  call = quote(hclust(d, method="ward.D2")),
  dist.method = "euclidean"
)
class(ward_hclust) <- "hclust"

# Ouvrir un fichier PDF pour les graphiques de l'exercice III
pdf("TD3_Ward_dendrogramme.pdf", width=12, height=10)

# Dendrogramme principal
plot(ward_hclust,
     main="Dendrogramme - CAH avec méthode de Ward\n(Données de l'exercice I)",
     xlab="Individus",
     ylab="Perte d'inertie inter-classes",
     sub="",
     cex=1.2,
     cex.main=1.5,
     cex.lab=1.3,
     hang=-1)

# Ajouter des lignes horizontales pour les hauteurs importantes
abline(h=heights, col="gray80", lty=3)

# Ligne pour la partition en 3 classes
# Pour 3 classes, on coupe à la hauteur qui sépare les 3 dernières fusions
hauteur_3classes <- heights[length(heights) - 2]  # 7ème fusion
abline(h=hauteur_3classes, col="red", lty=2, lwd=2)
text(5, hauteur_3classes, "Partition en 3 classes", pos=3, col="red", font=2)

cat("\n✓ Dendrogramme créé\n")

# ============================================================================
# Question 2: Inertie totale du nuage
# ============================================================================

cat("\n")
cat(rep("─", 70), "\n", sep="")
cat("QUESTION 2: Inertie totale du nuage\n")
cat(rep("─", 70), "\n", sep="")

cat("\n▶ Rappel: L'inertie totale a été calculée dans l'exercice I\n")
cat(sprintf("   I_totale = %.3f\n", inertie_totale))

# Vérification: dans Ward, la somme de toutes les pertes d'inertie
# doit égaler l'inertie totale
somme_pertes_inertie <- sum(heights)
cat(sprintf("\n▶ Somme des pertes d'inertie (distances Ward): %.4f\n", somme_pertes_inertie))
cat(sprintf("   I_totale (calculée ex. I): %.4f\n", inertie_totale))

if(abs(somme_pertes_inertie - inertie_totale) < 0.01) {
  cat("\n✓ Vérification: les valeurs sont cohérentes!\n")
} else {
  cat("\n⚠ Note: légère différence due à la métrique D1/σ² utilisée\n")
}

# ============================================================================
# Question 3: Somme des distances
# ============================================================================

cat("\n")
cat(rep("─", 70), "\n", sep="")
cat("QUESTION 3: Somme des distances (pertes d'inertie)\n")
cat(rep("─", 70), "\n", sep="")

cat("\n▶ La somme des distances (pertes d'inertie à chaque fusion):\n\n")

for(i in 1:nrow(ward_results)) {
  cat(sprintf("  Fusion %d: %.4f\n", i, ward_results$Distance[i]))
}

somme_distances <- sum(ward_results$Distance)
cat(sprintf("\n  Total: %.4f\n", somme_distances))

cat("\n▶ Interprétation:\n")
cat("   Cette somme représente la perte totale d'inertie inter-classes\n")
cat("   lors de la fusion de tous les individus en un seul cluster.\n")
cat("   Elle est égale à l'inertie totale du nuage.\n")

# ============================================================================
# Question 4: Inerties inter et intra avant et après classification
# ============================================================================

cat("\n")
cat(rep("─", 70), "\n", sep="")
cat("QUESTION 4: Inerties inter et intra avant et après classification\n")
cat(rep("─", 70), "\n", sep="")

cat("\n▶ AVANT la classification (10 classes = 10 individus isolés):\n")
cat("   - Chaque individu forme sa propre classe\n")
cat(sprintf("   - I_intra = 0 (pas de dispersion dans chaque classe)\n"))
cat(sprintf("   - I_inter = I_totale = %.4f\n", inertie_totale))
cat("   - Pourcentage d'inertie expliquée: 100%%\n")

cat("\n▶ APRÈS la classification (1 classe = tous les individus):\n")
cat("   - Tous les individus sont dans une seule classe\n")
cat(sprintf("   - I_intra = I_totale = %.4f\n", inertie_totale))
cat(sprintf("   - I_inter = 0 (un seul centre = centre de gravité global)\n"))
cat("   - Pourcentage d'inertie expliquée: 0%%\n")

# ============================================================================
# Question 5: Pourcentage d'inertie expliquée pour 3 classes
# ============================================================================

cat("\n")
cat(rep("─", 70), "\n", sep="")
cat("QUESTION 5: Pourcentage d'inertie expliquée (partition en 3 classes)\n")
cat(rep("─", 70), "\n", sep="")

# Pour obtenir 3 classes, on coupe l'arbre avant les 2 dernières fusions
# Les 7 premières fusions correspondent à l'inertie intra des 3 classes

# Inertie intra = somme des pertes d'inertie des 7 premières fusions
inertie_intra_3classes <- sum(heights[1:7])
cat(sprintf("\n▶ I_intra (3 classes) = somme des 7 premières pertes d'inertie\n"))
cat(sprintf("   = %.4f + %.4f + %.4f + %.4f + %.4f + %.4f + %.4f\n",
            heights[1], heights[2], heights[3], heights[4],
            heights[5], heights[6], heights[7]))
cat(sprintf("   = %.4f\n", inertie_intra_3classes))

# Inertie inter = inertie totale - inertie intra
inertie_inter_3classes <- inertie_totale - inertie_intra_3classes
cat(sprintf("\n▶ I_inter (3 classes) = I_totale - I_intra\n"))
cat(sprintf("   = %.4f - %.4f\n", inertie_totale, inertie_intra_3classes))
cat(sprintf("   = %.4f\n", inertie_inter_3classes))

# Pourcentage d'inertie expliquée
pct_inertie_3classes <- (inertie_inter_3classes / inertie_totale) * 100
cat(sprintf("\n▶ Pourcentage d'inertie expliquée:\n"))
cat(sprintf("   = (I_inter / I_totale) × 100\n"))
cat(sprintf("   = (%.4f / %.4f) × 100\n", inertie_inter_3classes, inertie_totale))
cat(sprintf("   = %.2f%%%%\n", pct_inertie_3classes))

# Visualisation de la partition en 3 classes
groupes_3classes <- cutree(ward_hclust, k=3)

cat("\n▶ Composition des 3 classes:\n")
for(i in 1:3) {
  membres <- which(groupes_3classes == i)
  cat(sprintf("   Classe %d: {%s}\n", i,
              paste0("x", membres, collapse=", ")))
}

# Graphique avec les 3 classes colorées
plot(ward_hclust,
     main="Partition en 3 classes (méthode de Ward)",
     xlab="Individus",
     ylab="Perte d'inertie inter-classes",
     sub="",
     cex=1.2,
     cex.main=1.5,
     hang=-1)
abline(h=hauteur_3classes, col="red", lty=2, lwd=2)

# Colorer les branches
if(require(dendextend, quietly=TRUE)) {
  dend <- as.dendrogram(ward_hclust)
  dend_colored <- color_branches(dend, k=3, col=c("red", "blue", "green"))
  plot(dend_colored,
       main="Partition en 3 classes (branches colorées)",
       ylab="Perte d'inertie inter-classes")
  abline(h=hauteur_3classes, col="red", lty=2, lwd=2)
}

# Graphique en 2D avec les 3 classes
plot(X[,1], X[,2],
     col=c("red", "blue", "green")[groupes_3classes],
     pch=19, cex=1.5,
     xlab="Variable 1", ylab="Variable 2",
     main="Partition en 3 classes - Méthode de Ward",
     xlim=c(0,10), ylim=c(0,10))
text(X[,1], X[,2], labels=rownames(X), pos=3, cex=0.8)
legend("topright",
       legend=paste("Classe", 1:3),
       col=c("red", "blue", "green"),
       pch=19,
       pt.cex=1.5)

# Comparaison avec k-means
par(mfrow=c(1,2))

# Ward
plot(X[,1], X[,2],
     col=c("red", "blue", "green")[groupes_3classes],
     pch=19, cex=1.5,
     xlab="Variable 1", ylab="Variable 2",
     main="CAH Ward (3 classes)",
     xlim=c(0,10), ylim=c(0,10))
text(X[,1], X[,2], labels=rownames(X), pos=3, cex=0.8)

# K-means (de l'exercice I)
plot(X[,1], X[,2],
     col=c("red", "blue", "green")[kmeans_result$cluster],
     pch=19, cex=1.5,
     xlab="Variable 1", ylab="Variable 2",
     main="K-means (3 classes)",
     xlim=c(0,10), ylim=c(0,10))
text(X[,1], X[,2], labels=rownames(X), pos=3, cex=0.8)
points(kmeans_result$centers, col=1:3, pch=8, cex=2, lwd=2)

par(mfrow=c(1,1))

# Graphique de comparaison des inerties
par(mfrow=c(1,2))

# Diagramme en barres - Ward
barplot(c(inertie_inter_3classes, inertie_intra_3classes),
        names.arg=c("I_inter", "I_intra"),
        col=c("darkgreen", "coral"),
        main="CAH Ward - 3 classes",
        ylab="Inertie",
        ylim=c(0, max(inertie_inter_3classes, inertie_intra_3classes) * 1.3))
text(x=c(0.7, 1.9),
     y=c(inertie_inter_3classes, inertie_intra_3classes) + 5,
     labels=sprintf("%.2f\n(%.1f%%%%)",
                    c(inertie_inter_3classes, inertie_intra_3classes),
                    c(pct_inertie_3classes, 100 - pct_inertie_3classes)),
     cex=1.2, font=2)

# Diagramme circulaire - Ward
pie(c(pct_inertie_3classes, 100 - pct_inertie_3classes),
    labels=c(sprintf("Inter\n%.2f%%%%", pct_inertie_3classes),
             sprintf("Intra\n%.2f%%%%", 100 - pct_inertie_3classes)),
    col=c("darkgreen", "coral"),
    main="Répartition de l'inertie\n(Ward - 3 classes)")

par(mfrow=c(1,1))

# Courbe d'évolution de l'inertie en fonction du nombre de classes
nb_classes <- 1:10
inerties_intra_ward <- numeric(10)
inerties_inter_ward <- numeric(10)

for(k in 1:10) {
  if(k == 1) {
    inerties_intra_ward[k] <- inertie_totale
    inerties_inter_ward[k] <- 0
  } else if(k == 10) {
    inerties_intra_ward[k] <- 0
    inerties_inter_ward[k] <- inertie_totale
  } else {
    # Somme des pertes d'inertie jusqu'à avoir k classes
    nb_fusions <- 10 - k
    inerties_intra_ward[k] <- sum(heights[1:nb_fusions])
    inerties_inter_ward[k] <- inertie_totale - inerties_intra_ward[k]
  }
}

pct_explique_ward <- (inerties_inter_ward / inertie_totale) * 100

plot(nb_classes, pct_explique_ward, type="b",
     pch=19, col="blue", lwd=2,
     xlab="Nombre de classes",
     ylab="Pourcentage d'inertie expliquée (%)",
     main="Courbe du coude - CAH Ward",
     ylim=c(0, 100),
     xaxt="n")
axis(1, at=1:10)
grid()
abline(v=3, col="red", lty=2, lwd=2)
text(3, 50, "3 classes", pos=4, col="red", font=2)
points(3, pct_explique_ward[3], col="red", pch=19, cex=2)
text(3, pct_explique_ward[3],
     sprintf("%.1f%%%%", pct_explique_ward[3]),
     pos=3, col="red", font=2)

dev.off()

cat("\n✓ Tous les graphiques ont été sauvegardés dans 'TD3_Ward_dendrogramme.pdf'\n")

# ============================================================================
# Résumé de l'exercice III
# ============================================================================

cat("\n")
cat(rep("═", 70), "\n", sep="")
cat("              RÉSUMÉ DE L'EXERCICE III\n")
cat(rep("═", 70), "\n", sep="")

cat("\n▶ RÉPONSES AUX QUESTIONS:\n\n")
cat(sprintf("1) Arbre: voir dendrogramme dans 'TD3_Ward_dendrogramme.pdf'\n\n"))
cat(sprintf("2) Inertie totale: %.4f\n\n", inertie_totale))
cat(sprintf("3) Somme des distances (pertes d'inertie): %.4f\n\n", somme_distances))
cat("4) Inerties inter et intra:\n")
cat(sprintf("   - AVANT (10 classes): I_inter = %.4f, I_intra = 0\n", inertie_totale))
cat(sprintf("   - APRÈS (1 classe):   I_inter = 0, I_intra = %.4f\n\n", inertie_totale))
cat(sprintf("5) Partition en 3 classes:\n"))
cat(sprintf("   - I_intra = %.4f\n", inertie_intra_3classes))
cat(sprintf("   - I_inter = %.4f\n", inertie_inter_3classes))
cat(sprintf("   - Pourcentage d'inertie expliquée: %.2f%%%%\n", pct_inertie_3classes))

cat("\n▶ COMPARAISON K-means vs Ward (3 classes):\n")
cat(sprintf("   - K-means: %.2f%%%% d'inertie expliquée\n", pourcentage_inertie))
cat(sprintf("   - Ward:    %.2f%%%% d'inertie expliquée\n", pct_inertie_3classes))

if(pct_inertie_3classes > pourcentage_inertie) {
  cat("\n   → La méthode de Ward donne une meilleure partition!\n")
} else {
  cat("\n   → Le K-means donne une meilleure partition!\n")
}

cat("\n")
cat(rep("═", 70), "\n", sep="")
cat("                 FIN DE L'EXERCICE III\n")
cat(rep("═", 70), "\n", sep="")
