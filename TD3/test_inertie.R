# Test du calcul d'inertie
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
rownames(X) <- paste0("x", 1:10)
# Centres finaux trouvés
centres <- matrix(c(5.5, 7.5,    # Classe 1
                    1.667, 2.333, # Classe 2
                    7.333, 2.333), # Classe 3
                  ncol=2, byrow=TRUE)
clusters <- c(2, 2, 2, 3, 1, 1, 1, 1, 3, 3)
cat("Vérification des affectations:\n")
for(i in 1:10) {
  cat(sprintf("%s: Classe %d\n", rownames(X)[i], clusters[i]))
}
# Calcul du centre de gravité global
G <- colMeans(X)
cat(sprintf("\nCentre de gravité global G: (%.3f, %.3f)\n", G[1], G[2]))
# Inertie totale
I_tot <- sum(apply(X, 1, function(x) sum((x - G)^2)))
cat(sprintf("Inertie totale: %.3f\n", I_tot))
# Détail du calcul de l'inertie totale
cat("\nDétail inertie totale:\n")
for(i in 1:10) {
  d2 <- sum((X[i,] - G)^2)
  cat(sprintf("  %s: ||%s - G||² = %.3f\n", rownames(X)[i], 
              paste(sprintf("%.1f", X[i,]), collapse=","), d2))
}
# Inertie intra-classe
cat("\n=== INERTIE INTRA-CLASSE ===\n")
I_intra <- 0
for(k in 1:3) {
  indices <- which(clusters == k)
  cat(sprintf("\nClasse %d: %s\n", k, paste(rownames(X)[indices], collapse=", ")))
  cat(sprintf("Centre: (%.3f, %.3f)\n", centres[k,1], centres[k,2]))
  I_k <- 0
  for(i in indices) {
    d2 <- sum((X[i,] - centres[k,])^2)
    I_k <- I_k + d2
    cat(sprintf("  %s: ||%s - c%d||² = %.3f\n", rownames(X)[i],
                paste(sprintf("%.1f", X[i,]), collapse=","), k, d2))
  }
  cat(sprintf("Inertie intra classe %d: %.3f\n", k, I_k))
  I_intra <- I_intra + I_k
}
cat(sprintf("\nInertie intra-classe totale: %.3f\n", I_intra))
# Inertie inter-classe (méthode 1: par différence)
I_inter_diff <- I_tot - I_intra
cat(sprintf("\nInertie inter-classe (par différence): %.3f\n", I_inter_diff))
# Inertie inter-classe (méthode 2: directe)
cat("\n=== INERTIE INTER-CLASSE (calcul direct) ===\n")
I_inter_direct <- 0
for(k in 1:3) {
  indices <- which(clusters == k)
  n_k <- length(indices)
  d2 <- sum((centres[k,] - G)^2)
  contribution <- n_k * d2
  cat(sprintf("Classe %d: n=%d, ||c%d - G||² = %.3f, contribution = %d × %.3f = %.3f\n", 
              k, n_k, k, d2, n_k, d2, contribution))
  I_inter_direct <- I_inter_direct + contribution
}
cat(sprintf("\nInertie inter-classe (calcul direct): %.3f\n", I_inter_direct))
# Vérification
cat(sprintf("\nVérification: I_intra + I_inter = %.3f + %.3f = %.3f\n", 
            I_intra, I_inter_diff, I_intra + I_inter_diff))
cat(sprintf("Différence avec I_tot: %.10f\n", abs(I_tot - (I_intra + I_inter_diff))))
# Pourcentage d'inertie expliquée
pct <- (I_inter_diff / I_tot) * 100
cat(sprintf("\n✓ Pourcentage d'inertie expliquée: %.2f%%\n", pct))
