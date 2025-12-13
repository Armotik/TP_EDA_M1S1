# ══════════════════════════════════════════════════════════════════════════════════════
# 1. NOTIONS DE BASE - STATISTIQUES DESCRIPTIVES
# ══════════════════════════════════════════════════════════════════════════════════════

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ MESURES DE TENDANCE CENTRALE                                                         │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │ Moyenne      : μ = (1/n) Σ xᵢ                    → Sensible aux outliers            │
# │ Médiane      : Valeur qui sépare en 2 moitiés   → Robuste aux outliers              │
# │ Mode         : Valeur la plus fréquente                                              │
# └──────────────────────────────────────────────────────────────────────────────────────┘

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ MESURES DE DISPERSION                                                                │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │ Variance     : σ² = (1/(n-1)) Σ (xᵢ - μ)²       → Dispersion autour de la moyenne   │
# │ Écart-type   : σ = √variance                    → Même unité que les données        │
# │ Étendue      : max - min                                                             │
# │ IQR          : Q3 - Q1 (intervalle interquartile)                                    │
# └──────────────────────────────────────────────────────────────────────────────────────┘


# ══════════════════════════════════════════════════════════════════════════════════════
# 2. MATRICES IMPORTANTES
# ══════════════════════════════════════════════════════════════════════════════════════

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ MATRICE DE VARIANCE-COVARIANCE (V ou Σ)                                              │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   V = (1/(n-1)) · Xᵀ_centré · X_centré                                               │
# │                                                                                      │
# │   • Matrice carrée (p × p) où p = nombre de variables                                │
# │   • DIAGONALE : variances des variables (Vᵢᵢ = Var(Xᵢ))                              │
# │   • HORS-DIAGONALE : covariances entre variables (Vᵢⱼ = Cov(Xᵢ, Xⱼ))                 │
# │   • Matrice SYMÉTRIQUE (Vᵢⱼ = Vⱼᵢ)                                                   │
# │   • Utilisée pour l'ACP non normée                                                   │
# │                                                                                      │
# │   Cov(X,Y) > 0 : X et Y varient dans le même sens                                    │
# │   Cov(X,Y) < 0 : X et Y varient en sens inverse                                      │
# │   Cov(X,Y) = 0 : pas de relation linéaire                                            │
# └──────────────────────────────────────────────────────────────────────────────────────┘

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ MATRICE DE CORRÉLATION (R)                                                           │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   Rᵢⱼ = Cov(Xᵢ, Xⱼ) / (σᵢ · σⱼ)    ou    R = D⁻¹ · V · D⁻¹                          │
# │                                          (D = diag des écarts-types)                 │
# │                                                                                      │
# │   • Covariance NORMALISÉE (sans unité)                                               │
# │   • Valeurs entre -1 et +1                                                           │
# │   • DIAGONALE = 1 (Cor(X,X) = 1)                                                     │
# │   • Matrice SYMÉTRIQUE                                                               │
# │   • Utilisée pour l'ACP normée (scale.=TRUE)                                         │
# │                                                                                      │
# │   |r| > 0.7 : corrélation forte                                                      │
# │   |r| ∈ [0.3, 0.7] : corrélation modérée                                             │
# │   |r| < 0.3 : corrélation faible                                                     │
# └──────────────────────────────────────────────────────────────────────────────────────┘


# ══════════════════════════════════════════════════════════════════════════════════════
# 3. DISTANCES
# ══════════════════════════════════════════════════════════════════════════════════════

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ DISTANCE EUCLIDIENNE                                                                 │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   d(x, y) = √[ Σᵢ (xᵢ - yᵢ)² ]                                                       │
# │                                                                                      │
# │   • Distance "à vol d'oiseau"                                                        │
# │   • SENSIBLE À L'ÉCHELLE → nécessite standardisation                                 │
# │   • Utilisée par défaut dans K-means, ACP                                            │
# │   • Cas particulier de Mahalanobis avec M = I (identité)                             │
# └──────────────────────────────────────────────────────────────────────────────────────┘

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ DISTANCE DE MANHATTAN                                                                │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   d(x, y) = Σᵢ |xᵢ - yᵢ|                                                             │
# │                                                                                      │
# │   • Distance "en taxi" (déplacements orthogonaux)                                    │
# │   • Plus robuste aux outliers que l'euclidienne                                      │
# └──────────────────────────────────────────────────────────────────────────────────────┘

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ DISTANCE DE MAHALANOBIS                                                              │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   d_M(x, y) = √[ (x-y)ᵀ · M · (x-y) ]                                                │
# │                                                                                      │
# │   • Distance GÉNÉRALISÉE avec matrice de métrique M                                  │
# │   • Si M = I (identité) → distance euclidienne                                       │
# │   • Si M = Σ⁻¹ (inverse de covariance) → tient compte des corrélations              │
# │   • Permet de normaliser par la variance : M = diag(1/σ²)                            │
# └──────────────────────────────────────────────────────────────────────────────────────┘

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ INERTIE                                                                              │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   I = (1/n) Σᵢ d²(xᵢ, g)    où g = barycentre (centre de gravité)                   │
# │                                                                                      │
# │   • Mesure de DISPERSION globale des données                                         │
# │   • Somme des distances² au centre                                                   │
# │   • En clustering : Inertie = Inertie_intra + Inertie_inter                          │
# │   • Objectif K-means : minimiser l'inertie intra-cluster                             │
# └──────────────────────────────────────────────────────────────────────────────────────┘


# ══════════════════════════════════════════════════════════════════════════════════════
# 4. ACP (ANALYSE EN COMPOSANTES PRINCIPALES)
# ══════════════════════════════════════════════════════════════════════════════════════

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ OBJECTIF DE L'ACP                                                                    │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   • RÉDUIRE LA DIMENSIONNALITÉ en conservant un maximum d'information                │
# │   • Trouver de nouveaux axes (composantes principales) NON CORRÉLÉS                  │
# │   • Visualiser des données multidimensionnelles en 2D/3D                             │
# │   • Identifier les variables les plus importantes                                    │
# │                                                                                      │
# └──────────────────────────────────────────────────────────────────────────────────────┘

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ ÉTAPES DE L'ACP                                                                      │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   1. CENTRER les données (soustraire la moyenne) [OBLIGATOIRE]                       │
# │   2. RÉDUIRE les données (diviser par écart-type) [si échelles différentes]          │
# │   3. Calculer la matrice de COVARIANCE (ou corrélation si réduit)                    │
# │   4. Calculer les VALEURS PROPRES et VECTEURS PROPRES                                │
# │   5. TRIER par valeurs propres décroissantes                                         │
# │   6. PROJETER les données sur les nouveaux axes                                      │
# │                                                                                      │
# └──────────────────────────────────────────────────────────────────────────────────────┘

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ INTERPRÉTATION DES RÉSULTATS                                                         │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   VALEURS PROPRES (λ) :                                                              │
# │   • = Variance expliquée par chaque composante principale                            │
# │   • Ordonnées de façon décroissante : λ₁ ≥ λ₂ ≥ ... ≥ λₚ                             │
# │   • Somme des λ = variance totale                                                    │
# │   • % variance expliquée = λₖ / Σλ × 100                                             │
# │                                                                                      │
# │   VECTEURS PROPRES (loadings) :                                                      │
# │   • = Directions des nouvelles composantes principales                               │
# │   • Coefficients = contribution de chaque variable originale à la CP                 │
# │   • Forment une BASE ORTHONORMÉE : vᵢᵀvⱼ = 0 si i≠j, ||vᵢ|| = 1                     │
# │                                                                                      │
# │   SCORES :                                                                           │
# │   • = Coordonnées des individus dans le nouvel espace                                │
# │   • Scores = X_centré × Vecteurs_propres                                             │
# │                                                                                      │
# └──────────────────────────────────────────────────────────────────────────────────────┘

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ COMBIEN DE COMPOSANTES GARDER ?                                                      │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   1. CRITÈRE DE KAISER : garder les CP avec λ > 1 (ACP normée)                       │
# │      → "Une CP doit expliquer plus qu'une variable standardisée"                     │
# │                                                                                      │
# │   2. CRITÈRE DU COUDE (Scree plot) : point d'inflexion                               │
# │      → Garder les CP avant le "coude" du graphique                                   │
# │                                                                                      │
# │   3. POURCENTAGE DE VARIANCE : garder jusqu'à 80-90% de variance cumulée             │
# │                                                                                      │
# └──────────────────────────────────────────────────────────────────────────────────────┘

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ QUALITÉ DE REPRÉSENTATION (cos²)                                                     │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   cos²ᵢₖ = (loadingᵢₖ)²                                                              │
# │                                                                                      │
# │   • Indique à quel point une VARIABLE est bien représentée sur un axe                │
# │   • cos² proche de 1 → bonne représentation                                          │
# │   • cos² proche de 0 → mauvaise représentation (information perdue)                  │
# │   • Pour un plan : cos²_plan = cos²_PC1 + cos²_PC2                                   │
# │                                                                                      │
# └──────────────────────────────────────────────────────────────────────────────────────┘

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ CONTRIBUTION DES INDIVIDUS                                                           │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   contribᵢₖ = (scoreᵢₖ)² / (n × λₖ) × 100                                            │
# │                                                                                      │
# │   • Indique l'influence d'un individu sur la construction d'un axe                   │
# │   • Contribution moyenne attendue = 100/n                                            │
# │   • Si contrib > 3× moyenne → individu influent (potentiel outlier)                  │
# │                                                                                      │
# └──────────────────────────────────────────────────────────────────────────────────────┘

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ BIPLOT - INTERPRÉTATION                                                              │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   • Superpose INDIVIDUS (points) et VARIABLES (flèches)                              │
# │   • Longueur de flèche = qualité de représentation                                   │
# │   • Direction de flèche = corrélation avec les axes                                  │
# │   • Flèches dans même direction = variables corrélées positivement                   │
# │   • Flèches opposées = variables corrélées négativement                              │
# │   • Individu dans direction d'une variable = valeur élevée pour cette variable       │
# │                                                                                      │
# └──────────────────────────────────────────────────────────────────────────────────────┘


# ══════════════════════════════════════════════════════════════════════════════════════
# 5. CLUSTERING (PARTITIONNEMENT)
# ══════════════════════════════════════════════════════════════════════════════════════

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ OBJECTIF DU CLUSTERING                                                               │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   • Regrouper les individus SIMILAIRES dans des clusters                             │
# │   • Maximiser l'HOMOGÉNÉITÉ intra-cluster                                            │
# │   • Maximiser l'HÉTÉROGÉNÉITÉ inter-cluster                                          │
# │   • Apprentissage NON SUPERVISÉ (pas de labels)                                      │
# │                                                                                      │
# │   ⚠️  TOUJOURS STANDARDISER les données avant clustering !                           │
# │       (sinon les variables à grande échelle dominent)                                │
# │                                                                                      │
# └──────────────────────────────────────────────────────────────────────────────────────┘

# ╔══════════════════════════════════════════════════════════════════════════════════════╗
# ║ K-MEANS                                                                              ║
# ╠══════════════════════════════════════════════════════════════════════════════════════╣
# ║                                                                                      ║
# ║   ALGORITHME :                                                                       ║
# ║   1. Initialiser k centroïdes aléatoirement                                          ║
# ║   2. Assigner chaque point au centroïde le plus proche                               ║
# ║   3. Recalculer les centroïdes (moyenne des points du cluster)                       ║
# ║   4. Répéter 2-3 jusqu'à convergence                                                 ║
# ║                                                                                      ║
# ║   OBJECTIF : Minimiser l'inertie intra-cluster W = Σₖ Σᵢ∈Cₖ ||xᵢ - μₖ||²            ║
# ║                                                                                      ║
# ║   PARAMÈTRES :                                                                       ║
# ║   • k (centers) : nombre de clusters (à spécifier)                                   ║
# ║   • nstart : nombre d'exécutions (garde le meilleur, évite optima locaux)            ║
# ║                                                                                      ║
# ║   ✅ AVANTAGES :                        ❌ INCONVÉNIENTS :                            ║
# ║   • Rapide O(nkt)                       • Nécessite de choisir k                     ║
# ║   • Simple à comprendre                 • Sensible à l'initialisation                ║
# ║   • Efficace pour clusters sphériques   • Sensible aux outliers                      ║
# ║                                         • Clusters convexes seulement                ║
# ║                                                                                      ║
# ╚══════════════════════════════════════════════════════════════════════════════════════╝

# ╔══════════════════════════════════════════════════════════════════════════════════════╗
# ║ CLUSTERING HIÉRARCHIQUE                                                              ║
# ╠══════════════════════════════════════════════════════════════════════════════════════╣
# ║                                                                                      ║
# ║   ALGORITHME (ascendant/agglomératif) :                                              ║
# ║   1. Chaque point est un cluster                                                     ║
# ║   2. Fusionner les 2 clusters les plus proches                                       ║
# ║   3. Répéter jusqu'à n'avoir qu'un seul cluster                                      ║
# ║   4. Couper le dendrogramme au niveau souhaité                                       ║
# ║                                                                                      ║
# ║   MÉTHODES DE LIAISON (linkage) :                                                    ║
# ║   ┌─────────────┬────────────────────────────────┬────────────────────────────────┐  ║
# ║   │ Méthode     │ Distance inter-clusters        │ Caractéristiques               │  ║
# ║   ├─────────────┼────────────────────────────────┼────────────────────────────────┤  ║
# ║   │ SINGLE      │ Min des distances (plus        │ Effet de CHAÎNAGE              │  ║
# ║   │             │ proches voisins)               │ Clusters allongés              │  ║
# ║   ├─────────────┼────────────────────────────────┼────────────────────────────────┤  ║
# ║   │ COMPLETE    │ Max des distances (plus        │ Clusters COMPACTS              │  ║
# ║   │             │ lointains voisins)             │ Sensible aux outliers          │  ║
# ║   ├─────────────┼────────────────────────────────┼────────────────────────────────┤  ║
# ║   │ AVERAGE     │ Moyenne des distances          │ Compromis                      │  ║
# ║   ├─────────────┼────────────────────────────────┼────────────────────────────────┤  ║
# ║   │ WARD ★      │ Minimise l'augmentation        │ Clusters ÉQUILIBRÉS            │  ║
# ║   │             │ de variance intra-cluster      │ Souvent le meilleur choix      │  ║
# ║   └─────────────┴────────────────────────────────┴────────────────────────────────┘  ║
# ║                                                                                      ║
# ║   DENDROGRAMME :                                                                     ║
# ║   • Arbre montrant les fusions successives                                           ║
# ║   • Hauteur = dissimilarité au moment de la fusion                                   ║
# ║   • Couper horizontalement pour obtenir k clusters                                   ║
# ║                                                                                      ║
# ║   ✅ AVANTAGES :                        ❌ INCONVÉNIENTS :                            ║
# ║   • Pas besoin de k au départ           • Lent O(n²) ou O(n³)                        ║
# ║   • Visualisation (dendrogramme)        • Choix de la méthode de liaison             ║
# ║   • Déterministe                        • Irréversible (pas de réassignation)        ║
# ║                                                                                      ║
# ╚══════════════════════════════════════════════════════════════════════════════════════╝

# ╔══════════════════════════════════════════════════════════════════════════════════════╗
# ║ CLUSTERING SPECTRAL                                                                  ║
# ╠══════════════════════════════════════════════════════════════════════════════════════╣
# ║                                                                                      ║
# ║   PRINCIPE :                                                                         ║
# ║   1. Construire un GRAPHE de similarité entre les points                             ║
# ║   2. Calculer la matrice LAPLACIENNE du graphe                                       ║
# ║   3. Calculer les k premiers VECTEURS PROPRES                                        ║
# ║   4. Projeter les données dans cet espace                                            ║
# ║   5. Appliquer K-means dans le nouvel espace                                         ║
# ║                                                                                      ║
# ║   INTUITION : basé sur la CONNECTIVITÉ, pas seulement la distance                    ║
# ║                                                                                      ║
# ║   ✅ AVANTAGES :                        ❌ INCONVÉNIENTS :                            ║
# ║   • Clusters NON CONVEXES               • Plus lent                                  ║
# ║   • Structures complexes                • Nécessite k                                ║
# ║   • Basé sur connectivité               • Choix du noyau/similarité                  ║
# ║                                                                                      ║
# ╚══════════════════════════════════════════════════════════════════════════════════════╝

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ ÉVALUATION DU CLUSTERING                                                             │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   AVEC labels vrais (évaluation externe) :                                           │
# │   • Matrice de confusion                                                             │
# │   • Pureté = Σₖ max_j(nₖⱼ) / n                                                       │
# │   • Adjusted Rand Index (ARI)                                                        │
# │                                                                                      │
# │   SANS labels (évaluation interne) :                                                 │
# │   • Inertie intra-cluster (à minimiser)                                              │
# │   • Silhouette (à maximiser, entre -1 et 1)                                          │
# │   • Méthode du coude pour choisir k                                                  │
# │                                                                                      │
# └──────────────────────────────────────────────────────────────────────────────────────┘


# ══════════════════════════════════════════════════════════════════════════════════════
# 6. ANALYSE DES CORRESPONDANCES
# ══════════════════════════════════════════════════════════════════════════════════════

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ CA (Correspondence Analysis) - ANALYSE FACTORIELLE DES CORRESPONDANCES               │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   OBJECTIF : Analyser la relation entre 2 VARIABLES QUALITATIVES                     │
# │                                                                                      │
# │   DONNÉES : Table de CONTINGENCE (effectifs croisés)                                 │
# │                                                                                      │
# │             Eye                                                                      │
# │   Hair     Brown  Blue  Hazel  Green                                                 │
# │   Black      68    20     15     5                                                   │
# │   Brown     119    84     54    29                                                   │
# │   Red        26    17     14    14                                                   │
# │   Blond      7     94     10    16                                                   │
# │                                                                                      │
# │   PRINCIPE :                                                                         │
# │   • Calcule les PROFILS lignes et colonnes (fréquences conditionnelles)              │
# │   • Mesure l'écart à l'INDÉPENDANCE (profil attendu si aucune association)           │
# │   • ACP pondérée sur les écarts à l'indépendance                                     │
# │                                                                                      │
# │   RÉSULTATS :                                                                        │
# │   • Valeurs propres = part d'inertie (écart à l'indépendance) par axe                │
# │   • Coordonnées des modalités lignes et colonnes                                     │
# │                                                                                      │
# └──────────────────────────────────────────────────────────────────────────────────────┘

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ MCA (Multiple Correspondence Analysis) - ACM                                         │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   OBJECTIF : Analyser PLUSIEURS variables qualitatives simultanément                 │
# │                                                                                      │
# │   DIFFÉRENCE AVEC CA :                                                               │
# │   • CA : 2 variables (table de contingence)                                          │
# │   • MCA : p variables (tableau disjonctif complet)                                   │
# │                                                                                      │
# │   TABLEAU DISJONCTIF COMPLET :                                                       │
# │   • Chaque modalité devient une colonne binaire (0/1)                                │
# │   • Exemple : Couleur (R,V,B) → 3 colonnes : R(0/1), V(0/1), B(0/1)                  │
# │                                                                                      │
# │   ANALOGIE : MCA est l'équivalent de l'ACP pour les données QUALITATIVES             │
# │                                                                                      │
# └──────────────────────────────────────────────────────────────────────────────────────┘

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │ INTERPRÉTATION DU BIPLOT CA/MCA                                                      │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   • Modalités PROCHES = souvent observées ENSEMBLE (association)                     │
# │   • Modalités ÉLOIGNÉES = rarement observées ensemble                                │
# │   • Distance à l'ORIGINE = écart au profil moyen (marginal)                          │
# │     → Proche de l'origine = modalité "moyenne", fréquente                            │
# │     → Loin de l'origine = modalité distinctive, rare                                 │
# │   • Les DIMENSIONS opposent des groupes de modalités                                 │
# │                                                                                      │
# │   Exemple CA Hair/Eye :                                                              │
# │   • Blond proche de Blue → cheveux blonds associés aux yeux bleus                    │
# │   • Black proche de Brown → cheveux noirs associés aux yeux marrons                  │
# │   • Dim 1 oppose clair (blond/blue) vs foncé (black/brown)                           │
# │                                                                                      │
# └──────────────────────────────────────────────────────────────────────────────────────┘


# ══════════════════════════════════════════════════════════════════════════════════════
# 7. RÉSUMÉ : QUAND UTILISER QUELLE MÉTHODE ?
# ══════════════════════════════════════════════════════════════════════════════════════

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │                          GUIDE DE CHOIX DES MÉTHODES                                 │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   RÉDUCTION DE DIMENSION :                                                           │
# │   ┌───────────────────────┬─────────────────────────────────────────────────────────┐│
# │   │ Type de variables     │ Méthode                                                 ││
# │   ├───────────────────────┼─────────────────────────────────────────────────────────┤│
# │   │ Quantitatives         │ ACP (prcomp)                                            ││
# │   │ Qualitatives (2 var)  │ CA (AFC)                                                ││
# │   │ Qualitatives (p var)  │ MCA (ACM)                                               ││
# │   │ Mixtes                │ FAMD (FactoMineR)                                       ││
# │   └───────────────────────┴─────────────────────────────────────────────────────────┘│
# │                                                                                      │
# │   CLUSTERING :                                                                       │
# │   ┌───────────────────────┬─────────────────────────────────────────────────────────┐│
# │   │ Situation             │ Méthode recommandée                                     ││
# │   ├───────────────────────┼─────────────────────────────────────────────────────────┤│
# │   │ Grand n, k connu      │ K-means (rapide)                                        ││
# │   │ Exploration, k ?      │ Hiérarchique (dendrogramme)                             ││
# │   │ Clusters non convexes │ Spectral                                                ││
# │   │ Outliers              │ DBSCAN                                                  ││
# │   └───────────────────────┴─────────────────────────────────────────────────────────┘│
# │                                                                                      │
# │   STANDARDISATION :                                                                  │
# │   • ACP normée (scale.=TRUE) : si échelles différentes                               │
# │   • Clustering : TOUJOURS standardiser                                               │
# │   • CA/MCA : pas de standardisation (déjà normalisé)                                 │
# │                                                                                      │
# └──────────────────────────────────────────────────────────────────────────────────────┘


# ══════════════════════════════════════════════════════════════════════════════════════
# 8. PIÈGES À ÉVITER ⚠️
# ══════════════════════════════════════════════════════════════════════════════════════

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │                              ERREURS FRÉQUENTES                                      │
# ├──────────────────────────────────────────────────────────────────────────────────────┤
# │                                                                                      │
# │   ❌ Oublier de STANDARDISER avant clustering                                        │
# │   ❌ Oublier de CENTRER pour l'ACP                                                   │
# │   ❌ Inclure des variables QUALITATIVES dans l'ACP (utiliser MCA)                    │
# │   ❌ Inclure des variables QUANTITATIVES dans CA/MCA (utiliser ACP)                  │
# │   ❌ Garder les NA dans les données                                                  │
# │   ❌ Confondre COVARIANCE et CORRÉLATION                                             │
# │   ❌ Interpréter les clusters sans vérifier leur pertinence                          │
# │   ❌ Oublier nstart dans kmeans (risque d'optimum local)                             │
# │   ❌ Confondre scores (individus) et loadings (variables) en ACP                     │
# │   ❌ Utiliser single linkage sans connaître l'effet de chaînage                      │
# │                                                                                      │
# └──────────────────────────────────────────────────────────────────────────────────────┘


# ══════════════════════════════════════════════════════════════════════════════════════
# 9. FORMULES ESSENTIELLES À RETENIR
# ══════════════════════════════════════════════════════════════════════════════════════

# ┌──────────────────────────────────────────────────────────────────────────────────────┐
# │                                                                                      │
# │   Variance :        Var(X) = (1/(n-1)) Σ(xᵢ - μ)²                                    │
# │                                                                                      │
# │   Covariance :      Cov(X,Y) = (1/(n-1)) Σ(xᵢ - μₓ)(yᵢ - μᵧ)                         │
# │                                                                                      │
# │   Corrélation :     Cor(X,Y) = Cov(X,Y) / (σₓ · σᵧ)                                  │
# │                                                                                      │
# │   Dist. Euclid. :   d(x,y) = √[Σ(xᵢ - yᵢ)²]                                          │
# │                                                                                      │
# │   Dist. Mahal. :    d_M(x,y) = √[(x-y)ᵀ M (x-y)]                                     │
# │                                                                                      │
# │   Var. expliquée :  %λₖ = λₖ / Σλ × 100                                              │
# │                                                                                      │
# │   Contribution :    contribᵢₖ = scoreᵢₖ² / (n × λₖ) × 100                            │
# │                                                                                      │
# │   Pureté :          Σₖ max_j(nₖⱼ) / n                                                │
# │                                                                                      │
# │   Matrice cov :     V = (1/(n-1)) · Xᵀ_centré · X_centré                             │
# │                                                                                      │
# └──────────────────────────────────────────────────────────────────────────────────────┘