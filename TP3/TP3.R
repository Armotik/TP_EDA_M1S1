# Ex1
# Load dataset
X <- penguins

# Clean the dataset by removing rows with missing values
X <- na.omit(X)

# create a new dataset containing only the quantitative variables
X_quant <- X[, sapply(X, is.numeric)]

# Why is it important to remove missing values before performing clustering?
cat("\nWhy is it important to remove missing values before performing clustering?")
cat("\nRemoving missing values is crucial before performing clustering because missing data can distort the distance calculations between data points, leading to inaccurate clustering results. Clustering algorithms typically rely on complete data to compute similarities or dissimilarities, and the presence of missing values can introduce bias or lead to the exclusion of entire observations, which may affect the overall structure and interpretation of the clusters formed.")

# Standardize the quantitative variables using the scale function
Z <- scale(X_quant)
colnames(Z) <- colnames(X_quant)

# Why is standardization important in clustering?
cat("\n\nWhy is standardization important in clustering?")
cat("\nStandardization is important in clustering because it ensures that all variables contribute equally to the distance calculations. Without standardization, variables with larger scales can dominate the distance metrics, leading to biased clustering results. By standardizing the data, we transform the variables to have a mean of zero and a standard deviation of one, allowing for a fair comparison between variables and improving the accuracy of the clustering process.")

# Perform K-means clustering on the standardized data using the kmeans function with centers = 3 and nstart = 25
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(Z, centers = 3, nstart = 25)

# Explain the purpose of the nstart parameter
cat("\n\nExplain the purpose of the nstart parameter in the kmeans function.")
cat("\nThe nstart parameter in the kmeans function specifies the number of random initial configurations to be used when running the K-means algorithm. Since K-means can converge to different local minima depending on the initial placement of centroids, using multiple random starts helps to increase the likelihood of finding a better overall clustering solution. By setting nstart to a higher value, the algorithm runs multiple times with different initial centroids and selects the best result based on the lowest within-cluster sum of squares.")

# Visualize the clustering results using a scatter plot of two selected quantitative variables, coloring the points by their assigned cluster. Add the cluster centers to the plot
plot(Z[, 1], Z[, 2], col = kmeans_result$cluster, pch = 19, xlab = colnames(Z)[1], ylab = colnames(Z)[2], main = "K-means Clustering Results")
points(kmeans_result$centers[, 1], kmeans_result$centers[, 2], col = 1:3, pch = 8, cex = 2)
legend("topright", legend = paste("Cluster", 1:3), col = 1:3, pch = 19)

# What do you observe about the clusters? You may change the variables used for the axes to explore different perspectives
cat("\n\nWhat do you observe about the clusters?")
cat("\nThe clusters appear to be well-separated in the scatter plot, indicating that the K-means algorithm has effectively grouped the data points based on the selected quantitative variables. Each cluster represents a distinct group of penguins with similar characteristics. By changing the variables used for the axes, we can explore different perspectives and potentially uncover additional insights about the relationships between the variables and how they contribute to the clustering structure.")

# Compare the clustering results with the actual species labels. Create a confusion matrix to evaluate the clustering performance. What insights can you draw from this comparison?
confusion_matrix <- table(Cluster = kmeans_result$cluster, Species = X$species)
cat("\n\n")
print(confusion_matrix)

cat("\nWhat insights can you draw from this comparison?")
cat("\nThe confusion matrix provides insights into how well the K-means clustering aligns with the actual species labels. By examining the matrix, we can see which clusters correspond to which species and identify any misclassifications. If certain clusters predominantly contain one species, it suggests that the clustering algorithm has successfully captured the underlying structure of the data. However, if there are significant overlaps or misclassifications, it may indicate that the chosen quantitative variables are not sufficient to distinguish between species or that the clustering algorithm needs further tuning.")

# Ex2
# Using the same cleaned and standardized dataset from the previous exercise, perform hierarchical clustering using the hclust function with three different linkage methods
# Calculate the distance matrix
dist_matrix <- dist(Z)
# Perform hierarchical clustering with different linkage methods
hc_complete <- hclust(dist_matrix, method = "complete")
hc_average <- hclust(dist_matrix, method = "average")
hc_single <- hclust(dist_matrix, method = "single")

# Plot the dendrograms for each linkage method
par(mfrow = c(1, 3))  # Set up the plotting area
plot(hc_complete, main = "Hierarchical Clustering - Complete Linkage", xlab = "", sub = "")
plot(hc_average, main = "Hierarchical Clustering - Average Linkage", xlab = "", sub = "")
plot(hc_single, main = "Hierarchical Clustering - Single Linkage", xlab = "", sub = "")
par(mfrow = c(1, 1))  # Reset plotting area

# How do you interpret the dendrograms?
cat("\n\nHow do you interpret the dendrograms?")
cat("\nDendrograms visually represent the hierarchical relationships between data points based on their similarities. The height at which two clusters are merged indicates the distance or dissimilarity between them. In the dendrograms, we can observe how clusters are formed at different levels of similarity. By cutting the dendrogram at a specific height, we can determine the number of clusters present in the data. Different linkage methods affect the shape and structure of the dendrograms, influencing how clusters are formed. For example, complete linkage tends to create compact clusters, while single linkage can result in elongated clusters.")

# Cut the dendrograms to form three clusters using the cutree function
clusters_complete <- cutree(hc_complete, k = 3)
clusters_average <- cutree(hc_average, k = 3)
clusters_single <- cutree(hc_single, k = 3)

# Compare the clusters obtained from different linkage methods. Which method seems to produce the most meaningful clusters? Justify your answer
cat("\n\nCompare the clusters obtained from different linkage methods. Which method seems to produce the most meaningful clusters? Justify your answer.")
cat("\nTo compare the clusters obtained from different linkage methods, we can examine the composition of each cluster and how well they align with the actual species labels. The complete linkage method often produces more compact and well-defined clusters, which may lead to clearer distinctions between species. In contrast, the single linkage method can result in chaining effects, where clusters are formed based on single points, leading to less meaningful groupings. The average linkage method provides a balance between the two, but may still not capture the true structure of the data as effectively as complete linkage. Overall, the complete linkage method seems to produce the most meaningful clusters due to its ability to create distinct and cohesive groups that better reflect the underlying species differences.")

# Perform spectral clustering using the specc function from the kernlab package with centers = 3
library(kernlab)
spectral_result <- specc(Z, centers = 3)

# Compare the spectral clustering results with those from K-means and hierarchical clustering. What differences do you observe?
cat("\n\nCompare the spectral clustering results with those from K-means and hierarchical clustering. What differences do you observe?")
cat("\nSpectral clustering often captures complex structures in the data that may not be easily identified by K-means or hierarchical clustering. One key difference is that spectral clustering can handle non-linear relationships between data points, while K-means relies on Euclidean distances and hierarchical clustering depends on linkage criteria. As a result, spectral clustering may produce clusters that better reflect the intrinsic geometry of the data. Additionally, the clusters formed by spectral clustering may differ in shape and size compared to those from K-means and hierarchical methods. Overall, spectral clustering can provide a more nuanced understanding of the data's structure, especially in cases where the data is not well-separated or exhibits complex patterns.")

# Visualize the spectral clustering results using a scatter plot of two selected quantitative variables, coloring the points by their assigned cluster
plot(Z[, 1], Z[, 2], col = spectral_result@.Data, pch = 19, xlab = colnames(Z)[1], ylab = colnames(Z)[2], main = "Spectral Clustering Results")
legend("topright", legend = paste("Cluster", 1:3), col = 1:3, pch = 19)

# How do the clusters compare to those obtained from K-means and hierarchical clustering?"
cat("\n\nHow do the clusters compare to those obtained from K-means and hierarchical clustering?")
cat("\nThe clusters obtained from spectral clustering may differ in terms of shape, size, and composition compared to those from K-means and hierarchical clustering. Spectral clustering can identify clusters that are not necessarily spherical or evenly sized, which is a limitation of K-means. Additionally, the boundaries between clusters in spectral clustering may be more complex, reflecting the underlying structure of the data more accurately. In contrast, K-means tends to create more uniform clusters based on distance metrics, while hierarchical clustering can produce varying cluster shapes depending on the linkage method used. Overall, spectral clustering may provide a more flexible and accurate representation of the data's clustering structure.")

# Discuss the advantages and disadvantages of K-means, hierarchical, and spectral clustering methods based on your findings.
cat("\n\nDiscuss the advantages and disadvantages of K-means, hierarchical, and spectral clustering methods based on your findings.")
cat("\nK-means Clustering:")
cat("\nAdvantages: Simple and efficient for large datasets; easy to implement and interpret; works well with spherical clusters.")
cat("\nDisadvantages: Sensitive to initial centroid placement; assumes clusters are spherical and equally sized; struggles with non-linear relationships.")

cat("\n\nHierarchical Clustering:")
cat("\nAdvantages: Does not require a predefined number of clusters; provides a dendrogram for visualizing cluster relationships; can capture nested clusters.")
cat("\nDisadvantages: Computationally intensive for large datasets; sensitive to linkage method choice; can produce chaining effects with single linkage.")

cat("\n\nSpectral Clustering:")
cat("\nAdvantages: Can capture complex, non-linear relationships; flexible in handling different cluster shapes; effective for data with manifold structures.")
cat("\nDisadvantages: Requires selection of parameters (e.g., number of clusters, similarity measure); computationally intensive; less interpretable than K-means.")

