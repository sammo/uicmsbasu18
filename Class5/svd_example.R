rm(list=ls())

# Load Data file ####
x = read.csv(file = "DataFrame.csv",
                      header = TRUE, quote = '"')

# Variance threshold
varThresh = NULL

# Number of clusters to group data points by
nClusters = 3

# Keep numeric variables
idxKeep = which(sapply(x, class) %in% c("integer", "numeric"))
x = x[, idxKeep]

# Scale and center
xc = scale(x = x, center = T, scale = F)

# Decompose singular values
xSVD = svd(xc)
xSVD$cumperc = cumsum(xSVD$d)/sum(xSVD$d)

if (is.null(varThresh)) {
  varThresh = 1
}

newDim = min(which(xSVD$cumperc >= varThresh))

if (is.null(nClusters)) {
  eigencolor = "orange red"
  propcolor = "dark cyan"
  par(mar = c(5, 4, 4, 4), cex.axis = 0.8)
  plot(x = 1:length(xSVD$d), y = xSVD$d, main = "Scree Plot (SVD)", 
       pch = 21, cex = 0.8, col = eigencolor, axes = F, 
       xlab = NA, ylab = NA, type = "b", lty = 5)
  mtext(side = 1, text = "Principal Component", padj = 4, 
        cex = 0.9)
  mtext(side = 2, text = "Eigenvalue", padj = -4, cex = 0.9, 
        col = eigencolor)
  axis(side = 2, col = eigencolor, col.axis = eigencolor)
  box()
  par(new = T)
  plot(x = 1:length(xSVD$d), y = xSVD$cumperc, col = propcolor, 
       cex = 0.8, axes = F, xlab = NA, ylab = NA, pch = 23, 
       type = "b", lty = 5)
  axis(side = 1)
  axis(side = 4, col = propcolor, col.axis = propcolor, 
       cex.lab = 0.8)
  mtext(side = 4, text = "Proportion", padj = 4, cex = 0.9, 
        col = propcolor)
  abline(v = newDim)
  mtext(newDim, side = 1, at = newDim, padj = 0.5, cex = 0.8, 
        col = eigencolor)
}
if (!is.null(nClusters)) {
  colorVector = rainbow(n = nClusters, s = 0.5)
  # Calculate the scores
  xc.N = xc %*% xSVD$v

  # xc.N = xSVD$u %*% xSVD$sigma
  clusters = kmeans(x = xc.N, centers = nClusters)
  plot(x = xc.N[, 1], y = xc.N[, 2], pch = 21, 
       bg = colorVector[clusters$cluster], 
       xlab = "SVD Component 1", ylab = "SVD Component 2", 
       cex.lab = 0.8, main = sprintf("Clustering via SVD/Kmeans\n(%i clusters)", 
                                     nClusters))
}

result = t(xSVD$v)[, 1:newDim]

# Compare to prcomp
pc = prcomp(x, center = T, scale. = F)
pc$rotation #loadings
pc$x #scores

xSVD$sigma = matrix(0, nrow = length(xSVD$d), length(xSVD$d))
diag(xSVD$sigma) = xSVD$d

xSVD$scores = xSVD$u %*% xSVD$sigma

# Check if PCA scores are equal to U*sigma
all(round(xSVD$scores, digits = 5) == round(pc$x, digits = 5))

# Check if loadings equal right singular vectors
all(round(xSVD$v, digits = 5) == round(pc$rotation, digits = 5))

