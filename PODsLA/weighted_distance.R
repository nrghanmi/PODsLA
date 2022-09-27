xpand <- function(d) do.call("expand.grid", rep(list(1:nrow(d)), 2))
euc_norm <- function(x) sqrt(sum(x^2))
euc_dist <- function(mat, weights) {
  iter <- xpand(mat)
  vec <- mapply(function(i,j) euc_norm(weights[i]*(mat[i,] - mat[j,])), 
                iter[,1], iter[,2])
  matrix(vec,nrow(mat), nrow(mat))
}