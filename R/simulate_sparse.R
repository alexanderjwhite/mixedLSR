#' Simulate Heterogeneous, Low-Rank Data with Varying Sparsity
#'
#' @param k The number of groups, default = 2.
#' @param dense The density ratio (must be greater than 0).
#'
#' @return A list of simulation values, including x matrix, y matrix, coefficients and true clustering assignments.
#' @export
#'
#' @examples
#' simulate_lsr()
simulate_sparse <- function(k = 2, dense = 0.1){

  case <- "independent"
  h <- 0.2
  b <- 1
  N <- 100
  p <- 200
  m <- 250
  d <- 20

  rank <- 1
  prob <- rep(1/k,k)
  int <- cumsum(prob)
  rand_assign <- stats::runif(N)

  clust_assign_true <- sapply(rand_assign, function(x){min(which(x <= int))})
  n <- sapply(seq(1,k), function(x){sum(clust_assign_true==x)})

  x <- matrix(0, nrow = N, ncol = p)
  y <- matrix(0, nrow = N, ncol = m)
  a <- NULL
  comp <- rep(1:2, length.out = k)

  np_0 <- ceiling((1-dense)*p)
  np_d <- p-np_0
  nm_0 <- ceiling((1-dense)*m)
  nm_d <- m-nm_0

  for(i in seq(1, k)){

    u1 <- c(sample(c(1,-1), np_d, replace = TRUE)*runif(np_d,h,1),rep(0,np_0))
    u1 <- u1/norm(u1, type = "2")
    u2 <- c(rep(0,np_0),sample(c(1,-1), np_d, replace = TRUE)*runif(np_d,h,1))
    u2 <- u2/norm(u2, type = "2")
    u <- matrix(cbind(u1,u2)[,comp[i]],ncol = rank)

    v1 <- c(sample(c(1,-1), nm_d, replace = TRUE),rep(0,nm_0))
    v1 <- v1/norm(v1, type = "2")
    v2 <- c(rep(0,nm_0),sample(c(1,-1), nm_d, replace = TRUE))
    v2 <- v2/norm(v2, type = "2")
    v <- matrix(cbind(v1,v2)[,comp[i]],ncol = rank)

    u_shuffle <- sample(1:p, p)
    v_shuffle <- sample(1:m, m)

    u <- matrix(u[u_shuffle,], ncol = rank)
    v <- matrix(v[v_shuffle,], ncol = rank)

    C <- u%*%d%*%t(v)

    if(case=="independent"){rho = 0} else {rho = sample(seq(0.125,0.625,0.125),1)}

    Sigma <- matrix(1,p,p)
    for(j in 1:p) for (k in 1:p) Sigma[j,k] <- rho^abs(j-k)


    X <- MASS::mvrnorm(n[i],rep(0,p),Sigma)
    E <- matrix(stats::rnorm(n[i]*m, sd = sum(diag(t(C)%*%Sigma%*%C))/(n[i]*m*b)),n[i],m)
    Y <- X%*%C + E

    a <- c(a,list(C))
    x[clust_assign_true==i,] <- X
    y[clust_assign_true==i,] <- Y
  }
  list(x = x, y = y, a = a, true = clust_assign_true)

}
